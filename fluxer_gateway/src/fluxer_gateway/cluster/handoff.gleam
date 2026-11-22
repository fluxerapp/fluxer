// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/membership.{type Move}
import fluxer_gateway/cluster/membership/state
import fluxer_gateway/cluster/metrics
import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node, type Role}
import fluxer_gateway/gateway/socket
import fluxer_gateway/guild/manager as guild_manager
import fluxer_gateway/session/manager/transfer as session_transfer
import fluxer_gateway/util/spawn
import fluxer_gateway/voice/call/manager as call_manager
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/set.{type Set}
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.cluster().handoff_init_timeout_ms
}

fn debounce_ms() -> Int {
  durations.cluster().handoff_debounce_ms
}

fn worker_watch_ms() -> Int {
  durations.cluster().handoff_worker_watch_ms
}

fn call_timeout_ms() -> Int {
  durations.cluster().handoff_call_timeout_ms
}

fn resubscribe_ms() -> Int {
  durations.cluster().handoff_resubscribe_ms
}

fn reconcile_ms() -> Int {
  durations.cluster().handoff_reconcile_ms
}

pub type Outcome {
  Outcome(guilds: Int, sessions: Int, calls: Int)
}

pub type Exporter =
  fn(Role, List(Node)) -> Outcome

pub opaque type Message {
  Moved(Move)
  Debounced(token: Int)
  WorkerDone(pid: Pid, outcome: Outcome)
  Down(process.Down)
  WorkerOverdue(token: Int)
  Subscribe
  Reconcile
  Drain
  Undrain(reply: Subject(Bool))
  AwaitDrained(reply: Subject(Nil))
  Settled(reply: Subject(Bool))
}

type Worker {
  Worker(
    pid: Pid,
    monitor: Monitor,
    token: Int,
    targets: List(Node),
    version: Int,
    started_ms: Int,
  )
}

type Snapshot {
  Snapshot(members: Set(Node), serving: Set(Node))
}

type State {
  State(
    self: Subject(Message),
    moves: Subject(Move),
    membership: Option(Pid),
    known: Snapshot,
    role: Role,
    export: Exporter,
    pending: Option(List(Node)),
    debounce: Int,
    worker: Option(Worker),
    next_token: Int,
    rerun: Bool,
    drained: Bool,
    waiters: List(Subject(Nil)),
  )
}

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_cluster_handoff")
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervised_with(settings, run)
}

pub fn supervised_with(
  settings: Settings,
  export: Exporter,
) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings, export) })
}

fn start(
  settings: Settings,
  export: Exporter,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    let moves = process.new_subject()
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_map(moves, Moved)
      |> process.select_monitors(Down)
    process.send_after(self, debounce_ms(), Reconcile)
    State(
      self:,
      moves:,
      membership: None,
      known: snapshot(),
      role: settings.role,
      export:,
      pending: None,
      debounce: 0,
      worker: None,
      next_token: 1,
      rerun: False,
      drained: False,
      waiters: [],
    )
    |> subscribe
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

pub fn holds_sockets() -> Bool {
  case settings.get().role {
    node.Websocket | node.WebsocketSessions | node.AllRoles -> True
    _ -> False
  }
}

pub fn reconnect_sockets() -> Nil {
  case holds_sockets() {
    True -> {
      let count = socket.reconnect_all()
      logging.log(
        logging.Info,
        "Gateway drain: broadcast session reconnect to "
          <> int.to_string(count)
          <> " websocket connections",
      )
    }
    False -> Nil
  }
}

pub fn drain_dispatch() -> Nil {
  case membership.announce_drain() {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      let _ = remote.send_local(name(), Drain)
      Nil
    }
  }
}

pub fn redrain() -> Nil {
  let _ = remote.send_local(name(), Drain)
  Nil
}

pub fn await_drained(timeout_ms: Int) -> Bool {
  let reply = process.new_subject()
  case remote.send_local(name(), AwaitDrained(reply)) {
    Error(Nil) -> True
    Ok(Nil) -> process.receive(reply, timeout_ms) == Ok(Nil)
  }
}

pub fn settled(timeout_ms: Int) -> Bool {
  let reply = process.new_subject()
  case remote.send_local(name(), Settled(reply)) {
    Error(Nil) -> False
    Ok(Nil) -> process.receive(reply, timeout_ms) == Ok(True)
  }
}

pub fn undrain() -> Result(Bool, Nil) {
  let reply = process.new_subject()
  case remote.send_local(name(), Undrain(reply)) {
    Error(Nil) -> {
      clear_draining()
      Ok(True)
    }
    Ok(Nil) -> process.receive(reply, call_timeout_ms())
  }
}

fn clear_draining() -> Nil {
  draining.clear_draining()
  let _ = membership.announce_undrain()
  Nil
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  actor.continue(settle(step(state, message)))
}

fn settle(state: State) -> State {
  case state.drained, state.worker, state.pending, state.rerun {
    True, None, None, False -> {
      list.each(state.waiters, process.send(_, Nil))
      State(..state, waiters: [])
    }
    _, _, _, _ -> state
  }
}

fn step(state: State, message: Message) -> State {
  case message {
    Moved(membership.MoveOut(targets, state.MovedDraining)) -> {
      metrics.record_moves(
        membership.move_reason(state.MovedDraining),
        list.length(targets),
      )
      let pending = Some(merge(state.pending, targets))
      maybe_start(State(..state, pending:, drained: True))
    }
    Moved(membership.MoveOut(targets, reason)) -> {
      metrics.record_moves(membership.move_reason(reason), list.length(targets))
      schedule(state, merge(state.pending, targets))
    }
    Moved(membership.MoveIn(from, reason)) -> {
      logging.log(
        logging.Info,
        "Gateway cluster takes over keys of "
          <> node.to_string(from)
          <> " ("
          <> membership.move_reason(reason)
          <> ")",
      )
      state
    }
    Debounced(token) if token == state.debounce -> maybe_start(state)
    Debounced(_) -> state
    Drain -> {
      let targets =
        router.members()
        |> list.filter(fn(peer) { peer != node.self() })
      let rerun = option.is_some(state.worker)
      maybe_start(
        State(
          ..state,
          pending: Some(merge(state.pending, targets)),
          rerun:,
          drained: True,
        ),
      )
    }
    AwaitDrained(reply) -> State(..state, waiters: [reply, ..state.waiters])
    Settled(reply) -> {
      process.send(
        reply,
        state.known == snapshot()
          && state.pending == None
          && state.worker == None,
      )
      state
    }
    Undrain(reply) -> {
      case state.worker {
        None -> {
          clear_draining()
          process.send(reply, True)
          list.each(state.waiters, process.send(_, Nil))
          State(..state, drained: False, rerun: False, waiters: [])
        }
        Some(_) -> {
          process.send(reply, False)
          state
        }
      }
    }
    WorkerDone(pid, outcome) ->
      case state.worker {
        Some(worker) if worker.pid == pid -> {
          process.demonitor_process(worker.monitor)
          logging.log(
            logging.Info,
            "Gateway cluster handoff complete: guilds="
              <> int.to_string(outcome.guilds)
              <> " sessions="
              <> int.to_string(outcome.sessions)
              <> " calls="
              <> int.to_string(outcome.calls),
          )
          after_worker(State(..state, worker: None), worker)
        }
        _ -> state
      }
    Subscribe -> subscribe(state)
    Reconcile -> {
      process.send_after(state.self, reconcile_ms(), Reconcile)
      reconcile(state)
    }
    Down(process.ProcessDown(pid:, reason:, ..)) ->
      case state.membership == Some(pid), state.worker {
        True, _ -> {
          logging.log(
            logging.Warning,
            "Gateway cluster handoff lost its membership subscription, resubscribing",
          )
          subscribe(State(..state, membership: None))
        }
        False, Some(worker) if worker.pid == pid -> {
          logging.log(
            logging.Warning,
            "Gateway cluster handoff worker failed: " <> string.inspect(reason),
          )
          retry(State(..state, worker: None), worker.targets)
        }
        False, _ -> state
      }
    Down(process.PortDown(..)) -> state
    WorkerOverdue(token) ->
      case state.worker {
        Some(worker) if worker.token == token -> {
          logging.log(
            logging.Warning,
            "Gateway cluster handoff worker still running after "
              <> int.to_string(clock.monotonic_ms() - worker.started_ms)
              <> " ms",
          )
          process.send_after(
            state.self,
            worker_watch_ms(),
            WorkerOverdue(token),
          )
          state
        }
        _ -> state
      }
  }
}

fn subscribe(state: State) -> State {
  case state.membership {
    Some(_) -> state
    None ->
      case membership.subscribe_moves(state.moves) {
        Ok(pid) -> {
          let _ = process.monitor(pid)
          State(..state, membership: Some(pid))
        }
        Error(Nil) -> {
          process.send_after(state.self, resubscribe_ms(), Subscribe)
          state
        }
      }
  }
}

fn snapshot() -> Snapshot {
  Snapshot(
    members: set.from_list(router.members()),
    serving: set.from_list(router.serving()),
  )
}

fn reconcile(state: State) -> State {
  let current = snapshot()
  case current == state.known {
    True -> state
    False -> {
      let added =
        set.union(
          set.difference(current.members, state.known.members),
          set.difference(current.serving, state.known.serving),
        )
        |> set.delete(node.self())
        |> set.to_list
      let state = State(..state, known: current)
      case added {
        [] -> state
        targets -> schedule(state, merge(state.pending, targets))
      }
    }
  }
}

fn merge(pending: Option(List(Node)), targets: List(Node)) -> List(Node) {
  case pending {
    Some(existing) -> list.unique(list.append(existing, targets))
    None -> list.unique(targets)
  }
}

fn schedule(state: State, targets: List(Node)) -> State {
  let token = state.next_token
  process.send_after(state.self, debounce_ms(), Debounced(token))
  State(..state, pending: Some(targets), debounce: token, next_token: token + 1)
}

fn retry(state: State, targets: List(Node)) -> State {
  schedule(state, merge(state.pending, targets))
}

fn after_worker(state: State, finished: Worker) -> State {
  case state.rerun, state.pending {
    True, Some(_) -> maybe_start(State(..state, rerun: False))
    True, None -> State(..state, rerun: False)
    False, _ -> after_moves(state, finished)
  }
}

fn after_moves(state: State, finished: Worker) -> State {
  case state.pending {
    Some(targets) ->
      case covered(finished, targets) {
        True -> State(..state, pending: None)
        False -> schedule(state, targets)
      }
    None -> state
  }
}

fn covered(finished: Worker, targets: List(Node)) -> Bool {
  router.version() == finished.version
  && list.all(targets, list.contains(finished.targets, _))
}

fn maybe_start(state: State) -> State {
  case state.worker, state.pending {
    Some(_), _ -> state
    None, None -> state
    None, Some(targets) -> {
      let token = state.next_token
      let parent = state.self
      let role = state.role
      let export = state.export
      let #(pid, monitor) =
        spawn.monitored(fn() {
          let outcome = export(role, targets)
          process.send(parent, WorkerDone(process.self(), outcome))
        })
      process.send_after(state.self, worker_watch_ms(), WorkerOverdue(token))
      State(
        ..state,
        known: snapshot(),
        pending: None,
        worker: Some(Worker(
          pid:,
          monitor:,
          token:,
          targets:,
          version: router.version(),
          started_ms: clock.monotonic_ms(),
        )),
        next_token: token + 1,
      )
    }
  }
}

fn run(role: Role, targets: List(Node)) -> Outcome {
  logging.log(
    logging.Info,
    "Gateway cluster handoff start: "
      <> string.join(list.map(targets, node.to_string), ","),
  )
  let calls = case node.runs(role, node.Calls) {
    True -> call_manager.export_for(role_targets(node.Calls, targets))
    False -> 0
  }
  let guilds = case node.runs(role, node.Guilds) {
    True -> guild_manager.export_for(role_targets(node.Guilds, targets))
    False -> 0
  }
  let sessions = case node.runs(role, node.Sessions) {
    True -> session_transfer.export_for(role_targets(node.Sessions, targets))
    False -> 0
  }
  Outcome(guilds:, sessions:, calls:)
}

fn role_targets(role: Role, targets: List(Node)) -> List(Node) {
  let role_nodes =
    router.nodes_with(role)
    |> list.filter(fn(peer) { peer != node.self() })
  case list.filter(role_nodes, fn(peer) { list.contains(targets, peer) }) {
    [] -> role_nodes
    found -> found
  }
}
