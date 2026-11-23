// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain_report
import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router as cluster_router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/core/session
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/loads
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import fluxer_gateway/session/seed.{type Seed}
import fluxer_gateway/session/session_metrics
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.session().transfer_init_timeout_ms
}

pub type Message {
  Receive(seed: Seed, nonce: Int, reply: Subject(Bool))
  Commit(
    session_id: String,
    nonce: Int,
    reply: Subject(Result(session.SessionRef, Nil)),
  )
  Discard(session_id: String, nonce: Int)
  Abort(session_id: String, nonce: Int, reply: Subject(Bool))
  Locate(session_id: String, reply: Subject(Result(session.SessionRef, Nil)))
  Cleanup
}

pub opaque type Pushed {
  Pushed(target: Node, session_id: String, nonce: Int, deadline: clock.Deadline)
}

pub type Committed {
  Adopted(session.SessionRef)
  Refused
  Unconfirmed
}

type Entry {
  Entry(seed: Seed, nonce: Int, stored_at: Int)
}

type Store {
  Store(entries: Dict(String, Entry), self: Subject(Message))
}

fn ttl_ms() -> Int {
  durations.session().transfer_ttl_ms
}

fn cleanup_interval_ms() -> Int {
  durations.session().transfer_cleanup_interval_ms
}

fn hand_off_timeout_ms() -> Int {
  durations.session().transfer_hand_off_timeout_ms
}

fn push_timeout_ms() -> Int {
  durations.session().transfer_push_timeout_ms
}

fn locate_timeout_ms() -> Int {
  durations.session().transfer_locate_timeout_ms
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> actor.StartResult(Nil) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    process.send_after(self, cleanup_interval_ms(), Cleanup)
    actor.initialised(Store(entries: dict.new(), self:))
    |> actor.selecting(
      process.new_selector()
      |> process.select(named)
      |> process.select(self),
    )
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(names.transfer())
  |> actor.start
}

fn handle(store: Store, message: Message) -> actor.Next(Store, Message) {
  case message {
    Receive(arrived, nonce, reply) -> {
      process.send(reply, True)
      actor.continue(put(store, seed.from_wire(arrived), nonce))
    }
    Commit(key, nonce, reply) ->
      case dict.get(store.entries, key) {
        Ok(entry) if entry.nonce == nonce -> {
          adopt(key, entry.seed, reply)
          actor.continue(
            Store(..store, entries: dict.delete(store.entries, key)),
          )
        }
        _ -> {
          process.send(reply, Error(Nil))
          actor.continue(store)
        }
      }
    Discard(key, nonce) ->
      case dict.get(store.entries, key) {
        Ok(entry) if entry.nonce == nonce ->
          actor.continue(
            Store(..store, entries: dict.delete(store.entries, key)),
          )
        _ -> actor.continue(store)
      }
    Abort(key, nonce, reply) ->
      case dict.get(store.entries, key) {
        Ok(entry) if entry.nonce == nonce -> {
          process.send(reply, True)
          actor.continue(
            Store(..store, entries: dict.delete(store.entries, key)),
          )
        }
        _ -> {
          process.send(reply, False)
          actor.continue(store)
        }
      }
    Locate(key, reply) -> {
      process.send(reply, registry.lookup(key))
      actor.continue(store)
    }
    Cleanup -> {
      let now = clock.monotonic_ms()
      process.send_after(store.self, cleanup_interval_ms(), Cleanup)
      actor.continue(
        Store(
          ..store,
          entries: dict.filter(store.entries, fn(_, entry) {
            now - entry.stored_at < ttl_ms()
          }),
        ),
      )
    }
  }
}

fn put(store: Store, seed: Seed, nonce: Int) -> Store {
  let key = session.session_id_to_string(seed.id)
  let entry = Entry(seed:, nonce:, stored_at: clock.monotonic_ms())
  Store(..store, entries: dict.insert(store.entries, key, entry))
}

fn adopt(
  key: String,
  seed: Seed,
  reply: Subject(Result(session.SessionRef, Nil)),
) -> Nil {
  case shards.whereis(key) {
    Ok(shard) -> {
      process.spawn_unlinked(fn() { relay_adoption(shard, key, seed, reply) })
      Nil
    }
    Error(Nil) -> adopt_failed(key, reply)
  }
}

fn relay_adoption(
  shard: Pid,
  key: String,
  seed: Seed,
  reply: Subject(Result(session.SessionRef, Nil)),
) -> Nil {
  let monitor = process.monitor(shard)
  let answer = process.new_subject()
  shards.send_to(shard, key, shards.Adopt(seed, answer))
  let outcome =
    process.new_selector()
    |> process.select_map(answer, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(push_timeout_ms())
  case outcome {
    Ok(Ok(adopted)) -> process.send(reply, adopted)
    Ok(Error(Nil)) -> adopt_failed(key, reply)
    Error(Nil) -> Nil
  }
}

fn adopt_failed(
  key: String,
  reply: Subject(Result(session.SessionRef, Nil)),
) -> Nil {
  logging.log(
    logging.Warning,
    "session_transfer_adopt_failed: session_id=" <> key,
  )
  process.send(reply, Error(Nil))
}

pub fn locate_elsewhere(session_id: String) -> Result(session.SessionRef, Nil) {
  let self = node.self()
  cluster_router.reachable_with(node.Sessions)
  |> list.filter(fn(peer) { peer != self })
  |> locate_on(session_id, locate_timeout_ms())
}

pub fn locate_live(session_id: String) -> Result(session.SessionRef, Nil) {
  cluster_router.reachable_with(node.Sessions)
  |> locate_on(session_id, locate_timeout_ms())
}

pub fn locate_on(
  peers: List(Node),
  session_id: String,
  timeout_ms: Int,
) -> Result(session.SessionRef, Nil) {
  let reply = process.new_subject()
  let asked =
    list.count(peers, fn(peer) {
      remote.send_named(peer, names.transfer(), Locate(session_id, reply))
      |> result.is_ok
    })
  await_located(reply, asked, clock.deadline_in(timeout_ms))
}

fn await_located(
  reply: Subject(Result(session.SessionRef, Nil)),
  waiting: Int,
  deadline: clock.Deadline,
) -> Result(session.SessionRef, Nil) {
  case waiting > 0 {
    False -> Error(Nil)
    True ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Ok(Ok(found)) -> Ok(found)
        Ok(Error(Nil)) -> await_located(reply, waiting - 1, deadline)
        Error(Nil) -> Error(Nil)
      }
  }
}

pub fn push(
  target: Node,
  outgoing: Seed,
  speaks_moves: Bool,
) -> Result(Pushed, Nil) {
  let key = session.session_id_to_string(outgoing.id)
  let nonce = unique_integer()
  let deadline = clock.deadline_in(push_timeout_ms())
  let reply = process.new_subject()
  case
    remote.send_named(
      target,
      names.transfer(),
      Receive(seed.for_peer(outgoing, speaks_moves), nonce, reply),
    )
  {
    Error(_) -> Error(Nil)
    Ok(Nil) ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Ok(True) -> Ok(Pushed(target:, session_id: key, nonce:, deadline:))
        _ -> {
          let _ =
            remote.send_named(target, names.transfer(), Discard(key, nonce))
          Error(Nil)
        }
      }
  }
}

pub fn commit(pushed: Pushed) -> Committed {
  let Pushed(target:, session_id:, nonce:, deadline:) = pushed
  let reply = process.new_subject()
  case
    remote.send_named(
      target,
      names.transfer(),
      Commit(session_id, nonce, reply),
    )
  {
    Error(_) -> Refused
    Ok(Nil) ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Ok(Ok(ref)) -> Adopted(ref)
        Ok(Error(Nil)) -> Refused
        Error(Nil) -> abort(target, session_id, nonce)
      }
  }
}

fn abort(target: Node, session_id: String, nonce: Int) -> Committed {
  let reply = process.new_subject()
  case
    remote.send_named(target, names.transfer(), Abort(session_id, nonce, reply))
  {
    Error(_) -> Unconfirmed
    Ok(Nil) ->
      case process.receive(reply, locate_timeout_ms()) {
        Ok(True) -> Refused
        Ok(False) | Error(Nil) -> Unconfirmed
      }
  }
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

const spread_lanes = 8

const load_timeout_ms = 2000

pub fn export_spread(targets: List(Node)) -> Int {
  let placed = loads.balance(registry.all(), loads.of(targets, load_timeout_ms))
  case placed {
    [] -> 0
    _ ->
      placed
      |> list.index_map(fn(pair, index) { #(index % spread_lanes, pair) })
      |> list.group(fn(entry) { entry.0 })
      |> dict.values
      |> list.map(fn(lane) {
        let reply = process.new_subject()
        let #(_, monitor) =
          spawn.monitored(fn() {
            process.send(
              reply,
              list.count(lane, fn(entry) {
                let #(_, #(ref, target)) = entry
                hand_off(target, ref)
              }),
            )
          })
        #(reply, monitor)
      })
      |> list.fold(0, fn(total, pending) {
        let #(reply, monitor) = pending
        let moved =
          process.new_selector()
          |> process.select(reply)
          |> process.select_specific_monitor(monitor, fn(_) { 0 })
          |> process.selector_receive_forever
        process.demonitor_process(monitor)
        total + moved
      })
  }
}

pub fn export_for(nodes: List(Node)) -> Int {
  registry.all()
  |> list.fold(0, fn(count, ref) {
    let key = session.session_id_to_string(ref.id)
    case target_for(key, nodes) {
      Ok(target) ->
        case hand_off(target, ref) {
          True -> count + 1
          False -> count
        }
      Error(Nil) -> count
    }
  })
}

fn target_for(session_id: String, nodes: List(Node)) -> Result(Node, Nil) {
  use owner <- result.try(cluster_router.owner_node(
    cluster_router.SessionKey(session_id),
    node.Sessions,
  ))
  case owner != node.self() && list.contains(nodes, owner) {
    True -> Ok(owner)
    False -> Error(Nil)
  }
}

fn hand_off(target: Node, ref: session.SessionRef) -> Bool {
  let started = clock.monotonic_ms()
  let moved = control.hand_off(ref, target, hand_off_timeout_ms())
  session_metrics.moved(moved, clock.monotonic_ms() - started)
  metrics.counter(
    "fluxer_gateway_session_moves_total",
    [
      #("outcome", case moved {
        True -> "moved"
        False -> "failed"
      }),
    ],
    1,
  )
  let id = ref.id
  case draining.is_draining(), moved {
    True, True -> {
      rollout_metrics.count(
        rollout_metrics.drain_entities,
        [#("kind", "session"), #("outcome", "moved")],
        1,
      )
      drain_report.entity_moved("session", id)
    }
    True, False -> drain_report.entity_failed("session", id)
    False, _ -> Nil
  }
  case moved {
    True -> True
    False -> {
      logging.log(
        logging.Warning,
        "session_state_push_failed: session_id="
          <> session.session_id_to_string(ref.id)
          <> " target_node="
          <> node.to_string(target),
      )
      False
    }
  }
}
