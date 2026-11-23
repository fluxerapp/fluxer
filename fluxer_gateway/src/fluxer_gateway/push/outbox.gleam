// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId, type MessageId, type UserId}
import fluxer_gateway/core/node
import fluxer_gateway/push/job.{type Job, type Kind}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/named
import fluxer_gateway/push/outbox/attempt
import fluxer_gateway/push/outbox/state.{type Outbox}
import fluxer_gateway/push/rule.{type Class}
import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.push().outbox_init_timeout_ms
}

fn enqueue_timeout_ms() -> Int {
  durations.push().outbox_enqueue_timeout_ms
}

fn prune_interval_ms() -> Int {
  durations.push().outbox_prune_interval_ms
}

fn gauge_interval_ms() -> Int {
  durations.push().outbox_gauge_interval_ms
}

pub type EnqueueError {
  OutboxUnavailable
  EnqueueTimeout
}

pub opaque type Message {
  Enqueue(job: Job, reply: Subject(Nil))
  AttemptReply(attempt: Int, result: state.SendResult)
  AttemptDown(down: process.Down)
  SlotDeadline(attempt: Int)
  WindowEnd(attempt: Int)
  Retry(seq: Int)
  TruncateRead(user: Int, channel: Int, message: Int)
  SessionActive(user: Int)
  Dropped(kind: Kind, class: Class, reason: String)
  GaugeTick
  PruneTick
  Export(
    bucket: Int,
    before_ms: Int,
    reply: Subject(Result(#(List(state.Carried), List(state.ReadMark)), Nil)),
  )
  Reclaim(bucket: Int, carried: List(state.Carried))
  Hand(bucket: Int, to: state.Target)
  HandAll(to: state.Target, reply: Subject(Nil))
  Arrive(bucket: Int, carried: List(state.Carried), marks: List(state.ReadMark))
  Adopt(carried: List(state.Carried))
  Buckets(reply: Subject(List(Int)))
  Idle(reply: Subject(Bool))
  PendingCount(reply: Subject(Int))
}

pub type Sender =
  fn(String, BitArray, Int) -> state.SendResult

type Server {
  Server(
    self: Subject(Message),
    send: Sender,
    outbox: Outbox,
    pids: Dict(Int, Pid),
    attempts_by_pid: Dict(Pid, Int),
  )
}

@external(erlang, "erlang", "binary_to_atom")
fn name_atom(name: String) -> Name(Message)

const outbox_name = "fluxer_gateway_push_outbox"

pub fn name() -> Name(Message) {
  name_atom(outbox_name)
}

@external(erlang, "gleam_stdlib", "identity")
fn name_text_atom(name: Name(Message)) -> atom.Atom

pub fn target_at(on: node.Node, outbox: Name(Message)) -> state.Target {
  state.Target(
    node: node.to_string(on),
    outbox: atom.to_string(name_text_atom(outbox)),
  )
}

pub fn target_on(on: node.Node) -> state.Target {
  state.Target(node: node.to_string(on), outbox: outbox_name)
}

pub fn supervised(settings: Settings) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() { start(name(), settings, attempt.send) })
}

pub fn start(
  name: Name(Message),
  settings: Settings,
  send: Sender,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    let outbox =
      state.new(
        state.default_limits(settings.push_outbox_request_timeout_ms),
        settings.push_enrolled_clear_notifications_enabled,
      )
    let self = process.new_subject()
    process.send_after(self, prune_interval_ms(), PruneTick)
    process.send_after(self, gauge_interval_ms(), GaugeTick)
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select(self)
      |> process.select_monitors(AttemptDown)
    actor.initialised(Server(
      self:,
      send:,
      outbox:,
      pids: dict.new(),
      attempts_by_pid: dict.new(),
    ))
    |> actor.selecting(selector)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(name)
  |> actor.start
}

fn handle(server: Server, message: Message) -> actor.Next(Server, Message) {
  let now = clock.monotonic_ms()
  let server = case draining.is_draining() {
    True -> server
    False -> Server(..server, outbox: state.undrained(server.outbox))
  }
  case message {
    Enqueue(job:, reply:) -> {
      process.send(reply, Nil)
      run(server, state.enqueue(server.outbox, job, now))
    }
    AttemptReply(attempt:, result:) -> {
      let server = forget_attempt(server, attempt)
      run(server, state.reply(server.outbox, attempt, result, now))
    }
    AttemptDown(process.ProcessDown(pid:, reason:, ..)) ->
      case dict.get(server.attempts_by_pid, pid) {
        Ok(attempt) -> {
          let server = forget_attempt(server, attempt)
          let result = case reason {
            process.Killed -> state.Failed("timeout")
            _ -> state.Failed("worker_down")
          }
          run(server, state.reply(server.outbox, attempt, result, now))
        }
        Error(Nil) -> actor.continue(server)
      }
    AttemptDown(_) -> actor.continue(server)
    SlotDeadline(attempt:) ->
      run(server, state.slot_deadline(server.outbox, attempt, now))
    WindowEnd(attempt:) ->
      run(server, state.window_end(server.outbox, attempt, now))
    Retry(seq:) -> run(server, state.retry_due(server.outbox, seq, now))
    TruncateRead(user:, channel:, message:) ->
      run(
        server,
        state.truncate_read(
          server.outbox,
          user:,
          channel:,
          message:,
          now_ms: now,
        ),
      )
    SessionActive(user:) ->
      actor.continue(
        Server(..server, outbox: state.session_active(server.outbox, user, now)),
      )
    Dropped(kind:, class:, reason:) -> {
      metrics.outbox_dropped(kind, class, reason)
      actor.continue(server)
    }
    GaugeTick -> {
      process.send_after(server.self, gauge_interval_ms(), GaugeTick)
      report(state.stats(server.outbox))
      actor.continue(server)
    }
    PruneTick -> {
      process.send_after(server.self, prune_interval_ms(), PruneTick)
      actor.continue(Server(..server, outbox: state.prune(server.outbox, now)))
    }
    Export(before_ms:, reply:, ..) if now > before_ms -> {
      process.send(reply, Error(Nil))
      actor.continue(server)
    }
    Export(bucket:, reply:, ..) ->
      case state.export(server.outbox, bucket, now) {
        Ok(#(outbox, carried, marks)) -> {
          process.send(reply, Ok(#(carried, marks)))
          actor.continue(Server(..server, outbox:))
        }
        Error(Nil) -> {
          process.send(reply, Error(Nil))
          actor.continue(server)
        }
      }
    Reclaim(bucket:, carried:) ->
      run(server, state.reclaim(server.outbox, bucket, carried, now))
    Hand(bucket:, to:) -> run(server, state.hand(server.outbox, bucket, to))
    HandAll(to:, reply:) -> {
      process.send(reply, Nil)
      run(server, state.hand_all(server.outbox, to, now))
    }
    Arrive(bucket:, carried:, marks:) ->
      run(
        server,
        state.arrive(server.outbox, bucket)
          |> state.adopt(carried, marks, now),
      )
    Adopt(carried:) -> run(server, state.adopt(server.outbox, carried, [], now))
    Buckets(reply:) -> {
      process.send(reply, state.buckets(server.outbox))
      actor.continue(server)
    }
    Idle(reply:) -> {
      process.send(reply, state.is_idle(server.outbox))
      actor.continue(server)
    }
    PendingCount(reply:) -> {
      process.send(reply, state.pending(server.outbox))
      actor.continue(server)
    }
  }
}

fn forget_attempt(server: Server, attempt: Int) -> Server {
  case dict.get(server.pids, attempt) {
    Ok(pid) ->
      Server(
        ..server,
        pids: dict.delete(server.pids, attempt),
        attempts_by_pid: dict.delete(server.attempts_by_pid, pid),
      )
    Error(Nil) -> server
  }
}

fn run(
  server: Server,
  step: #(Outbox, List(state.Effect)),
) -> actor.Next(Server, Message) {
  let #(outbox, effects) = step
  let server = list.fold(effects, Server(..server, outbox:), apply)
  actor.continue(server)
}

fn apply(server: Server, effect: state.Effect) -> Server {
  case effect {
    state.Start(attempt: attempt_id, subject:, body:, timeout_ms:) -> {
      metrics.count("push_outbox_attempts_total", 1)
      let outbox = server.self
      let send = server.send
      let #(pid, _) =
        spawn.monitored(fn() {
          let started = clock.monotonic_ms()
          let result = send(subject, body, timeout_ms)
          metrics.attempted(subject, result, clock.monotonic_ms() - started)
          process.send(outbox, AttemptReply(attempt_id, result))
        })
      Server(
        ..server,
        pids: dict.insert(server.pids, attempt_id, pid),
        attempts_by_pid: dict.insert(server.attempts_by_pid, pid, attempt_id),
      )
    }
    state.Kill(attempt:) ->
      case dict.get(server.pids, attempt) {
        Ok(pid) -> {
          process.kill(pid)
          forget_attempt(server, attempt)
        }
        Error(Nil) -> server
      }
    state.ArmSlotDeadline(attempt:, after_ms:) -> {
      let _ = process.send_after(server.self, after_ms, SlotDeadline(attempt))
      server
    }
    state.ArmWindowEnd(attempt:, after_ms:) -> {
      let _ = process.send_after(server.self, after_ms, WindowEnd(attempt))
      server
    }
    state.ArmRetry(seq:, after_ms:) -> {
      let jitter = int.random(int.max(1, after_ms / 4)) + 1
      let _ = process.send_after(server.self, after_ms + jitter, Retry(seq))
      server
    }
    state.Count(name:, class:, by:) -> {
      metrics.outbox(name, class, by)
      server
    }
    state.Drop(kind:, class:, reason:) -> {
      metrics.outbox_dropped(kind, class, reason)
      server
    }
    state.HandOver(to:, carried:) ->
      case hand_over(to, carried) {
        Ok(Nil) -> server
        Error(Nil) -> {
          let #(outbox, effects) =
            state.keep(server.outbox, carried, clock.monotonic_ms())
          list.fold(effects, Server(..server, outbox:), apply)
        }
      }
  }
}

fn hand_over(
  to: state.Target,
  carried: List(state.Carried),
) -> Result(Nil, Nil) {
  let target = node.from_atom(atom.create(to.node))
  let outbox = name_atom(to.outbox)
  let sent = case target == node.self() {
    True -> named.send(outbox, Adopt(carried))
    False ->
      remote.send_named(target, outbox, Adopt(carried))
      |> result.replace_error(Nil)
  }
  case sent {
    Ok(Nil) -> {
      metrics.count("push_outbox_handed_total", list.length(carried))
      metrics.lost_or_moved("moved", list.length(carried))
    }
    Error(Nil) ->
      logging.log(
        logging.Warning,
        "Push outbox: hand over to "
          <> to.node
          <> " failed, keeping "
          <> int.to_string(list.length(carried))
          <> " jobs",
      )
  }
  sent
}

const late_reply_margin_ms = 1000

fn ask(
  at: Name(Message),
  make: fn(Subject(a)) -> Message,
  absent: a,
) -> Result(a, Nil) {
  let reply = process.new_subject()
  case named.send(at, make(reply)) {
    Ok(Nil) ->
      process.receive(reply, enqueue_timeout_ms() + late_reply_margin_ms)
    Error(Nil) -> Ok(absent)
  }
}

pub fn export_bucket(
  at: Name(Message),
  bucket: Int,
) -> Result(#(List(state.Carried), List(state.ReadMark)), Nil) {
  let before_ms = clock.monotonic_ms() + enqueue_timeout_ms()
  ask(at, Export(bucket, before_ms, _), Ok(#([], [])))
  |> result.flatten
}

pub fn reclaim_bucket(
  at: Name(Message),
  bucket: Int,
  carried: List(state.Carried),
) -> Nil {
  named.send(at, Reclaim(bucket, carried)) |> result.unwrap(Nil)
}

pub fn hand_bucket(at: Name(Message), bucket: Int, to: state.Target) -> Nil {
  named.send(at, Hand(bucket, to)) |> result.unwrap(Nil)
}

pub fn hand_all(at: Name(Message), to: state.Target) -> Nil {
  let _ = ask(at, HandAll(to, _), Nil)
  Nil
}

pub fn arrive_bucket(
  at: Name(Message),
  bucket: Int,
  carried: List(state.Carried),
  marks: List(state.ReadMark),
) -> Nil {
  named.send(at, Arrive(bucket, carried, marks)) |> result.unwrap(Nil)
}

pub fn busy_buckets(at: Name(Message)) -> Result(List(Int), Nil) {
  ask(at, Buckets, [])
}

pub fn is_idle(at: Name(Message)) -> Bool {
  ask(at, Idle, True) |> result.unwrap(False)
}

pub fn pending_jobs(at: Name(Message)) -> Result(Int, Nil) {
  ask(at, PendingCount, 0)
}

fn report(stats: state.Stats) -> Nil {
  metrics.gauge("push_outbox_depth", [#("class", "direct")], stats.depth_direct)
  metrics.gauge("push_outbox_depth", [#("class", "bulk")], stats.depth_bulk)
  metrics.gauge(
    "push_outbox_inflight",
    [#("class", "direct")],
    stats.inflight_direct,
  )
  metrics.gauge(
    "push_outbox_inflight",
    [#("class", "bulk")],
    stats.inflight_bulk,
  )
  metrics.gauge(
    "push_outbox_unknown",
    [#("class", "direct")],
    stats.unknown_direct,
  )
  metrics.gauge("push_outbox_unknown", [#("class", "bulk")], stats.unknown_bulk)
}

pub fn enqueue(job: Job) -> Result(Nil, EnqueueError) {
  enqueue_to(name(), job)
}

pub fn enqueue_to(
  outbox: Name(Message),
  job: Job,
) -> Result(Nil, EnqueueError) {
  let reply = process.new_subject()
  case named.send(outbox, Enqueue(job, reply)) {
    Error(Nil) -> Error(OutboxUnavailable)
    Ok(Nil) ->
      process.receive(reply, enqueue_timeout_ms())
      |> result.replace_error(EnqueueTimeout)
  }
}

pub fn record_dropped(kind: Kind, class: Class, reason: String) -> Nil {
  case named.send(name(), Dropped(kind, class, reason)) {
    Ok(Nil) -> Nil
    Error(Nil) -> metrics.outbox_dropped(kind, class, reason)
  }
}

pub fn truncate_read(
  user: UserId,
  channel: ChannelId,
  message: MessageId,
) -> Nil {
  broadcast(TruncateRead(
    id.user_id_to_int(user),
    id.channel_id_to_int(channel),
    id.message_id_to_int(message),
  ))
}

pub fn note_session_active(user: UserId) -> Nil {
  broadcast(SessionActive(id.user_id_to_int(user)))
}

fn broadcast(message: Message) -> Nil {
  let nodes = case router.nodes_with(node.Push) {
    [] -> [node.self()]
    nodes -> nodes
  }
  list.each(nodes, fn(target) {
    case target == node.self() {
      True -> named.send(name(), message) |> result.unwrap(Nil)
      False -> {
        let _ = remote.send_named(target, name(), message)
        Nil
      }
    }
  })
}
