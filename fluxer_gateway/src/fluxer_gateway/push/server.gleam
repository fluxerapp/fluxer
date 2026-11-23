// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key
import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings as config
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId, type MessageId, type UserId}
import fluxer_gateway/core/node
import fluxer_gateway/push/format
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/named
import fluxer_gateway/push/outbox
import fluxer_gateway/push/publisher
import fluxer_gateway/push/read_state
import fluxer_gateway/push/recipients
import fluxer_gateway/push/settings_cache
import fluxer_gateway/push/throttle
import fluxer_gateway/util/spawn
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.push().server_init_timeout_ms
}

const max_pending_jobs = 4096

fn pending_max_age_ms() -> Int {
  durations.push().server_pending_max_age_ms
}

fn pending_drain_interval_ms() -> Int {
  durations.push().server_pending_drain_interval_ms
}

const max_push_workers = 256

const utilization_warn_percent = 80

fn utilization_warn_interval_ms() -> Int {
  durations.push().server_utilization_warn_interval_ms
}

fn job_deadline_ms() -> Int {
  durations.push().server_job_deadline_ms
}

pub opaque type Message {
  MessageCreateCast(create: MessageCreate)
  ClearCast(user: Int, channel: Int, message: Int)
  DeleteBlocked(user: Int)
  WorkerDown(process.Down)
  DrainPending
  ExportPending(bucket: Int, before_ms: Int, reply: Subject(List(Waiting)))
  Requeue(waiting: List(Waiting))
  BusyBuckets(reply: Subject(List(Int)))
  Idle(reply: Subject(Bool))
  PendingCount(reply: Subject(Int))
}

pub type Waiting {
  Waiting(age_ms: Int, create: MessageCreate)
}

type Pending {
  Pending(enqueued_at_ms: Int, create: MessageCreate)
}

type Queue {
  Queue(front: List(Pending), back: List(Pending))
}

pub type Work =
  fn(MessageCreate, fn() -> Bool) -> Nil

type Gate

type GateOption {
  Signed(Bool)
}

type Exchanged

@external(erlang, "atomics", "new")
fn gate_new(arity: Int, options: List(GateOption)) -> Gate

@external(erlang, "atomics", "compare_exchange")
fn gate_exchange(
  gate: Gate,
  index: Int,
  expected: Int,
  desired: Int,
) -> Exchanged

@external(erlang, "erlang", "is_integer")
fn lost_race(exchanged: Exchanged) -> Bool

const gate_open = 0

const gate_publishing = 1

const gate_handed = 2

fn close_gate(gate: Gate, to: Int) -> Bool {
  !lost_race(gate_exchange(gate, 1, gate_open, to))
}

type Worker {
  Worker(
    key: Int,
    create: MessageCreate,
    waited_ms: Int,
    gate: Gate,
    started_ms: Int,
  )
}

type Server {
  Server(
    self: Subject(Message),
    work: Work,
    workers: Dict(Pid, Worker),
    pending: Queue,
    pending_len: Int,
    drain_scheduled: Bool,
    last_warning_ms: Int,
  )
}

@external(erlang, "erlang", "binary_to_atom")
fn name_atom(name: String) -> Name(Message)

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Int) -> Int

pub fn name() -> Name(Message) {
  name_atom("fluxer_gateway_push")
}

pub fn supervised(
  _settings: config.Settings,
) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() { start(name(), target) })
}

pub fn start(
  name: Name(Message),
  work: Work,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    let _ = process_flag(atom.create("fullsweep_after"), 10)
    throttle.setup()
    read_state.setup()
    let self = process.new_subject()
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select(self)
      |> process.select_monitors(WorkerDown)
    actor.initialised(Server(
      self:,
      work:,
      workers: dict.new(),
      pending: Queue(front: [], back: []),
      pending_len: 0,
      drain_scheduled: False,
      last_warning_ms: clock.monotonic_ms() - utilization_warn_interval_ms(),
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
  case message {
    MessageCreateCast(create:) -> {
      metrics.received("message_create")
      case server.pending_len {
        0 ->
          case spawn_worker(server, create, 0) {
            Ok(server) -> actor.continue(server)
            Error(server) ->
              actor.continue(schedule_drain(enqueue_pending(server, create)))
          }
        _ -> actor.continue(schedule_drain(enqueue_pending(server, create)))
      }
    }
    ClearCast(user:, channel:, message:) -> {
      metrics.received("clear")
      case id.user_id(user), id.channel_id(channel), id.message_id(message) {
        Ok(user), Ok(channel), Ok(message) -> {
          let _ = publisher.publish_clear(user, channel, message)
          Nil
        }
        _, _, _ -> Nil
      }
      actor.continue(server)
    }
    DeleteBlocked(user:) -> {
      settings_cache.delete_blocked(user)
      actor.continue(server)
    }
    WorkerDown(process.ProcessDown(pid:, ..)) -> {
      case dict.get(server.workers, pid) {
        Ok(worker) ->
          metrics.job_finished(clock.monotonic_ms() - worker.started_ms)
        Error(Nil) -> Nil
      }
      let workers = dict.delete(server.workers, pid)
      metrics.gauge("push_workers_active", [], dict.size(workers))
      actor.continue(Server(..server, workers:))
    }
    WorkerDown(_) -> actor.continue(server)
    DrainPending ->
      actor.continue(
        schedule_drain(drain(Server(..server, drain_scheduled: False))),
      )
    ExportPending(bucket:, before_ms:, reply:) -> {
      let now = clock.monotonic_ms()
      use <- bool.lazy_guard(now > before_ms, fn() { actor.continue(server) })
      let #(leaving, kept) =
        queued(server.pending)
        |> list.partition(fn(job) {
          key.bucket(message.route_key(job.create)) == bucket
        })
      let #(server, running) = claim_workers(server, bucket)
      let queued =
        list.map(leaving, fn(job) {
          Waiting(
            age_ms: int.max(0, now - job.enqueued_at_ms),
            create: job.create,
          )
        })
      process.send(reply, list.append(running, queued))
      metrics.pending(list.length(kept))
      actor.continue(
        Server(
          ..server,
          pending: Queue(front: kept, back: []),
          pending_len: list.length(kept),
        ),
      )
    }
    Requeue(waiting:) -> {
      let now = clock.monotonic_ms()
      list.fold(waiting, server, fn(server, found) {
        requeue(
          server,
          Pending(enqueued_at_ms: now - found.age_ms, create: found.create),
        )
      })
      |> schedule_drain
      |> actor.continue
    }
    BusyBuckets(reply:) -> {
      queued(server.pending)
      |> list.map(fn(job) { message.route_key(job.create) })
      |> list.append(
        dict.values(server.workers) |> list.map(fn(worker) { worker.key }),
      )
      |> list.map(key.bucket)
      |> list.unique
      |> process.send(reply, _)
      actor.continue(server)
    }
    Idle(reply:) -> {
      process.send(
        reply,
        dict.is_empty(server.workers) && server.pending_len == 0,
      )
      actor.continue(server)
    }
    PendingCount(reply:) -> {
      process.send(reply, dict.size(server.workers) + server.pending_len)
      actor.continue(server)
    }
  }
}

fn queued(queue: Queue) -> List(Pending) {
  list.append(queue.front, list.reverse(queue.back))
}

fn requeue(server: Server, job: Pending) -> Server {
  case server.pending_len >= max_pending_jobs {
    True -> {
      log_drop()
      server
    }
    False ->
      Server(
        ..server,
        pending: push_back(server.pending, job),
        pending_len: server.pending_len + 1,
      )
  }
}

fn claim_workers(server: Server, bucket: Int) -> #(Server, List(Waiting)) {
  dict.fold(server.workers, #(server, []), fn(acc, pid, worker) {
    let #(server, claimed) = acc
    case
      key.bucket(worker.key) == bucket && close_gate(worker.gate, gate_handed)
    {
      False -> acc
      True -> {
        process.kill(pid)
        let workers = dict.delete(server.workers, pid)
        metrics.gauge("push_workers_active", [], dict.size(workers))
        #(Server(..server, workers:), [
          Waiting(age_ms: worker.waited_ms, create: worker.create),
          ..claimed
        ])
      }
    }
  })
}

fn spawn_worker(
  server: Server,
  create: MessageCreate,
  waited_ms: Int,
) -> Result(Server, Server) {
  let active = dict.size(server.workers)
  case active >= max_push_workers {
    True -> Error(server)
    False -> {
      let server = maybe_warn(server, active + 1)
      let gate = gate_new(1, [Signed(True)])
      let work = server.work
      let #(pid, _) =
        spawn.monitored(fn() {
          work(create, fn() { close_gate(gate, gate_publishing) })
        })
      let workers =
        dict.insert(
          server.workers,
          pid,
          Worker(
            key: message.route_key(create),
            create:,
            waited_ms:,
            gate:,
            started_ms: clock.monotonic_ms(),
          ),
        )
      metrics.job_waited(waited_ms)
      metrics.gauge("push_workers_active", [], dict.size(workers))
      Ok(Server(..server, workers:))
    }
  }
}

fn maybe_warn(server: Server, active: Int) -> Server {
  let now = clock.monotonic_ms()
  case
    active * 100 / max_push_workers >= utilization_warn_percent
    && now - server.last_warning_ms >= utilization_warn_interval_ms()
  {
    True -> {
      logging.log(logging.Warning, "Push worker pool high utilisation")
      Server(..server, last_warning_ms: now)
    }
    False -> server
  }
}

fn target(create: MessageCreate, may_publish: fn() -> Bool) -> Nil {
  case message.validate(create) {
    Error(_) -> Nil
    Ok(create) -> {
      let eligible =
        recipients.eligible(create, clock.deadline_in(job_deadline_ms()))
      use <- bool.guard(eligible == [] || !may_publish(), Nil)
      let _ = publisher.publish_message(create, eligible)
      Nil
    }
  }
}

fn enqueue_pending(server: Server, create: MessageCreate) -> Server {
  requeue(server, Pending(enqueued_at_ms: clock.monotonic_ms(), create:))
}

fn log_drop() -> Nil {
  metrics.count("push_loss_worker_pool", 1)
  logging.log(
    logging.Debug,
    "Push: worker pool saturated, dropping message create",
  )
}

fn drain(server: Server) -> Server {
  case pop_front(server.pending) {
    Error(Nil) ->
      Server(..server, pending: Queue(front: [], back: []), pending_len: 0)
    Ok(#(job, rest)) -> {
      let next =
        Server(..server, pending: rest, pending_len: server.pending_len - 1)
      let waited_ms = clock.monotonic_ms() - job.enqueued_at_ms
      case waited_ms > pending_max_age_ms() {
        True -> {
          log_drop()
          drain(next)
        }
        False ->
          case spawn_worker(next, job.create, int.max(0, waited_ms)) {
            Ok(next) -> drain(next)
            Error(_) -> server
          }
      }
    }
  }
}

fn push_back(queue: Queue, item: Pending) -> Queue {
  Queue(..queue, back: [item, ..queue.back])
}

fn pop_front(queue: Queue) -> Result(#(Pending, Queue), Nil) {
  case queue.front {
    [first, ..rest] -> Ok(#(first, Queue(..queue, front: rest)))
    [] ->
      case list.reverse(queue.back) {
        [] -> Error(Nil)
        [first, ..rest] -> Ok(#(first, Queue(front: rest, back: [])))
      }
  }
}

fn schedule_drain(server: Server) -> Server {
  metrics.pending(server.pending_len)
  case server.pending_len, server.drain_scheduled {
    0, _ -> server
    _, True -> server
    _, False -> {
      let _ =
        process.send_after(
          server.self,
          pending_drain_interval_ms(),
          DrainPending,
        )
      Server(..server, drain_scheduled: True)
    }
  }
}

fn is_push_active() -> Bool {
  config.get().push_enabled
}

pub fn handle_message_create(create: MessageCreate) -> Nil {
  case is_push_active() {
    True -> cast_to_owner(message.route_key(create), MessageCreateCast(create))
    False -> Nil
  }
}

pub fn handle_buffered_message_creates(
  buffered: List(read_state.Buffered),
) -> Nil {
  case is_push_active() {
    True -> read_state.handle_buffered(buffered, handle_message_create)
    False -> Nil
  }
}

pub fn private_message(
  data: Dynamic,
  recipient: UserId,
  author: UserId,
) -> Result(MessageCreate, Nil) {
  use decoded <- result.try(message.decode_data(data))
  message.private_message(
    decoded,
    recipient,
    author,
    format.private_markdown_context(decoded),
  )
  |> result.replace_error(Nil)
}

pub fn clear_channel_notifications(
  user: UserId,
  channel: ChannelId,
  message: MessageId,
) -> Nil {
  case is_push_active() {
    True -> {
      outbox.truncate_read(user, channel, message)
      case clear_notifications_enabled() {
        True ->
          cast_to_owner(
            id.user_id_to_int(user),
            ClearCast(
              id.user_id_to_int(user),
              id.channel_id_to_int(channel),
              id.message_id_to_int(message),
            ),
          )
        False -> Nil
      }
    }
    False -> Nil
  }
}

fn clear_notifications_enabled() -> Bool {
  config.get().push_enrolled_clear_notifications_enabled
}

pub fn invalidate_blocked_ids(user: UserId) -> Nil {
  let raw = id.user_id_to_int(user)
  list.each([node.self(), ..node.connected()], fn(target) {
    send_to(target, DeleteBlocked(raw))
  })
}

pub fn invalidate_user_guild_settings_local(user: Int, guild: Int) -> Nil {
  settings_cache.delete_settings(user, guild)
}

fn cast_to_owner(key: Int, message: Message) -> Nil {
  case router.owner(router.PushKey(key), node.Push) {
    router.Local -> send_to(node.self(), message)
    router.Remote(target) -> send_to(target, message)
    router.OwnerUnavailable(_) -> metrics.count("push_owner_unavailable", 1)
  }
}

fn send_to(target: node.Node, message: Message) -> Nil {
  case target == node.self() {
    True -> named.send(name(), message) |> result.unwrap(Nil)
    False -> {
      let _ = remote.send_named(target, name(), message)
      Nil
    }
  }
}

const late_reply_margin_ms = 1000

fn ask(
  at: Name(Message),
  make: fn(Subject(a)) -> Message,
  absent: a,
) -> Result(a, Nil) {
  let reply = process.new_subject()
  case named.send(at, make(reply)) {
    Ok(Nil) -> process.receive(reply, init_timeout_ms() + late_reply_margin_ms)
    Error(Nil) -> Ok(absent)
  }
}

pub fn export_pending(
  at: Name(Message),
  bucket: Int,
) -> Result(List(Waiting), Nil) {
  let before_ms = clock.monotonic_ms() + init_timeout_ms()
  ask(at, ExportPending(bucket, before_ms, _), [])
}

pub fn requeue_local(at: Name(Message), waiting: List(Waiting)) -> Nil {
  case waiting {
    [] -> Nil
    _ -> named.send(at, Requeue(waiting)) |> result.unwrap(Nil)
  }
}

pub fn requeue_at(
  on: node.Node,
  at: Name(Message),
  waiting: List(Waiting),
) -> Result(Nil, Nil) {
  case waiting, on == node.self() {
    [], _ -> Ok(Nil)
    _, True -> named.send(at, Requeue(waiting))
    _, False ->
      remote.send_named(on, at, Requeue(waiting)) |> result.replace_error(Nil)
  }
}

pub fn busy_buckets(at: Name(Message)) -> Result(List(Int), Nil) {
  ask(at, BusyBuckets, [])
}

pub fn is_idle(at: Name(Message)) -> Bool {
  ask(at, Idle, True) |> result.unwrap(False)
}

pub fn pending_jobs(at: Name(Message)) -> Result(Int, Nil) {
  ask(at, PendingCount, 0)
}
