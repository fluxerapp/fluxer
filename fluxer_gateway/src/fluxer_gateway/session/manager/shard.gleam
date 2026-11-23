// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/rollout
import fluxer_gateway/core/clock
import fluxer_gateway/core/session.{
  type Identify, type Peer, type ResumeError, type SessionId, type SessionRef,
  type SocketRef, type StartError, ResumeUnavailable, SessionGone,
  StartAuthFailed, StartRateLimited, StartShardingRequired, StartTooManySessions,
  StartUnavailable, StartUnknownError,
}
import fluxer_gateway/core/user
import fluxer_gateway/gateway/start_gate
import fluxer_gateway/session/control
import fluxer_gateway/session/manager/leases
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards.{
  type Message, Adopt, Fetched, NotFoundElsewhere, ResumeSession, StartSession,
  Watched,
}
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/seed.{type Seed}
import fluxer_gateway/session/server
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Down, type Monitor, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/factory_supervisor as factory
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.session().shard_init_timeout_ms
}

type RemoteResume {
  Answered(Result(SessionRef, ResumeError))
  Vanished
}

type Pending {
  Pending(
    socket: SocketRef,
    reply: Subject(Result(SessionRef, StartError)),
    settled: Subject(Nil),
    worker_monitor: Monitor,
    socket_monitor: Monitor,
  )
}

type Shard {
  Shard(
    index: Int,
    self: Subject(Message),
    pending: Dict(Int, Pending),
    next_token: Int,
    sessions: Dict(Monitor, SessionRef),
  )
}

fn permit_wait_ms() -> Int {
  durations.session().shard_permit_wait_ms
}

fn fetch_timeout_ms() -> Int {
  durations.session().shard_fetch_timeout_ms
}

fn unavailable_retry_ms() -> Int {
  durations.session().shard_unavailable_retry_ms
}

fn remote_resume_timeout_ms() -> Int {
  durations.session().shard_remote_resume_timeout_ms
}

pub fn supervised(
  index: Int,
) -> ChildSpecification(static_supervisor.Supervisor) {
  let sessions =
    factory.worker_child(server.start)
    |> factory.restart_strategy(supervision.Temporary)
    |> factory.named(names.factory(index))
    |> factory.supervised
  static_supervisor.new(static_supervisor.OneForAll)
  |> static_supervisor.add(sessions)
  |> static_supervisor.add(supervision.worker(fn() { start(index) }))
  |> static_supervisor.supervised
}

fn start(index: Int) -> actor.StartResult(Nil) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    process_flag(atom.create("fullsweep_after"), 0)
    leases.create_table()
    leases.sweep()
    sweep_registry()
    let selector =
      process.new_selector()
      |> process.select(self)
      |> process.select_monitors(Watched)
    actor.initialised(Shard(
      index:,
      self:,
      pending: dict.new(),
      next_token: 1,
      sessions: dict.new(),
    ))
    |> actor.selecting(selector)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(names.shard(index))
  |> actor.start
}

fn sweep_registry() -> Nil {
  registry.sweep_dead()
  |> list.each(fn(row) {
    let #(ref, pid) = row
    process_registry.unregister_pid(process_registry.sessions, ref.id, pid)
  })
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Int) -> Int

fn sessions_factory(
  shard: Shard,
) -> factory.Supervisor(server.Start, SessionRef) {
  factory.get_by_name(names.factory(shard.index))
}

fn handle(shard: Shard, message: Message) -> actor.Next(Shard, Message) {
  case message {
    StartSession(session_id, identify, peer, socket, reply, settled) ->
      case draining.is_draining() {
        True -> {
          process.send(reply, Error(StartUnavailable(unavailable_retry_ms())))
          process.send(settled, Nil)
          actor.continue(shard)
        }
        False ->
          actor.continue(begin(
            shard,
            session_id,
            identify,
            peer,
            socket,
            reply,
            settled,
          ))
      }
    Fetched(token, result) ->
      case dict.get(shard.pending, token) {
        Error(Nil) -> actor.continue(shard)
        Ok(pending) -> {
          process.demonitor_process(pending.worker_monitor)
          process.demonitor_process(pending.socket_monitor)
          let shard = Shard(..shard, pending: dict.delete(shard.pending, token))
          actor.continue(finish(shard, pending, result))
        }
      }
    ResumeSession(session_id, token, seq, socket, reply, settled) -> {
      resume(session_id, token, seq, socket, reply)
      process.send(settled, Nil)
      actor.continue(shard)
    }
    NotFoundElsewhere(session_id, token, seq, socket, reply, settled) -> {
      case registry.lookup(session_id) {
        Ok(ref) ->
          spawn_ask(ref, token, seq, socket, reply, fn() {
            process.send(reply, Error(SessionGone))
          })
        Error(Nil) -> process.send(reply, Error(SessionGone))
      }
      process.send(settled, Nil)
      actor.continue(shard)
    }
    Adopt(found, reply) -> actor.continue(adopt(shard, found, reply))
    Watched(down) -> actor.continue(on_down(shard, down))
  }
}

fn begin(
  shard: Shard,
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, StartError)),
  settled: Subject(Nil),
) -> Shard {
  let token = shard.next_token
  let self = shard.self
  let #(_, worker_monitor) =
    spawn.monitored(fn() {
      process.send(self, Fetched(token, fetch(session_id, identify, peer)))
    })
  let pending =
    Pending(
      socket:,
      reply:,
      settled:,
      worker_monitor:,
      socket_monitor: process.monitor(socket.pid),
    )
  Shard(
    ..shard,
    pending: dict.insert(shard.pending, token, pending),
    next_token: token + 1,
  )
}

fn fetch(
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
) -> Result(Seed, StartError) {
  use permit <- result.try(acquire_permit())
  let outcome = fetch_seed(session_id, identify, peer)
  start_gate.release_session_start(permit)
  outcome
}

fn acquire_permit() -> Result(start_gate.StartPermit, StartError) {
  case start_gate.try_session_start() {
    Ok(permit) -> Ok(permit)
    Error(_) -> {
      let waiter = process.new_subject()
      start_gate.wait_session_start(waiter)
      case process.receive(waiter, permit_wait_ms()) {
        Ok(permit) -> Ok(permit)
        Error(Nil) -> {
          start_gate.cancel_session_start(waiter)
          case process.receive(waiter, 0) {
            Ok(permit) -> start_gate.release_session_start(permit)
            Error(Nil) -> Nil
          }
          Error(StartUnavailable(unavailable_retry_ms()))
        }
      }
    }
  }
}

fn fetch_seed(
  session_id: SessionId,
  identify: Identify,
  peer: Peer,
) -> Result(Seed, StartError) {
  let fields =
    [
      #("token", json.string(identify.token)),
      #("version", json.int(peer.api_version)),
      #("ip", json.string(peer.ip)),
    ]
    |> add_coordinate("latitude", identify.latitude)
    |> add_coordinate("longitude", identify.longitude)
    |> add_flag("thread_channels_capable", identify.flags.channel_threads)
    |> add_flag("guild_visits_capable", identify.flags.guild_visits)
  let started = clock.monotonic_ms()
  let reply =
    api.call_raw_with_retry(
      "session",
      fields,
      clock.deadline_in(fetch_timeout_ms()),
    )
  log_fetch(reply, clock.monotonic_ms() - started, peer)
  case reply {
    Error(api.ApiHttp(401)) -> Error(StartAuthFailed)
    Error(api.ApiHttp(429)) -> Error(StartRateLimited(unavailable_retry_ms()))
    Error(_) -> Error(StartUnavailable(unavailable_retry_ms()))
    Ok(data) ->
      case seed.from_api(session_id, identify, peer.api_version, data) {
        Error(seed.MissingUser) -> Error(StartUnknownError)
        Error(seed.ShardingRequired) -> Error(StartShardingRequired)
        Ok(found) -> admit(found)
      }
  }
}

fn add_coordinate(
  fields: List(#(String, json.Json)),
  key: String,
  value: Option(String),
) -> List(#(String, json.Json)) {
  case value {
    Some(text) -> [#(key, json.string(text)), ..fields]
    None -> fields
  }
}

fn add_flag(
  fields: List(#(String, json.Json)),
  key: String,
  on: Bool,
) -> List(#(String, json.Json)) {
  case on {
    True -> [#(key, json.bool(True)), ..fields]
    False -> fields
  }
}

fn log_fetch(
  reply: Result(a, api.ApiError),
  latency_ms: Int,
  peer: Peer,
) -> Nil {
  case reply {
    Ok(_) -> Nil
    Error(_) ->
      logging.log(
        logging.Warning,
        "Session RPC failed: latency_ms="
          <> int.to_string(latency_ms)
          <> " peer_ip="
          <> peer.ip,
      )
  }
}

fn admit(found: Seed) -> Result(Seed, StartError) {
  case rollout.is_session_eligible(user.id(found.user)) {
    False -> Error(StartUnavailable(unavailable_retry_ms()))
    True -> Ok(found)
  }
}

fn reserve(found: Seed) -> Result(Bool, StartError) {
  case user.is_bot(found.user) {
    True -> Ok(False)
    False ->
      case leases.reserve(process.self(), user.id(found.user)) {
        Ok(Nil) -> Ok(True)
        Error(_) -> Error(StartTooManySessions)
      }
  }
}

fn release_reservation(reserved: Bool) -> Nil {
  case reserved {
    True -> leases.release(process.self())
    False -> Nil
  }
}

fn finish(
  shard: Shard,
  pending: Pending,
  result: Result(Seed, StartError),
) -> Shard {
  let shard = case
    result
    |> result.try(fn(found) {
      reserve(found) |> result.map(fn(reserved) { #(found, reserved) })
    })
  {
    Error(reason) -> {
      process.send(pending.reply, Error(reason))
      shard
    }
    Ok(#(found, reserved)) -> {
      let outcome =
        launch(shard, found, Some(pending.socket), Some(pending.reply))
      release_reservation(reserved)
      case outcome {
        Ok(#(shard, _)) -> shard
        Error(reason) -> {
          process.send(pending.reply, Error(reason))
          shard
        }
      }
    }
  }
  process.send(pending.settled, Nil)
  shard
}

fn launch(
  shard: Shard,
  found: Seed,
  socket: Option(SocketRef),
  reply: Option(Subject(Result(SessionRef, StartError))),
) -> Result(#(Shard, SessionRef), StartError) {
  case
    factory.start_child(
      sessions_factory(shard),
      server.Start(found, socket, reply),
    )
  {
    Ok(started) -> {
      let monitor = process.monitor(started.pid)
      Ok(#(
        Shard(
          ..shard,
          sessions: dict.insert(shard.sessions, monitor, started.data),
        ),
        started.data,
      ))
    }
    Error(_) -> Error(StartUnavailable(unavailable_retry_ms()))
  }
}

fn adopt(
  shard: Shard,
  found: Seed,
  reply: Subject(Result(SessionRef, Nil)),
) -> Shard {
  let key = session.session_id_to_string(found.id)
  case draining.is_draining(), registry.lookup(key) {
    True, _ -> {
      logging.log(
        logging.Info,
        "session_transfer_refused_draining: session_id=" <> key,
      )
      process.send(reply, Error(Nil))
      shard
    }
    False, Ok(_) -> {
      logging.log(
        logging.Warning,
        "session_transfer_duplicate: session_id=" <> key,
      )
      process.send(reply, Error(Nil))
      shard
    }
    False, Error(Nil) ->
      case launch(shard, found, None, None) {
        Ok(#(shard, ref)) -> {
          process.send(reply, Ok(ref))
          shard
        }
        Error(_) -> {
          logging.log(
            logging.Warning,
            "session_transfer_adopt_failed: session_id=" <> key,
          )
          process.send(reply, Error(Nil))
          shard
        }
      }
  }
}

fn resume(
  session_id: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  let elsewhere = fn() {
    resume_elsewhere(session_id, token, seq, socket, reply)
  }
  case registry.lookup(session_id) {
    Ok(ref) -> spawn_ask(ref, token, seq, socket, reply, elsewhere)
    Error(Nil) -> {
      process.spawn_unlinked(elsewhere)
      Nil
    }
  }
}

fn spawn_ask(
  ref: SessionRef,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
  vanished: fn() -> Nil,
) -> Nil {
  process.spawn_unlinked(fn() {
    case process.subject_owner(ref.inbox) {
      Error(Nil) -> vanished()
      Ok(pid) ->
        case ask_resume(pid, token, seq, socket) {
          Ok(Answered(result)) -> process.send(reply, result)
          Ok(Vanished) -> vanished()
          Error(Nil) ->
            process.send(
              reply,
              Error(ResumeUnavailable(unavailable_retry_ms())),
            )
        }
    }
  })
  Nil
}

fn ask_resume(
  pid: process.Pid,
  token: String,
  seq: Int,
  socket: SocketRef,
) -> Result(RemoteResume, Nil) {
  let monitor = process.monitor(pid)
  let answer = process.new_subject()
  process.send(control.subject(pid), control.Resume(token, seq, socket, answer))
  let outcome =
    process.new_selector()
    |> process.select_map(answer, Answered)
    |> process.select_specific_monitor(monitor, fn(_) { Vanished })
    |> process.selector_receive(remote_resume_timeout_ms())
  process.demonitor_process(monitor)
  outcome
}

fn resume_elsewhere(
  session_id: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  let owner =
    transfer.locate_elsewhere(session_id)
    |> result.try(fn(ref) { process.subject_owner(ref.inbox) })
  case owner {
    Error(Nil) -> not_found_elsewhere(session_id, token, seq, socket, reply)
    Ok(pid) ->
      case ask_resume(pid, token, seq, socket) {
        Ok(Answered(result)) -> process.send(reply, result)
        Ok(Vanished) ->
          not_found_elsewhere(session_id, token, seq, socket, reply)
        Error(Nil) ->
          process.send(reply, Error(ResumeUnavailable(unavailable_retry_ms())))
      }
  }
}

fn not_found_elsewhere(
  session_id: String,
  token: String,
  seq: Int,
  socket: SocketRef,
  reply: Subject(Result(SessionRef, ResumeError)),
) -> Nil {
  let delivery =
    shards.deliver(session_id, fn(settled) {
      NotFoundElsewhere(session_id, token, seq, socket, reply, settled)
    })
  case delivery {
    shards.NoShard | shards.ShardDown -> process.send(reply, Error(SessionGone))
    shards.Settled | shards.Unanswered -> Nil
  }
}

fn on_down(shard: Shard, down: Down) -> Shard {
  case down {
    process.ProcessDown(monitor, pid, reason) ->
      case dict.get(shard.sessions, monitor) {
        Ok(ref) -> {
          leases.release(pid)
          registry.remove(ref)
          process_registry.unregister_pid(
            process_registry.sessions,
            ref.id,
            pid,
          )
          Shard(..shard, sessions: dict.delete(shard.sessions, monitor))
        }
        Error(Nil) -> pending_down(shard, monitor, reason)
      }
    process.PortDown(..) -> shard
  }
}

fn pending_down(
  shard: Shard,
  monitor: Monitor,
  reason: process.ExitReason,
) -> Shard {
  let found =
    shard.pending
    |> dict.to_list
    |> list.find(fn(entry) {
      let #(_, pending) = entry
      pending.worker_monitor == monitor || pending.socket_monitor == monitor
    })
  case found {
    Error(Nil) -> shard
    Ok(#(token, pending)) -> {
      let socket_gone = pending.socket_monitor == monitor
      case socket_gone {
        True -> process.demonitor_process(pending.worker_monitor)
        False -> process.demonitor_process(pending.socket_monitor)
      }
      case socket_gone, reason {
        True, _ -> Nil
        False, process.Normal -> Nil
        False, _ ->
          process.send(
            pending.reply,
            Error(StartUnavailable(unavailable_retry_ms())),
          )
      }
      process.send(pending.settled, Nil)
      Shard(..shard, pending: dict.delete(shard.pending, token))
    }
  }
}
