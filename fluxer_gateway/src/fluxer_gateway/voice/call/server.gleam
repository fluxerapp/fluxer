// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/message.{type MoveId}
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move as handoff_move
import fluxer_gateway/presence/client as presence
import fluxer_gateway/push/job
import fluxer_gateway/push/publisher
import fluxer_gateway/push/recipients as push_recipients
import fluxer_gateway/session/control
import fluxer_gateway/voice/call/handoff/codec as call_codec
import fluxer_gateway/voice/call/state.{
  type Call, type Caller, type Init, type Member, type View, Call, PendingJoin,
  Slot,
}
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/metrics as voice_metrics
import fluxer_gateway/voice/p2p
import fluxer_gateway/voice/store
import fluxer_gateway/voice/token
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set

fn init_timeout_ms() -> Int {
  durations.voice().call_server_init_timeout_ms
}

fn ask_timeout_ms() -> Int {
  durations.voice().server_ask_timeout_ms
}

fn ring_timeout_ms() -> Int {
  durations.voice().server_ring_timeout_ms
}

fn idle_timeout_ms() -> Int {
  durations.voice().server_idle_timeout_ms
}

fn pending_timeout_ms() -> Int {
  durations.voice().server_pending_timeout_ms
}

fn handoff_timeout_ms() -> Int {
  durations.voice().server_handoff_timeout_ms
}

fn forward_grace_ms() -> Int {
  durations.voice().server_forward_grace_ms
}

fn rebind_timeout_ms() -> Int {
  durations.voice().server_rebind_timeout_ms
}

pub opaque type Message {
  GetState(reply: Subject(View))
  UpdateRegion(region: Option(String), reply: Subject(Nil))
  Ring(users: List(UserId), caller: Option(Caller), reply: Subject(Nil))
  StopRinging(users: List(UserId), reply: Subject(Nil))
  Join(
    user: UserId,
    value: Entry,
    session_id: String,
    member: Member,
    connection: Option(ConnectionId),
    max: Option(Int),
    geo: Geo,
    reply: Subject(Result(Nil, GatewayError)),
  )
  Confirm(connection: ConnectionId, reply: Subject(Json))
  DisconnectUser(
    user: UserId,
    channel: ChannelId,
    connection: Option(ConnectionId),
    reply: Subject(Json),
  )
  Leave(session_id: String, reply: Subject(Result(Nil, Nil)))
  DropConnection(
    session_id: String,
    connection: ConnectionId,
    reply: Subject(Result(Nil, Nil)),
  )
  Rebind(session_id: String, member: Member)
  UpdateVoiceState(user: UserId, value: Entry, reply: Subject(Result(Nil, Nil)))
  Signal(session_id: String, to: String, data: RawJson)
  RoomKey(reply: Subject(String))
  Reissued(
    session_id: String,
    connection: ConnectionId,
    payload: RawJson,
    ack: Subject(Nil),
  )
  Reissuer(redirect: Subject(Subject(Message)))
  ReissuerDone(redirect: Subject(Subject(Message)))
  Export(reply: Subject(Call))
  Absorb(call: Call)
  StopForHandoff(target: Subject(Message))
  Thaw(generation: Option(Int))
  SessionDown(process.Down)
  RingTimeout(UserId)
  PendingTimeout(ConnectionId)
  RebindTimeout(session_id: String, detached_at: Int)
  IdleTimeout
  Moving(MoveSignal)
}

pub opaque type MoveSignal {
  MoveFreeze(
    move: MoveId,
    version: Int,
    epoch: Epoch,
    reply: Subject(Result(BitArray, MoveRefusal)),
  )
  MoveCommit(target: Subject(Message), reply: Subject(Result(ChannelId, Nil)))
  MoveThaw(reply: Subject(Bool))
  MoveRelease(seq: Int, bytes: BitArray)
  MoveSeal(last_seq: Int, reply: Subject(Bool))
  MoveAbort
  MoveProbe(reply: Subject(Bool))
  MoveGraceOver
}

pub type MoveRefusal {
  MoveBusy
  MoveUnencodable
}

type Mode {
  Serving
  Frozen(move: MoveId, version: Int, epoch: Int, held: List(Message))
  Handing(target: Subject(Message))
  Arriving(
    tail: Option(call_codec.Tail),
    applied: Int,
    early: Dict(Int, BitArray),
    stash: List(Message),
  )
}

pub type Start {
  Fresh(Init)
  Restored(Call)
  Arrived(call_codec.Tail)
}

type Server {
  Server(
    self: Subject(Message),
    call: Call,
    frozen: Option(List(Message)),
    generation: Int,
    reissuers: List(Subject(Subject(Message))),
    located: Dict(ConnectionId, Geo),
    mode: Mode,
  )
}

type Delivery {
  Acked
  Moved(Subject(Message))
  Gone
}

type Outcome {
  Running(Server)
  Ended
}

pub fn start(
  start: Start,
) -> Result(actor.Started(Subject(Message)), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let #(call, located, mode) = case start {
      Fresh(init) -> #(init_fresh(self, init), dict.new(), Serving)
      Restored(call) -> #(restore(self, call), dict.new(), Serving)
      Arrived(tail) -> #(
        tail.call,
        tail.located,
        Arriving(tail: Some(tail), applied: 0, early: dict.new(), stash: []),
      )
    }
    let selector =
      process.new_selector()
      |> process.select(self)
      |> process.select_map(move_subject(process.self()), Moving)
      |> process.select_monitors(SessionDown)
    actor.initialised(Server(
      self:,
      call:,
      frozen: None,
      generation: 0,
      reissuers: [],
      located:,
      mode:,
    ))
    |> actor.selecting(selector)
    |> actor.returning(self)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn now() -> Int {
  clock.system_ms()
}

fn ask(
  subject: Subject(Message),
  timeout: Int,
  make: fn(Subject(a)) -> Message,
) -> Result(a, Nil) {
  let reply = process.new_subject()
  process.send(subject, make(reply))
  process.receive(reply, timeout)
}

pub type AskError {
  CallGone
  CallFailed
}

fn ask_watched(
  subject: Subject(Message),
  timeout: Int,
  make: fn(Subject(a)) -> Message,
) -> Result(a, AskError) {
  case process.subject_owner(subject) {
    Error(Nil) ->
      ask(subject, timeout, make) |> result.replace_error(CallFailed)
    Ok(pid) -> {
      let deadline = clock.monotonic_ms() + timeout
      let monitor = process.monitor(pid)
      let reply = process.new_subject()
      process.send(subject, make(reply))
      let outcome =
        process.new_selector()
        |> process.select_map(reply, Ok)
        |> process.select_specific_monitor(monitor, Error)
        |> process.selector_receive(timeout)
      process.demonitor_process(monitor)
      case outcome {
        Ok(Ok(value)) -> Ok(value)
        Ok(Error(down)) ->
          case handed_off(down) {
            True ->
              process.receive(
                reply,
                int.max(deadline - clock.monotonic_ms(), 0),
              )
              |> result.replace_error(CallFailed)
            False -> Error(down_error(down))
          }
        Error(Nil) -> Error(CallFailed)
      }
    }
  }
}

fn handoff_reason() -> #(atom.Atom, atom.Atom) {
  #(atom.create("shutdown"), atom.create("handoff"))
}

fn handoff_decoder() -> Decoder(Bool) {
  use first <- decode.field(0, atom.decoder())
  use second <- decode.field(1, atom.decoder())
  use longer <- decode.optional_field(2, False, decode.success(True))
  decode.success(#(first, second) == handoff_reason() && !longer)
}

fn noproc(term: process.ExitReason) -> Bool {
  case term {
    process.Abnormal(reason) ->
      decode.run(reason, atom.decoder()) == Ok(atom.create("noproc"))
    process.Normal | process.Killed -> False
  }
}

fn handed_off(down: process.Down) -> Bool {
  case down {
    process.ProcessDown(reason:, ..) -> handed_off_reason(reason)
    process.PortDown(..) -> False
  }
}

fn handed_off_reason(reason: process.ExitReason) -> Bool {
  case reason {
    process.Abnormal(term) ->
      decode.run(term, handoff_decoder()) |> result.unwrap(False)
    process.Normal | process.Killed -> False
  }
}

fn down_error(down: process.Down) -> AskError {
  case down {
    process.ProcessDown(reason:, ..) ->
      case noproc(reason) {
        True -> CallGone
        False -> CallFailed
      }
    process.PortDown(..) -> CallFailed
  }
}

pub fn get_state(subject: Subject(Message), timeout: Int) -> Result(View, Nil) {
  ask(subject, timeout, GetState)
}

pub fn fetch_state(
  subject: Subject(Message),
  timeout: Int,
) -> Result(View, AskError) {
  ask_watched(subject, timeout, GetState)
}

pub fn update_region(
  subject: Subject(Message),
  region: Option(String),
  timeout: Int,
) -> Result(Nil, Nil) {
  ask(subject, timeout, UpdateRegion(region, _))
}

pub fn ring(
  subject: Subject(Message),
  users: List(UserId),
  caller: Option(Caller),
  timeout: Int,
) -> Result(Nil, Nil) {
  ask(subject, timeout, Ring(users, caller, _))
}

pub fn stop_ringing(
  subject: Subject(Message),
  users: List(UserId),
  timeout: Int,
) -> Result(Nil, Nil) {
  ask(subject, timeout, StopRinging(users, _))
}

pub fn join(
  subject: Subject(Message),
  user: UserId,
  value: Entry,
  session_id: String,
  member: Member,
  connection: Option(ConnectionId),
  max: Option(Int),
  geo: Geo,
) -> Result(Result(Nil, GatewayError), Nil) {
  ask(subject, ask_timeout_ms(), Join(
    user,
    value,
    session_id,
    state.for_peer(member, speaks_moves(subject)),
    connection,
    max,
    geo,
    _,
  ))
}

pub fn confirm(
  subject: Subject(Message),
  connection: ConnectionId,
  timeout: Int,
) -> Result(Json, AskError) {
  ask_watched(subject, timeout, Confirm(connection, _))
}

pub fn disconnect_user(
  subject: Subject(Message),
  user: UserId,
  channel: ChannelId,
  connection: Option(ConnectionId),
  timeout: Int,
) -> Result(Json, AskError) {
  ask_watched(subject, timeout, DisconnectUser(user, channel, connection, _))
}

pub fn leave(subject: Subject(Message), session_id: String) -> Nil {
  let _ = ask(subject, ask_timeout_ms(), Leave(session_id, _))
  Nil
}

pub fn drop_connection(
  subject: Subject(Message),
  session_id: String,
  connection: ConnectionId,
) -> Nil {
  let _ =
    ask(subject, ask_timeout_ms(), DropConnection(session_id, connection, _))
  Nil
}

pub fn rebind(
  subject: Subject(Message),
  session_id: String,
  member: Member,
) -> Nil {
  process.send(
    subject,
    Rebind(session_id, state.for_peer(member, speaks_moves(subject))),
  )
}

fn speaks_moves(subject: Subject(Message)) -> Bool {
  case process.subject_owner(subject) {
    Ok(pid) -> wire.pid_speaks(pid, wire.move_forms_epoch)
    Error(Nil) -> True
  }
}

pub fn update_voice_state(
  subject: Subject(Message),
  user: UserId,
  value: Entry,
  timeout: Int,
) -> Result(Nil, Nil) {
  ask(subject, timeout, UpdateVoiceState(user, value, _))
  |> result.flatten
}

pub fn signal(
  subject: Subject(Message),
  session_id: String,
  to: String,
  data: RawJson,
) -> Nil {
  process.send(subject, Signal(session_id, to, data))
}

pub fn room_key(
  subject: Subject(Message),
  timeout: Int,
) -> Result(String, Nil) {
  ask(subject, timeout, RoomKey)
}

pub fn export(subject: Subject(Message), timeout: Int) -> Result(Call, Nil) {
  ask(subject, timeout, Export)
}

pub fn absorb(subject: Subject(Message), call: Call) -> Nil {
  process.send(subject, Absorb(call))
}

pub fn stop_for_handoff(
  subject: Subject(Message),
  target: Subject(Message),
) -> Nil {
  process.send(subject, StopForHandoff(target))
}

pub fn thaw(subject: Subject(Message)) -> Nil {
  process.send(subject, Thaw(None))
}

pub fn connections(
  subject: Subject(Message),
  timeout: Int,
) -> Result(List(Entry), Nil) {
  get_state(subject, timeout)
  |> result.map(fn(view) { view.connections })
}

fn init_fresh(self: Subject(Message), init: Init) -> Call {
  let call = state.new(init, now()) |> state.put_caller(init.caller)
  let call = Call(..call, initiator_ready: True)
  let #(call, _) = dispatch_pending_ringing(self, call, False)
  let call = reset_idle(self, call)
  voice_metrics.call_started()
  dispatch_create(call)
}

fn restore(self: Subject(Message), call: Call) -> Call {
  let call =
    Call(
      ..call,
      sessions: monitor_members(call.sessions, call.detached),
      ring_timers: dict.new(),
      idle_timer: None,
    )
    |> start_ring_timers(self, _, call.ringing)
  dict.each(call.pending, fn(conn, pending) {
    let elapsed = now() - pending.joined_at
    let delay = int.max(pending_timeout_ms() - elapsed, 0)
    process.send_after(self, delay, PendingTimeout(conn))
    Nil
  })
  arm_rebind_timers(self, call.detached)
  list.each(state.entries(call), count_table.upsert)
  reset_idle(self, call)
}

fn monitor_members(
  sessions: Dict(String, Member),
  detached: Dict(String, Int),
) -> Dict(String, Member) {
  dict.map_values(sessions, fn(session_id, member) {
    case dict.has_key(detached, session_id) {
      True -> state.Member(..member, monitor: None, owner_monitor: None)
      False -> watch(member)
    }
  })
}

fn arm_rebind_timers(
  self: Subject(Message),
  detached: Dict(String, Int),
) -> Nil {
  dict.each(detached, fn(session_id, detached_at) {
    let delay = int.max(rebind_timeout_ms() - { now() - detached_at }, 0)
    process.send_after(self, delay, RebindTimeout(session_id, detached_at))
    Nil
  })
}

fn merge(self: Subject(Message), call: Call, imported: Call) -> Call {
  let fresh_sessions =
    dict.filter(imported.sessions, fn(session_id, _member) {
      !dict.has_key(call.sessions, session_id)
    })
  let imported_detached =
    dict.filter(imported.detached, fn(session_id, _at) {
      dict.has_key(fresh_sessions, session_id)
    })
  let imported_sessions = monitor_members(fresh_sessions, imported_detached)
  arm_rebind_timers(self, imported_detached)
  let imported_pending =
    dict.filter(imported.pending, fn(conn, _pending) {
      !dict.has_key(call.pending, conn)
    })
  dict.each(imported_pending, fn(conn, pending) {
    let delay = int.max(pending_timeout_ms() - { now() - pending.joined_at }, 0)
    process.send_after(self, delay, PendingTimeout(conn))
    Nil
  })
  let ordered = fn(source: Call) {
    source.voice_states
    |> store.to_list
    |> list.sort(fn(a, b) { int.compare({ a.1 }.seq, { b.1 }.seq) })
  }
  let #(voice_states, next_seq) =
    list.append(ordered(imported), ordered(call))
    |> list.fold(#(store.new(), 0), fn(acc, pair) {
      let #(states, seq) = acc
      let #(key, slot) = pair
      #(store.put(states, key, Slot(..slot, seq:)), seq + 1)
    })
  let merged =
    Call(
      ..call,
      message: imported.message,
      created_at: imported.created_at,
      room_key: option.or(imported.room_key, call.room_key),
      region: option.or(call.region, imported.region),
      caller: option.or(call.caller, imported.caller),
      recipients: state.unique_users(list.append(
        imported.recipients,
        call.recipients,
      )),
      voice_states:,
      next_seq:,
      sessions: dict.merge(imported_sessions, call.sessions),
      detached: dict.merge(imported_detached, call.detached),
      pending: dict.merge(imported_pending, call.pending),
      initiator_ready: call.initiator_ready || imported.initiator_ready,
      history: set.union(imported.history, call.history),
    )
  let connected = state.connected_users(merged)
  let still_ringing = fn(user) { !list.contains(connected, user) }
  let ringing =
    list.append(call.ringing, imported.ringing)
    |> list.filter(still_ringing)
    |> state.unique_users
  let pending_ringing =
    list.append(call.pending_ringing, imported.pending_ringing)
    |> list.filter(still_ringing)
    |> state.unique_users
  let answered =
    list.filter(list.append(call.ringing, imported.ringing), fn(user) {
      !still_ringing(user)
    })
  let merged =
    Call(..cancel_ring_timers(merged, answered), ringing:, pending_ringing:)
    |> start_ring_timers(self, _, ringing)
  sync_table(call, merged)
  let #(merged, _) = dispatch_pending_ringing(self, merged, False)
  merged
  |> reset_idle(self, _)
  |> publish_update
}

fn handle(server: Server, message: Message) -> actor.Next(Server, Message) {
  case message, server.mode {
    Moving(signal), _ -> on_move(server, signal)
    _, Frozen(..) -> actor.continue(hold(server, message))
    _, Handing(target:) -> {
      pass_on(target, message)
      actor.continue(server)
    }
    _, Arriving(..) -> actor.continue(stash(server, message))
    _, Serving ->
      case server.frozen {
        None -> finish(step(server, message))
        Some(deferred) -> frozen(server, deferred, message)
      }
  }
}

fn move_subject(pid: Pid) -> Subject(MoveSignal) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_call_handoff")),
  )
}

fn ask_move(pid: Pid, make: fn(Subject(a)) -> MoveSignal) -> Result(a, Nil) {
  let reply = process.new_subject()
  let monitor = process.monitor(pid)
  process.send(move_subject(pid), make(reply))
  let answer =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(handoff_timeout_ms())
  process.demonitor_process(monitor)
  case answer {
    Ok(Ok(found)) -> Ok(found)
    Ok(Error(Nil)) | Error(Nil) -> Error(Nil)
  }
}

pub fn freeze_for_move(
  pid: Pid,
  move: MoveId,
  version: Int,
  at: Epoch,
) -> Result(Result(BitArray, MoveRefusal), Nil) {
  ask_move(pid, MoveFreeze(move, version, at, _))
}

pub fn commit_move(
  pid: Pid,
  target: Subject(Message),
) -> Result(Result(ChannelId, Nil), Nil) {
  ask_move(pid, MoveCommit(target, _))
}

pub fn thaw_move(pid: Pid) -> Result(Bool, Nil) {
  ask_move(pid, MoveThaw)
}

pub fn thaw_late(pid: Pid) -> Nil {
  process.send(move_subject(pid), MoveThaw(process.new_subject()))
}

pub fn release_moved(pid: Pid, seq: Int, bytes: BitArray) -> Nil {
  process.send(move_subject(pid), MoveRelease(seq, bytes))
}

pub fn seal_moved(pid: Pid, last_seq: Int) -> Result(Bool, Nil) {
  ask_move(pid, MoveSeal(last_seq, _))
}

pub fn abort_move(pid: Pid) -> Nil {
  process.send(move_subject(pid), MoveAbort)
}

pub fn is_leaving(pid: Pid) -> Result(Bool, Nil) {
  ask_move(pid, MoveProbe)
}

fn on_move(server: Server, signal: MoveSignal) -> actor.Next(Server, Message) {
  case signal, server.mode {
    MoveFreeze(move:, version:, epoch:, reply:), Serving ->
      case server.frozen {
        Some(_) -> {
          process.send(reply, Error(MoveBusy))
          actor.continue(server)
        }
        None -> actor.continue(freeze_move(server, move, version, epoch, reply))
      }
    MoveFreeze(reply:, ..), _ -> {
      process.send(reply, Error(MoveBusy))
      actor.continue(server)
    }
    MoveCommit(target:, reply:), Frozen(..) -> {
      process.send(reply, Ok(server.call.channel))
      actor.continue(commit_move_to(server, target))
    }
    MoveCommit(reply:, ..), _ -> {
      process.send(reply, Error(Nil))
      actor.continue(server)
    }
    MoveThaw(reply:), Frozen(held:, ..) -> {
      process.send(reply, True)
      Server(..server, mode: Serving)
      |> replay(list.reverse(held))
      |> finish
    }
    MoveThaw(reply:), _ -> {
      process.send(reply, False)
      actor.continue(server)
    }
    MoveRelease(seq:, bytes:), Arriving(applied:, early:, ..) as mode ->
      case seq > applied {
        False -> actor.continue(server)
        True ->
          Server(
            ..server,
            mode: Arriving(..mode, early: dict.insert(early, seq, bytes)),
          )
          |> activate
          |> drain_released
          |> finish
      }
    MoveSeal(last_seq:, reply:), Arriving(..) ->
      case drain_released(activate(server)) {
        Ended -> {
          process.send(reply, False)
          actor.stop()
        }
        Running(server) -> {
          let #(complete, stashed) = case server.mode {
            Arriving(applied:, stash:, ..) -> #(
              applied >= last_seq,
              list.reverse(stash),
            )
            _ -> #(False, [])
          }
          process.send(reply, complete)
          Server(..server, mode: Serving)
          |> replay(stashed)
          |> finish
        }
      }
    MoveAbort, Arriving(tail: Some(_), ..) | MoveAbort, Frozen(..) ->
      actor.stop()
    MoveProbe(reply:), mode -> {
      process.send(reply, case mode {
        Frozen(..) | Handing(..) -> True
        Serving | Arriving(..) -> False
      })
      actor.continue(server)
    }
    MoveGraceOver, Handing(..) -> {
      process.send_abnormal_exit(process.self(), handoff_reason())
      actor.stop()
    }
    MoveRelease(..), _ | MoveSeal(..), _ | MoveAbort, _ | MoveGraceOver, _ ->
      actor.continue(server)
  }
}

fn freeze_move(
  server: Server,
  move: MoveId,
  version: Int,
  at: Epoch,
  reply: Subject(Result(BitArray, MoveRefusal)),
) -> Server {
  case
    handoff_codec.encode(
      call_codec.tail(),
      call_codec.Tail(call: server.call, located: server.located),
      version,
      epoch.to_int(at),
    )
  {
    Ok(tail) -> {
      process.send(reply, Ok(tail))
      Server(
        ..server,
        mode: Frozen(move:, version:, epoch: epoch.to_int(at), held: []),
      )
    }
    Error(_) -> {
      process.send(reply, Error(MoveUnencodable))
      server
    }
  }
}

fn hold(server: Server, message: Message) -> Server {
  case server.mode, message {
    _, Reissuer(redirect) ->
      Server(..server, reissuers: [redirect, ..server.reissuers])
    _, ReissuerDone(redirect) -> forget_reissuer(server, redirect)
    Frozen(move:, version:, epoch:, held:) as mode, _ -> {
      case forwardable(message) {
        True -> forward_moved(move, version, epoch, message)
        False -> Nil
      }
      Server(..server, mode: Frozen(..mode, held: [message, ..held]))
    }
    _, _ -> server
  }
}

fn forward_moved(
  move: MoveId,
  version: Int,
  epoch: Int,
  message: Message,
) -> Nil {
  case handoff_codec.encode(call_codec.inbox(), message, version, epoch) {
    Ok(bytes) -> handoff_move.forward(move, bytes)
    Error(_) -> Nil
  }
}

fn commit_move_to(server: Server, target: Subject(Message)) -> Server {
  let move = case server.mode {
    Frozen(move:, ..) -> Some(move)
    _ -> None
  }
  case process.subject_owner(target) {
    Ok(successor) ->
      dict.each(server.call.sessions, fn(_, member) {
        case process.subject_owner(member.inbox) {
          Ok(session) ->
            process.send(
              control.subject(session),
              control.CallMonitor(server.call.channel, successor),
            )
          Error(Nil) -> Nil
        }
      })
    Error(Nil) -> Nil
  }
  redirect_reissuers(server, target)
  count_table.drop_owner(process.self())
  option.map(move, handoff_move.repointed)
  let _ =
    process.send_after(
      move_subject(process.self()),
      forward_grace_ms(),
      MoveGraceOver,
    )
  Server(..server, mode: Handing(target:))
}

fn pass_on(target: Subject(Message), message: Message) -> Nil {
  case message {
    Reissuer(redirect) -> process.send(redirect, target)
    _ ->
      case forwardable(message) {
        True -> process.send(target, message)
        False -> Nil
      }
  }
}

fn stash(server: Server, message: Message) -> Server {
  case server.mode {
    Arriving(stash:, ..) as mode ->
      Server(..server, mode: Arriving(..mode, stash: [message, ..stash]))
    _ -> server
  }
}

fn activate(server: Server) -> Server {
  case server.mode {
    Arriving(tail: Some(tail), ..) as mode ->
      Server(
        ..server,
        call: restore(server.self, tail.call),
        mode: Arriving(..mode, tail: None),
      )
    _ -> server
  }
}

fn drain_released(server: Server) -> Outcome {
  case server.mode {
    Arriving(applied:, early:, ..) as mode ->
      case dict.get(early, applied + 1) {
        Error(Nil) -> Running(server)
        Ok(bytes) -> {
          let server =
            Server(
              ..server,
              mode: Arriving(
                ..mode,
                applied: applied + 1,
                early: dict.delete(early, applied + 1),
              ),
            )
          case handoff_codec.decode(call_codec.inbox(), bytes) {
            Ok(#(_, message)) ->
              case step(server, message) {
                Running(server) -> drain_released(server)
                Ended -> Ended
              }
            Error(_) -> drain_released(server)
          }
        }
      }
    _ -> Running(server)
  }
}

fn finish(outcome: Outcome) -> actor.Next(Server, Message) {
  case outcome {
    Running(server) -> actor.continue(server)
    Ended -> actor.stop()
  }
}

fn frozen(
  server: Server,
  deferred: List(Message),
  message: Message,
) -> actor.Next(Server, Message) {
  case message {
    GetState(reply) -> {
      process.send(reply, state.view(server.call))
      actor.continue(server)
    }
    Export(reply) -> {
      process.send(reply, server.call)
      actor.continue(server)
    }
    StopForHandoff(target) -> {
      deferred
      |> list.reverse
      |> list.filter(forwardable)
      |> list.each(process.send(target, _))
      redirect_reissuers(server, target)
      process.send_abnormal_exit(process.self(), handoff_reason())
      actor.stop()
    }
    Reissuer(redirect) ->
      actor.continue(
        Server(..server, reissuers: [redirect, ..server.reissuers]),
      )
    ReissuerDone(redirect) -> actor.continue(forget_reissuer(server, redirect))
    Thaw(Some(generation)) if generation != server.generation ->
      actor.continue(server)
    Thaw(_) ->
      Server(..server, frozen: None)
      |> replay(list.reverse(deferred))
      |> finish
    other -> actor.continue(Server(..server, frozen: Some([other, ..deferred])))
  }
}

fn forwardable(message: Message) -> Bool {
  case message {
    SessionDown(_)
    | RingTimeout(_)
    | PendingTimeout(_)
    | RebindTimeout(..)
    | IdleTimeout
    | Reissued(..)
    | Reissuer(_)
    | ReissuerDone(_) -> False
    _ -> True
  }
}

fn redirect_reissuers(server: Server, target: Subject(Message)) -> Nil {
  list.each(server.reissuers, process.send(_, target))
}

fn forget_reissuer(
  server: Server,
  redirect: Subject(Subject(Message)),
) -> Server {
  Server(
    ..server,
    reissuers: list.filter(server.reissuers, fn(found) { found != redirect }),
  )
}

fn replay(server: Server, messages: List(Message)) -> Outcome {
  case messages {
    [] -> Running(server)
    [message, ..rest] ->
      case step(server, message) {
        Running(server) -> replay(server, rest)
        Ended -> Ended
      }
  }
}

fn step(server: Server, message: Message) -> Outcome {
  let self = server.self
  let call = server.call
  case message {
    GetState(reply) -> {
      process.send(reply, state.view(call))
      Running(server)
    }
    UpdateRegion(region, reply) -> {
      let old = call.region
      let call = Call(..call, region:)
      let call = case p2p.mode(state.entries(call)) {
        p2p.AllP2p ->
          convert_to_sfu(self, call, [], server.located) |> publish_update
        _ -> {
          let call = publish_update(call)
          case old != region && !store.is_empty(call.voice_states), region {
            True, Some(_) ->
              reissue(
                self,
                call,
                store.keys(call.voice_states),
                region,
                server.located,
              )
            _, _ -> call
          }
        }
      }
      process.send(reply, Nil)
      continue(server, call)
    }
    Ring(users, caller, reply) -> {
      let call = state.put_caller(call, caller)
      let connected = state.connected_users(call)
      let additions =
        list.filter(users, fn(user) { !list.contains(connected, user) })
      let call =
        Call(
          ..call,
          pending_ringing: state.unique_users(list.append(
            call.pending_ringing,
            additions,
          )),
        )
      let #(call, _) = dispatch_pending_ringing(self, call, True)
      process.send(reply, Nil)
      continue(server, call)
    }
    StopRinging(users, reply) -> {
      let call =
        call
        |> cancel_ring_timers(users)
        |> state.remove_users_from_ringing(users)
        |> publish_update
      process.send(reply, Nil)
      continue(server, call)
    }
    Join(user, value, session_id, member, connection, max, geo, reply) ->
      Server(..server, located: locate(server.located, value, geo))
      |> join_session(
        user,
        value,
        session_id,
        state.from_wire(member),
        connection,
        max,
      )
      |> reply_with(reply)
    Confirm(conn, reply) -> {
      let found = dict.has_key(call.pending, conn)
      case found || state.has_connection(call, conn) {
        False -> {
          process.send(
            reply,
            json.object([
              #("success", json.bool(False)),
              #("error", json.string("connection_not_found")),
            ]),
          )
          Running(server)
        }
        True -> {
          let call =
            Call(
              ..call,
              initiator_ready: True,
              pending: dict.delete(call.pending, conn),
            )
          let #(call, _) = dispatch_pending_ringing(self, call, True)
          let fields = case found {
            True -> [#("success", json.bool(True))]
            False -> [
              #("success", json.bool(True)),
              #("already_confirmed", json.bool(True)),
            ]
          }
          process.send(reply, json.object(fields))
          continue(server, call)
        }
      }
    }
    DisconnectUser(user, channel, conn, reply) ->
      disconnect_user_in(server, user, channel, conn, reply)
    Leave(session_id, reply) ->
      case dict.get(call.sessions, session_id) {
        Error(Nil) -> {
          process.send(reply, Error(Nil))
          Running(server)
        }
        Ok(member) -> {
          process.send(reply, Ok(Nil))
          drop_session(server, session_id, member)
        }
      }
    DropConnection(session_id, conn, reply) ->
      case
        state.session_entries(call, session_id)
        |> list.find(fn(pair) { voice_key.connection(pair.0) == conn }),
        dict.get(call.sessions, session_id)
      {
        Ok(#(key, _)), Ok(member) -> {
          process.send(reply, Ok(Nil))
          drop_slot(server, session_id, member, key)
        }
        _, _ -> {
          process.send(reply, Error(Nil))
          Running(server)
        }
      }
    UpdateVoiceState(user, value, reply) -> {
      case
        state.entry_at(call, entry.connection_id(value))
        |> result.map(fn(pair) {
          #(pair.0, pair.1, keep_routing(value, pair.1))
        })
      {
        Ok(#(key, old, value)) if old != value ->
          case entry.user(old) == user {
            True -> {
              let next =
                apply_update(self, call, key, old, value, server.located)
              sync_counts(call, next)
              process.send(reply, Ok(Nil))
              continue(server, publish_update(next))
            }
            False -> {
              process.send(reply, Error(Nil))
              Running(server)
            }
          }
        Ok(#(_, old, _)) -> {
          process.send(reply, case entry.user(old) == user {
            True -> Ok(Nil)
            False -> Error(Nil)
          })
          Running(server)
        }
        Error(Nil) -> {
          process.send(reply, Error(Nil))
          Running(server)
        }
      }
    }
    Signal(session_id, to, data) -> {
      case p2p.signal_route(state.entries(call), session_id, to) {
        Ok(#(sender, target)) ->
          case
            option.then(entry.session_id(target), fn(target_session) {
              dict.get(call.sessions, target_session) |> option.from_result
            })
          {
            Some(member) ->
              process.send(
                member.inbox,
                session.Dispatch(
                  "VOICE_SIGNAL",
                  p2p.signal_payload(None, call.channel, sender, data),
                ),
              )
            None -> Nil
          }
        Error(Nil) -> Nil
      }
      Running(server)
    }
    Reissued(session_id, conn, payload, ack) -> {
      process.send(ack, Nil)
      case state.entry_at(call, conn), dict.get(call.sessions, session_id) {
        Ok(#(_, value)), Ok(member) ->
          case entry.session_id(value) == Some(session_id) {
            True -> {
              voice_metrics.server_updates(voice_metrics.CallScope, 1)
              process.send(
                member.inbox,
                session.Dispatch("VOICE_SERVER_UPDATE", payload),
              )
            }
            False -> Nil
          }
        _, _ -> Nil
      }
      Running(server)
    }
    RoomKey(reply) -> {
      let #(key, call) = state.room_key(call)
      process.send(reply, key)
      continue(server, call)
    }
    Export(reply) -> {
      process.send(reply, call)
      let generation = server.generation + 1
      process.send_after(self, handoff_timeout_ms(), Thaw(Some(generation)))
      Running(Server(..server, frozen: Some([]), generation:))
    }
    Absorb(imported) -> continue(server, merge(self, call, imported))
    StopForHandoff(target) -> {
      redirect_reissuers(server, target)
      process.send_abnormal_exit(process.self(), handoff_reason())
      Ended
    }
    Reissuer(redirect) ->
      Running(Server(..server, reissuers: [redirect, ..server.reissuers]))
    ReissuerDone(redirect) -> Running(forget_reissuer(server, redirect))
    Thaw(_) -> Running(server)
    Rebind(session_id, member) -> {
      let member = state.from_wire(member)
      case state.rebind(call, session_id, member) {
        Error(Nil) -> Running(server)
        Ok(#(previous, call)) -> {
          unwatch(previous)
          let sessions = dict.insert(call.sessions, session_id, watch(member))
          list.each(state.entries(call), count_table.upsert)
          continue(server, Call(..call, sessions:))
        }
      }
    }
    SessionDown(process.ProcessDown(pid:, reason:, ..)) ->
      case
        dict.to_list(call.sessions)
        |> list.find(fn(pair) {
          let #(_, member) = pair
          option.is_some(member.monitor)
          && process.subject_owner(member.inbox) == Ok(pid)
        })
      {
        Ok(#(session_id, member)) ->
          case reason == process.Normal || handed_off_reason(reason) {
            True -> {
              let detached_at = now()
              process.send_after(
                self,
                rebind_timeout_ms(),
                RebindTimeout(session_id, detached_at),
              )
              continue(server, state.detach(call, session_id, detached_at))
            }
            False -> drop_session(server, session_id, member)
          }
        Error(Nil) ->
          case
            dict.to_list(call.sessions)
            |> list.find(fn(pair) {
              let #(_, member) = pair
              option.is_some(member.owner_monitor) && member.owner == Some(pid)
            })
          {
            Ok(#(session_id, member)) ->
              case owner_crashed(reason) {
                True -> drop_session(server, session_id, member)
                False -> {
                  let sessions =
                    dict.insert(
                      call.sessions,
                      session_id,
                      state.Member(..member, owner_monitor: None),
                    )
                  Running(Server(..server, call: Call(..call, sessions:)))
                }
              }
            Error(Nil) -> Running(server)
          }
      }
    SessionDown(_) -> Running(server)
    RebindTimeout(session_id, detached_at) ->
      case
        state.detach_expired(call, session_id, detached_at),
        dict.get(call.sessions, session_id)
      {
        True, Ok(member) -> drop_session(server, session_id, member)
        _, _ -> Running(server)
      }
    RingTimeout(user) ->
      case dict.has_key(call.ring_timers, id.user_id_to_int(user)) {
        False -> Running(server)
        True -> {
          let next =
            call
            |> cancel_ring_timers([user])
            |> state.remove_users_from_ringing([user])
            |> publish_update
          stop_if_idle(server, next)
        }
      }
    PendingTimeout(conn) ->
      case dict.get(call.pending, conn) {
        Error(Nil) -> Running(server)
        Ok(pending) -> {
          let call = Call(..call, pending: dict.delete(call.pending, conn))
          let held = state.session_held(call, pending)
          let in_voice = state.has_session_entries(call, pending.session_id)
          case held || in_voice {
            True -> continue(server, call)
            False -> continue(server, remove_session(call, pending.session_id))
          }
        }
      }
    Moving(_) -> Running(server)
    IdleTimeout ->
      case has_activity(call) {
        True -> continue(server, reset_idle(self, call))
        False -> {
          end_call(call)
          Ended
        }
      }
  }
}

fn reply_with(
  outcome: #(Outcome, Result(Nil, GatewayError)),
  reply: Subject(Result(Nil, GatewayError)),
) -> Outcome {
  process.send(reply, outcome.1)
  outcome.0
}

fn continue(server: Server, call: Call) -> Outcome {
  let live =
    state.entries(call)
    |> list.map(entry.connection_id)
    |> list.append(dict.keys(call.pending))
  Running(Server(..server, call:, located: dict.take(server.located, live)))
}

fn locate(
  located: Dict(ConnectionId, Geo),
  value: Entry,
  at: Geo,
) -> Dict(ConnectionId, Geo) {
  let conn = entry.connection_id(value)
  let known = dict.get(located, conn) |> result.unwrap(geo.unknown())
  let next = geo.or(at, known)
  case geo.is_known(next) {
    True -> dict.insert(located, conn, next)
    False -> located
  }
}

fn join_session(
  server: Server,
  user: UserId,
  value: Entry,
  session_id: String,
  member: Member,
  connection: Option(ConnectionId),
  max: Option(Int),
) -> #(Outcome, Result(Nil, GatewayError)) {
  let call = server.call
  let same_member = case dict.get(call.sessions, session_id) {
    Ok(existing) -> existing.user == user && existing.inbox == member.inbox
    Error(Nil) -> False
  }
  let current =
    state.entry_at(call, entry.connection_id(value))
    |> result.map(fn(pair) { pair.1 })
  let same_connection = case current {
    Ok(found) -> entry.session_id(found) == Some(session_id)
    Error(Nil) -> False
  }
  let candidate = state.put_entry(call, value)
  case same_member && same_connection {
    True ->
      case current == Ok(value) {
        True -> #(Running(server), Ok(Nil))
        False -> {
          let next = state.replace_entry(call, value)
          case join_violation(next, max) {
            Error(reason) -> #(Running(server), Error(reason))
            Ok(Nil) -> {
              sync_counts(call, next)
              #(continue(server, publish_update(next)), Ok(Nil))
            }
          }
        }
      }
    False ->
      case join_violation(candidate, max) {
        Error(reason) -> #(Running(server), Error(reason))
        Ok(Nil) -> {
          let next =
            admit(
              server.self,
              call,
              user,
              value,
              session_id,
              member,
              connection,
            )
          #(continue(server, next), Ok(Nil))
        }
      }
  }
}

fn join_violation(call: Call, max: Option(Int)) -> Result(Nil, GatewayError) {
  let connections = state.entries(call)
  case p2p.mode(connections) {
    p2p.Mixed -> p2p.invariant_violation(connections, max)
    _ ->
      connections
      |> list.map(fn(value) { #(entry.user(value), value) })
      |> dict.from_list
      |> dict.values
      |> p2p.invariant_violation(max)
  }
}

fn keep_routing(value: Entry, old: Entry) -> Entry {
  entry.set_routing(
    value,
    option.or(entry.region_id(value), entry.region_id(old)),
    option.or(entry.server_id(value), entry.server_id(old)),
  )
}

fn apply_update(
  self: Subject(Message),
  call: Call,
  key: VoiceKey,
  old: Entry,
  value: Entry,
  located: Dict(ConnectionId, Geo),
) -> Call {
  case entry.is_p2p(old), entry.is_p2p(value) {
    True, False ->
      convert_to_sfu(self, state.put_entry(call, value), [key], located)
    False, True -> {
      let converted = entry.to_sfu(value)
      let next = state.put_entry(call, converted)
      notify_converted(next, converted)
      dispatch_voice_state(next, converted)
      next
    }
    _, _ -> state.put_entry(call, value)
  }
}

fn admit(
  self: Subject(Message),
  call: Call,
  user: UserId,
  value: Entry,
  session_id: String,
  member: Member,
  connection: Option(ConnectionId),
) -> Call {
  let base =
    call
    |> cancel_ring_timers([user])
    |> state.remove_users_from_ringing([user])
  let base = remove_session(base, session_id)
  let pending = case entry.is_p2p(value), connection {
    False, Some(conn) -> {
      process.send_after(self, pending_timeout_ms(), PendingTimeout(conn))
      dict.insert(base.pending, conn, PendingJoin(user, session_id, now()))
    }
    _, _ -> base.pending
  }
  let next =
    Call(
      ..base,
      sessions: dict.insert(base.sessions, session_id, watch(member)),
      pending:,
      history: set.insert(base.history, user),
    )
    |> state.put_entry(value)
    |> reset_idle(self, _)
  sync_counts(call, next)
  publish_update(next)
}

fn watch(member: Member) -> Member {
  state.Member(
    ..member,
    monitor: process.subject_owner(member.inbox)
      |> result.map(process.monitor)
      |> option.from_result,
    owner_monitor: option.map(member.owner, process.monitor),
  )
}

fn unwatch(member: Member) -> Nil {
  option.map(member.monitor, process.demonitor_process)
  option.map(member.owner_monitor, process.demonitor_process)
  Nil
}

fn owner_crashed(reason: process.ExitReason) -> Bool {
  case reason {
    process.Normal -> False
    process.Killed -> True
    process.Abnormal(_) -> !noproc(reason) && !handed_off_reason(reason)
  }
}

fn remove_session(call: Call, session_id: String) -> Call {
  case dict.get(call.sessions, session_id) {
    Ok(member) -> {
      unwatch(member)
      Call(
        ..call,
        sessions: dict.delete(call.sessions, session_id),
        detached: dict.delete(call.detached, session_id),
      )
    }
    Error(Nil) -> call
  }
}

fn drop_pending_for(call: Call, session_ids: List(String)) -> Call {
  Call(
    ..call,
    pending: dict.filter(call.pending, fn(_conn, pending) {
      !list.contains(session_ids, pending.session_id)
    }),
  )
}

fn drop_session(server: Server, session_id: String, member: Member) -> Outcome {
  let call = server.call
  let next =
    remove_session(call, session_id)
    |> state.drop_sessions([session_id])
    |> drop_pending_for([session_id])
    |> cancel_ring_timers([member.user])
    |> state.remove_users_from_ringing([member.user])
  sync_counts(call, next)
  stop_or_update(server, next)
}

fn drop_slot(
  server: Server,
  session_id: String,
  member: Member,
  key: VoiceKey,
) -> Outcome {
  let call = state.drop_entries(server.call, [key])
  let next = case state.has_session_entries(call, session_id) {
    True -> call
    False ->
      remove_session(call, session_id)
      |> drop_pending_for([session_id])
      |> cancel_ring_timers([member.user])
      |> state.remove_users_from_ringing([member.user])
  }
  let next =
    Call(..next, pending: dict.delete(next.pending, voice_key.connection(key)))
  sync_counts(server.call, next)
  stop_or_update(server, next)
}

fn disconnect_user_in(
  server: Server,
  user: UserId,
  channel: ChannelId,
  conn: Option(ConnectionId),
  reply: Subject(Json),
) -> Outcome {
  let call = server.call
  let ignored = fn(reason) {
    json.object([
      #("success", json.bool(True)),
      #("ignored", json.bool(True)),
      #("reason", json.string(reason)),
    ])
  }
  let drop_pending = fn(call: Call) {
    case conn {
      Some(conn) -> Call(..call, pending: dict.delete(call.pending, conn))
      None -> call
    }
  }
  let mine =
    state.keyed_entries(call)
    |> list.filter(fn(pair) { voice_key.user(pair.0) == user })
  let targets = case conn {
    Some(conn) ->
      list.filter(mine, fn(pair) { voice_key.connection(pair.0) == conn })
    None -> mine
  }
  let in_channel =
    list.any(targets, fn(pair) { entry.in_channel(pair.1, channel) })
  case targets, in_channel {
    [], _ -> {
      process.send(reply, ignored("not_in_call"))
      continue(server, drop_pending(call))
    }
    _, False -> {
      process.send(reply, ignored("channel_mismatch"))
      Running(server)
    }
    _, True -> {
      let dropped = list.map(targets, fn(pair) { pair.0 })
      let remaining = state.drop_entries(call, dropped)
      let target_sessions =
        list.filter_map(targets, fn(pair) {
          option.to_result(entry.session_id(pair.1), Nil)
        })
        |> list.unique
      let session_ids = case conn {
        Some(_) ->
          list.filter(target_sessions, fn(session_id) {
            !state.has_session_entries(remaining, session_id)
          })
        None ->
          call.sessions
          |> dict.filter(fn(_session, member) { member.user == user })
          |> dict.keys
          |> list.append(target_sessions)
          |> list.unique
      }
      let notified = case conn {
        Some(_) -> target_sessions
        None -> session_ids
      }
      list.each(notified, fn(session_id) {
        case dict.get(call.sessions, session_id) {
          Ok(member) ->
            state.force_disconnect(member, session_id, call.channel, conn)
          Error(Nil) -> Nil
        }
      })
      let sessions =
        list.fold(session_ids, call.sessions, fn(acc, session_id) {
          case dict.get(acc, session_id) {
            Ok(member) -> {
              unwatch(member)
              dict.delete(acc, session_id)
            }
            Error(Nil) -> acc
          }
        })
      let user_gone = !state.has_user(remaining, user)
      let next =
        Call(
          ..remaining,
          sessions:,
          detached: dict.drop(call.detached, session_ids),
        )
        |> drop_pending
      let next = case user_gone {
        True ->
          next
          |> cancel_ring_timers([user])
          |> state.remove_users_from_ringing([user])
        False -> next
      }
      sync_counts(call, next)
      process.send(reply, json.object([#("success", json.bool(True))]))
      stop_or_update(server, next)
    }
  }
}

fn stop_or_update(server: Server, call: Call) -> Outcome {
  case store.is_empty(call.voice_states) {
    True -> {
      end_call(call)
      Ended
    }
    False -> continue(server, publish_update(call))
  }
}

fn stop_if_idle(server: Server, call: Call) -> Outcome {
  case has_activity(call) {
    True -> continue(server, call)
    False -> {
      end_call(call)
      Ended
    }
  }
}

fn has_activity(call: Call) -> Bool {
  !store.is_empty(call.voice_states) || call.ringing != []
}

fn sync_counts(old: Call, new: Call) -> Nil {
  voice_metrics.transitions(
    voice_metrics.CallScope,
    store.to_dict(old.voice_states),
    store.to_dict(new.voice_states),
  )
  sync_table(old, new)
}

fn sync_table(old: Call, new: Call) -> Nil {
  store.keys(old.voice_states)
  |> list.each(fn(key) {
    case store.has_key(new.voice_states, key) {
      True -> Nil
      False -> count_table.remove(key)
    }
  })
  list.each(state.entries(new), count_table.upsert)
}

fn reset_idle(self: Subject(Message), call: Call) -> Call {
  case call.idle_timer {
    Some(timer) -> {
      process.cancel_timer(timer)
      Nil
    }
    None -> Nil
  }
  Call(
    ..call,
    idle_timer: Some(process.send_after(self, idle_timeout_ms(), IdleTimeout)),
  )
}

fn start_ring_timers(
  self: Subject(Message),
  call: Call,
  users: List(UserId),
) -> Call {
  list.fold(users, call, fn(acc, user) {
    let key = id.user_id_to_int(user)
    case dict.has_key(acc.ring_timers, key) {
      True -> acc
      False ->
        Call(
          ..acc,
          ring_timers: dict.insert(
            acc.ring_timers,
            key,
            process.send_after(self, ring_timeout_ms(), RingTimeout(user)),
          ),
        )
    }
  })
}

fn cancel_ring_timers(call: Call, users: List(UserId)) -> Call {
  list.fold(users, call, fn(acc, user) {
    let key = id.user_id_to_int(user)
    case dict.get(acc.ring_timers, key) {
      Ok(timer) -> {
        process.cancel_timer(timer)
        Call(..acc, ring_timers: dict.delete(acc.ring_timers, key))
      }
      Error(Nil) -> acc
    }
  })
}

fn dispatch_pending_ringing(
  self: Subject(Message),
  call: Call,
  dispatch: Bool,
) -> #(Call, Bool) {
  case call.initiator_ready {
    False -> #(call, False)
    True -> {
      let pending = state.unique_users(call.pending_ringing)
      let connected = state.connected_users(call)
      let to_add =
        list.filter(pending, fn(user) {
          !list.contains(connected, user) && !list.contains(call.ringing, user)
        })
      let call =
        Call(..call, pending_ringing: [], ringing: case to_add {
          [] -> call.ringing
          _ -> state.unique_users(list.append(call.ringing, to_add))
        })
      publish_rings(call, to_add)
      let call = start_ring_timers(self, call, to_add)
      case to_add, dispatch {
        [], _ | _, False -> #(call, False)
        _, True -> #(publish_update(call), True)
      }
    }
  }
}

fn publish_rings(call: Call, users: List(UserId)) -> Nil {
  case users {
    [] -> Nil
    _ -> {
      let started = now()
      let recipients = call.recipients
      let caller = call.caller
      let channel = call.channel
      let message = call.message
      process.spawn_unlinked(fn() {
        let blocked =
          users
          |> list.map(id.user_id_to_int)
          |> push_recipients.known_blocked
        list.each(users, fn(user) {
          let others = list.filter(recipients, fn(other) { other != user })
          let suppressed = case
            others,
            dict.get(blocked, id.user_id_to_int(user))
          {
            [], _ -> False
            _, Error(Nil) -> True
            _, Ok(ids) ->
              list.all(others, fn(other) {
                list.contains(ids, id.user_id_to_int(other))
              })
          }
          case suppressed {
            True -> Nil
            False -> {
              let _ =
                publisher.publish_ring(
                  user:,
                  channel:,
                  message:,
                  started_at_ms: started,
                  expires_at_ms: started + ring_timeout_ms(),
                  caller: option.map(caller, fn(found) {
                    job.Caller(
                      id: id.user_id_to_int(found.id),
                      name: found.name,
                      avatar: found.avatar,
                    )
                  }),
                )
              Nil
            }
          }
        })
      })
      Nil
    }
  }
}

fn to_recipients(call: Call, event: String, payload: RawJson) -> Nil {
  let recipients = call.recipients
  process.spawn_unlinked(fn() {
    list.each(recipients, fn(user) {
      presence.dispatch_to_user(user, event, payload)
    })
  })
  Nil
}

fn dispatch_create(call: Call) -> Call {
  let states = state.event_states(call)
  to_recipients(call, "CALL_CREATE", state.event_of(call, states))
  Call(..call, last_event: Some(state.event_key(call, states)))
}

fn publish_update(call: Call) -> Call {
  let states = state.event_states(call)
  let key = state.event_key(call, states)
  case call.last_event == Some(key) {
    True -> call
    False -> {
      to_recipients(call, "CALL_UPDATE", state.event_of(call, states))
      Call(..call, last_event: Some(key))
    }
  }
}

fn dispatch_voice_state(call: Call, value: Entry) -> Nil {
  to_recipients(
    call,
    "VOICE_STATE_UPDATE",
    entry.to_raw(value, entry.Broadcast),
  )
}

fn end_call(call: Call) -> Nil {
  voice_metrics.call_ended()
  to_recipients(
    call,
    "CALL_DELETE",
    json.object([
      #("channel_id", id.channel_json(call.channel)),
    ])
      |> raw_json.from_json,
  )
  dict.each(call.ring_timers, fn(_user, timer) {
    process.cancel_timer(timer)
    Nil
  })
  let fields = [
    #("channel_id", id.channel_json(call.channel)),
    #("message_id", id.message_json(call.message)),
    #(
      "participants",
      json.array(set.to_list(call.history), fn(user) { id.user_json(user) }),
    ),
    #("ended_timestamp", json.int(now())),
  ]
  process.spawn_unlinked(fn() { token.api_send("call_ended", fields) })
  list.each(store.keys(call.voice_states), count_table.remove)
}

fn convert_to_sfu(
  self: Subject(Message),
  call: Call,
  extra: List(VoiceKey),
  located: Dict(ConnectionId, Geo),
) -> Call {
  let converted =
    call.voice_states
    |> store.to_list
    |> list.filter(fn(pair) { entry.is_p2p({ pair.1 }.entry) })
    |> list.map(fn(pair) {
      #(pair.0, Slot(..pair.1, entry: entry.to_sfu({ pair.1 }.entry)))
    })
  let reconnecting =
    list.append(extra, list.map(converted, fn(pair) { pair.0 }))
  case reconnecting {
    [] -> call
    _ -> {
      let next =
        Call(
          ..call,
          voice_states: list.fold(converted, call.voice_states, fn(acc, pair) {
            store.put(acc, pair.0, pair.1)
          }),
        )
      list.each(converted, fn(pair) {
        notify_converted(next, { pair.1 }.entry)
        dispatch_voice_state(next, { pair.1 }.entry)
      })
      reissue(self, next, reconnecting, call.region, located)
    }
  }
}

fn notify_converted(call: Call, value: Entry) -> Nil {
  case
    option.then(entry.session_id(value), fn(session_id) {
      dict.get(call.sessions, session_id)
      |> result.map(fn(member) { #(session_id, member) })
      |> option.from_result
    })
  {
    Some(#(session_id, member)) ->
      state.converted(member, session_id, entry.connection_id(value))
    None -> Nil
  }
}

fn reissue(
  self: Subject(Message),
  call: Call,
  keys: List(VoiceKey),
  region: Option(String),
  located: Dict(ConnectionId, Geo),
) -> Call {
  let channel = call.channel
  let targets =
    keys
    |> list.unique
    |> list.filter_map(fn(key) {
      use slot <- result.try(store.get(call.voice_states, key))
      use session_id <- result.try(option.to_result(
        entry.session_id(slot.entry),
        Nil,
      ))
      use member <- result.map(dict.get(call.sessions, session_id))
      #(session_id, member.user, slot.entry)
    })
  let #(key, call) =
    conversion_key(
      call,
      list.map(targets, fn(target) { target.2 }),
      e2ee.enabled_for_dm(),
    )
  let rtc_region = case region {
    Some("automatic") -> None
    other -> other
  }
  process.spawn_unlinked(fn() {
    let redirect = process.new_subject()
    process.send(self, Reissuer(redirect))
    let last =
      list.fold(targets, self, fn(current, target) {
        let #(session_id, user, value) = target
        let conn = entry.connection_id(value)
        let request =
          token.Request(
            ..token.request(
              guild: None,
              channel:,
              user:,
              connection_id: Some(conn),
            ),
            region: rtc_region,
            geo: dict.get(located, conn) |> result.unwrap(geo.unknown()),
          )
        case token.fetch_sfu(request) {
          Ok(granted) -> {
            let payload =
              server_update(channel, granted, case entry.e2ee_capable(value) {
                True -> key
                False -> None
              })
            deliver(current, redirect, Reissued(session_id, conn, payload, _))
          }
          Error(_) -> current
        }
      })
    process.send(latest(last, redirect), ReissuerDone(redirect))
  })
  call
}

fn latest(
  target: Subject(Message),
  redirect: Subject(Subject(Message)),
) -> Subject(Message) {
  case process.receive(redirect, 0) {
    Ok(moved) -> {
      process.send(moved, Reissuer(redirect))
      latest(moved, redirect)
    }
    Error(Nil) -> target
  }
}

fn deliver(
  target: Subject(Message),
  redirect: Subject(Subject(Message)),
  make: fn(Subject(Nil)) -> Message,
) -> Subject(Message) {
  let target = latest(target, redirect)
  case process.subject_owner(target) {
    Error(Nil) -> target
    Ok(pid) -> {
      let ack = process.new_subject()
      let monitor = process.monitor(pid)
      process.send(target, make(ack))
      let outcome =
        process.new_selector()
        |> process.select_map(ack, fn(_) { Acked })
        |> process.select_map(redirect, Moved)
        |> process.select_specific_monitor(monitor, fn(_) { Gone })
        |> process.selector_receive_forever
      process.demonitor_process(monitor)
      case outcome {
        Moved(moved) -> {
          process.send(moved, Reissuer(redirect))
          deliver(moved, redirect, make)
        }
        Acked | Gone -> target
      }
    }
  }
}

pub fn conversion_key(
  call: Call,
  targets: List(Entry),
  wanted: Bool,
) -> #(Option(String), Call) {
  case wanted && list.any(targets, entry.e2ee_capable) {
    False -> #(None, call)
    True -> {
      let #(key, call) = state.room_key(call)
      #(Some(key), call)
    }
  }
}

pub fn server_update(
  channel: ChannelId,
  granted: token.Token,
  key: Option(String),
) -> RawJson {
  let base = [
    #("token", json.string(granted.token)),
    #("endpoint", json.string(granted.endpoint)),
    #("channel_id", id.channel_json(channel)),
    #("connection_id", voice_key.connection_json(granted.connection_id)),
  ]
  let fields = case key {
    Some(key) -> list.append(base, [#("e2ee_key", json.string(key))])
    None -> base
  }
  json.object(fields) |> raw_json.from_json
}
