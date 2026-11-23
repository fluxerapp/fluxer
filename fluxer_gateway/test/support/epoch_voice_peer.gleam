// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/worker.{Entry, Profile, Ready, WorkerId}
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/liveness
import fluxer_gateway/cluster/router
import fluxer_gateway/cluster/topology
import fluxer_gateway/config/env
import fluxer_gateway/config/settings
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionInbox}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/geo
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/store.{type Store}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import support/voice_ids
import support/voice_session as fake

@external(erlang, "net_adm", "ping")
fn ping(node: Atom) -> Atom

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: #(String, Subject(state.MemberNote))) -> Bool

@external(erlang, "erlang", "append_element")
fn append(member: state.Member, value: a) -> state.Member

@external(erlang, "erlang", "tuple_size")
fn size(value: Dynamic) -> Int

@external(erlang, "erlang", "element")
fn disconnect_of(
  index: Int,
  member: Dynamic,
) -> fn(ChannelId, Option(ConnectionId)) -> Dynamic

@external(erlang, "erlang", "element")
fn converted_of(index: Int, member: Dynamic) -> fn(ConnectionId) -> Dynamic

@external(erlang, "sys", "get_state")
fn sys_get_state(pid: Pid) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

pub fn channel(value: Int) -> ChannelId {
  let assert Ok(found) = id.channel_id(value)
  found
}

pub fn connection(
  user: Int,
  channel_value: Int,
  session_id: String,
  conn: String,
  p2p: Bool,
) -> Entry {
  let assert Ok(user) = id.user_id(user)
  entry.new(
    user:,
    guild: None,
    channel: channel(channel_value),
    connection_id: voice_ids.conn(conn),
    session_id: Some(session_id),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
  |> entry.set_p2p(p2p)
}

fn hold_tables() -> Nil {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    registry.create_tables()
    process.send(ready, Nil)
    process.sleep_forever()
  })
  let assert Ok(Nil) = process.receive(ready, 5000)
  Nil
}

pub fn treat_as_epoch_four(peer_name: String) -> Bool {
  let peer = atom.create(peer_name)
  let entry =
    Entry(
      worker: WorkerId(peer, 1),
      profile: Profile("calls", "default", "dev", "epoch-four", 4, []),
      status: Ready,
      last_beat_ms: 1,
    )
  links.replace([entry])
  atom.to_string(ping(peer)) == "pong"
}

pub type DmQuery {
  DmQuery(reply: Subject(List(String)))
}

type DmEvent {
  Proper(state.MemberNote)
  Misaddressed(state.MemberNote)
  Asked(DmQuery)
}

pub fn legacy_dm(
  session_id: String,
  user: Int,
) -> #(Subject(DmQuery), Dynamic) {
  hold_tables()
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let own: Subject(state.MemberNote) = process.new_subject()
    let inbox: Subject(SessionInbox) = process.new_subject()
    let query = process.new_subject()
    let assert True =
      ets_insert(registry.dm_sessions_table(), #(session_id, own))
    let assert Ok(user) = id.user_id(user)
    let member =
      state.Member(
        user:,
        inbox:,
        monitor: None,
        owner: Some(process.self()),
        owner_monitor: None,
      )
      |> append(fn(found: ChannelId, conn: Option(ConnectionId)) {
        process.send(own, state.ForceDisconnect(found, conn))
      })
      |> append(fn(conn: ConnectionId) {
        process.send(own, state.Converted(conn))
      })
    process.send(ready, #(query, dynamic_of(member)))
    process.new_selector()
    |> process.select_map(own, Proper)
    |> process.select_map(state.notes(process.self()), Misaddressed)
    |> process.select_map(query, Asked)
    |> legacy_dm_loop([])
  })
  let assert Ok(started) = process.receive(ready, 5000)
  started
}

fn dynamic_of(value: a) -> Dynamic {
  coerce(value)
}

fn legacy_dm_loop(selector: process.Selector(DmEvent), seen: List(String)) {
  case process.selector_receive_forever(selector) {
    Proper(note) -> legacy_dm_loop(selector, [describe(note), ..seen])
    Misaddressed(note) ->
      legacy_dm_loop(selector, ["misaddressed " <> describe(note), ..seen])
    Asked(DmQuery(reply)) -> {
      process.send(reply, list.reverse(seen))
      legacy_dm_loop(selector, seen)
    }
  }
}

fn describe(note: state.MemberNote) -> String {
  case note {
    state.ForceDisconnect(_, Some(conn)) -> "force " <> voice_ids.text(conn)
    state.ForceDisconnect(_, None) -> "force all"
    state.Converted(conn) -> "converted " <> voice_ids.text(conn)
  }
}

pub fn legacy_dm_seen(dm: Subject(DmQuery)) -> List(String) {
  let reply = process.new_subject()
  process.send(dm, DmQuery(reply))
  let assert Ok(seen) = process.receive(reply, 5000)
  seen
}

pub fn serve_legacy_member(
  member: Dynamic,
  user: Int,
  channel_value: Int,
  session_id: String,
  conn: String,
) -> Bool {
  let assert Ok(started) =
    call_server.start(
      call_server.Fresh(state.Init(
        channel: channel(channel_value),
        message: first_message(),
        region: None,
        ringing: [],
        recipients: [],
        caller: None,
      )),
    )
  let call = started.data
  let assert Ok(user_id) = id.user_id(user)
  let sfu = connection(user, channel_value, session_id, conn, False)
  let joined =
    call_server.join(
      call,
      user_id,
      sfu,
      session_id,
      coerce(member),
      Some(voice_ids.conn(conn)),
      None,
      geo.unknown(),
    )
  let updated =
    call_server.update_voice_state(
      call,
      user_id,
      entry.set_p2p(sfu, True),
      5000,
    )
  let forced =
    call_server.disconnect_user(
      call,
      user_id,
      channel(channel_value),
      Some(voice_ids.conn(conn)),
      5000,
    )
  joined == Ok(Ok(Nil)) && updated == Ok(Nil) && result.is_ok(forced)
}

pub type CallControl {
  Convert(conn: String, reply: Subject(Bool))
  Drop(conn: String, reply: Subject(Bool))
  Rebound(reply: Subject(Bool))
  Stall
}

type CallInput {
  Called(Dynamic)
  Controlled(CallControl)
}

type FakeCall {
  FakeCall(
    channel: ChannelId,
    recipients: List(id.UserId),
    connections: List(Entry),
    member: Option(Dynamic),
    stalled: Bool,
  )
}

pub fn legacy_call(
  channel_value: Int,
  user: Int,
  session_id: String,
  conn: String,
) -> Subject(CallControl) {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let asks: Subject(Dynamic) = process.new_subject()
    let controls = process.new_subject()
    add_legacy_call(channel_value, asks)
    process.send(ready, controls)
    let assert Ok(user_id) = id.user_id(user)
    process.new_selector()
    |> process.select_map(asks, Called)
    |> process.select_map(controls, Controlled)
    |> legacy_call_loop(FakeCall(
      channel: channel(channel_value),
      recipients: [user_id],
      connections: [connection(user, channel_value, session_id, conn, True)],
      member: None,
      stalled: False,
    ))
  })
  let assert Ok(controls) = process.receive(ready, 5000)
  controls
}

fn add_legacy_call(channel_value: Int, call: Subject(Dynamic)) -> Nil {
  let name: process.Name(Dynamic) =
    env.process_name("fluxer_voice_call_manager")
  case process.named(name) {
    Ok(_) -> Nil
    Error(Nil) -> {
      let ready = process.new_subject()
      process.spawn_unlinked(fn() {
        let assert Ok(Nil) = process.register(process.self(), name)
        process.send(ready, Nil)
        legacy_manager_loop(process.named_subject(name), dict.new())
      })
      let assert Ok(Nil) = process.receive(ready, 5000)
      Nil
    }
  }
  process.send(
    process.named_subject(name),
    coerce(#(atom.create("fake_add"), channel(channel_value), call)),
  )
}

fn legacy_manager_loop(
  inbox: Subject(Dynamic),
  calls: Dict(Dynamic, Subject(Dynamic)),
) -> Nil {
  let message = process.receive_forever(inbox)
  let channel_value = decode.run(message, decode.at([1], decode.dynamic))
  case tag(message), channel_value {
    "fake_add", Ok(found) -> {
      let assert Ok(call) = decode.run(message, decode.at([2], decode.dynamic))
      legacy_manager_loop(inbox, dict.insert(calls, found, coerce(call)))
    }
    "lookup", Ok(found) | "lookup_adopted", Ok(found) -> {
      case dict.get(calls, found) {
        Ok(call) -> process.send(reply_at(message, 2), Ok(call))
        Error(Nil) -> process.send(reply_at(message, 2), Error(Nil))
      }
      legacy_manager_loop(inbox, calls)
    }
    _, _ -> legacy_manager_loop(inbox, calls)
  }
}

fn tag(message: Dynamic) -> String {
  case decode.run(message, decode.at([0], atom.decoder())) {
    Ok(found) -> atom.to_string(found)
    Error(_) -> ""
  }
}

fn reply_at(message: Dynamic, index: Int) -> Subject(a) {
  let decoder = {
    use owner <- decode.subfield([index, 1], decode.dynamic)
    use reference <- decode.subfield([index, 2], decode.dynamic)
    decode.success(process.unsafely_create_subject(coerce(owner), reference))
  }
  let assert Ok(subject) = decode.run(message, decoder)
  subject
}

fn legacy_call_loop(selector: process.Selector(CallInput), call: FakeCall) {
  case process.selector_receive_forever(selector) {
    Called(message) -> legacy_call_loop(selector, answer(call, message))
    Controlled(Stall) ->
      legacy_call_loop(selector, FakeCall(..call, stalled: True))
    Controlled(Convert(conn, reply)) -> {
      let conn = voice_ids.conn(conn)
      let connections =
        list.map(call.connections, fn(value) {
          case entry.connection_id(value) == conn {
            True -> entry.to_sfu(value)
            False -> value
          }
        })
      process.send(reply, case call.member {
        Some(member) -> {
          let _ = converted_of(8, member)(conn)
          size(member) == 8
        }
        _ -> False
      })
      legacy_call_loop(selector, FakeCall(..call, connections:))
    }
    Controlled(Rebound(reply)) -> {
      process.send(reply, case call.member {
        Some(member) -> size(member) == 8
        None -> False
      })
      legacy_call_loop(selector, call)
    }
    Controlled(Drop(conn, reply)) -> {
      let conn = voice_ids.conn(conn)
      let connections =
        list.filter(call.connections, fn(value) {
          entry.connection_id(value) != conn
        })
      process.send(reply, case call.member {
        Some(member) -> {
          let _ = disconnect_of(7, member)(call.channel, Some(conn))
          size(member) == 8
        }
        None -> False
      })
      legacy_call_loop(selector, FakeCall(..call, connections:))
    }
  }
}

fn first_message() -> id.MessageId {
  let assert Ok(found) = id.message_id(1)
  found
}

fn answer(call: FakeCall, message: Dynamic) -> FakeCall {
  case tag(message) {
    "get_state" if call.stalled -> call
    "get_state" -> {
      process.send(
        reply_at(message, 1),
        state.View(
          channel: call.channel,
          message: first_message(),
          region: None,
          ringing: [],
          recipients: call.recipients,
          voice_states: call.connections,
          connections: call.connections,
          created_at: 0,
        ),
      )
      call
    }
    "rebind" -> {
      let assert Ok(member) =
        decode.run(message, decode.at([2], decode.dynamic))
      FakeCall(..call, member: Some(member))
    }
    "leave" -> {
      process.send(reply_at(message, 2), Nil)
      call
    }
    "drop_connection" -> {
      process.send(reply_at(message, 3), Nil)
      call
    }
    "update_voice_state" -> {
      process.send(reply_at(message, 3), Ok(Nil))
      call
    }
    _ -> call
  }
}

pub fn legacy_call_convert(
  controls: Subject(CallControl),
  conn: String,
) -> Bool {
  let reply = process.new_subject()
  process.send(controls, Convert(conn, reply))
  process.receive(reply, 5000) == Ok(True)
}

pub fn legacy_call_stall(controls: Subject(CallControl)) -> Nil {
  process.send(controls, Stall)
}

pub fn legacy_call_rebound(controls: Subject(CallControl)) -> Bool {
  let reply = process.new_subject()
  process.send(controls, Rebound(reply))
  process.receive(reply, 5000) == Ok(True)
}

pub fn legacy_call_drop(controls: Subject(CallControl), conn: String) -> Bool {
  let reply = process.new_subject()
  process.send(controls, Drop(conn, reply))
  process.receive(reply, 5000) == Ok(True)
}

pub fn route_calls_to(peer_name: String) -> Bool {
  let peer = node.from_atom(atom.create(peer_name))
  let ready = treat_as_epoch_four(peer_name)
  hold_tables()
  liveness.init([])
  liveness.set(peer, True)
  topology.new(node.self(), node.Websocket, [])
  |> topology.set_role(peer, node.Calls, [])
  |> topology.place(peer)
  |> topology.mark_joined
  |> topology.bump
  |> router.publish
  let current = settings.get()
  settings.store(
    settings.Settings(
      ..current,
      durations: settings.Durations(
        ..current.durations,
        voice: settings.VoiceDurations(
          ..current.durations.voice,
          dm_legacy_call_check_interval_ms: 100,
        ),
      ),
    ),
  )
  ready
}

pub fn dm_session(user: Int) -> #(fake.Fake, String) {
  let session = fake.start(user)
  #(session, fake.text(session))
}

pub fn adopt(session: fake.Fake, channel_value: Int) -> Nil {
  dm_server.adopt(session.ref, [channel(channel_value)])
}

pub fn dm_connection(session: fake.Fake, conn: String) -> String {
  case dm_server.lookup(session.ref) {
    Error(Nil) -> "no dm"
    Ok(subject) -> {
      let assert Ok(pid) = process.subject_owner(subject)
      let assert Ok(states) =
        decode.run(sys_get_state(pid), decode.at([5], decode.dynamic))
      let states: Store(Entry) = coerce(states)
      case store.by_connection(states, voice_ids.conn(conn)) {
        Ok(#(_, value)) ->
          case entry.is_p2p(value) {
            True -> "p2p"
            False -> "sfu"
          }
        Error(Nil) -> "missing"
      }
    }
  }
}
