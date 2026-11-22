// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/voice_key
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/geo
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid, type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/voice_ids

const channel_value = 1_539_791_936_896_897_100

const user_a = 1_539_791_936_896_897_001

const user_b = 1_539_791_936_896_897_002

const user_c = 1_539_791_936_896_897_003

type Device {
  Device(owner: Pid, value: Entry)
}

type Forced =
  #(String, Option(String))

fn channel() -> id.ChannelId {
  let assert Ok(found) = id.channel_id(channel_value)
  found
}

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn start_call() -> Subject(call_server.Message) {
  let assert Ok(message) = id.message_id(1)
  let assert Ok(started) =
    call_server.start(
      call_server.Fresh(state.Init(
        channel: channel(),
        message:,
        region: None,
        ringing: [],
        recipients: [],
        caller: None,
      )),
    )
  started.data
}

fn connection(owner: Int, conn: String) -> Entry {
  entry.new(
    user: user(owner),
    guild: None,
    channel: channel(),
    connection_id: voice_ids.conn(conn),
    session_id: Some(session_id(conn)),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
}

fn session_id(conn: String) -> String {
  "session-" <> conn
}

fn member(
  owner: Int,
  forced: Subject(Forced),
  conn: String,
) -> #(Pid, state.Member) {
  let reply = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      process.send(reply, process.new_subject())
      forward(state.notes(process.self()), forced, conn)
    })
  let assert Ok(inbox) = process.receive(reply, 1000)
  #(
    pid,
    state.Member(
      user: user(owner),
      inbox:,
      monitor: None,
      owner: Some(pid),
      owner_monitor: None,
    ),
  )
}

fn forward(
  notes: Subject(state.MemberNote),
  forced: Subject(Forced),
  conn: String,
) -> Nil {
  case process.receive_forever(notes) {
    state.ForceDisconnect(_, target) ->
      process.send(forced, #(
        session_id(conn),
        option.map(target, voice_key.connection_text),
      ))
    state.Converted(_) -> Nil
  }
  forward(notes, forced, conn)
}

fn join_reply(
  call: Subject(call_server.Message),
  forced: Subject(Forced),
  value: Entry,
  max: Option(Int),
) -> #(Pid, Result(Nil, error.GatewayError)) {
  let owner = id.user_id_to_int(entry.user(value))
  let conn = voice_ids.of(value)
  let #(pid, found) = member(owner, forced, conn)
  let assert Ok(reply) =
    call_server.join(
      call,
      entry.user(value),
      value,
      session_id(conn),
      found,
      Some(entry.connection_id(value)),
      max,
      geo.unknown(),
    )
  #(pid, reply)
}

fn join(
  call: Subject(call_server.Message),
  forced: Subject(Forced),
  value: Entry,
) -> Device {
  let #(pid, reply) = join_reply(call, forced, value, None)
  assert reply == Ok(Nil)
  Device(pid, value)
}

fn view(call: Subject(call_server.Message)) -> state.View {
  let assert Ok(found) = call_server.get_state(call, 1000)
  found
}

fn connection_ids(call: Subject(call_server.Message)) -> List(String) {
  view(call).connections
  |> list.map(voice_ids.of)
  |> list.sort(string.compare)
}

fn drain(forced: Subject(Forced)) -> List(Forced) {
  case process.receive(forced, 50) {
    Ok(item) -> [item, ..drain(forced)]
    Error(Nil) -> []
  }
}

fn disconnect(
  call: Subject(call_server.Message),
  target: Int,
  conn: Option(String),
) -> String {
  let assert Ok(reply) =
    call_server.disconnect_user(
      call,
      user(target),
      channel(),
      option.map(conn, voice_ids.conn),
      1000,
    )
  json.to_string(reply)
}

fn eventually(check: fn() -> Bool, attempts: Int) -> Bool {
  case check(), attempts {
    True, _ -> True
    False, 0 -> False
    False, _ -> {
      process.sleep(20)
      eventually(check, attempts - 1)
    }
  }
}

pub fn two_devices_of_one_user_are_both_in_the_call_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  join(call, forced, connection(user_a, "a2"))
  assert connection_ids(call) == ["a1", "a2"]
  assert list.map(view(call).voice_states, voice_ids.of) == ["a1", "a2"]
}

pub fn one_device_leaving_keeps_the_other_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  join(call, forced, connection(user_a, "a2"))
  call_server.leave(call, session_id("a2"))
  assert connection_ids(call) == ["a1"]
}

pub fn a_device_going_down_keeps_the_other_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  let second = join(call, forced, connection(user_a, "a2"))
  process.kill(second.owner)
  assert eventually(fn() { connection_ids(call) == ["a1"] }, 50)
}

pub fn disconnect_with_connection_id_removes_only_that_device_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  join(call, forced, connection(user_a, "a2"))
  assert disconnect(call, user_a, Some("a2")) == "{\"success\":true}"
  assert connection_ids(call) == ["a1"]
  assert drain(forced) == [#(session_id("a2"), Some("a2"))]
  let assert Ok(exported) = call_server.export(call, 1000)
  call_server.thaw(call)
  assert dict.keys(exported.sessions) == [session_id("a1")]
}

pub fn disconnect_without_connection_id_removes_every_device_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_b, "b1"))
  join(call, forced, connection(user_a, "a1"))
  join(call, forced, connection(user_a, "a2"))
  assert disconnect(call, user_a, None) == "{\"success\":true}"
  assert connection_ids(call) == ["b1"]
  assert list.sort(drain(forced), fn(a, b) { string.compare(a.0, b.0) })
    == [#(session_id("a1"), None), #(session_id("a2"), None)]
}

pub fn devices_with_different_self_deaf_do_not_flap_test() {
  let forced = process.new_subject()
  let call = start_call()
  let deaf =
    connection(user_a, "a1") |> entry.set_flag(voice_state.SelfDeaf, True)
  let hearing = connection(user_a, "a2")
  join(call, forced, deaf)
  join(call, forced, hearing)
  let replies =
    list.repeat([deaf, hearing], 5)
    |> list.flatten
    |> list.map(call_server.update_voice_state(call, user(user_a), _, 1000))
  assert replies == list.repeat(Ok(Nil), 10)
  assert list.map(view(call).voice_states, fn(value) {
      #(voice_ids.of(value), entry.has(value, voice_state.SelfDeaf))
    })
    == [#("a1", True), #("a2", False)]
}

pub fn update_for_an_unknown_connection_is_rejected_test() {
  let forced = process.new_subject()
  let call = start_call()
  let value = connection(user_a, "a1")
  join(call, forced, value)
  assert call_server.update_voice_state(
      call,
      user(user_a),
      connection(user_a, "zz"),
      1000,
    )
    == Error(Nil)
  assert call_server.update_voice_state(call, user(user_b), value, 1000)
    == Error(Nil)
}

pub fn max_participants_counts_users_test() {
  let forced = process.new_subject()
  let call = start_call()
  let p2p = fn(owner, conn) { connection(owner, conn) |> entry.set_p2p(True) }
  assert join_reply(call, forced, p2p(user_a, "a1"), Some(2)).1 == Ok(Nil)
  assert join_reply(call, forced, p2p(user_a, "a2"), Some(2)).1 == Ok(Nil)
  assert join_reply(call, forced, p2p(user_b, "b1"), Some(2)).1 == Ok(Nil)
  assert join_reply(call, forced, p2p(user_c, "c1"), Some(2)).1
    == Error(error.VoiceChannelFull)
  assert connection_ids(call) == ["a1", "a2", "b1"]
}

pub fn ringing_skips_a_user_who_is_in_the_call_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  let assert Ok(Nil) =
    call_server.ring(call, [user(user_a), user(user_b)], None, 1000)
  assert view(call).ringing == [user(user_b)]
}

pub fn handoff_keeps_every_connection_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(call, forced, connection(user_a, "a1"))
  join(call, forced, connection(user_a, "a2"))
  let assert Ok(exported) = call_server.export(call, 1000)
  let assert Ok(restored) = call_server.start(call_server.Restored(exported))
  assert connection_ids(restored.data) == ["a1", "a2"]
  call_server.thaw(call)
}

pub fn an_update_keeps_the_voice_server_routing_test() {
  let forced = process.new_subject()
  let call = start_call()
  let routed =
    connection(user_a, "a1") |> entry.set_routing(Some("ams"), Some("ams-1"))
  join(call, forced, routed)
  let update =
    connection(user_a, "a1") |> entry.set_flag(voice_state.SelfMute, True)
  assert call_server.update_voice_state(call, user(user_a), update, 1000)
    == Ok(Nil)
  let assert [stored] = view(call).connections
  assert #(
      entry.has(stored, voice_state.SelfMute),
      entry.region_id(stored),
      entry.server_id(stored),
    )
    == #(True, Some("ams"), Some("ams-1"))
}

pub fn call_create_after_ready_hides_voice_server_routing_test() {
  let forced = process.new_subject()
  let call = start_call()
  join(
    call,
    forced,
    connection(user_a, "a1") |> entry.set_routing(None, Some("ams-1")),
  )
  let current = view(call)
  let event = state.view_event(current)
  let server_ids =
    decode.at(
      ["voice_states"],
      decode.list(decode.optional_field(
        "server_id",
        None,
        decode.optional(decode.string),
        decode.success,
      )),
    )
  let assert Ok(sent) =
    raw_json.read(event, {
      use states <- decode.then(server_ids)
      use keys <- decode.then(decode.dict(decode.string, decode.dynamic))
      decode.success(#(states, list.sort(dict.keys(keys), string.compare)))
    })
  assert sent
    == #([None], [
      "channel_id",
      "message_id",
      "region",
      "ringing",
      "voice_states",
    ])
  let assert Ok(rpc_view) =
    json.parse(json.to_string(state.view_json(current)), server_ids)
  assert rpc_view == [Some("ams-1")]
}
