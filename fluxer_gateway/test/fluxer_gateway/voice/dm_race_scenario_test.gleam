// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/rpc
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import support/rollout_guard
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake

@external(erlang, "sys", "get_state")
fn get_state(pid: Pid) -> Dynamic

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> #(Atom, Int)

@external(erlang, "gleam_stdlib", "identity")
fn to_entry(value: Dynamic) -> Entry

fn dm_channel(recipients: List(Int)) -> api.Rule {
  api.Reply(
    api.Data(
      json.object([
        #(
          "channel",
          json.object([
            #("type", json.int(1)),
            #(
              "recipients",
              json.array(recipients, fn(user) {
                json.object([#("id", json.string(int.to_string(user)))])
              }),
            ),
          ]),
        ),
      ]),
    ),
  )
}

fn create_call(channel: Int, recipients: List(Int)) -> Nil {
  let assert Ok(_) =
    rpc.handle(
      "call.create",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("message_id", json.string(int.to_string(channel + 1000))),
          #("region", json.null()),
          #("ringing", json.array([], json.string)),
          #(
            "recipients",
            json.array(recipients, fn(user) { json.string(int.to_string(user)) }),
          ),
        ]),
      ),
    )
  Nil
}

fn connection_ids(channel: Int) -> List(String) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(view) -> list.map(view.connections, voice_ids.of)
        Error(Nil) -> []
      }
    Error(_) -> []
  }
}

fn send(session: fake.Fake, data: Json) -> Nil {
  client.dm_voice_update(session.ref, g.client_update(data), calls: [])
}

fn server_updates(session: fake.Fake) -> List(Dynamic) {
  fake.dispatched(session, "VOICE_SERVER_UPDATE")
}

fn server_update(session: fake.Fake, count: Int) -> Dynamic {
  let assert True =
    api.eventually(fn() { list.length(server_updates(session)) >= count })
  let assert Ok(last) = list.last(server_updates(session))
  last
}

fn joined(session: fake.Fake, channel: Int, data: Json, count: Int) -> String {
  send(session, data)
  let update = server_update(session, count)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert api.eventually(fn() { list.contains(connection_ids(channel), conn) })
  conn
}

fn leave() -> Json {
  g.update(None, None, [])
}

fn call_gone(channel: Int) -> Bool {
  manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
}

fn p2p_or_token(body: Dynamic) -> api.Response {
  let call = api.Call("voice_get_token", body)
  case api.bool_field(call, "p2p") {
    Some(True) ->
      api.p2p_grant("p2p-" <> option.unwrap(api.field(call, "user_id"), "x"))
    _ ->
      api.token(option.unwrap(
        api.field(call, "connection_id"),
        "sfu-" <> option.unwrap(api.field(call, "user_id"), "x"),
      ))
  }
}

fn dm_pid(session: fake.Fake) -> Pid {
  let assert Ok(subject) = dm_server.lookup(session.ref)
  let assert Ok(pid) = process.subject_owner(subject)
  pid
}

fn queued(pid: Pid) -> Int {
  process_info(pid, atom.create("message_queue_len")).1
}

fn dm_entries(session: fake.Fake) -> List(Entry) {
  let assert Ok(states) =
    decode.run(
      get_state(dm_pid(session)),
      decode.at([5, 1], decode.dict(decode.dynamic, decode.dynamic)),
    )
  states |> dict.values |> list.map(to_entry)
}

fn capable() -> List(#(String, Json)) {
  [#("e2ee_capable", json.bool(True))]
}

pub fn dm_sfu_switch_clears_the_peers_p2p_flag_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5101, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let p2p = [#("p2p", json.bool(True))]
  let c1 = joined(one, 5101, g.join_with(5101, p2p), 1)
  let _ = joined(two, 5101, g.join_with(5101, p2p), 1)
  assert list.all(dm_entries(two), entry.is_p2p)
  send(one, g.update(Some(5101), Some(c1), [#("p2p", json.bool(False))]))
  let _ = server_update(two, 2)
  assert api.eventually(fn() {
    let found = dm_entries(two)
    found != [] && !list.any(found, entry.is_p2p)
  })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5101) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_e2ee_key_is_shared_on_join_and_region_switch_test() {
  use <- rollout_guard.with_platform_e2ee
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5102, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, 5102, g.join_with(5102, capable()), 1)
  let _ = joined(two, 5102, g.join_with(5102, capable()), 1)
  let assert Some(key) = g.key_of(server_update(one, 1))
  assert g.key_of(server_update(two, 1)) == Some(key)
  let assert Ok(call) = manager.lookup(g.channel_id(5102))
  assert call_server.update_region(call, Some("us-east"), 2000) == Ok(Nil)
  assert g.key_of(server_update(one, 2)) == Some(key)
  assert g.key_of(server_update(two, 2)) == Some(key)
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5102) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_e2ee_key_survives_the_p2p_to_sfu_switch_test() {
  use <- rollout_guard.with_platform_e2ee
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5104, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let p2p = [#("p2p", json.bool(True)), ..capable()]
  let c1 = joined(one, 5104, g.join_with(5104, p2p), 1)
  let _ = joined(two, 5104, g.join_with(5104, p2p), 1)
  send(
    one,
    g.update(Some(5104), Some(c1), [#("p2p", json.bool(False)), ..capable()]),
  )
  let first = server_update(one, 2)
  let second = server_update(two, 2)
  assert g.key_of(first) != None
  assert g.key_of(first) == g.key_of(second)
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5104) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_reissue_finishing_after_a_call_handoff_reaches_the_successor_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5103, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, 5103, g.join(5103), 1)
  let c2 = joined(two, 5103, g.join(5103), 1)
  let assert Ok(old) = manager.lookup(g.channel_id(5103))
  let assert Ok(old_pid) = process.subject_owner(old)
  api.set(stub, "voice_get_token", api.Hold)
  assert call_server.update_region(old, Some("us-east"), 2000) == Ok(Nil)
  assert api.await_held(stub, "voice_get_token", 1)
  let assert Ok(exported) = call_server.export(old, 2000)
  let assert Ok(started) = call_server.start(call_server.Restored(exported))
  process.unlink(started.pid)
  call_server.stop_for_handoff(old, started.data)
  assert api.eventually(fn() { !process.is_alive(old_pid) })
  assert api.release_token(stub)
  assert api.await_held(stub, "voice_get_token", 1)
  assert api.release_token(stub)
  assert g.text_of(server_update(one, 2), "connection_id") == Some(c1)
  assert g.text_of(server_update(two, 2), "connection_id") == Some(c2)
  process.kill(started.pid)
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_reissue_held_by_a_frozen_call_is_delivered_after_thaw_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5105, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, 5105, g.join(5105), 1)
  let _ = joined(two, 5105, g.join(5105), 1)
  let assert Ok(call) = manager.lookup(g.channel_id(5105))
  api.set(stub, "voice_get_token", api.Hold)
  assert call_server.update_region(call, Some("us-east"), 2000) == Ok(Nil)
  assert api.await_held(stub, "voice_get_token", 1)
  let assert Ok(_) = call_server.export(call, 2000)
  assert api.release_token(stub)
  let _ = api.eventually(fn() { False })
  assert list.length(server_updates(one)) + list.length(server_updates(two))
    == 2
  call_server.thaw(call)
  assert api.await_held(stub, "voice_get_token", 1)
  assert api.release_token(stub)
  let _ = server_update(one, 2)
  let _ = server_update(two, 2)
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5105) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

fn confirm_text(channel: Int, conn: String) -> String {
  let assert Ok(reply) =
    rpc.handle(
      "call.confirm_connection",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("connection_id", json.string(conn)),
        ]),
      ),
    )
  json.to_string(reply)
}

pub fn dm_late_confirm_for_a_removed_connection_is_refused_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5106, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, 5106, g.join(5106), 1)
  let c2 = joined(two, 5106, g.join(5106), 1)
  let assert Ok(_) =
    rpc.handle(
      "call.disconnect_user_if_in_channel",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string("5106")),
          #("user_id", json.string("1")),
          #("connection_id", json.string(c1)),
        ]),
      ),
    )
  assert api.eventually(fn() { connection_ids(5106) == [c2] })
  assert confirm_text(5106, c1)
    == "{\"success\":false,\"error\":\"connection_not_found\"}"
  assert confirm_text(5106, c2) == "{\"success\":true}"
  assert confirm_text(5106, c2)
    == "{\"success\":true,\"already_confirmed\":true}"
  assert confirm_text(5106, "not-joined-yet")
    == "{\"success\":false,\"error\":\"connection_not_found\"}"
  send(two, leave())
  assert api.eventually(fn() { call_gone(5106) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_crashed_call_is_replaced_and_members_rejoin_cleanly_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5107, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, 5107, g.join(5107), 1)
  let _ = joined(two, 5107, g.join(5107), 1)
  let assert Ok(crashed) = manager.lookup(g.channel_id(5107))
  let assert Ok(pid) = process.subject_owner(crashed)
  process.kill(pid)
  assert api.eventually(fn() { call_gone(5107) })
  create_call(5107, [1, 2])
  let again = joined(one, 5107, g.join(5107), 2)
  assert again != c1
  assert connection_ids(5107) == [again]
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5107) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_late_join_report_from_a_crashed_call_spares_the_rejoined_connection_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5110, [1, 2])
  let one = fake.start(1)
  let assert Ok(crashed) = manager.lookup(g.channel_id(5110))
  let assert Ok(_) = call_server.export(crashed, 2000)
  send(one, g.join(5110))
  assert api.eventually(fn() { dm_entries(one) != [] })
  let dm = dm_pid(one)
  assert suspend_process(dm)
  call_server.thaw(crashed)
  let assert Some(first) = g.text_of(server_update(one, 1), "connection_id")
  assert api.eventually(fn() { connection_ids(5110) == [first] })
  let assert Ok(pid) = process.subject_owner(crashed)
  process.kill(pid)
  assert api.eventually(fn() { call_gone(5110) })
  create_call(5110, [1, 2])
  send(one, g.join(5110))
  assert api.eventually(fn() { queued(dm) == 2 })
  assert resume_process(dm)
  let assert Some(again) = g.text_of(server_update(one, 2), "connection_id")
  assert api.eventually(fn() { connection_ids(5110) == [again] })
  assert api.eventually(fn() {
    list.map(dm_entries(one), voice_ids.of) == [again]
  })
  send(one, leave())
  assert api.eventually(fn() { call_gone(5110) })
  fake.stop(one)
  api.stop(stub)
}

pub fn dm_confirm_for_a_connection_the_call_never_saw_is_refused_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5108, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, 5108, g.join(5108), 1)
  assert confirm_text(5108, "sfu-2")
    == "{\"success\":false,\"error\":\"connection_not_found\"}"
  let c2 = joined(two, 5108, g.join(5108), 1)
  assert confirm_text(5108, c2) == "{\"success\":true}"
  assert confirm_text(5108, c1) == "{\"success\":true}"
  assert confirm_text(5108, c1)
    == "{\"success\":true,\"already_confirmed\":true}"
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5108) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_server_update_waits_for_the_call_to_accept_the_join_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5109, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, 5109, g.join(5109), 1)
  let assert Ok(call) = manager.lookup(g.channel_id(5109))
  let assert Ok(_) = call_server.export(call, 2000)
  send(two, g.join(5109))
  api.settle()
  assert server_updates(two) == []
  call_server.thaw(call)
  let update = server_update(two, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert confirm_text(5109, conn) == "{\"success\":true}"
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5109) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}
