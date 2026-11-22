// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/session/control
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/rpc
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/geo_probe as probe
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake
import support/wait

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> #(Atom, Int)

fn queued(pid: Pid) -> Int {
  process_info(pid, atom.create("message_queue_len")).1
}

@external(erlang, "sys", "get_state")
fn sys_get_state(pid: Pid) -> Dynamic

fn dm_server_pid(ref: session.SessionRef) -> Pid {
  let assert Ok(subject) = dm_server.lookup(ref)
  let assert Ok(pid) = process.subject_owner(subject)
  pid
}

fn monitors_call(session: fake.Fake) -> Bool {
  list.any(fake.controls(session), fn(found) {
    case found {
      control.CallMonitor(..) -> True
      _ -> False
    }
  })
}

fn dm_channel(kind: Int, recipients: List(Int)) -> api.Rule {
  api.Reply(
    api.Data(
      json.object([
        #(
          "channel",
          json.object([
            #("type", json.int(kind)),
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
  let ids =
    json.array(recipients, fn(user) { json.string(int.to_string(user)) })
  let assert Ok(_) =
    rpc.handle(
      "call.create",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string(int.to_string(channel))),
          #("message_id", json.string(int.to_string(channel + 1000))),
          #("region", json.null()),
          #("ringing", json.array([], json.string)),
          #("recipients", ids),
        ]),
      ),
    )
  Nil
}

fn connections(channel: Int) -> Option(List(entry.Entry)) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(view) -> Some(view.connections)
        Error(Nil) -> None
      }
    Error(_) -> None
  }
}

fn connection_ids(channel: Int) -> List(String) {
  connections(channel)
  |> option.unwrap([])
  |> list.map(voice_ids.of)
}

fn send(session: fake.Fake, data: Json) -> Nil {
  client.dm_voice_update(session.ref, g.client_update(data), calls: [])
}

fn server_update(session: fake.Fake, count: Int) -> Dynamic {
  let assert True =
    api.eventually(fn() {
      list.length(fake.dispatched(session, "VOICE_SERVER_UPDATE")) >= count
    })
  let assert Ok(last) =
    list.last(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
  last
}

fn joined(session: fake.Fake, data: Json, count: Int) -> String {
  send(session, data)
  let update = server_update(session, count)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert api.eventually(fn() {
    list.contains(connection_ids(channel_of(update)), conn)
  })
  conn
}

fn channel_of(update: Dynamic) -> Int {
  let assert Some(text) = g.text_of(update, "channel_id")
  let assert Ok(value) = int.parse(text)
  value
}

fn leave() -> Json {
  g.update(None, None, [])
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

fn call_gone(channel: Int) -> Bool {
  manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
}

pub fn dm_call_join_confirm_and_end_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5001, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, g.join(5001), 1)
  let c2 = joined(two, g.join(5001), 1)
  assert c1 != c2
  let assert Ok(reply) =
    rpc.handle(
      "call.confirm_connection",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string("5001")),
          #("connection_id", json.string(c1)),
        ]),
      ),
    )
  assert json.to_string(reply) == "{\"success\":true}"
  send(one, leave())
  assert api.eventually(fn() { connection_ids(5001) == [c2] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5001) })
  assert api.eventually(fn() { api.calls_of(stub, "call_ended") != [] })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_disconnect_all_with_a_pending_join_leaves_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  let one = fake.start(1)
  let two = fake.start(2)
  send(one, g.join(5002))
  assert api.eventually(fn() { api.calls_of(stub, "voice_get_token") != [] })
  send(one, leave())
  create_call(5002, [1, 2])
  let c2 = joined(two, g.join(5002), 1)
  let _ = api.eventually(fn() { False })
  let _ = api.eventually(fn() { False })
  assert connection_ids(5002) == [c2]
  send(two, leave())
  assert api.eventually(fn() { call_gone(5002) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_p2p_pair_switches_to_sfu_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5003, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let p2p = [#("p2p", json.bool(True))]
  let c1 = joined(one, g.join_with(5003, p2p), 1)
  let _ = joined(two, g.join_with(5003, p2p), 1)
  assert option.unwrap(connections(5003), []) |> list.all(entry.is_p2p)
  send(one, g.update(Some(5003), Some(c1), [#("p2p", json.bool(False))]))
  let switched1 = server_update(one, 2)
  let switched2 = server_update(two, 2)
  assert g.text_of(switched1, "token") != None
  assert g.text_of(switched2, "token") != None
  assert api.eventually(fn() {
    connections(5003)
    |> option.unwrap([])
    |> list.all(fn(value) { !entry.is_p2p(value) })
  })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5003) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_api_force_disconnect_frees_the_connection_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5004, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, g.join(5004), 1)
  let c2 = joined(two, g.join(5004), 1)
  let assert Ok(_) =
    rpc.handle(
      "call.disconnect_user_if_in_channel",
      raw_json.from_json(
        json.object([
          #("channel_id", json.string("5004")),
          #("user_id", json.string("1")),
          #("connection_id", json.string(c1)),
        ]),
      ),
    )
  assert api.eventually(fn() { connection_ids(5004) == [c2] })
  let again = joined(one, g.join(5004), 2)
  assert api.eventually(fn() {
    list.sort(connection_ids(5004), fn(a, b) { string.compare(a, b) })
    == list.sort([again, c2], fn(a, b) { string.compare(a, b) })
  })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5004) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_session_hand_off_rebinds_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5005, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, g.join(5005), 1)
  let c2 = joined(two, g.join(5005), 1)
  client.session_handed_off(one.ref)
  let moved = fake.start(1)
  let rebound = session.SessionRef(..moved.ref, id: one.ref.id)
  client.session_rehydrated(rebound, [g.channel_id(5005)])
  fake.exit_normally(one)
  assert api.eventually(fn() {
    list.any(fake.controls(moved), fn(found) {
      case found {
        control.CallMonitor(..) -> True
        _ -> False
      }
    })
  })
  let assert Ok(call) = manager.lookup(g.channel_id(5005))
  assert call_server.update_region(call, Some("us-east"), 2000) == Ok(Nil)
  let switched = server_update(moved, 1)
  assert g.text_of(switched, "connection_id") == Some(c1)
  assert fake.dispatched(one, "VOICE_SERVER_UPDATE") |> list.length == 1
  assert list.contains(connection_ids(5005), c1)
  assert list.contains(connection_ids(5005), c2)
  client.dm_voice_update(rebound, g.client_update(leave()), calls: [])
  assert api.eventually(fn() { connection_ids(5005) == [c2] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5005) })
  fake.stop(moved)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_adoption_waits_out_a_hand_off_still_queued_on_the_old_server_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5015, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, g.join(5015), 1)
  let c2 = joined(two, g.join(5015), 1)
  let old = dm_server_pid(one.ref)
  assert suspend_process(old)
  client.session_handed_off(one.ref)
  let base = queued(old)
  let moved = fake.start(1)
  let rebound = session.SessionRef(..moved.ref, id: one.ref.id)
  client.session_rehydrated(rebound, [g.channel_id(5015)])
  assert wait.until(5000, fn() { queued(old) > base })
  assert resume_process(old)
  fake.exit_normally(one)
  assert api.eventually(fn() { monitors_call(moved) })
  assert dm_server_pid(rebound) != old
  assert list.contains(connection_ids(5015), c1)
  client.dm_voice_update(rebound, g.client_update(leave()), calls: [])
  assert api.eventually(fn() { connection_ids(5015) == [c2] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5015) })
  fake.stop(moved)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_rehydration_before_the_old_session_hands_off_keeps_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5016, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let c1 = joined(one, g.join(5016), 1)
  let c2 = joined(two, g.join(5016), 1)
  let old = dm_server_pid(one.ref)
  let moved = fake.start(1)
  let rebound = session.SessionRef(..moved.ref, id: one.ref.id)
  client.session_rehydrated(rebound, [g.channel_id(5016)])
  assert api.eventually(fn() { monitors_call(moved) })
  client.session_handed_off(one.ref)
  client.session_closed(one.ref)
  fake.exit_normally(one)
  assert wait.until(5000, fn() { !process.is_alive(old) })
  let current = dm_server_pid(rebound)
  let _ = sys_get_state(current)
  assert process.is_alive(current)
  assert list.contains(connection_ids(5016), c1)
  assert list.contains(connection_ids(5016), c2)
  client.dm_voice_update(rebound, g.client_update(leave()), calls: [])
  assert api.eventually(fn() { connection_ids(5016) == [c2] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5016) })
  fake.stop(moved)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_region_switch_token_for_a_departed_member_is_dropped_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5006, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, g.join(5006), 1)
  let c2 = joined(two, g.join(5006), 1)
  api.set(stub, "voice_get_token", api.Hold)
  let assert Ok(call) = manager.lookup(g.channel_id(5006))
  assert call_server.update_region(call, Some("us-east"), 2000) == Ok(Nil)
  assert api.await_held(stub, "voice_get_token", 1)
  send(one, leave())
  assert api.eventually(fn() { connection_ids(5006) == [c2] })
  assert api.release_token(stub)
  assert api.await_held(stub, "voice_get_token", 1)
  assert api.release_token(stub)
  let switched = server_update(two, 2)
  assert g.text_of(switched, "connection_id") == Some(c2)
  let _ = api.eventually(fn() { False })
  assert list.length(fake.dispatched(one, "VOICE_SERVER_UPDATE")) == 1
  send(two, leave())
  assert api.eventually(fn() { call_gone(5006) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_move_between_calls_leaves_the_old_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(3, [1, 2, 3]))
  create_call(5007, [1, 2, 3])
  create_call(5008, [1, 2, 3])
  let one = fake.start(1)
  let two = fake.start(2)
  let three = fake.start(3)
  let c1 = joined(one, g.join(5007), 1)
  let c2 = joined(two, g.join(5007), 1)
  let c3 = joined(three, g.join(5008), 1)
  send(one, g.update(Some(5008), Some(c1), []))
  let moved = server_update(one, 2)
  assert g.text_of(moved, "channel_id") == Some("5008")
  assert api.eventually(fn() { connection_ids(5007) == [c2] })
  assert api.eventually(fn() {
    list.sort(connection_ids(5008), string.compare)
    == list.sort([c1, c3], string.compare)
  })
  send(one, leave())
  send(two, leave())
  send(three, leave())
  assert api.eventually(fn() { call_gone(5007) && call_gone(5008) })
  fake.stop(one)
  fake.stop(two)
  fake.stop(three)
  api.stop(stub)
}

pub fn dm_crashed_session_leaves_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5009, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, g.join(5009), 1)
  let c2 = joined(two, g.join(5009), 1)
  fake.stop(one)
  assert api.eventually(fn() { connection_ids(5009) == [c2] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5009) })
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_connection_limit_counts_live_connections_only_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5010, [1, 2])
  let two = fake.start(2)
  let c2 = joined(two, g.join(5010), 1)
  let one = fake.start(1)
  let first = joined(one, g.join(5010), 1)
  send(one, g.update(None, Some(first), []))
  assert api.eventually(fn() { connection_ids(5010) == [c2] })
  let second = joined(one, g.join(5010), 2)
  assert second != first
  assert api.eventually(fn() { list.length(connection_ids(5010)) == 2 })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5010) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

fn located(call: api.Call) -> Bool {
  api.field(call, "latitude") == Some(probe.latitude)
  && api.field(call, "longitude") == Some(probe.longitude)
}

fn token_calls(stub: api.Api) -> List(api.Call) {
  api.calls_of(stub, "voice_get_token")
}

pub fn dm_p2p_to_sfu_reissue_keeps_each_callers_location_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  create_call(5010, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let p2p = [#("p2p", json.bool(True)), ..probe.fields()]
  let c1 = joined(one, g.join_with(5010, p2p), 1)
  let _ = joined(two, g.join_with(5010, p2p), 1)
  assert list.length(token_calls(stub)) == 2
  send(one, g.update(Some(5010), Some(c1), [#("p2p", json.bool(False))]))
  let _ = server_update(one, 2)
  let _ = server_update(two, 2)
  let calls = token_calls(stub)
  assert list.length(calls) >= 4
  assert list.all(calls, located)
  assert !list.any(fake.dispatched(two, "VOICE_SERVER_UPDATE"), fn(update) {
    probe.leaks(string.inspect(update))
  })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5010) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_region_switch_reissue_keeps_each_callers_location_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  create_call(5011, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let _ = joined(one, g.join_with(5011, probe.fields()), 1)
  let _ = joined(two, g.join(5011), 1)
  let assert Ok(call) = manager.lookup(g.channel_id(5011))
  assert call_server.update_region(call, Some("us-east"), 2000) == Ok(Nil)
  let _ = server_update(one, 2)
  let _ = server_update(two, 2)
  let assert [first, second, ..reissued] = token_calls(stub)
  assert located(first)
  assert !located(second)
  assert list.length(reissued) == 2
  let by_user =
    list.map(reissued, fn(found) {
      #(api.field(found, "user_id"), located(found))
    })
  assert list.contains(by_user, #(Some("1"), True))
  assert list.contains(by_user, #(Some("2"), False))
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5011) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn dm_caller_gets_the_server_update_before_the_call_exists_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel(1, [1, 2]))
  let one = fake.start(1)
  send(one, g.join(5101))
  let update = server_update(one, 1)
  assert call_gone(5101)
  let assert Some(conn) = g.text_of(update, "connection_id")
  create_call(5101, [1, 2])
  assert api.eventually(fn() { connection_ids(5101) == [conn] })
  send(one, leave())
  assert api.eventually(fn() { call_gone(5101) })
  fake.stop(one)
  api.stop(stub)
}
