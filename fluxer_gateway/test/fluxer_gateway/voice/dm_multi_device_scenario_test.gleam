// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/session/control
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client
import fluxer_gateway/voice/rpc
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids
import support/voice_session as fake

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

fn view_of(channel: Int) -> Result(#(List(String), List(String)), Nil) {
  case manager.lookup(g.channel_id(channel)) {
    Ok(call) ->
      case call_server.get_state(call, 2000) {
        Ok(view) ->
          Ok(#(
            list.map(view.connections, voice_ids.of)
              |> list.sort(string.compare),
            list.map(view.voice_states, voice_ids.of)
              |> list.sort(string.compare),
          ))
        Error(Nil) -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

fn connection_ids(channel: Int) -> List(String) {
  case view_of(channel) {
    Ok(#(connections, _)) -> connections
    Error(Nil) -> []
  }
}

fn listed(channel: Int) -> List(String) {
  case view_of(channel) {
    Ok(#(_, states)) -> states
    Error(Nil) -> []
  }
}

fn sorted(items: List(String)) -> List(String) {
  list.sort(items, string.compare)
}

fn send(session: fake.Fake, data: Json) -> Nil {
  client.dm_voice_update(session.ref, g.client_update(data), calls: [])
}

fn joined(session: fake.Fake, data: Json, count: Int) -> String {
  send(session, data)
  assert api.eventually(fn() {
    list.length(fake.dispatched(session, "VOICE_SERVER_UPDATE")) >= count
  })
  let assert Ok(update) =
    list.last(fake.dispatched(session, "VOICE_SERVER_UPDATE"))
  let assert Some(conn) = g.text_of(update, "connection_id")
  let assert Some(channel) = g.text_of(update, "channel_id")
  let assert Ok(channel) = int.parse(channel)
  assert api.eventually(fn() { list.contains(connection_ids(channel), conn) })
  conn
}

fn leave() -> Json {
  g.update(None, None, [])
}

fn call_gone(channel: Int) -> Bool {
  manager.lookup(g.channel_id(channel)) == Error(manager.NotFound)
}

fn disconnect(channel: Int, user: Int, conn: option.Option(String)) -> String {
  let base = [
    #("channel_id", json.string(int.to_string(channel))),
    #("user_id", json.string(int.to_string(user))),
  ]
  let fields = case conn {
    Some(conn) -> [#("connection_id", json.string(conn)), ..base]
    None -> base
  }
  let assert Ok(reply) =
    rpc.handle(
      "call.disconnect_user_if_in_channel",
      raw_json.from_json(json.object(fields)),
    )
  json.to_string(reply)
}

pub fn two_devices_of_one_user_are_both_in_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5201, [1, 2])
  let phone = fake.start(1)
  let desktop = fake.start(1)
  let two = fake.start(2)
  let a = joined(phone, g.join(5201), 1)
  let b = joined(desktop, g.join(5201), 1)
  let c = joined(two, g.join(5201), 1)
  assert connection_ids(5201) == sorted([a, b, c])
  assert listed(5201) == sorted([a, b, c])
  send(phone, leave())
  assert api.eventually(fn() { connection_ids(5201) == sorted([b, c]) })
  send(desktop, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5201) })
  fake.stop(phone)
  fake.stop(desktop)
  fake.stop(two)
  api.stop(stub)
}

pub fn one_session_with_two_connections_keeps_both_in_the_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5202, [1, 2])
  let one = fake.start(1)
  let a = joined(one, g.join(5202), 1)
  let b = joined(one, g.join(5202), 2)
  assert a != b
  assert connection_ids(5202) == sorted([a, b])
  send(one, g.update(None, Some(a), []))
  assert api.eventually(fn() { connection_ids(5202) == [b] })
  send(one, g.update(None, Some(b), []))
  assert api.eventually(fn() { call_gone(5202) })
  fake.stop(one)
  api.stop(stub)
}

pub fn disconnecting_one_device_by_rpc_keeps_the_others_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5203, [1, 2])
  let one = fake.start(1)
  let phone = fake.start(1)
  let two = fake.start(2)
  let a = joined(one, g.join(5203), 1)
  let b = joined(one, g.join(5203), 2)
  let p = joined(phone, g.join(5203), 1)
  let c = joined(two, g.join(5203), 1)
  assert disconnect(5203, 1, Some(a)) == "{\"success\":true}"
  assert api.eventually(fn() { connection_ids(5203) == sorted([b, p, c]) })
  assert disconnect(5203, 1, Some(a))
    == "{\"success\":true,\"ignored\":true,\"reason\":\"not_in_call\"}"
  assert disconnect(5203, 1, None) == "{\"success\":true}"
  assert api.eventually(fn() { connection_ids(5203) == [c] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5203) })
  fake.stop(one)
  fake.stop(phone)
  fake.stop(two)
  api.stop(stub)
}

pub fn moving_one_connection_leaves_the_sibling_in_the_old_call_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5204, [1, 2])
  create_call(5205, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let a = joined(one, g.join(5204), 1)
  let b = joined(one, g.join(5204), 2)
  let c = joined(two, g.join(5205), 1)
  send(one, g.update(Some(5205), Some(a), []))
  assert api.eventually(fn() { connection_ids(5205) == sorted([a, c]) })
  assert api.eventually(fn() { connection_ids(5204) == [b] })
  send(one, leave())
  assert api.eventually(fn() { call_gone(5204) })
  assert api.eventually(fn() { connection_ids(5205) == [c] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5205) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn a_handed_off_session_keeps_both_of_its_connections_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5206, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let a = joined(one, g.join(5206), 1)
  let b = joined(one, g.join(5206), 2)
  let c = joined(two, g.join(5206), 1)
  client.session_handed_off(one.ref)
  let moved = fake.start(1)
  let rebound = session.SessionRef(..moved.ref, id: one.ref.id)
  client.session_rehydrated(rebound, [g.channel_id(5206)])
  fake.exit_normally(one)
  assert api.eventually(fn() {
    list.any(fake.controls(moved), fn(found) {
      case found {
        control.CallMonitor(..) -> True
        _ -> False
      }
    })
  })
  assert connection_ids(5206) == sorted([a, b, c])
  client.dm_voice_update(
    rebound,
    g.client_update(g.update(None, Some(a), [])),
    calls: [],
  )
  assert api.eventually(fn() { connection_ids(5206) == sorted([b, c]) })
  client.dm_voice_update(rebound, g.client_update(leave()), calls: [])
  assert api.eventually(fn() { connection_ids(5206) == [c] })
  send(two, leave())
  assert api.eventually(fn() { call_gone(5206) })
  fake.stop(moved)
  fake.stop(two)
  api.stop(stub)
}

fn media_kicks(stub: api.Api) -> List(#(String, option.Option(String))) {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.filter_map(fn(call) {
    case api.field(call, "connection_id") {
      Some(conn) -> Ok(#(conn, api.field(call, "guild_id")))
      None -> Error(Nil)
    }
  })
}

pub fn disconnecting_another_device_kicks_it_from_the_media_room_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5204, [1, 2])
  let phone = fake.start(1)
  let desktop = fake.start(1)
  let two = fake.start(2)
  let p = joined(phone, g.join(5204), 1)
  let d = joined(desktop, g.join(5204), 1)
  let c = joined(two, g.join(5204), 1)
  client.dm_voice_update(
    desktop.ref,
    g.client_update(g.update(None, Some(p), [])),
    calls: [g.channel_id(5204)],
  )
  assert api.eventually(fn() { connection_ids(5204) == sorted([d, c]) })
  assert api.eventually(fn() { list.contains(media_kicks(stub), #(p, None)) })
  assert !list.any(media_kicks(stub), fn(kick) { kick.0 == d })
  send(desktop, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5204) })
  fake.stop(phone)
  fake.stop(desktop)
  fake.stop(two)
  api.stop(stub)
}

pub fn disconnecting_another_users_device_is_ignored_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5205, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let a = joined(one, g.join(5205), 1)
  let c = joined(two, g.join(5205), 1)
  client.dm_voice_update(
    one.ref,
    g.client_update(g.update(None, Some(c), [])),
    calls: [g.channel_id(5205)],
  )
  api.settle()
  assert connection_ids(5205) == sorted([a, c])
  assert !list.any(media_kicks(stub), fn(kick) { kick.0 == c })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5205) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}

pub fn disconnecting_an_own_device_kicks_it_from_the_media_room_test() {
  let stub = api.start()
  api.set(stub, "get_dm_channel", dm_channel([1, 2]))
  create_call(5206, [1, 2])
  let one = fake.start(1)
  let two = fake.start(2)
  let a = joined(one, g.join(5206), 1)
  let b = joined(one, g.join(5206), 2)
  let c = joined(two, g.join(5206), 1)
  send(one, g.update(None, Some(a), []))
  assert api.eventually(fn() { connection_ids(5206) == sorted([b, c]) })
  assert api.eventually(fn() { list.contains(media_kicks(stub), #(a, None)) })
  send(one, leave())
  send(two, leave())
  assert api.eventually(fn() { call_gone(5206) })
  fake.stop(one)
  fake.stop(two)
  api.stop(stub)
}
