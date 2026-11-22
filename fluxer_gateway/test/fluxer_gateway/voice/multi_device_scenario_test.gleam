// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/count_table
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids

const ok = "{\"success\":true}"

fn server_updates(host: g.Host, ref: SessionRef) -> List(Dynamic) {
  g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE")
}

fn await_server_update(host: g.Host, ref: SessionRef, count: Int) -> Dynamic {
  let assert True =
    api.eventually(fn() { list.length(server_updates(host, ref)) >= count })
  let assert Ok(last) = list.last(server_updates(host, ref))
  last
}

fn nonce_for(stub: api.Api, conn: String) -> Option(String) {
  let tokens = api.calls_of(stub, "voice_get_token") |> list.reverse
  let exact =
    list.find(tokens, fn(call) {
      api.field(call, "connection_id") == Some(conn)
    })
  case exact {
    Ok(call) -> api.field(call, "token_nonce")
    Error(Nil) -> None
  }
}

fn fresh_nonce(stub: api.Api, count: Int) -> Option(String) {
  api.calls_of(stub, "voice_get_token")
  |> list.filter(fn(call) { api.field(call, "connection_id") == None })
  |> list.drop(count - 1)
  |> list.first
  |> option.from_result
  |> option.then(api.field(_, "token_nonce"))
}

fn fresh_tokens(stub: api.Api) -> Int {
  api.calls_of(stub, "voice_get_token")
  |> list.count(fn(call) { api.field(call, "connection_id") == None })
}

fn joined(
  stub: api.Api,
  host: g.Host,
  ref: SessionRef,
  channel: Int,
) -> String {
  let before = list.length(server_updates(host, ref))
  let fresh = fresh_tokens(stub)
  g.client(host, ref, g.join(channel))
  let update = await_server_update(host, ref, before + 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(host, conn, fresh_nonce(stub, fresh + 1)) == ok
  conn
}

fn moved(stub: api.Api, host: g.Host, conn: String) -> Nil {
  assert api.eventually(fn() { nonce_for(stub, conn) != None })
  assert g.confirm(host, conn, nonce_for(stub, conn)) == ok
}

fn placement(host: g.Host) -> List(#(String, String, Option(String))) {
  g.states(host)
  |> list.map(fn(view) { #(view.user, view.connection_id, view.channel) })
  |> list.sort(fn(a, b) { string.compare(a.1, b.1) })
}

fn ordered(rows: List(#(String, String, Option(String)))) {
  list.sort(rows, fn(a, b) { string.compare(a.1, b.1) })
}

fn muted(host: g.Host) -> List(#(String, Bool)) {
  let voice = g.inspect(host).0
  guild_handler.voice_states(voice)
  |> list.map(fn(found) {
    let assert Ok(pair) =
      json.parse(json.to_string(guild_handler.voice_state_json(voice, found)), {
        use conn <- decode.field("connection_id", decode.string)
        use mute <- decode.field("mute", decode.bool)
        decode.success(#(conn, mute))
      })
    pair
  })
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
}

fn forced(stub: api.Api) -> List(String) {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.filter_map(fn(call) {
    option.to_result(api.field(call, "connection_id"), Nil)
  })
  |> list.unique
  |> list.sort(string.compare)
}

fn channel_states(host: g.Host, channel: Int) -> List(String) {
  let assert Ok(found) =
    g.rpc(host, guild_state.ChannelStates(g.channel_id(channel)))
  let assert Ok(conns) =
    json.parse(
      json.to_string(found),
      decode.at(
        ["voice_states"],
        decode.list(decode.at(["connection_id"], decode.string)),
      ),
    )
  conns
}

fn deny(world: g.World, channel: Int, user: Int) -> g.World {
  g.World(
    ..world,
    channels: list.map(world.channels, fn(found) {
      case found.id == channel {
        True -> g.VoiceChannel(..found, denied: [user, ..found.denied])
        False -> found
      }
    }),
  )
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(Positive)) -> Int

type Positive {
  Positive
}

fn unique_token(body: Dynamic) -> api.Response {
  let call = api.Call("voice_get_token", body)
  api.token(case api.field(call, "connection_id") {
    Some(conn) -> conn
    None -> "device-" <> int.to_string(unique_integer([Positive]))
  })
}

fn start_stub() -> api.Api {
  let stub = api.start()
  api.set(stub, "voice_get_token", api.Respond(unique_token))
  stub
}

fn with_world(world: g.World, body: fn(api.Api, g.Host) -> Nil) -> Nil {
  let stub = start_stub()
  let host = g.start(world)
  body(stub, host)
  g.stop(host)
  api.stop(stub)
}

fn scenario(guild: Int, body: fn(api.Api, g.Host) -> Nil) -> Nil {
  with_world(g.World(..g.world(), guild:), body)
}

pub fn two_devices_join_the_same_channel_test() {
  use stub, host <- scenario(951)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 10)
  assert a != b
  assert placement(host)
    == ordered([#("1", a, Some("10")), #("1", b, Some("10"))])
  let counted = voice_ids.counted()
  assert list.contains(counted, a) && list.contains(counted, b)
}

pub fn three_devices_in_two_channels_are_three_states_test() {
  use stub, host <- scenario(952)
  let a = joined(stub, host, g.session(1), 10)
  let b = joined(stub, host, g.session(1), 20)
  let c = joined(stub, host, g.session(1), 10)
  let other = joined(stub, host, g.session(2), 10)
  assert placement(host)
    == ordered([
      #("1", a, Some("10")),
      #("1", b, Some("20")),
      #("1", c, Some("10")),
      #("2", other, Some("10")),
    ])
  let channels =
    channel_states(host, 10)
    |> list.sort(string.compare)
  assert channels == list.sort([a, c, other], string.compare)
}

pub fn moving_one_device_leaves_the_others_in_place_test() {
  use stub, host <- scenario(953)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 10)
  g.client(host, phone, g.update(Some(20), Some(a), []))
  let update = await_server_update(host, phone, 2)
  assert g.text_of(update, "channel_id") == Some("20")
  moved(stub, host, a)
  assert placement(host)
    == ordered([#("1", a, Some("20")), #("1", b, Some("10"))])
  assert list.length(server_updates(host, desktop)) == 1
}

pub fn server_mute_reaches_every_device_of_the_user_test() {
  use stub, host <- scenario(954)
  let a = joined(stub, host, g.session(1), 10)
  let b = joined(stub, host, g.session(1), 20)
  let c = joined(stub, host, g.session(2), 10)
  assert g.rpc_text(
      host,
      guild_state.MemberVoice(g.user_id(1), mute: True, deaf: False),
    )
    == ok
  assert muted(host)
    == list.sort([#(a, True), #(b, True), #(c, False)], fn(x, y) {
      string.compare(x.0, y.0)
    })
  let joined_later = joined(stub, host, g.session(1), 10)
  assert list.contains(muted(host), #(joined_later, True))
}

pub fn moderator_move_of_the_user_moves_every_device_test() {
  let base = g.world()
  let world =
    g.World(..base, guild: 955, channels: [
      g.VoiceChannel(50, 0, False, [], True),
      ..base.channels
    ])
  use stub, host <- with_world(world)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 50)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(
        g.user_id(1),
        g.user_id(2),
        Some(g.channel_id(20)),
        None,
      ),
    )
    == ok
  let to_phone = await_server_update(host, phone, 2)
  let to_desktop = await_server_update(host, desktop, 2)
  assert g.text_of(to_phone, "channel_id") == Some("20")
  assert g.text_of(to_desktop, "channel_id") == Some("20")
  moved(stub, host, a)
  moved(stub, host, b)
  assert placement(host)
    == ordered([#("1", a, Some("20")), #("1", b, Some("20"))])
}

pub fn moderator_move_of_one_connection_moves_only_that_device_test() {
  use stub, host <- scenario(956)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 10)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(
        g.user_id(1),
        g.user_id(2),
        Some(g.channel_id(20)),
        Some(voice_ids.conn(b)),
      ),
    )
    == ok
  let update = await_server_update(host, desktop, 2)
  assert g.text_of(update, "channel_id") == Some("20")
  moved(stub, host, b)
  assert placement(host)
    == ordered([#("1", a, Some("10")), #("1", b, Some("20"))])
  assert list.length(server_updates(host, phone)) == 1
}

pub fn permission_loss_drops_every_device_in_that_channel_only_test() {
  let world = g.World(..g.world(), guild: 957)
  use stub, host <- with_world(world)
  let a = joined(stub, host, g.session(1), 10)
  let b = joined(stub, host, g.session(1), 10)
  let c = joined(stub, host, g.session(1), 20)
  let other = joined(stub, host, g.session(2), 10)
  g.update_world(host, deny(world, 10, 1))
  g.run(host, guild_handler.on_permissions_change)
  assert api.eventually(fn() {
    forced(stub) == list.sort([a, b], string.compare)
  })
  api.settle()
  assert forced(stub) == list.sort([a, b], string.compare)
  assert !list.contains(forced(stub), c)
  assert !list.contains(forced(stub), other)
}

pub fn disconnecting_one_device_keeps_the_others_test() {
  use stub, host <- scenario(958)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 10)
  let c = joined(stub, host, desktop, 20)
  g.client(host, phone, g.update(None, Some(a), []))
  assert api.eventually(fn() {
    placement(host) == ordered([#("1", b, Some("10")), #("1", c, Some("20"))])
  })
  assert api.eventually(fn() { forced(stub) == [a] })
  assert api.eventually(fn() { !list.contains(voice_ids.counted(), a) })
  let counted = voice_ids.counted()
  assert list.contains(counted, b) && list.contains(counted, c)
}

pub fn leaving_from_one_session_keeps_the_other_sessions_test() {
  use stub, host <- scenario(959)
  let phone = g.session(1)
  let desktop = g.session(1)
  let a = joined(stub, host, phone, 10)
  let b = joined(stub, host, desktop, 10)
  g.client(host, desktop, g.update(None, None, []))
  assert api.eventually(fn() { placement(host) == [#("1", a, Some("10"))] })
  assert api.eventually(fn() { forced(stub) == [b] })
}

pub fn disconnect_user_removes_every_device_in_the_channel_test() {
  use stub, host <- scenario(960)
  let a = joined(stub, host, g.session(1), 10)
  let _b = joined(stub, host, g.session(1), 10)
  let c = joined(stub, host, g.session(1), 20)
  let other = joined(stub, host, g.session(2), 10)
  assert g.rpc_text(
      host,
      guild_state.DisconnectIfInChannel(g.user_id(1), g.channel_id(10), None),
    )
    == ok
  assert placement(host)
    == ordered([#("1", c, Some("20")), #("2", other, Some("10"))])
  assert g.rpc_text(
      host,
      guild_state.DisconnectIfInChannel(
        g.user_id(1),
        g.channel_id(20),
        Some(voice_ids.conn(a)),
      ),
    )
    == "{\"success\":true,\"ignored\":true}"
  assert placement(host)
    == ordered([#("1", c, Some("20")), #("2", other, Some("10"))])
}

pub fn disconnect_one_connection_by_rpc_keeps_the_rest_test() {
  use stub, host <- scenario(961)
  let a = joined(stub, host, g.session(1), 10)
  let b = joined(stub, host, g.session(1), 10)
  assert g.rpc_text(
      host,
      guild_state.DisconnectIfInChannel(
        g.user_id(1),
        g.channel_id(10),
        Some(voice_ids.conn(a)),
      ),
    )
    == ok
  assert placement(host) == [#("1", b, Some("10"))]
}

pub fn member_leaving_the_guild_drops_every_device_test() {
  let world = g.World(..g.world(), guild: 962)
  use stub, host <- with_world(world)
  let _a = joined(stub, host, g.session(1), 10)
  let _b = joined(stub, host, g.session(1), 20)
  let c = joined(stub, host, g.session(2), 10)
  g.update_world(
    host,
    g.World(..world, members: list.filter(world.members, fn(m) { m != 1 })),
  )
  g.run(host, fn(voice, ctx) {
    guild_handler.on_member_change(voice, ctx, g.user_id(1))
  })
  assert api.eventually(fn() { placement(host) == [#("2", c, Some("10"))] })
}

pub fn hand_off_keeps_every_device_of_the_user_test() {
  let world = g.World(..g.world(), guild: 963)
  let stub = start_stub()
  let source = g.start(world)
  let a = joined(stub, source, g.session(1), 10)
  let b = joined(stub, source, g.session(1), 10)
  let c = joined(stub, source, g.session(1), 20)
  let #(voice, _successor) = g.freeze(source)
  let target = g.adopt(world, voice)
  g.forward(source)
  assert api.eventually(fn() { !process.is_alive(source.pid) })
  assert placement(target)
    == ordered([
      #("1", a, Some("10")),
      #("1", b, Some("10")),
      #("1", c, Some("20")),
    ])
  assert api.eventually(fn() {
    list.all([a, b, c], fn(conn) {
      list.any(count_table.rows(), fn(row) {
        voice_ids.row_text(row) == conn && row.owner == target.pid
      })
    })
  })
  assert g.rpc_text(
      target,
      guild_state.DisconnectIfInChannel(
        g.user_id(1),
        g.channel_id(10),
        Some(voice_ids.conn(b)),
      ),
    )
    == ok
  assert placement(target)
    == ordered([#("1", a, Some("10")), #("1", c, Some("20"))])
  g.stop(target)
  api.stop(stub)
}
