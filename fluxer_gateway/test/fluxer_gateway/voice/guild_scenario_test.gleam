// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/error
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids

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
  let fresh =
    list.find(tokens, fn(call) { api.field(call, "connection_id") == None })
  case exact, fresh {
    Ok(call), _ -> api.field(call, "token_nonce")
    Error(Nil), Ok(call) -> api.field(call, "token_nonce")
    Error(Nil), Error(Nil) -> None
  }
}

fn confirmed(stub: api.Api, host: g.Host, conn: String) -> String {
  g.confirm(host, conn, nonce_for(stub, conn))
}

fn join_as(
  stub: api.Api,
  host: g.Host,
  ref: SessionRef,
  data: json.Json,
) -> String {
  let before = list.length(server_updates(host, ref))
  g.client(host, ref, data)
  let update = await_server_update(host, ref, before + 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert confirmed(stub, host, conn) == ok
  conn
}

fn joined(
  stub: api.Api,
  host: g.Host,
  ref: SessionRef,
  channel: Int,
) -> String {
  join_as(stub, host, ref, g.join(channel))
}

const ok = "{\"success\":true}"

fn view_of(host: g.Host, conn: String) -> Option(g.View) {
  g.states(host)
  |> list.find(fn(view) { view.connection_id == conn })
  |> option.from_result
}

fn in_channel(host: g.Host, conn: String) -> Option(String) {
  view_of(host, conn) |> option.then(fn(view) { view.channel })
}

fn with_world(world: g.World, body: fn(api.Api, g.Host) -> Nil) -> Nil {
  let stub = api.start()
  let host = g.start(world)
  body(stub, host)
  g.stop(host)
  api.stop(stub)
}

fn scenario(body: fn(api.Api, g.Host) -> Nil) -> Nil {
  with_world(g.world(), body)
}

fn e2ee_world() -> g.World {
  g.World(..g.world(), features: ["VOICE_E2EE"])
}

fn capable() -> List(#(String, json.Json)) {
  [#("e2ee_capable", json.bool(True))]
}

pub fn join_confirm_and_leave_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  assert in_channel(host, conn) == Some("10")
  assert api.eventually(fn() { api.calls_of(stub, "voice_state_upsert") != [] })
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  assert api.eventually(fn() { api.calls_of(stub, "voice_state_remove") != [] })
}

pub fn token_refusal_stores_nothing_test() {
  use stub, host <- scenario
  api.set(
    stub,
    "voice_get_token",
    api.Reply(api.Status(
      403,
      "{\"code\":\"UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS\"}",
    )),
  )
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  assert api.eventually(fn() { api.calls_of(stub, "voice_get_token") != [] })
  api.settle()
  assert server_updates(host, s1) == []
  assert g.states(host) == []
}

pub fn wrong_nonce_is_refused_test() {
  use _stub, host <- scenario
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  let update = await_server_update(host, s1, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(host, conn, Some("wrong"))
    == "{\"success\":false,\"error\":\"voice_nonce_mismatch\"}"
  assert g.states(host) == []
}

pub fn client_move_switches_channel_after_confirm_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  g.client(host, s1, g.update(Some(20), Some(conn), []))
  let update = await_server_update(host, s1, 2)
  assert g.text_of(update, "channel_id") == Some("20")
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("20")
  assert list.length(g.states(host)) == 1
}

pub fn moderator_move_reissues_into_the_target_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
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
  let update = await_server_update(host, s1, 2)
  assert g.text_of(update, "channel_id") == Some("20")
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("20")
}

pub fn region_switch_hands_the_room_key_to_e2ee_members_test() {
  use stub, host <- with_world(e2ee_world())
  let s1 = g.session(1)
  let s2 = g.session(2)
  let c1 = join_as(stub, host, s1, g.join_with(10, capable()))
  let c2 = join_as(stub, host, s2, g.join_with(10, capable()))
  let assert Ok(first) = list.first(server_updates(host, s1))
  let assert Some(key) = g.key_of(first)
  assert g.rpc_text(host, guild_state.SwitchRegion(g.channel_id(10))) == ok
  let switched1 = await_server_update(host, s1, 2)
  let switched2 = await_server_update(host, s2, 2)
  assert g.key_of(switched1) == Some(key)
  assert g.key_of(switched2) == Some(key)
  assert confirmed(stub, host, c1) == ok
  assert confirmed(stub, host, c2) == ok
  assert list.all(g.states(host), fn(view) { view.e2ee_capable })
}

pub fn region_switch_token_for_a_departed_user_is_dropped_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
  assert g.rpc_text(host, guild_state.SwitchRegion(g.channel_id(10))) == ok
  assert api.await_held(stub, "voice_get_token", 1)
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  assert api.release(stub, "voice_get_token", api.token(conn))
  api.settle()
  assert list.length(server_updates(host, s1)) == 1
  assert g.confirm(host, conn, nonce_for(stub, conn)) != ok
  assert g.states(host) == []
}

pub fn permission_loss_force_disconnects_test() {
  let world = g.world()
  use stub, host <- with_world(world)
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  g.update_world(host, deny(world, 10, 1))
  g.run(host, guild_handler.on_permissions_change)
  assert api.eventually(fn() {
    api.calls_of(stub, "voice_force_disconnect_participant")
    |> list.any(fn(call) { api.field(call, "connection_id") == Some(conn) })
  })
  let again = g.session(1)
  g.client(host, again, g.join(10))
  api.settle()
  assert server_updates(host, again) == []
}

pub fn permission_loss_before_confirm_refuses_the_pending_join_test() {
  let world = g.world()
  use stub, host <- with_world(world)
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  let update = await_server_update(host, s1, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  g.update_world(host, deny(world, 10, 1))
  g.run(host, guild_handler.on_permissions_change)
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
}

pub fn permission_loss_removes_p2p_members_test() {
  let world = g.world()
  use stub, host <- with_world(world)
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  let s1 = g.session(1)
  g.client(host, s1, g.join_with(10, [#("p2p", json.bool(True))]))
  assert api.eventually(fn() { list.length(g.states(host)) == 1 })
  g.update_world(host, deny(world, 10, 1))
  g.run(host, guild_handler.on_permissions_change)
  assert api.eventually(fn() { g.states(host) == [] })
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

fn p2p_or_token(body: Dynamic) -> api.Response {
  let call = api.Call("voice_get_token", body)
  case api.bool_field(call, "p2p") {
    Some(True) ->
      api.p2p_grant(
        "p2p-"
        <> option.unwrap(api.field(call, "user_id"), "x")
        <> "-"
        <> option.unwrap(api.field(call, "token_nonce"), "n"),
      )
    _ ->
      api.token(option.unwrap(
        api.field(call, "connection_id"),
        "sfu-" <> option.unwrap(api.field(call, "user_id"), "x"),
      ))
  }
}

pub fn disconnect_all_drops_pending_joins_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let live = joined(stub, host, s1, 10)
  let before = list.length(server_updates(host, s1))
  g.client(host, s1, g.join(20))
  let update = await_server_update(host, s1, before + 1)
  let assert Some(pending) = g.text_of(update, "connection_id")
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  assert confirmed(stub, host, pending) != ok
  assert g.states(host) == []
  assert view_of(host, live) == None
}

pub fn disconnect_all_cancels_in_flight_joins_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  g.client(host, s1, g.update(None, None, []))
  api.settle()
  assert api.release(stub, "voice_get_token", api.token("late"))
  api.settle()
  assert server_updates(host, s1) == []
  assert confirmed(stub, host, "late") != ok
  assert g.states(host) == []
}

pub fn disconnect_all_during_client_move_keeps_nothing_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s1, g.update(Some(20), Some(conn), []))
  assert api.await_held(stub, "voice_get_token", 1)
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  assert api.release(stub, "voice_get_token", api.token(conn))
  api.settle()
  assert list.length(server_updates(host, s1)) == 1
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
}

pub fn p2p_pair_then_sfu_switch_reissues_everyone_test() {
  use stub, host <- with_world(e2ee_world())
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  let s1 = g.session(1)
  let s2 = g.session(2)
  let p2p = [#("p2p", json.bool(True)), ..capable()]
  g.client(host, s1, g.join_with(10, p2p))
  assert api.eventually(fn() { list.length(g.states(host)) == 1 })
  g.client(host, s2, g.join_with(10, p2p))
  assert api.eventually(fn() { list.length(g.states(host)) == 2 })
  assert list.all(g.states(host), fn(view) { view.p2p })
  let s3 = g.session(3)
  g.client(host, s3, g.join_with(10, capable()))
  api.settle()
  assert g.errors_to(g.effects(host), s3) == [error.VoiceP2pConsentRequired]
  let assert Ok(first) = list.first(server_updates(host, s1))
  let assert Some(c1) = g.text_of(first, "connection_id")
  g.client(
    host,
    s1,
    g.update(Some(10), Some(c1), [#("p2p", json.bool(False)), ..capable()]),
  )
  let switched1 = await_server_update(host, s1, 2)
  let switched2 = await_server_update(host, s2, 2)
  assert g.text_of(switched1, "token") != None
  assert g.key_of(switched1) != None
  assert g.key_of(switched1) == g.key_of(switched2)
  assert list.all(g.states(host), fn(view) { !view.p2p })
  let assert Some(c2) = g.text_of(switched2, "connection_id")
  assert confirmed(stub, host, c1) == ok
  assert confirmed(stub, host, c2) == ok
  assert list.length(g.states(host)) == 2
  g.client(host, s3, g.join_with(10, capable()))
  let joined3 = await_server_update(host, s3, 1)
  assert g.text_of(joined3, "token") != None
}

pub fn p2p_signal_reaches_the_peer_only_test() {
  use stub, host <- scenario
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  let s1 = g.session(1)
  let s2 = g.session(2)
  g.client(host, s1, g.join_with(10, [#("p2p", json.bool(True))]))
  assert api.eventually(fn() { list.length(g.states(host)) == 1 })
  g.client(host, s2, g.join_with(10, [#("p2p", json.bool(True))]))
  assert api.eventually(fn() { list.length(g.states(host)) == 2 })
  let assert Ok(second) = list.first(server_updates(host, s2))
  let assert Some(c2) = g.text_of(second, "connection_id")
  g.signal(
    host,
    s1,
    json.object([
      #("channel_id", json.string("10")),
      #("to", json.string(c2)),
      #("data", json.object([#("sdp", json.string("offer"))])),
    ]),
  )
  assert api.eventually(fn() {
    g.sent_to(g.effects(host), s2, "VOICE_SIGNAL") != []
  })
  assert g.sent_to(g.effects(host), s1, "VOICE_SIGNAL") == []
}

pub fn hand_off_keeps_states_and_forwards_buffered_updates_test() {
  let world = g.world()
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  let conn = joined(stub, source, s1, 10)
  let s2 = g.session(2)
  let #(voice, _successor) = g.freeze(source)
  g.client(source, s2, g.join(10))
  api.settle()
  let target = g.adopt(world, voice)
  g.forward(source)
  assert in_channel(target, conn) == Some("10")
  let update = await_server_update(target, s2, 1)
  let assert Some(c2) = g.text_of(update, "connection_id")
  assert confirmed(stub, target, c2) == ok
  assert list.length(g.states(target)) == 2
  g.stop(target)
  api.stop(stub)
}

pub fn hand_off_with_a_fetch_in_flight_completes_on_the_target_test() {
  let world = g.world()
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(source, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  let #(voice, _successor) = g.freeze(source)
  let target = g.adopt(world, voice)
  g.forward(source)
  assert api.await_held(stub, "voice_get_token", 2)
  assert api.release(stub, "voice_get_token", api.token("first"))
  assert api.release(stub, "voice_get_token", api.token("second"))
  let update = await_server_update(target, s1, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert confirmed(stub, target, conn) == ok
  assert list.length(g.states(target)) == 1
  g.stop(target)
  api.stop(stub)
}

fn hidden(world: g.World, channel: Int, user: Int) -> g.World {
  deny(world, channel, user)
}

pub fn moderator_move_into_a_hidden_channel_grants_access_until_leave_test() {
  let s1 = g.session(1)
  let world =
    g.World(
      ..hidden(g.world(), 20, 1),
      sessions: dict.from_list([#(1, [g.session_text(s1)])]),
    )
  use stub, host <- with_world(world)
  let conn = joined(stub, host, s1, 10)
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
  assert api.eventually(fn() {
    g.sent_to(g.effects(host), s1, "CHANNEL_CREATE") != []
  })
  let _ = await_server_update(host, s1, 2)
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("20")
  g.run(host, guild_handler.on_permissions_change)
  api.settle()
  assert in_channel(host, conn) == Some("20")
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() {
    g.sent_to(g.effects(host), s1, "CHANNEL_DELETE") != []
  })
  let #(voice, _) = g.inspect(host)
  assert guild_handler.virtual_channels(voice, g.user_id(1)) == []
}

pub fn closing_one_session_keeps_the_other_sessions_pending_join_test() {
  use stub, host <- scenario
  let phone = g.session(1)
  let desktop = g.session(1)
  let live = joined(stub, host, phone, 10)
  g.client(host, desktop, g.join(20))
  let update = await_server_update(host, desktop, 1)
  let assert Some(pending) = g.text_of(update, "connection_id")
  g.client(host, phone, g.update(None, None, []))
  assert api.eventually(fn() { view_of(host, live) == None })
  assert confirmed(stub, host, pending) == ok
  assert in_channel(host, pending) == Some("20")
}

pub fn removed_member_gets_no_token_from_an_in_flight_join_test() {
  let world = g.world()
  use stub, host <- with_world(world)
  let s1 = g.session(1)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  g.update_world(host, g.World(..world, members: [2, 3, 4, 5]))
  g.run(host, fn(voice, ctx) {
    guild_handler.on_member_change(voice, ctx, g.user_id(1))
  })
  api.settle()
  assert api.release(stub, "voice_get_token", api.token("kicked"))
  api.settle()
  assert server_updates(host, s1) == []
  assert confirmed(stub, host, "kicked") != ok
  assert g.states(host) == []
}

fn region_switch_then(
  remove: fn(g.Host, String) -> Nil,
) -> fn(api.Api, g.Host) -> Nil {
  fn(stub, host) {
    let s1 = g.session(1)
    let conn = joined(stub, host, s1, 10)
    api.set(stub, "voice_get_token", api.Hold)
    assert g.rpc_text(host, guild_state.SwitchRegion(g.channel_id(10))) == ok
    assert api.await_held(stub, "voice_get_token", 1)
    remove(host, conn)
    assert api.eventually(fn() { g.states(host) == [] })
    assert api.release(stub, "voice_get_token", api.token(conn))
    api.settle()
    assert list.length(server_updates(host, s1)) == 1
    assert g.states(host) == []
  }
}

pub fn livekit_leave_during_region_switch_drops_the_reissue_test() {
  use host, conn <- scenario_with(region_switch_then)
  assert g.rpc_text(
      host,
      guild_state.DisconnectIfInChannel(
        g.user_id(1),
        g.channel_id(10),
        Some(voice_ids.conn(conn)),
      ),
    )
    == ok
}

pub fn moderator_disconnect_during_region_switch_drops_the_reissue_test() {
  use host, _conn <- scenario_with(region_switch_then)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(g.user_id(1), g.user_id(2), None, None),
    )
    == ok
}

fn scenario_with(
  make: fn(fn(g.Host, String) -> Nil) -> fn(api.Api, g.Host) -> Nil,
  remove: fn(g.Host, String) -> Nil,
) -> Nil {
  scenario(make(remove))
}

pub fn moderator_disconnect_before_a_moved_member_connects_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
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
  let _ = await_server_update(host, s1, 2)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(g.user_id(1), g.user_id(2), None, None),
    )
    == ok
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
  assert api.eventually(fn() {
    api.calls_of(stub, "voice_force_disconnect_participant")
    |> list.any(fn(call) { api.field(call, "connection_id") == Some(conn) })
  })
}

pub fn moderator_disconnect_during_a_move_token_fetch_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
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
  assert api.await_held(stub, "voice_get_token", 1)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(g.user_id(1), g.user_id(2), None, None),
    )
    == ok
  assert api.release(stub, "voice_get_token", api.token(conn))
  api.settle()
  assert list.length(server_updates(host, s1)) == 1
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
}

fn timed_out_sync(offset_ms: Int) -> #(api.Api, g.Host, String) {
  let world = g.world()
  let stub = api.start()
  let host = g.start(world)
  let conn = joined(stub, host, g.session(1), 10)
  g.update_world(
    host,
    g.World(
      ..world,
      timeouts: dict.from_list([#(1, clock.system_ms() + offset_ms)]),
    ),
  )
  g.run(host, fn(voice, ctx) {
    guild_handler.on_member_change(voice, ctx, g.user_id(1))
  })
  #(stub, host, conn)
}

fn media_kicked(stub: api.Api, conn: String) -> Bool {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.any(fn(call) { api.field(call, "connection_id") == Some(conn) })
}

pub fn timed_out_member_is_disconnected_test() {
  let #(stub, host, conn) = timed_out_sync(60_000)
  assert api.eventually(fn() { media_kicked(stub, conn) })
  g.stop(host)
  api.stop(stub)
}

pub fn expired_timeout_keeps_member_connected_test() {
  let #(stub, host, conn) = timed_out_sync(-60_000)
  assert api.eventually(fn() {
    api.calls_of(stub, "voice_update_participant")
    |> list.any(fn(call) { api.bool_field(call, "can_speak") == Some(True) })
  })
  api.settle()
  assert !media_kicked(stub, conn)
  assert in_channel(host, conn) == Some("10")
  g.stop(host)
  api.stop(stub)
}
