// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
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

fn raw_state(host: g.Host, conn: String) -> Option(Dynamic) {
  let voice = g.inspect(host).0
  guild_handler.voice_states(voice)
  |> list.find_map(fn(found) {
    let text = json.to_string(guild_handler.voice_state_json(voice, found))
    case json.parse(text, decode.dynamic) {
      Ok(value) ->
        case g.text_of(value, "connection_id") == Some(conn) {
          True -> Ok(value)
          False -> Error(Nil)
        }
      Error(_) -> Error(Nil)
    }
  })
  |> option.from_result
}

fn muted(host: g.Host, conn: String) -> Option(Bool) {
  raw_state(host, conn)
  |> option.then(fn(value) {
    decode.run(value, decode.at(["mute"], decode.bool)) |> option.from_result
  })
}

fn in_channel(host: g.Host, conn: String) -> Option(String) {
  g.states(host)
  |> list.find(fn(view) { view.connection_id == conn })
  |> option.from_result
  |> option.then(fn(view) { view.channel })
}

fn in_region(region: String) -> api.Rule {
  api.Respond(fn(body) {
    let conn =
      api.field(api.Call("voice_get_token", body), "connection_id")
      |> option.unwrap("fresh")
    api.Data(
      json.object([
        #("token", json.string("token-" <> conn <> "-" <> region)),
        #("endpoint", json.string("wss://" <> region <> ".sfu.test")),
        #("connectionId", json.string(conn)),
        #("regionId", json.string(region)),
        #("serverId", json.string(region <> "-sfu")),
      ]),
    )
  })
}

fn for_channel(channel: String) -> fn(api.Call) -> Bool {
  fn(call) { api.field(call, "channel_id") == Some(channel) }
}

fn release_in(stub: api.Api, channel: String) -> Bool {
  api.release_where(stub, "voice_get_token", for_channel(channel), fn(call) {
    api.token(option.unwrap(api.field(call, "connection_id"), "fresh"))
  })
}

pub fn moderator_mute_while_the_join_is_pending_survives_confirm_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  let update = await_server_update(host, s1, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.rpc_text(host, guild_state.MemberVoice(g.user_id(1), True, False))
    == ok
  assert confirmed(stub, host, conn) == ok
  assert muted(host, conn) == Some(True)
}

pub fn moderator_mute_during_a_moderator_move_survives_confirm_test() {
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
  assert g.rpc_text(host, guild_state.MemberVoice(g.user_id(1), True, True))
    == ok
  assert api.release_token(stub)
  let _ = await_server_update(host, s1, 2)
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("20")
  assert muted(host, conn) == Some(True)
}

pub fn region_switch_confirm_takes_the_new_routing_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", in_region("us"))
  assert g.rpc_text(host, guild_state.SwitchRegion(g.channel_id(10))) == ok
  let switched = await_server_update(host, s1, 2)
  assert g.text_of(switched, "endpoint") == Some("wss://us.sfu.test")
  assert confirmed(stub, host, conn) == ok
  let listed = g.rpc_text(host, guild_state.ChannelStates(g.channel_id(10)))
  assert string.contains(listed, "\"region_id\":\"us\"")
  assert string.contains(listed, "\"server_id\":\"us-sfu\"")
}

pub fn moderator_move_wins_over_an_older_client_move_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s1, g.update(Some(20), Some(conn), []))
  assert api.await_held(stub, "voice_get_token", 1)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(
        g.user_id(1),
        g.user_id(2),
        Some(g.channel_id(30)),
        None,
      ),
    )
    == ok
  assert api.await_held(stub, "voice_get_token", 2)
  assert release_in(stub, "30")
  let moved = await_server_update(host, s1, 2)
  assert g.text_of(moved, "channel_id") == Some("30")
  assert release_in(stub, "20")
  api.settle()
  assert list.length(server_updates(host, s1)) == 2
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("30")
}

pub fn moderator_move_wins_when_the_client_move_answers_first_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let conn = joined(stub, host, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s1, g.update(Some(20), Some(conn), []))
  assert api.await_held(stub, "voice_get_token", 1)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(
        g.user_id(1),
        g.user_id(2),
        Some(g.channel_id(30)),
        None,
      ),
    )
    == ok
  assert api.await_held(stub, "voice_get_token", 2)
  assert release_in(stub, "20")
  api.settle()
  assert release_in(stub, "30")
  let moved = await_server_update(host, s1, 2)
  assert g.text_of(moved, "channel_id") == Some("30")
  api.settle()
  assert list.length(server_updates(host, s1)) == 2
  assert confirmed(stub, host, conn) == ok
  assert in_channel(host, conn) == Some("30")
}

pub fn livekit_leave_racing_a_pending_join_keeps_nothing_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  let update = await_server_update(host, s1, 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.rpc_text(
      host,
      guild_state.DisconnectIfInChannel(
        g.user_id(1),
        g.channel_id(10),
        Some(voice_ids.conn(conn)),
      ),
    )
    == "{\"success\":true,\"ignored\":true}"
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
}

pub fn client_move_into_an_e2ee_channel_shares_its_key_test() {
  use stub, host <- with_world(e2ee_world())
  let s1 = g.session(1)
  let s2 = g.session(2)
  let c1 = join_as(stub, host, s1, g.join_with(10, capable()))
  let _ = join_as(stub, host, s2, g.join_with(20, capable()))
  let assert Ok(there) = list.first(server_updates(host, s2))
  let assert Some(key20) = g.key_of(there)
  g.client(host, s1, g.update(Some(20), Some(c1), capable()))
  let moved = await_server_update(host, s1, 2)
  assert g.key_of(moved) == Some(key20)
}

pub fn moderator_move_into_an_e2ee_channel_shares_its_key_test() {
  use stub, host <- with_world(e2ee_world())
  let s1 = g.session(1)
  let s2 = g.session(2)
  let c1 = join_as(stub, host, s1, g.join_with(10, capable()))
  let _ = join_as(stub, host, s2, g.join_with(20, capable()))
  let assert Ok(there) = list.first(server_updates(host, s2))
  let assert Some(key20) = g.key_of(there)
  assert g.rpc_text(
      host,
      guild_state.MoveMember(
        g.user_id(1),
        g.user_id(3),
        Some(g.channel_id(20)),
        None,
      ),
    )
    == ok
  let moved = await_server_update(host, s1, 2)
  assert g.key_of(moved) == Some(key20)
  assert confirmed(stub, host, c1) == ok
}

pub fn pending_e2ee_joiners_share_one_key_test() {
  use _stub, host <- with_world(e2ee_world())
  let s1 = g.session(1)
  let s2 = g.session(2)
  g.client(host, s1, g.join_with(10, capable()))
  let first = await_server_update(host, s1, 1)
  g.client(host, s2, g.join_with(10, capable()))
  let second = await_server_update(host, s2, 1)
  assert g.key_of(first) != None
  assert g.key_of(first) == g.key_of(second)
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

pub fn sfu_switch_under_churn_reissues_only_the_stayers_test() {
  use stub, host <- with_world(e2ee_world())
  api.set(stub, "voice_get_token", api.Respond(p2p_or_token))
  let s1 = g.session(1)
  let s2 = g.session(2)
  let s3 = g.session(3)
  let p2p = [#("p2p", json.bool(True)), ..capable()]
  g.client(host, s1, g.join_with(10, p2p))
  assert api.eventually(fn() { list.length(g.states(host)) == 1 })
  g.client(host, s2, g.join_with(10, p2p))
  assert api.eventually(fn() { list.length(g.states(host)) == 2 })
  let assert Ok(first) = list.first(server_updates(host, s1))
  let assert Some(c1) = g.text_of(first, "connection_id")
  let assert Ok(second) = list.first(server_updates(host, s2))
  let assert Some(c2) = g.text_of(second, "connection_id")
  api.set(stub, "voice_get_token", api.Hold)
  g.client(
    host,
    s1,
    g.update(Some(10), Some(c1), [#("p2p", json.bool(False)), ..capable()]),
  )
  assert api.await_held(stub, "voice_get_token", 2)
  g.client(host, s2, g.update(None, Some(c2), []))
  g.client(host, s3, g.join_with(10, capable()))
  assert api.await_held(stub, "voice_get_token", 3)
  assert api.release_token(stub)
  assert api.release_token(stub)
  assert api.release_token(stub)
  let switched = await_server_update(host, s1, 2)
  let fresh = await_server_update(host, s3, 1)
  api.settle()
  assert list.length(server_updates(host, s2)) == 1
  assert g.key_of(switched) != None
  assert g.key_of(switched) == g.key_of(fresh)
  let assert Some(c3) = g.text_of(fresh, "connection_id")
  assert confirmed(stub, host, c1) == ok
  assert confirmed(stub, host, c3) == ok
  let states = g.states(host)
  assert list.length(states) == 2
  assert list.all(states, fn(view) { !view.p2p })
  assert confirmed(stub, host, c2) != ok
}

pub fn unavailable_guild_disconnects_everyone_in_voice_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let s2 = g.session(2)
  let s3 = g.session(3)
  let c1 = joined(stub, host, s1, 10)
  g.client(host, s2, g.join(20))
  let pending = await_server_update(host, s2, 1)
  let assert Some(c2) = g.text_of(pending, "connection_id")
  api.set(stub, "voice_get_token", api.Hold)
  g.client(host, s3, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  g.run(host, fn(voice, ctx) {
    guild_handler.on_unavailable(voice, ctx, fn(_) { False })
  })
  assert api.eventually(fn() { g.states(host) == [] })
  assert api.eventually(fn() {
    api.calls_of(stub, "voice_force_disconnect_participant")
    |> list.any(fn(call) { api.field(call, "connection_id") == Some(c1) })
  })
  assert api.release_token(stub)
  api.settle()
  assert server_updates(host, s3) == []
  assert confirmed(stub, host, c2) != ok
  assert g.states(host) == []
}

pub fn unavailable_guild_keeps_staff_in_voice_test() {
  use stub, host <- scenario
  let s1 = g.session(1)
  let s2 = g.session(2)
  let c1 = joined(stub, host, s1, 10)
  let c2 = joined(stub, host, s2, 10)
  let staff = g.user_id(2)
  g.run(host, fn(voice, ctx) {
    guild_handler.on_unavailable(voice, ctx, fn(user) { user == staff })
  })
  assert api.eventually(fn() { list.length(g.states(host)) == 1 })
  assert in_channel(host, c2) == Some("10")
  assert in_channel(host, c1) == None
}

pub fn crashed_voice_host_is_skipped_by_collectors_and_rejoin_starts_clean_test() {
  let world = g.world()
  let stub = api.start()
  let crashed = g.start(world)
  let s1 = g.session(1)
  let lost = joined(stub, crashed, s1, 10)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(crashed, g.session(2), g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  g.stop(crashed)
  let reports = guild_handler.collect_all(500)
  assert list.all(reports, fn(report) { result.is_ok(report.1) })
  let collected =
    list.flat_map(reports, fn(report) { result.unwrap(report.1, []) })
  assert !list.any(collected, fn(found) { voice_ids.of(found) == lost })
  assert api.release_token(stub)
  api.set(
    stub,
    "voice_get_token",
    api.Respond(fn(body) {
      api.token(
        api.field(api.Call("voice_get_token", body), "connection_id")
        |> option.unwrap("fresh-after-crash"),
      )
    }),
  )
  let restarted = g.start(world)
  let again = g.session(1)
  let conn = joined(stub, restarted, again, 10)
  assert conn == "fresh-after-crash"
  assert list.length(g.states(restarted)) == 1
  g.stop(restarted)
  api.stop(stub)
}

fn by_user(user: String) -> fn(api.Call) -> Bool {
  fn(call) { api.field(call, "user_id") == Some(user) }
}

pub fn p2p_grant_arriving_after_an_sfu_member_settled_is_refused_test() {
  use stub, host <- scenario
  api.set(stub, "voice_get_token", api.Hold)
  let s1 = g.session(1)
  let s3 = g.session(3)
  g.client(host, s1, g.join_with(10, [#("p2p", json.bool(True))]))
  assert api.await_held(stub, "voice_get_token", 1)
  g.client(host, s3, g.join(10))
  assert api.await_held(stub, "voice_get_token", 2)
  assert api.release_where(stub, "voice_get_token", by_user("3"), fn(call) {
    api.token(option.unwrap(api.field(call, "connection_id"), "sfu-3"))
  })
  let update = await_server_update(host, s3, 1)
  let assert Some(c3) = g.text_of(update, "connection_id")
  assert confirmed(stub, host, c3) == ok
  assert api.release_where(stub, "voice_get_token", by_user("1"), fn(_) {
    api.p2p_grant("p2p-1")
  })
  api.settle()
  let states = g.states(host)
  assert list.map(states, fn(view) { view.connection_id }) == [c3]
  assert list.all(states, fn(view) { !view.p2p })
  assert server_updates(host, s1) == []
  assert g.errors_to(g.effects(host), s1) == [error.VoiceP2pUnavailable]
}

pub fn p2p_joiners_racing_into_an_empty_channel_stay_consistent_test() {
  use stub, host <- scenario
  api.set(stub, "voice_get_token", api.Hold)
  let sessions = list.map([1, 2, 3, 4, 5], g.session)
  list.each(sessions, fn(ref) {
    g.client(host, ref, g.join_with(10, [#("p2p", json.bool(True))]))
  })
  assert api.await_held(stub, "voice_get_token", 5)
  list.each([5, 4, 3, 2, 1], fn(user) {
    let text = int.to_string(user)
    assert api.release_where(stub, "voice_get_token", by_user(text), fn(_) {
      api.p2p_grant("p2p-" <> text)
    })
  })
  api.settle()
  let states = g.states(host)
  assert list.length(states) == 4
  assert list.all(states, fn(view) { view.p2p })
}
