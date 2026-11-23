// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{type Durations, Durations, VoiceDurations}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/guild_state
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import support/isolation
import support/voice_api as api
import support/voice_guild as g

const ok = "{\"success\":true}"

fn fast(change: fn(settings.VoiceDurations) -> settings.VoiceDurations) -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(..d, voice: change(d.voice))
  })
}

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

fn pending_conn(host: g.Host, ref: SessionRef, data: json.Json) -> String {
  let before = list.length(server_updates(host, ref))
  g.client(host, ref, data)
  let update = await_server_update(host, ref, before + 1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  conn
}

fn force_disconnected(stub: api.Api, conn: String) -> Bool {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.any(fn(call) { api.field(call, "connection_id") == Some(conn) })
}

pub fn unconfirmed_join_expires_and_is_force_disconnected_test() {
  fast(fn(v) {
    VoiceDurations(
      ..v,
      connection_pending_ttl_ms: 100,
      guild_handler_sweep_interval_ms: 40,
    )
  })
  let stub = api.start()
  let host = g.start(g.world())
  let s1 = g.session(1)
  let conn = pending_conn(host, s1, g.join(10))
  assert api.eventually(fn() { force_disconnected(stub, conn) })
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
  g.stop(host)
  api.stop(stub)
}

pub fn unconfirmed_moderator_move_expires_with_a_leave_broadcast_test() {
  fast(fn(v) {
    VoiceDurations(
      ..v,
      region_pending_ttl_ms: 150,
      guild_handler_sweep_interval_ms: 40,
    )
  })
  let stub = api.start()
  let host = g.start(g.world())
  let s1 = g.session(1)
  let conn = pending_conn(host, s1, g.join(10))
  assert confirmed(stub, host, conn) == ok
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
  assert api.eventually(fn() { force_disconnected(stub, conn) })
  let last =
    g.broadcasts(g.effects(host))
    |> list.filter(fn(view) { view.connection_id == conn })
    |> list.last
  let assert Ok(view) = last
  assert view.channel == None
  assert confirmed(stub, host, conn) != ok
  assert g.states(host) == []
  g.stop(host)
  api.stop(stub)
}

pub fn stuck_token_fetch_expires_and_its_late_answer_is_dropped_test() {
  fast(fn(v) {
    VoiceDurations(
      ..v,
      guild_handler_fetch_ttl_ms: 100,
      guild_handler_sweep_interval_ms: 40,
    )
  })
  let stub = api.start()
  let host = g.start(g.world())
  api.set(stub, "voice_get_token", api.Hold)
  let s1 = g.session(1)
  g.client(host, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  api.settle()
  assert api.release(stub, "voice_get_token", api.token("too-late"))
  api.settle()
  assert server_updates(host, s1) == []
  assert confirmed(stub, host, "too-late") != ok
  assert g.states(host) == []
  g.client(host, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  assert api.release(stub, "voice_get_token", api.token("on-time"))
  let update = await_server_update(host, s1, 1)
  assert g.text_of(update, "connection_id") == Some("on-time")
  g.stop(host)
  api.stop(stub)
}

fn p2p_grant(body: Dynamic) -> api.Response {
  let call = api.Call("voice_get_token", body)
  api.p2p_grant("p2p-" <> option.unwrap(api.field(call, "user_id"), "x"))
}

pub fn p2p_member_whose_session_left_is_removed_after_two_checks_test() {
  fast(fn(v) { VoiceDurations(..v, guild_handler_p2p_check_interval_ms: 40) })
  let s1 = g.session(1)
  let s2 = g.session(2)
  let world =
    g.World(..g.world(), sessions: dict.from_list([#(1, [g.session_text(s1)])]))
  let stub = api.start()
  api.set(stub, "voice_get_token", api.Respond(p2p_grant))
  let host = g.start(world)
  let p2p = [#("p2p", json.bool(True))]
  g.client(host, s1, g.join_with(10, p2p))
  g.client(host, s2, g.join_with(10, p2p))
  assert api.eventually(fn() {
    case g.states(host) {
      [only] -> only.connection_id == "p2p-1"
      _ -> False
    }
  })
  api.settle()
  assert list.map(g.states(host), fn(view) { view.connection_id }) == ["p2p-1"]
  g.stop(host)
  api.stop(stub)
}
