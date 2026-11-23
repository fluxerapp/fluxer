// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{type Durations, Durations, VoiceDurations}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/count_table
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import support/isolation
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids

const ok = "{\"success\":true}"

fn fast(change: fn(settings.VoiceDurations) -> settings.VoiceDurations) -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(..d, voice: change(d.voice))
  })
}

fn server_updates(host: g.Host, ref: SessionRef) -> List(Dynamic) {
  g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE")
}

fn nonce_for(stub: api.Api, conn: String) -> Option(String) {
  api.calls_of(stub, "voice_get_token")
  |> list.reverse
  |> list.find(fn(call) {
    api.field(call, "connection_id") == Some(conn)
    || api.field(call, "connection_id") == None
  })
  |> option.from_result
  |> option.then(api.field(_, "token_nonce"))
}

fn pending_conn(host: g.Host, ref: SessionRef, channel: Int) -> String {
  let before = list.length(server_updates(host, ref))
  g.client(host, ref, g.join(channel))
  let assert True =
    api.eventually(fn() { list.length(server_updates(host, ref)) > before })
  let assert Ok(update) = list.last(server_updates(host, ref))
  let assert Some(conn) = g.text_of(update, "connection_id")
  conn
}

fn force_disconnected(stub: api.Api, conn: String) -> Bool {
  api.calls_of(stub, "voice_force_disconnect_participant")
  |> list.any(fn(call) { api.field(call, "connection_id") == Some(conn) })
}

fn hand_over(world: g.World, source: g.Host) -> g.Host {
  let #(voice, _successor) = g.freeze(source)
  let target = g.adopt(world, voice)
  g.forward(source)
  let assert True = api.eventually(fn() { !process.is_alive(source.pid) })
  target
}

fn owner_of(conn: String) -> Option(process.Pid) {
  count_table.rows()
  |> list.find(fn(row) { voice_ids.row_text(row) == conn })
  |> option.from_result
  |> option.map(fn(row) { row.owner })
}

pub fn a_pending_join_carried_by_a_hand_off_is_confirmed_on_the_target_test() {
  let world = g.World(..g.world(), guild: 931)
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  let conn = pending_conn(source, s1, 10)
  let target = hand_over(world, source)
  assert g.confirm(target, conn, nonce_for(stub, conn)) == ok
  assert list.map(g.states(target), fn(view) { view.connection_id }) == [conn]
  assert api.eventually(fn() { owner_of(conn) == Some(target.pid) })
  g.stop(target)
  assert api.eventually(fn() { owner_of(conn) == None })
  api.stop(stub)
}

pub fn a_pending_join_carried_by_a_hand_off_still_expires_on_the_target_test() {
  fast(fn(v) {
    VoiceDurations(
      ..v,
      connection_pending_ttl_ms: 400,
      guild_handler_sweep_interval_ms: 40,
    )
  })
  let world = g.World(..g.world(), guild: 932)
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  let conn = pending_conn(source, s1, 10)
  let target = hand_over(world, source)
  assert api.eventually(fn() { force_disconnected(stub, conn) })
  assert g.confirm(target, conn, nonce_for(stub, conn)) != ok
  assert g.states(target) == []
  g.stop(target)
  api.stop(stub)
}

pub fn a_confirm_held_by_a_frozen_source_is_answered_by_the_target_test() {
  let world = g.World(..g.world(), guild: 933)
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  let conn = pending_conn(source, s1, 10)
  let nonce = nonce_for(stub, conn)
  let #(voice, _successor) = g.freeze(source)
  let answer = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(answer, g.confirm(source, conn, nonce))
  })
  assert process.receive(answer, 300) == Error(Nil)
  let target = g.adopt(world, voice)
  g.forward(source)
  assert process.receive(answer, 5000) == Ok(ok)
  assert list.map(g.states(target), fn(view) { view.connection_id }) == [conn]
  g.stop(target)
  api.stop(stub)
}

pub fn a_hand_off_does_not_double_the_server_update_of_an_in_flight_join_test() {
  let world = g.World(..g.world(), guild: 934)
  let stub = api.start()
  let source = g.start(world)
  let s1 = g.session(1)
  api.set(stub, "voice_get_token", api.Hold)
  g.client(source, s1, g.join(10))
  assert api.await_held(stub, "voice_get_token", 1)
  let #(voice, _successor) = g.freeze(source)
  let target = g.adopt(world, voice)
  assert api.await_held(stub, "voice_get_token", 2)
  assert api.release(stub, "voice_get_token", api.token("early"))
  api.settle()
  g.forward(source)
  assert api.release(stub, "voice_get_token", api.token("late"))
  assert api.eventually(fn() { server_updates(target, s1) != [] })
  api.settle()
  assert list.length(server_updates(target, s1)) == 1
  let assert [update] = server_updates(target, s1)
  let assert Some(conn) = g.text_of(update, "connection_id")
  assert g.confirm(target, conn, nonce_for(stub, conn)) == ok
  assert list.length(g.states(target)) == 1
  g.stop(target)
  api.stop(stub)
}
