// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Idle, Offline, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/rpc
import fluxer_gateway/session/control
import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit/should
import support/presence_scenario as scn

fn cached_status(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user]))
  |> list.map(presence.status)
}

fn params(fields: List(#(String, dynamic.Dynamic))) -> dynamic.Dynamic {
  dynamic.properties(
    list.map(fields, fn(pair) { #(dynamic.string(pair.0), pair.1) }),
  )
}

fn user_field(user: Int) -> #(String, dynamic.Dynamic) {
  #("user_id", dynamic.string(int.to_string(user)))
}

pub fn reconnect_right_after_last_disconnect_succeeds_test() {
  let user = 7_300_000_000_000_001
  int.range(1, 21, Nil, fn(_, round) { reconnect_round(user, round) })
}

fn reconnect_round(user: Int, round: Int) -> Nil {
  let session = scn.fake_session(user, round)
  let link = scn.connect(session, Online, [])
  client.session_disconnect(link)
  let next = scn.fake_session(user, 100 + round)
  let assert Ok(again) =
    scn.connect_with(scn.connect_request(next, Online, [], False, None))
  client.session_disconnect(again)
}

pub fn connect_while_presence_crashes_succeeds_test() {
  let user = 7_300_000_000_000_002
  let session = scn.fake_session(user, 1)
  let _ = scn.connect(session, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  process.kill(pid)
  let next = scn.fake_session(user, 2)
  let assert Ok(link) =
    scn.connect_with(scn.connect_request(next, Idle, [], False, None))
  scn.sync(user)
  cached_status(user) |> should.equal([Idle])
  client.session_disconnect(link)
}

pub fn terminate_sessions_rpc_removes_session_test() {
  let user = 7_300_000_000_000_003
  let guild = 7_300_000_000_000_903
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 31)
  let _link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  let outcome =
    rpc.handle(
      "presence.terminate_sessions",
      params([
        user_field(user),
        #("session_id_hashes", dynamic.list([dynamic.string("abc")])),
      ]),
    )
  outcome |> should.equal(Ok(json.bool(True)))
  scn.wait_down(pid, 2000) |> should.be_true
  scn.presences(observer, user, 150)
  |> list.map(presence.status)
  |> should.equal([Offline])
  scn.stop_observing(guild)
}

pub fn terminate_sessions_kept_by_session_keeps_presence_test() {
  let user = 7_300_000_000_000_004
  let session = scn.fake_session_with(user, 41, False)
  let link = scn.connect(session, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  rpc.handle(
    "presence.terminate_sessions",
    params([
      user_field(user),
      #("session_id_hashes", dynamic.list([dynamic.string("abc")])),
    ]),
  )
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Online])
  client.session_disconnect(link)
}

pub fn settings_invisible_dispatch_hides_then_shows_test() {
  let user = 7_300_000_000_000_005
  let guild = 7_300_000_000_000_905
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 51)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let settings = fn(status: String) {
    dispatch(
      user,
      "USER_SETTINGS_UPDATE",
      json.object([#("status", json.string(status))]),
    )
  }
  settings("invisible") |> should.equal(Ok(json.bool(True)))
  let assert Ok(hidden) = scn.next_presence(observer, user, 2000)
  presence.status(hidden) |> should.equal(Offline)
  scn.sync(user)
  cached_status(user) |> should.equal([])
  let replaced =
    scn.drain_seen(session, 100) |> scn.dispatches("USER_SETTINGS_UPDATE")
  list.length(replaced) |> should.equal(1)
  settings("online") |> should.equal(Ok(json.bool(True)))
  let assert Ok(shown) = scn.next_presence(observer, user, 2000)
  presence.status(shown) |> should.equal(Online)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn invisible_user_disconnect_publishes_nothing_visible_test() {
  let user = 7_300_000_000_000_006
  let guild = 7_300_000_000_000_906
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 61)
  let link = scn.connect(session, presence.Invisible, [])
  scn.sync(user)
  let before = scn.presences(observer, user, 100) |> list.map(presence.status)
  list.filter(before, fn(found) { found != Offline }) |> should.equal([])
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.presences(observer, user, 100)
  |> list.filter(fn(found) { presence.status(found) != Offline })
  |> should.equal([])
  scn.stop_observing(guild)
}

pub fn initial_custom_status_shown_and_cleared_by_null_update_test() {
  let user = 7_300_000_000_000_007
  let guild = 7_300_000_000_000_907
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 71)
  let custom = raw_json.wrap(<<"{\"text\":\"busy\"}":utf8>>)
  let assert Ok(link) =
    scn.connect_with(scn.connect_request(
      session,
      Online,
      [],
      False,
      Some(custom),
    ))
  let assert Ok(shown) = scn.next_presence(observer, user, 2000)
  presence.custom_status(shown)
  |> option.map(fn(raw) { string.inspect(raw_json.to_bits(raw)) })
  |> option.is_some
  |> should.be_true
  client.session_update(link, Online, False, False, Some(raw_json.null()))
  let assert Ok(cleared) = scn.next_presence(observer, user, 2000)
  presence.custom_status(cleared) |> should.equal(None)
  client.session_update(link, presence.Invisible, False, False, None)
  let assert Ok(hidden) = scn.next_presence(observer, user, 2000)
  presence.custom_status(hidden) |> should.equal(None)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn expiry_tick_clears_expired_custom_status_test() {
  let user = 7_300_000_000_000_008
  let guild = 7_300_000_000_000_908
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 81)
  let custom =
    raw_json.wrap(<<
      "{\"text\":\"brb\",\"expires_at\":\"2000-01-01T00:00:00Z\"}":utf8,
    >>)
  let assert Ok(link) =
    scn.connect_with(scn.connect_request(
      session,
      Online,
      [],
      False,
      Some(custom),
    ))
  let assert Ok(shown) = scn.next_presence(observer, user, 2000)
  presence.custom_status(shown) |> should.equal(None)
  let assert Ok(inbox) = scn.presence_inbox(user)
  process.send(inbox, protocol.ExpiryTick)
  scn.sync(user)
  scn.presences(observer, user, 100) |> should.equal([])
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

fn terminated(seen: List(scn.Seen)) -> Bool {
  list.any(seen, fn(found) {
    case found {
      scn.Frame(session.Terminate(_)) -> True
      scn.Notice(control.TerminateMatching(..)) -> True
      _ -> False
    }
  })
}

pub fn terminate_all_reaches_session_that_was_mid_handoff_test() {
  let user = 7_300_000_000_000_009
  let session = scn.fake_session(user, 91)
  let _ = scn.connect(session, Online, [])
  scn.exit_session(session, protocol.handoff_exit())
  scn.sync(user)
  rpc.handle("presence.terminate_all_sessions", params([user_field(user)]))
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  let moved = scn.fake_session(user, 91)
  let link = scn.connect(moved, Online, [])
  scn.drain_seen(moved, 200) |> terminated |> should.be_true
  client.session_disconnect(link)
}

pub fn terminate_sessions_reaches_session_that_was_mid_handoff_test() {
  let user = 7_300_000_000_000_010
  let session = scn.fake_session(user, 101)
  let _ = scn.connect(session, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, protocol.handoff_exit())
  scn.sync(user)
  rpc.handle(
    "presence.terminate_sessions",
    params([
      user_field(user),
      #("session_id_hashes", dynamic.list([dynamic.string("abc")])),
    ]),
  )
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  process.is_alive(pid) |> should.be_true
  let moved = scn.fake_session(user, 101)
  let link = scn.connect(moved, Online, [])
  scn.presence_pid(user) |> should.equal(Ok(pid))
  scn.drain_seen(moved, 200) |> terminated |> should.be_true
  client.session_disconnect(link)
}

fn has_active(user: Int) {
  rpc.handle("presence.has_active", params([user_field(user)]))
}

pub fn has_active_turns_false_as_soon_as_presence_stops_test() {
  let user = 7_300_000_000_000_011
  let session = scn.fake_session(user, 111)
  let link = scn.connect(session, Online, [])
  has_active(user)
  |> should.equal(Ok(json.object([#("has_active", json.bool(True))])))
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  has_active(user)
  |> should.equal(Ok(json.object([#("has_active", json.bool(False))])))
}

fn dispatch(user: Int, event: String, data: json.Json) {
  let payload =
    json.object([
      #("user_id", json.string(int.to_string(user))),
      #("event", json.string(event)),
      #("data", data),
    ])
    |> raw_json.from_json
  case rpc.handle_raw("presence.dispatch", payload) {
    Ok(outcome) -> outcome
    Error(Nil) -> Ok(json.string("presence.dispatch was not routed"))
  }
}

fn user_data(user: Int, name: String, restricted: Bool) -> json.Json {
  json.object([
    #("id", json.string(int.to_string(user))),
    #("username", json.string(name)),
    #("profile_restricted", json.bool(restricted)),
  ])
}

pub fn user_update_reaches_guild_observers_while_visible_test() {
  let user = 7_300_000_000_000_012
  let guild = 7_300_000_000_000_912
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 121)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  dispatch(user, "USER_UPDATE", user_data(user, "renamed", False))
  |> should.equal(Ok(json.bool(True)))
  scn.user_updates(observer, 200)
  |> list.any(fn(wire) { string.contains(wire, "renamed") })
  |> should.be_true
  scn.drain_seen(session, 100)
  |> scn.dispatches("USER_UPDATE")
  |> list.length
  |> should.equal(1)
  client.session_update(link, presence.Invisible, False, False, None)
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  dispatch(user, "USER_UPDATE", user_data(user, "hidden name", False))
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  scn.user_updates(observer, 150)
  |> list.any(fn(wire) { string.contains(wire, "hidden name") })
  |> should.be_false
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn restricted_profile_hides_custom_status_test() {
  let user = 7_300_000_000_000_013
  let guild = 7_300_000_000_000_913
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 131)
  let custom = raw_json.wrap(<<"{\"text\":\"hello\"}":utf8>>)
  let assert Ok(link) =
    scn.connect_with(scn.connect_request(
      session,
      Online,
      [],
      False,
      Some(custom),
    ))
  let assert Ok(shown) = scn.next_presence(observer, user, 2000)
  presence.custom_status(shown) |> option.is_some |> should.be_true
  dispatch(user, "USER_UPDATE", user_data(user, "scenario", True))
  |> should.equal(Ok(json.bool(True)))
  scn.sync(user)
  let after = scn.presences(observer, user, 150)
  list.last(after)
  |> result.map(presence.custom_status)
  |> should.equal(Ok(None))
  client.session_disconnect(link)
  scn.stop_observing(guild)
}
