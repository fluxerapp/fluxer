// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Dnd, Idle, Invisible, Offline, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/protocol
import gleam/erlang/process
import gleam/list
import gleam/option.{None}
import gleam/string
import gleeunit/should
import support/presence_scenario as scn

fn cached_status(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user]))
  |> list.map(presence.status)
}

pub fn connect_publishes_online_and_caches_row_test() {
  let user = 7_100_000_000_000_001
  let observer = scn.observe(7_100_000_000_000_901, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Online)
  cached_status(user) |> should.equal([Online])
  let replace = raw_json.to_bits(client.sessions_replace(link))
  let assert Ok(text) = bit_array_text(replace)
  string.contains(text, "\"session_id\":\"all\"") |> should.be_true
  client.session_disconnect(link)
  let assert Ok(gone) = scn.next_presence(observer, user, 2000)
  presence.status(gone) |> should.equal(Offline)
  scn.stop_observing(7_100_000_000_000_901)
}

fn bit_array_text(bits: BitArray) -> Result(String, Nil) {
  scn.bits_to_string(bits)
}

pub fn status_update_reaches_observer_and_session_test() {
  let user = 7_100_000_000_000_002
  let observer = scn.observe(7_100_000_000_000_902, [user])
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let _ = scn.drain_seen(session, 50)
  client.session_update(link, Dnd, False, False, None)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Dnd)
  scn.sync(user)
  cached_status(user) |> should.equal([Dnd])
  scn.drain_seen(session, 50)
  |> scn.dispatches("SESSIONS_REPLACE")
  |> list.length
  |> should.equal(1)
  client.session_update(link, Dnd, False, False, None)
  scn.presences(observer, user, 100) |> should.equal([])
  client.session_disconnect(link)
  scn.stop_observing(7_100_000_000_000_902)
}

pub fn invisible_hides_from_observers_and_cache_test() {
  let user = 7_100_000_000_000_003
  let observer = scn.observe(7_100_000_000_000_903, [user])
  let session = scn.fake_session(user, 3)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  client.session_update(link, Invisible, False, False, None)
  let assert Ok(hidden) = scn.next_presence(observer, user, 2000)
  presence.status(hidden) |> should.equal(Offline)
  scn.sync(user)
  cached_status(user) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  let assert Ok(back) = scn.next_presence(observer, user, 2000)
  presence.status(back) |> should.equal(Idle)
  scn.sync(user)
  cached_status(user) |> should.equal([Idle])
  client.session_disconnect(link)
  scn.stop_observing(7_100_000_000_000_903)
}

pub fn settings_invisible_never_shows_online_test() {
  let user = 7_100_000_000_000_004
  let observer = scn.observe(7_100_000_000_000_904, [user])
  let session = scn.fake_session(user, 4)
  let assert Ok(link) =
    scn.connect_with(scn.connect_request(session, Online, [], True, None))
  scn.sync(user)
  scn.presences(observer, user, 200)
  |> list.filter(fn(found) { presence.status(found) != Offline })
  |> should.equal([])
  cached_status(user) |> should.equal([])
  client.session_disconnect(link)
  scn.presences(observer, user, 200)
  |> list.filter(fn(found) { presence.status(found) != Offline })
  |> should.equal([])
  scn.stop_observing(7_100_000_000_000_904)
}

pub fn last_disconnect_stops_presence_and_clears_row_test() {
  let user = 7_100_000_000_000_005
  let observer = scn.observe(7_100_000_000_000_905, [user])
  let first = scn.fake_session(user, 51)
  let second = scn.fake_session(user, 52)
  let link_a = scn.connect(first, Online, [])
  let link_b = scn.connect(second, Idle, [])
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link_a)
  scn.sync(user)
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Idle])
  client.session_disconnect(link_b)
  scn.wait_down(pid, 2000) |> should.be_true
  cached_status(user) |> should.equal([])
  scn.wait_until(fn() { scn.presence_inbox(user) == Error(Nil) }, 1000)
  |> should.be_true
  let statuses = scn.presences(observer, user, 200) |> list.map(presence.status)
  list.last(statuses) |> should.equal(Ok(Offline))
  scn.stop_observing(7_100_000_000_000_905)
}

pub fn friend_sees_status_changes_and_snapshot_test() {
  let alice = 7_100_000_000_000_006
  let bob = 7_100_000_000_000_007
  let bob_session = scn.fake_session(bob, 71)
  let bob_link = scn.connect(bob_session, Dnd, [alice])
  let alice_session = scn.fake_session(alice, 61)
  let alice_link = scn.connect(alice_session, Online, [bob])
  let snapshot =
    scn.wait_frames(alice_session, bob, 2000)
    |> list.map(scn.wire_status)
    |> list.unique
  snapshot |> should.equal(["dnd"])
  let bob_saw =
    scn.wait_frames(bob_session, alice, 2000) |> list.map(scn.wire_status)
  bob_saw |> should.equal(["online"])
  client.session_update(bob_link, Idle, False, False, None)
  scn.wait_frames(alice_session, bob, 2000)
  |> list.map(scn.wire_status)
  |> should.equal(["idle"])
  client.session_disconnect(bob_link)
  scn.wait_frames(alice_session, bob, 2000)
  |> list.map(scn.wire_status)
  |> should.equal(["offline"])
  client.session_disconnect(alice_link)
}

pub fn unsubscribe_sends_forced_offline_and_stops_frames_test() {
  let alice = 7_100_000_000_000_008
  let bob = 7_100_000_000_000_009
  let alice_session = scn.fake_session(alice, 81)
  let alice_link = scn.connect(alice_session, Online, [bob])
  let bob_session = scn.fake_session(bob, 91)
  let bob_link = scn.connect(bob_session, Online, [])
  scn.wait_frames(alice_session, bob, 2000)
  |> list.map(scn.wire_status)
  |> should.equal(["online"])
  client.unsubscribe_users(alice_link, packed_ids.from_list([bob]))
  scn.wait_frames(alice_session, bob, 2000)
  |> list.map(scn.wire_status)
  |> should.equal(["offline"])
  client.session_update(bob_link, Dnd, False, False, None)
  scn.sync(bob)
  scn.sync(alice)
  scn.drain_seen(alice_session, 100)
  |> scn.frames_about(bob)
  |> should.equal([])
  client.session_disconnect(bob_link)
  client.session_disconnect(alice_link)
}

pub fn dispatch_to_user_reaches_every_session_test() {
  let user = 7_100_000_000_000_010
  let first = scn.fake_session(user, 101)
  let second = scn.fake_session(user, 102)
  let link_a = scn.connect(first, Online, [])
  let link_b = scn.connect(second, Online, [])
  let _ = scn.drain_seen(first, 50)
  let _ = scn.drain_seen(second, 50)
  let assert Ok(user_id) = id.user_id(user)
  let assert Ok(Nil) =
    client.dispatch_to_user(user_id, "USER_NOTE_UPDATE", raw_json.null())
  scn.drain_seen(first, 100)
  |> scn.dispatches("USER_NOTE_UPDATE")
  |> list.length
  |> should.equal(1)
  scn.drain_seen(second, 100)
  |> scn.dispatches("USER_NOTE_UPDATE")
  |> list.length
  |> should.equal(1)
  client.session_disconnect(link_a)
  client.session_disconnect(link_b)
  let _ = protocol.TerminateAll
  Nil
}
