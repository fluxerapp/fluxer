// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Idle, Offline, Online}
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/registry
import gleam/dynamic
import gleam/erlang/process
import gleam/list
import gleeunit/should
import support/presence_scenario as scn

fn cached_status(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user]))
  |> list.map(presence.status)
}

fn statuses(observer, user: Int, quiet_ms: Int) -> List(presence.Status) {
  scn.presences(observer, user, quiet_ms) |> list.map(presence.status)
}

pub fn session_handoff_keeps_presence_without_offline_test() {
  let user = 7_200_000_000_000_001
  let guild = 7_200_000_000_000_901
  let observer = scn.observe(guild, [user])
  let old = scn.fake_session(user, 11)
  let old_link = scn.connect(old, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(old, protocol.handoff_exit())
  scn.sync(user)
  statuses(observer, user, 150) |> should.equal([])
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Online])
  let moved = scn.fake_session(user, 11)
  let new_link = scn.connect(moved, Online, [])
  scn.presence_pid(user) |> should.equal(Ok(pid))
  client.session_disconnect(old_link)
  scn.sync(user)
  statuses(observer, user, 150) |> should.equal([])
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Online])
  client.session_disconnect(new_link)
  scn.wait_down(pid, 2000) |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  scn.stop_observing(guild)
}

pub fn handed_off_session_stays_counted_beside_live_one_test() {
  let user = 7_200_000_000_000_002
  let guild = 7_200_000_000_000_902
  let observer = scn.observe(guild, [user])
  let desktop = scn.fake_session(user, 21)
  let phone = scn.fake_session(user, 22)
  let desktop_link = scn.connect(desktop, Online, [])
  let phone_link = scn.connect(phone, Idle, [])
  let _ = statuses(observer, user, 100)
  scn.exit_session(desktop, protocol.handoff_exit())
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  process.is_alive(pid) |> should.be_true
  statuses(observer, user, 100) |> should.equal([])
  cached_status(user) |> should.equal([Online])
  let back = scn.fake_session(user, 21)
  let back_link = scn.connect(back, Online, [])
  scn.sync(user)
  statuses(observer, user, 100) |> should.equal([])
  cached_status(user) |> should.equal([Online])
  client.session_disconnect(desktop_link)
  client.session_disconnect(phone_link)
  scn.sync(user)
  cached_status(user) |> should.equal([Online])
  client.session_disconnect(back_link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.stop_observing(guild)
}

pub fn closed_session_without_handoff_goes_offline_test() {
  let user = 7_200_000_000_000_003
  let guild = 7_200_000_000_000_903
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 31)
  let _link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, dynamic.string("crashed"))
  scn.wait_down(pid, 2000) |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  cached_status(user) |> should.equal([])
  scn.stop_observing(guild)
}

pub fn killed_presence_publishes_offline_and_restarts_cleanly_test() {
  let user = 7_200_000_000_000_004
  let guild = 7_200_000_000_000_904
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 41)
  let _link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  process.kill(pid)
  scn.wait_until(fn() { scn.presence_inbox(user) == Error(Nil) }, 2000)
  |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  cached_status(user) |> should.equal([])
  let again = scn.fake_session(user, 42)
  let link = scn.connect(again, Idle, [])
  statuses(observer, user, 150) |> should.equal([Idle])
  cached_status(user) |> should.equal([Idle])
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn crashed_restart_race_keeps_new_row_test() {
  let user = 7_200_000_000_000_005
  let guild = 7_200_000_000_000_905
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 51)
  let _ = scn.connect(session, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  process.kill(pid)
  let again = scn.fake_session(user, 52)
  let link = scn.connect(again, Online, [])
  let assert Ok(new_pid) = scn.presence_pid(user)
  { new_pid != pid } |> should.be_true
  scn.sync(user)
  process.sleep(100)
  cached_status(user) |> should.equal([Online])
  let _ = statuses(observer, user, 100)
  client.session_disconnect(link)
  scn.wait_down(new_pid, 2000) |> should.be_true
  cached_status(user) |> should.equal([])
  scn.stop_observing(guild)
}

pub fn stopped_presence_releases_its_subscriptions_test() {
  let alice = 7_200_000_000_000_006
  let bob = 7_200_000_000_000_007
  let session = scn.fake_session(alice, 61)
  let link = scn.connect(session, Online, [bob])
  let assert Ok(pid) = scn.presence_pid(alice)
  scn.wait_until(fn() { registry.local_subscribers(bob) != [] }, 1000)
  |> should.be_true
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.wait_until(fn() { registry.local_subscribers(bob) == [] }, 1000)
  |> should.be_true
}

pub fn guild_interest_drop_releases_subscription_test() {
  let user = 7_200_000_000_000_008
  let guild = 7_200_000_000_000_908
  let _observer = scn.observe(guild, [user])
  scn.wait_until(fn() { registry.local_subscribers(user) != [] }, 1000)
  |> should.be_true
  scn.stop_observing(guild)
  scn.wait_until(fn() { registry.local_subscribers(user) == [] }, 1000)
  |> should.be_true
}

pub fn friend_ends_online_after_presence_crash_and_reconnect_test() {
  let alice = 7_200_000_000_000_010
  let bob = 7_200_000_000_000_011
  let alice_session = scn.fake_session(alice, 101)
  let alice_link = scn.connect(alice_session, Online, [bob])
  let bob_session = scn.fake_session(bob, 111)
  let _ = scn.connect(bob_session, presence.Dnd, [])
  scn.wait_frames(alice_session, bob, 2000)
  |> list.map(scn.wire_status)
  |> should.equal(["dnd"])
  let assert Ok(pid) = scn.presence_pid(bob)
  process.kill(pid)
  let again = scn.fake_session(bob, 112)
  let bob_link = scn.connect(again, Idle, [])
  scn.sync(bob)
  scn.sync(alice)
  let seen =
    scn.drain_seen(alice_session, 200)
    |> scn.frames_about(bob)
    |> list.map(scn.wire_status)
  list.last(seen) |> should.equal(Ok("idle"))
  client.session_disconnect(bob_link)
  client.session_disconnect(alice_link)
}
