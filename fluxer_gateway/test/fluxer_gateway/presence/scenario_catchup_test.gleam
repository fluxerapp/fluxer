// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PresenceDurations}
import fluxer_gateway/core/presence.{Idle, Offline, Online}
import fluxer_gateway/presence/client
import fluxer_gateway/presence/route
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleeunit/should
import support/isolation
import support/presence_scenario as scn
import support/push_crash
import support/tick_trace

@external(erlang, "ets", "lookup")
fn ets_lookup(table: atom.Atom, key: Int) -> List(#(Int, Int))

fn buried(user: Int) -> Bool {
  ets_lookup(route.table(route.GravesTable, route.shard(user)), user) != []
}

fn registry_name(user: Int) -> String {
  "fluxer_presence_registry_" <> int.to_string(route.shard(user))
}

fn frozen_registry(user: Int) -> Pid {
  let assert Ok(pid) = tick_trace.whereis(registry_name(user))
  push_crash.suspend(pid)
  pid
}

pub fn a_guild_registering_while_the_user_connects_still_sees_online_test() {
  let user = 7_600_000_000_000_001
  let guild = 7_600_000_000_000_901
  let registry = frozen_registry(user)
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  tick_trace.resume(registry)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Online)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn a_status_change_while_the_guild_registers_is_caught_up_test() {
  let user = 7_600_000_000_000_002
  let guild = 7_600_000_000_000_902
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let registry = frozen_registry(user)
  let observer = scn.observe(guild, [user])
  client.session_update(link, Idle, False, False, None)
  scn.sync(user)
  tick_trace.resume(registry)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Idle)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn an_unchanged_presence_is_not_sent_again_on_registration_test() {
  let user = 7_600_000_000_000_003
  let guild = 7_600_000_000_000_903
  let session = scn.fake_session(user, 3)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  process.sleep(5)
  let observer = scn.observe(guild, [user])
  scn.sync_registry(user)
  scn.sync(user)
  scn.presences(observer, user, 150) |> should.equal([])
  client.session_update(link, Idle, False, False, None)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Idle)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn a_presence_that_stops_while_the_guild_registers_is_caught_up_offline_test() {
  let user = 7_600_000_000_000_004
  let guild = 7_600_000_000_000_904
  let session = scn.fake_session(user, 4)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  let registry = frozen_registry(user)
  let observer = scn.observe(guild, [user])
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  tick_trace.resume(registry)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Offline)
  scn.stop_observing(guild)
}

pub fn a_presence_killed_while_the_guild_registers_is_caught_up_offline_test() {
  let user = 7_600_000_000_000_005
  let guild = 7_600_000_000_000_905
  let session = scn.fake_session(user, 5)
  let _link = scn.connect(session, Online, [])
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  let registry = frozen_registry(user)
  let observer = scn.observe(guild, [user])
  process.kill(pid)
  scn.wait_until(fn() { scn.presence_pid(user) == Error(Nil) }, 2000)
  |> should.be_true
  tick_trace.resume(registry)
  let assert Ok(seen) = scn.next_presence(observer, user, 2000)
  presence.status(seen) |> should.equal(Offline)
  scn.stop_observing(guild)
}

pub fn a_presence_restarted_inside_the_window_ends_online_test() {
  let user = 7_600_000_000_000_006
  let guild = 7_600_000_000_000_906
  let first = scn.fake_session(user, 6)
  let link = scn.connect(first, Online, [])
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  let registry = frozen_registry(user)
  let observer = scn.observe(guild, [user])
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  let second = scn.fake_session(user, 7)
  let again = scn.connect(second, Idle, [])
  scn.sync(user)
  tick_trace.resume(registry)
  scn.sync_registry(user)
  scn.sync(user)
  let seen = scn.presences(observer, user, 300)
  let assert [_, ..] = seen
  let assert Ok(last) = list.last(seen)
  presence.status(last) |> should.equal(Idle)
  client.session_disconnect(again)
  scn.stop_observing(guild)
}

pub fn a_user_who_left_long_before_registration_gets_no_resend_test() {
  let user = 7_600_000_000_000_008
  let guild = 7_600_000_000_000_908
  let observer = scn.observe(guild, [user])
  scn.sync_registry(user)
  scn.presences(observer, user, 150) |> should.equal([])
  scn.stop_observing(guild)
}

fn fast_registry_ticks() -> Nil {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      presence: PresenceDurations(
        ..durations.presence,
        registry_digest_interval_ms: 50,
        registry_sweep_interval_ms: 50,
      ),
    )
  })
}

pub fn registry_ticks_do_not_multiply_across_restarts_test() {
  fast_registry_ticks()
  let name = "fluxer_presence_registry_3"
  tick_trace.inject(name, "digest_tick")
  tick_trace.inject(name, "sweep_tick")
  process.sleep(20)
  let assert Ok(_) = tick_trace.restart(name, 2000)
  let assert Ok(survivor) = tick_trace.whereis(name)
  let digests = tick_trace.count_received(survivor, "digest_tick", 600)
  let sweeps = tick_trace.count_received(survivor, "sweep_tick", 600)
  { digests <= 15 } |> should.be_true
  { sweeps <= 15 } |> should.be_true
}

pub fn a_grave_expires_after_its_ttl_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      presence: PresenceDurations(
        ..durations.presence,
        registry_grave_ttl_ms: 50,
        registry_sweep_interval_ms: 50,
      ),
    )
  })
  let _ = tick_trace.restart(registry_name(7_600_000_000_000_009), 2000)
  let user = 7_600_000_000_000_009
  let guild = 7_600_000_000_000_909
  let session = scn.fake_session(user, 9)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.wait_until(fn() { !buried(user) }, 5000) |> should.be_true
  let observer = scn.observe(guild, [user])
  scn.sync_registry(user)
  scn.presences(observer, user, 150) |> should.equal([])
  scn.stop_observing(guild)
}
