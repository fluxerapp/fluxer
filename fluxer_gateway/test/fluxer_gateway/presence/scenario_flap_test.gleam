// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Dnd, Idle, Invisible, Offline, Online}
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should
import support/isolation
import support/presence_scenario as scn

fn cached_status(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user]))
  |> list.map(presence.status)
}

fn statuses(observer, user: Int, quiet_ms: Int) -> List(presence.Status) {
  scn.presences(observer, user, quiet_ms) |> list.map(presence.status)
}

fn short_grace(grace_ms: Int) -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      presence: settings.PresenceDurations(
        ..d.presence,
        state_noconnection_grace_ms: grace_ms,
      ),
    )
  })
}

fn noconnection() -> dynamic.Dynamic {
  atom.to_dynamic(atom.create("noconnection"))
}

pub fn sessions_handed_off_together_do_not_flap_test() {
  let user = 7_800_000_000_000_001
  let guild = 7_800_000_000_000_901
  let observer = scn.observe(guild, [user])
  let desktop = scn.fake_session(user, 1)
  let phone = scn.fake_session(user, 2)
  let _ = scn.connect(desktop, Online, [])
  let _ = scn.connect(phone, Dnd, [])
  statuses(observer, user, 150) |> list.last |> should.equal(Ok(Dnd))
  scn.exit_session(desktop, protocol.handoff_exit())
  scn.exit_session(phone, protocol.handoff_exit())
  scn.sync(user)
  statuses(observer, user, 100) |> should.equal([])
  let desktop_back = scn.fake_session(user, 1)
  let desktop_link = scn.connect(desktop_back, Online, [])
  scn.sync(user)
  statuses(observer, user, 150) |> should.equal([])
  cached_status(user) |> should.equal([Dnd])
  let phone_back = scn.fake_session(user, 2)
  let phone_link = scn.connect(phone_back, Dnd, [])
  scn.sync(user)
  statuses(observer, user, 150) |> should.equal([])
  cached_status(user) |> should.equal([Dnd])
  client.session_disconnect(phone_link)
  let assert Ok(after) = scn.next_presence(observer, user, 2000)
  presence.status(after) |> should.equal(Online)
  client.session_disconnect(desktop_link)
  let assert Ok(gone) = scn.next_presence(observer, user, 2000)
  presence.status(gone) |> should.equal(Offline)
  scn.stop_observing(guild)
}

pub fn handed_off_session_that_never_returns_expires_after_grace_test() {
  short_grace(200)
  let user = 7_800_000_000_000_002
  let guild = 7_800_000_000_000_902
  let observer = scn.observe(guild, [user])
  let desktop = scn.fake_session(user, 1)
  let phone = scn.fake_session(user, 2)
  let _ = scn.connect(desktop, Dnd, [])
  let _ = scn.connect(phone, Online, [])
  let _ = statuses(observer, user, 100)
  scn.exit_session(desktop, protocol.handoff_exit())
  scn.exit_session(phone, protocol.handoff_exit())
  let phone_back = scn.fake_session(user, 2)
  let phone_link = scn.connect(phone_back, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  statuses(observer, user, 600) |> should.equal([Online])
  cached_status(user) |> should.equal([Online])
  process.is_alive(pid) |> should.be_true
  client.session_disconnect(phone_link)
  scn.wait_down(pid, 2000) |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  scn.stop_observing(guild)
}

pub fn crashed_session_is_replaced_at_once_by_new_session_test() {
  let user = 7_800_000_000_000_003
  let guild = 7_800_000_000_000_903
  let observer = scn.observe(guild, [user])
  let old = scn.fake_session(user, 1)
  let _ = scn.connect(old, Online, [])
  let _ = statuses(observer, user, 100)
  scn.exit_session(old, atom.to_dynamic(atom.create("noproc")))
  let fresh = scn.fake_session(user, 2)
  let link = scn.connect(fresh, Invisible, [])
  statuses(observer, user, 150) |> should.equal([Offline])
  cached_status(user) |> should.equal([])
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn node_blip_reconnect_within_grace_keeps_presence_test() {
  short_grace(300)
  let user = 7_800_000_000_000_004
  let guild = 7_800_000_000_000_904
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let _ = scn.connect(session, Idle, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, noconnection())
  process.sleep(100)
  let again = scn.fake_session(user, 1)
  let link = scn.connect(again, Idle, [])
  statuses(observer, user, 500) |> should.equal([])
  scn.presence_pid(user) |> should.equal(Ok(pid))
  cached_status(user) |> should.equal([Idle])
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  scn.stop_observing(guild)
}

pub fn node_down_without_return_goes_offline_after_grace_test() {
  short_grace(200)
  let user = 7_800_000_000_000_005
  let guild = 7_800_000_000_000_905
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let _ = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, noconnection())
  statuses(observer, user, 100) |> should.equal([])
  scn.wait_down(pid, 2000) |> should.be_true
  statuses(observer, user, 150) |> should.equal([Offline])
  cached_status(user) |> should.equal([])
  scn.stop_observing(guild)
}

pub fn flapping_reconnects_end_in_consistent_state_test() {
  short_grace(300)
  let user = 7_800_000_000_000_006
  let friend = 7_800_000_000_000_007
  let guild = 7_800_000_000_000_906
  let observer = scn.observe(guild, [user])
  let reasons = [
    protocol.handoff_exit(),
    dynamic.string("crashed"),
    atom.to_dynamic(atom.create("normal")),
    protocol.handoff_exit(),
  ]
  upto(1, 24)
  |> list.each(fn(round) {
    let session = scn.fake_session(user, 1 + round % 3)
    let link = scn.connect(session, Online, [friend])
    case round % 5 {
      0 -> client.session_disconnect(link)
      4 ->
        case scn.presence_pid(user) {
          Ok(pid) -> process.kill(pid)
          Error(Nil) -> Nil
        }
      index -> {
        let assert Ok(reason) = list.drop(reasons, index - 1) |> list.first
        scn.exit_session(session, reason)
      }
    }
  })
  let last = scn.fake_session(user, 99)
  let link = scn.connect(last, Dnd, [friend])
  scn.sync(user)
  scn.wait_until(fn() { cached_status(user) == [Dnd] }, 1000)
  |> should.be_true
  scn.presences(observer, user, 200)
  |> list.last
  |> result_status
  |> should.equal(Ok(Dnd))
  sweep(friend)
  scn.wait_until(
    fn() { list.length(registry.local_subscribers(friend)) == 1 },
    1000,
  )
  |> should.be_true
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  sweep(friend)
  scn.wait_until(fn() { registry.local_subscribers(friend) == [] }, 2000)
  |> should.be_true
  cached_status(user) |> should.equal([])
  scn.presences(observer, user, 200)
  |> list.last
  |> result_status
  |> should.equal(Ok(Offline))
  scn.stop_observing(guild)
}

fn result_status(
  found: Result(presence.Presence, Nil),
) -> Result(presence.Status, Nil) {
  case found {
    Ok(value) -> Ok(presence.status(value))
    Error(Nil) -> Error(Nil)
  }
}

pub fn interest_churn_releases_every_subscription_test() {
  let members = upto(1, 300) |> list.map(fn(n) { 7_800_000_000_100_000 + n })
  let guilds = upto(1, 12) |> list.map(fn(n) { 7_800_000_000_200_000 + n })
  let observers =
    list.index_map(guilds, fn(guild, index) {
      let slice = list.filter(members, fn(m) { { m + index } % 3 != 0 })
      scn.observe(guild, slice)
    })
  let _ = observers
  let sessions =
    list.take(members, 40)
    |> list.index_map(fn(member, index) {
      let session = scn.fake_session(member, 1)
      let link =
        scn.connect(session, Online, list.drop(members, 250 + index % 10))
      #(member, link)
    })
  list.index_map(guilds, fn(guild, index) {
    case index % 3 {
      0 -> scn.stop_observing(guild)
      1 -> {
        let assert Ok(guild_id) = id.guild_id(guild)
        client.unregister_guild_interest(
          guild_id,
          packed_ids.from_list(list.take(members, 150)),
        )
      }
      _ -> {
        let _ = scn.observe(guild, list.take(members, 10))
        Nil
      }
    }
  })
  list.each(sessions, fn(pair) {
    case int.is_even(pair.0) {
      True -> client.session_disconnect(pair.1)
      False ->
        case scn.presence_pid(pair.0) {
          Ok(pid) -> process.kill(pid)
          Error(Nil) -> Nil
        }
    }
  })
  list.each(guilds, fn(guild) {
    let _ = process.spawn(fn() { scn.stop_observing(guild) })
    Nil
  })
  list.each(guilds, scn.stop_observing)
  scn.wait_until(
    fn() { list.all(members, fn(m) { scn.presence_pid(m) == Error(Nil) }) },
    5000,
  )
  |> should.be_true
  list.each(members, sweep)
  let left = list.filter(members, fn(m) { registry.local_subscribers(m) != [] })
  let assert [] = left
    as string.inspect(list.map(left, registry.local_subscribers))
}

fn upto(from: Int, to: Int) -> List(Int) {
  int.range(from, to + 1, [], list.prepend) |> list.reverse
}

fn sweep(target: Int) -> Nil {
  let shard = route.shard(target)
  let _ =
    remote.send_registered(
      route.name(route.Registry, shard),
      registry.SweepTick,
    )
  scn.sync_registry(target)
  scn.sync_registry(target)
}
