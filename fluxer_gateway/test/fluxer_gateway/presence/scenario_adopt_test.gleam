// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Idle, Offline, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/client
import fluxer_gateway/presence/dispatch_id
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol.{
  type Handoff, Carried, CarriedSession, Published, StartRequest,
}
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/version
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/presence_scenario as scn

fn cached_status(user: Int) -> List(presence.Status) {
  cache.lookup_many(packed_ids.from_list([user]))
  |> list.map(presence.status)
}

fn carried_request(
  user: Int,
  sessions: List(scn.FakeSession),
  status: presence.Status,
  temporary: List(Int),
  delivered: dispatch_id.Recent,
) -> protocol.StartRequest {
  let user_wire = raw_json.to_bits(scn.wire_for(user))
  let shown = display.new(status:, mobile: False, afk: False)
  let current_version = version.next(version.first(version.new_epoch()))
  let current =
    event.status_changed(
      user:,
      version: current_version,
      display: shown,
      custom_status: <<"null":utf8>>,
      user_wire:,
    )
  StartRequest(
    user:,
    profile: profile.new(
      fields: [
        #(profile.Id, <<"\"":utf8, int.to_string(user):utf8, "\"":utf8>>),
      ],
      restricted: False,
      bot: False,
    ),
    custom_status: None,
    settings_invisible: False,
    guilds: id_set.empty(),
    targets: id_set.empty(),
    push_limits: push_buffer.default_limits(),
    carried: Some(
      Carried(
        version: current_version,
        last: Some(Published(shown, <<"null":utf8>>, <<"null":utf8>>)),
        current: Some(current),
        temporary_guilds: id_set.from_list(temporary),
        push: push_buffer.new(),
        sessions: list.map(sessions, fn(fake) {
          CarriedSession(session: fake.ref, status:, afk: False, mobile: False)
        }),
        validating: None,
        delivered:,
        doomed: [],
      ),
    ),
  )
}

fn start_handoff(
  request: protocol.StartRequest,
) -> Result(Result(Handoff, Nil), remote.CallError) {
  remote.call(
    remote.self_atom(),
    route.name(route.Manager, route.shard(request.user)),
    2000,
    fn(reply) { protocol.StartHandoff(request, reply) },
  )
}

pub fn handoff_spawn_keeps_status_and_watches_carried_sessions_test() {
  let user = 7_500_000_000_000_001
  let guild = 7_500_000_000_000_901
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let assert Ok(Ok(protocol.HandoffSpawned(inbox))) =
    start_handoff(carried_request(
      user,
      [session],
      Idle,
      [],
      dispatch_id.empty(),
    ))
  scn.presence_inbox(user) |> should.equal(Ok(inbox))
  scn.sync(user)
  scn.presences(observer, user, 100)
  |> list.map(presence.status)
  |> list.filter(fn(found) { found != Idle })
  |> should.equal([])
  cached_status(user) |> should.equal([Idle])
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, scn.closed_reason())
  scn.wait_down(pid, 2000) |> should.be_true
  scn.presences(observer, user, 150)
  |> list.map(presence.status)
  |> should.equal([Offline])
  cached_status(user) |> should.equal([])
  scn.stop_observing(guild)
}

pub fn handoff_into_running_presence_merges_sessions_test() {
  let user = 7_500_000_000_000_002
  let guild = 7_500_000_000_000_902
  let observer = scn.observe(guild, [user])
  let early = scn.fake_session(user, 21)
  let link = scn.connect(early, Online, [])
  let carried = scn.fake_session(user, 22)
  let _ = scn.drain_seen(early, 50)
  let _ = scn.presences(observer, user, 50)
  let request =
    carried_request(user, [early, carried], Online, [], dispatch_id.empty())
  let assert Ok(Ok(protocol.HandoffRunning(inbox))) = start_handoff(request)
  let assert Ok(protocol.Adopted) =
    remote.call_subject(inbox, 2000, fn(reply) {
      protocol.AdoptHandoff(request, reply)
    })
  scn.drain_seen(early, 100)
  |> scn.dispatches("SESSIONS_REPLACE")
  |> list.length
  |> should.equal(1)
  scn.presences(observer, user, 100)
  |> list.filter(fn(found) { presence.status(found) != Online })
  |> should.equal([])
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.sync(user)
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Online])
  scn.exit_session(carried, scn.closed_reason())
  scn.wait_down(pid, 2000) |> should.be_true
  scn.stop_observing(guild)
}

pub fn adopted_dispatch_ids_are_not_delivered_twice_test() {
  let user = 7_500_000_000_000_003
  let session = scn.fake_session(user, 31)
  let already = dispatch_id.mint()
  let assert Ok(Ok(protocol.HandoffSpawned(inbox))) =
    start_handoff(carried_request(
      user,
      [session],
      Online,
      [],
      dispatch_id.remember(dispatch_id.empty(), already),
    ))
  process.send(
    inbox,
    protocol.UserDispatch(
      id: already,
      event: "USER_NOTE_UPDATE",
      data: raw_json.null(),
      parsed: protocol.OtherEvent,
    ),
  )
  process.send(
    inbox,
    protocol.UserDispatch(
      id: dispatch_id.mint(),
      event: "USER_NOTE_UPDATE",
      data: raw_json.null(),
      parsed: protocol.OtherEvent,
    ),
  )
  scn.sync(user)
  scn.drain_seen(session, 100)
  |> scn.dispatches("USER_NOTE_UPDATE")
  |> list.length
  |> should.equal(1)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, scn.closed_reason())
  scn.wait_down(pid, 2000) |> should.be_true
}

pub fn adopted_temporary_guild_is_announced_to_sessions_test() {
  let user = 7_500_000_000_000_004
  let guild = 7_500_000_000_000_804
  let session = scn.fake_session(user, 41)
  let link = scn.connect(session, Online, [])
  let _ = scn.drain_seen(session, 50)
  let request =
    carried_request(user, [session], Online, [guild], dispatch_id.empty())
  let assert Ok(Ok(protocol.HandoffRunning(inbox))) = start_handoff(request)
  let assert Ok(protocol.Adopted) =
    remote.call_subject(inbox, 2000, fn(reply) {
      protocol.AdoptHandoff(request, reply)
    })
  scn.drain_seen(session, 100)
  |> list.count(fn(found) {
    case found {
      scn.Notice(notice) -> notice == scn.guild_join(guild)
      _ -> False
    }
  })
  |> should.equal(1)
  client.session_disconnect(link)
}

pub fn handoff_with_already_moved_session_waits_for_its_reconnect_test() {
  let user = 7_500_000_000_000_005
  let guild = 7_500_000_000_000_905
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 51)
  scn.exit_session(session, protocol.handoff_exit())
  let assert Ok(Ok(protocol.HandoffSpawned(_))) =
    start_handoff(carried_request(
      user,
      [session],
      Online,
      [],
      dispatch_id.empty(),
    ))
  scn.sync(user)
  process.sleep(100)
  let assert Ok(pid) = scn.presence_pid(user)
  process.is_alive(pid) |> should.be_true
  cached_status(user) |> should.equal([Online])
  let moved = scn.fake_session(user, 51)
  let link = scn.connect(moved, Online, [])
  scn.presence_pid(user) |> should.equal(Ok(pid))
  scn.presences(observer, user, 150)
  |> list.filter(fn(found) { presence.status(found) != Online })
  |> should.equal([])
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.presences(observer, user, 150)
  |> list.map(presence.status)
  |> should.equal([Offline])
  scn.stop_observing(guild)
}
