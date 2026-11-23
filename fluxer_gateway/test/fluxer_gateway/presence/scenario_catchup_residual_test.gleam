// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRequest, MemberPresence}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{Idle, Offline, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/client
import fluxer_gateway/presence/dispatch_id
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol.{
  type StartRequest, Carried, CarriedSession, GuildSubscriber, Published,
  StartRequest,
}
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/version
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import support/presence_scenario as scn
import support/push_crash
import support/tick_trace

@external(erlang, "erlang", "send")
fn raw_send(pid: Pid, message: a) -> a

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

fn shard_pid(prefix: String, user: Int) -> Pid {
  let name =
    "fluxer_presence_" <> prefix <> "_" <> int.to_string(route.shard(user))
  let assert Ok(pid) = tick_trace.whereis(name)
  pid
}

fn old_node() -> Atom {
  atom.create("presence_old_build@nowhere")
}

fn stopped(user: Int, tag: Int) -> Nil {
  let session = scn.fake_session(user, tag)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.wait_until(fn() { scn.presence_inbox(user) == Error(Nil) }, 1000)
  |> should.be_true
}

fn subscribe_everything(guild: Int, user: Int) -> Subject(GuildRequest) {
  let inbox = process.new_subject()
  registry.subscribe_many(
    GuildSubscriber(guild:, inbox:),
    id_set.from_list([user]),
    resend: True,
  )
  scn.sync_registry(user)
  inbox
}

fn unsubscribe(guild: Int, user: Int, inbox: Subject(GuildRequest)) -> Nil {
  registry.unsubscribe_many(
    GuildSubscriber(guild:, inbox:),
    id_set.from_list([user]),
  )
  scn.sync_registry(user)
}

pub fn everything_catch_up_reports_a_buried_presence_offline_test() {
  let user = 7_640_000_000_000_001
  let guild = 7_640_000_000_000_901
  stopped(user, 1)
  let inbox = subscribe_everything(guild, user)
  let assert Ok(seen) = scn.next_presence(inbox, user, 2000)
  presence.status(seen) |> should.equal(Offline)
  unsubscribe(guild, user, inbox)
}

pub fn everything_catch_up_of_a_restarted_presence_ends_online_test() {
  let user = 7_640_000_000_000_002
  let guild = 7_640_000_000_000_902
  stopped(user, 2)
  let link = scn.connect(scn.fake_session(user, 3), Idle, [])
  scn.sync(user)
  let inbox = subscribe_everything(guild, user)
  scn.sync(user)
  let seen = scn.presences(inbox, user, 300)
  list.map(seen, presence.status) |> should.equal([Idle])
  client.session_disconnect(link)
  unsubscribe(guild, user, inbox)
}

fn carried_request(user: Int, session: scn.FakeSession) -> StartRequest {
  let shown = display.new(status: Online, mobile: False, afk: False)
  let current_version = version.next(version.first(version.new_epoch()))
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
        current: Some(event.status_changed(
          user:,
          version: current_version,
          display: shown,
          custom_status: <<"null":utf8>>,
          user_wire: raw_json.to_bits(scn.wire_for(user)),
        )),
        temporary_guilds: id_set.empty(),
        push: push_buffer.new(),
        sessions: [
          CarriedSession(
            session: session.ref,
            status: Online,
            afk: False,
            mobile: False,
          ),
        ],
        validating: None,
        delivered: dispatch_id.empty(),
        doomed: [],
      ),
    ),
  )
}

fn start_handoff(
  request: StartRequest,
) -> Result(Result(protocol.Handoff, Nil), remote.CallError) {
  remote.call(
    remote.self_atom(),
    route.name(route.Manager, route.shard(request.user)),
    3000,
    fn(reply) { protocol.StartHandoff(request, reply) },
  )
}

type Probe {
  ProbeReady(release: Subject(Nil), pid: Pid)
  ManagerFrozen(found: Bool)
}

fn start_probe(
  guild: Int,
  user: Int,
  manager: Pid,
) -> #(Subject(Probe), Subject(Nil), Pid) {
  let events = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      let inbox = process.new_subject()
      let release = process.new_subject()
      let assert Ok(guild_id) = id.guild_id(guild)
      client.register_guild_interest(
        guild_id,
        packed_ids.from_list([user]),
        inbox,
      )
      process.send(events, ProbeReady(release, process.self()))
      probe_loop(inbox, release, user, manager, events)
    })
  let assert Ok(ProbeReady(release, pid)) = process.receive(events, 2000)
  #(events, release, pid)
}

fn probe_loop(
  inbox: Subject(GuildRequest),
  release: Subject(Nil),
  user: Int,
  manager: Pid,
  events: Subject(Probe),
) -> Nil {
  case process.receive_forever(inbox) {
    MemberPresence(found) ->
      case
        id.user_id_to_int(presence.user(found)) == user
        && presence.status(found) == Online
      {
        True -> {
          suspend_process(manager)
          process.send(
            events,
            ManagerFrozen(scn.presence_inbox(user) != Error(Nil)),
          )
          process.receive_forever(release)
          resume_process(manager)
          probe_loop(inbox, release, user, manager, events)
        }
        False -> probe_loop(inbox, release, user, manager, events)
      }
    _ -> probe_loop(inbox, release, user, manager, events)
  }
}

pub fn a_subscribe_between_the_first_publish_and_the_pid_record_sees_online_test() {
  let user = 7_640_000_000_000_003
  let probe_guild = 7_640_000_000_000_903
  let late_guild = 7_640_000_000_000_904
  let manager = shard_pid("manager", user)
  let registry_pid = shard_pid("registry", user)
  let #(events, release, probe) = start_probe(probe_guild, user, manager)
  scn.sync_registry(user)
  push_crash.suspend(registry_pid)
  let late = scn.observe(late_guild, [user])
  let session = scn.fake_session(user, 4)
  let started = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      process.send(started, start_handoff(carried_request(user, session)))
    })
  let frozen = process.receive(events, 2000)
  tick_trace.resume(registry_pid)
  scn.sync_registry(user)
  let seen = scn.next_presence(late, user, 1000)
  process.send(release, Nil)
  let assert Ok(ManagerFrozen(found)) = frozen
  found |> should.be_true
  let assert Ok(online) = seen
  presence.status(online) |> should.equal(Online)
  let assert Ok(Ok(Ok(protocol.HandoffSpawned(_)))) =
    process.receive(started, 5000)
  let assert Ok(pid) = scn.presence_pid(user)
  scn.exit_session(session, scn.closed_reason())
  scn.wait_down(pid, 2000) |> should.be_true
  scn.stop_observing(late_guild)
  let assert Ok(guild_id) = id.guild_id(probe_guild)
  client.drop_guild_interest_of(guild_id, probe)
}

fn registry_atom(user: Int) -> Atom {
  atom.create("fluxer_presence_registry_" <> int.to_string(route.shard(user)))
}

fn catch_up_atom(user: Int) -> Atom {
  atom.create(
    "fluxer_presence_registry_catch_up_" <> int.to_string(route.shard(user)),
  )
}

fn old_interest_add(user: Int, resend_to: Dynamic) -> Nil {
  let shard = shard_pid("registry", user)
  raw_send(shard, #(
    registry_atom(user),
    #(atom.create("interest_add"), old_node(), [user], resend_to),
  ))
  scn.sync_registry(user)
  shard_pid("registry", user) |> should.equal(shard)
}

fn forget_old_node(user: Int) -> Nil {
  raw_send(shard_pid("registry", user), #(
    registry_atom(user),
    registry.InterestRemove(old_node(), [user]),
  ))
  scn.sync_registry(user)
}

pub fn an_old_build_interest_add_with_a_resend_is_accepted_test() {
  let user = 7_640_000_000_000_005
  let guild = 7_640_000_000_000_905
  let link = scn.connect(scn.fake_session(user, 5), Online, [])
  scn.sync(user)
  let inbox = process.new_subject()
  old_interest_add(
    user,
    dynamic.array([
      atom.to_dynamic(atom.create("some")),
      coerce(GuildSubscriber(guild:, inbox:)),
    ]),
  )
  registry.interest(user) |> list.contains(old_node()) |> should.be_true
  let assert Ok(seen) = scn.next_presence(inbox, user, 2000)
  presence.status(seen) |> should.equal(Online)
  forget_old_node(user)
  client.session_disconnect(link)
}

pub fn an_old_build_interest_add_without_a_resend_is_accepted_test() {
  let user = 7_640_000_000_000_006
  old_interest_add(user, atom.to_dynamic(atom.create("none")))
  registry.interest(user) |> list.contains(old_node()) |> should.be_true
  forget_old_node(user)
  registry.interest(user) |> list.contains(old_node()) |> should.be_false
}

pub fn a_catch_up_since_under_its_own_tag_is_taken_test() {
  let user = 7_640_000_000_000_007
  let guild = 7_640_000_000_000_907
  let link = scn.connect(scn.fake_session(user, 7), Online, [])
  scn.sync(user)
  let inbox = process.new_subject()
  raw_send(shard_pid("registry", user), #(
    catch_up_atom(user),
    #(atom.create("catch_up_since"), GuildSubscriber(guild:, inbox:), [user], 0),
  ))
  scn.sync_registry(user)
  scn.sync(user)
  let assert Ok(seen) = scn.next_presence(inbox, user, 2000)
  presence.status(seen) |> should.equal(Online)
  client.session_disconnect(link)
}

pub fn a_shard_of_an_old_build_does_not_take_the_catch_up_message_test() {
  let user = 7_640_000_000_000_008
  let old_shard: Subject(Dynamic) =
    process.named_subject(route.name(route.Registry, route.shard(user)))
  raw_send(process.self(), #(
    catch_up_atom(user),
    #(atom.create("catch_up_since"), Nil, [user], 0),
  ))
  process.new_selector()
  |> process.select(old_shard)
  |> process.selector_receive(50)
  |> should.equal(Error(Nil))
  let leftover: Subject(Dynamic) =
    process.named_subject(route.name(route.RegistryCatchUp, route.shard(user)))
  process.receive(leftover, 50) |> should.be_ok
}

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> Dynamic
