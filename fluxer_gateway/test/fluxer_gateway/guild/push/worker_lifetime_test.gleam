// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, GuildDurations}
import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/guild/members
import fluxer_gateway/guild/push/engagement
import fluxer_gateway/guild/push/guild_info
import fluxer_gateway/guild/push/plan.{Capture}
import fluxer_gateway/guild/push/profiles
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config as ml_config
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/fixture as perms_fixture
import fluxer_gateway/perms/snapshot
import fluxer_gateway/push/message
import fluxer_gateway/push/permit
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/set
import support/guild_api
import support/guild_fixture as fixture
import support/isolation
import support/push_permit_probe as probe
import support/wait

const guild = 400

const text = 41

const view_channel = 1024

@external(erlang, "erlang", "system_info")
fn system_info(item: Atom) -> Int

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn message_json(channel: Int, message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn member(user: Int) {
  perms_fixture.make_member(
    user,
    None,
    "{\"id\":\"" <> int.to_string(user) <> "\",\"username\":\"u\"}",
  )
}

fn capture(users: List(Int)) -> #(perms.Perms, plan.Capture) {
  let assert Ok(Nil) = nif.load()
  let guild_id = perms_fixture.guild(guild)
  let source =
    list.fold(
      users,
      snapshot.new(guild_id)
        |> snapshot.set_roles([perms_fixture.make_role(guild, view_channel)])
        |> snapshot.add_channel(
          perms_fixture.make_channel(text, channel.Text, None, []),
        ),
      fn(acc, user) { snapshot.add_member(acc, member(user)) },
    )
  let #(handle, reader) = perms.new(guild_id, process.self())
  let assert Ok(_) = perms.load(handle, source)
  let table = members.new(None)
  members.put_many(table, list.map(users, member))
  let #(member_list, _) =
    member_list.new(guild_id, ml_config.default(), process.self(), handle)
  let assert Ok(data) =
    message.decode_data(fixture.to_dynamic(message_json(text, 9001)))
  let #(_, snapshot) = engagement.snapshot(engagement.new(), [])
  let found =
    Capture(
      guild: guild_id,
      data:,
      channel: text,
      message: 9001,
      author: 1,
      thread: None,
      snapshot:,
      reader:,
      model: perms.model(reader),
      members: table,
      list: member_list,
      roles: [],
      facts: core_guild.facts(
        id: guild_id,
        owner: None,
        nsfw_level: core_guild.NsfwDefault,
        features: [],
        member_count: list.length(users),
      ),
      info: guild_info.empty(),
      profiles: profiles.new(guild_id, fn(_) { Nil }),
      inbox: process.new_subject(),
    )
  #(handle, found)
}

pub fn a_released_index_is_not_read_as_zero_recipients_test() {
  let #(handle, found) = capture([2, 3])
  let assert Ok(routed) = plan.route(found, [2, 3], set.new())
  assert list.sort(routed.eligible, int.compare) == [2, 3]
  perms.release(handle)
  assert plan.route(found, [2, 3], set.new()) == Error(plan.PermsUnavailable)
  members.drop(found.members)
}

pub fn a_dropped_members_table_is_not_read_as_zero_recipients_test() {
  let #(handle, found) = capture([2, 3])
  let assert Ok(everyone) = plan.all_member_ids(found)
  assert list.sort(everyone, int.compare) == [2, 3]
  members.drop(found.members)
  assert plan.all_member_ids(found) == Error(plan.MembersUnavailable)
  assert plan.route(found, [2, 3], set.new()) == Error(plan.MembersUnavailable)
  perms.release(handle)
}

fn idle_grace(start_ms: Int, empty_ms: Int) -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      guild: GuildDurations(
        ..d.guild,
        server_idle_start_grace_ms: start_ms,
        server_auto_stop_grace_ms: empty_ms,
      ),
    )
  })
}

pub fn an_idle_guild_outlives_its_queued_wide_push_test() {
  idle_grace(200, 100)
  use api <- guild_api.with_api
  let reports = process.new_subject()
  let holders =
    int.range(
      0,
      2 * system_info(atom.create("schedulers_online")),
      [],
      fn(acc, _) {
        let holder = probe.spawn_holder(permit.GuildWide, 30_000, reports)
        let assert Ok(probe.Acquired(_, Ok(Nil))) =
          process.receive(reports, 2000)
        [holder, ..acc]
      },
    )
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
  let handle = fixture.started(api, found)
  fixture.dispatch(handle, "MESSAGE_CREATE", message_json(10, 7001))
  assert wait.until(2000, fn() { probe.waiting_count() == 1 })
  process.sleep(700)
  let alive = process.is_alive(handle.pid)
  list.each(holders, fn(holder) { process.send(holder.commands, probe.Release) })
  assert wait.until(5000, fn() { !process.is_alive(handle.pid) })
  list.each(holders, fn(holder) { process.kill(holder.pid) })
  assert wait.until(2000, fn() { probe.held_count() == 0 })
  assert alive
}
