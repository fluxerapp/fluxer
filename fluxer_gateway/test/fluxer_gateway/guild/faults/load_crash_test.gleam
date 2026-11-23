// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, GuildDurations}
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild
import fluxer_gateway/core/node
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/names
import fluxer_gateway/guild/read_model
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/result
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation
import support/wait

fn members_page(found: fixture.Guild) -> guild_api.Reply {
  guild_api.Data(
    json.object([
      #(
        "members",
        json.preprocessed_array(list.map(found.members, fn(m) { m.1 })),
      ),
      #("has_more", json.bool(False)),
      #("next_after_user_id", json.null()),
    ]),
  )
}

fn slow_members(api: guild_api.FakeApi, found: fixture.Guild, ms: Int) -> Nil {
  guild_api.override(
    api,
    "guild_collection",
    Some("members"),
    guild_api.Delayed(ms, members_page(found)),
  )
}

fn shard_pid(found: fixture.Guild) -> process.Pid {
  let assert Ok(pid) =
    process.named(names.shard(names.shard_of(fixture.guild_id(found))))
  pid
}

fn lookup_in_background(
  found: fixture.Guild,
) -> process.Subject(#(Result(_, shard.LookupError), Int)) {
  let replies = process.new_subject()
  process.spawn_unlinked(fn() {
    let started = clock.monotonic_ms()
    let result =
      manager.start_or_lookup(node.self(), fixture.guild_id(found), 10_000)
    process.send(replies, #(result, clock.monotonic_ms() - started))
  })
  replies
}

fn join_in_background(
  found: fixture.Guild,
  session: fake.FakeSession,
) -> process.Subject(guild.JoinReply) {
  let reply = process.new_subject()
  let assert Ok(Nil) =
    manager.join(
      node.self(),
      fixture.guild_id(found),
      fake.request(session, guild.Focused, guild.AsMember),
      reply,
    )
  reply
}

fn is_unavailable(reply: guild.JoinReply) -> Bool {
  case reply {
    guild.JoinUnavailable(..) -> True
    _ -> False
  }
}

pub fn shard_killed_mid_load_answers_its_waiters_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  slow_members(api, found, 600)
  let replies = lookup_in_background(found)
  process.sleep(150)
  process.kill(shard_pid(found))
  let assert Ok(#(Error(shard.LookupUnavailable(_)), elapsed)) =
    process.receive(replies, 5000)
  assert elapsed < 2000
  fixture.stop(found)
}

pub fn shard_killed_mid_load_leaves_no_orphan_guild_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  slow_members(api, found, 400)
  let _ = lookup_in_background(found)
  process.sleep(150)
  let old = shard_pid(found)
  process.kill(old)
  assert wait.until(2000, fn() {
    result.map(
      process.named(names.shard(names.shard_of(fixture.guild_id(found)))),
      fn(pid) { pid != old },
    )
    == Ok(True)
  })
  process.sleep(600)
  let orphan = read_model.get(fixture.guild_id(found))
  let handle = fixture.started(api, found)
  assert registry.lookup(fixture.guild_id(found)) == Ok(handle)
  let orphan_gone = case orphan {
    Ok(view) if view.handle.pid != handle.pid ->
      wait.until(3000, fn() { !process.is_alive(view.handle.pid) })
    _ -> True
  }
  assert orphan_gone
  fixture.stop(found)
}

pub fn failed_load_answers_every_queued_join_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
  fixture.publish(api, found)
  guild_api.override(
    api,
    "guild_collection",
    Some("members"),
    guild_api.Delayed(200, guild_api.Status(400, "{\"code\":\"BOOM\"}")),
  )
  let sessions = list.map([1, 2, 3], fake.start)
  let replies = list.map(sessions, join_in_background(found, _))
  list.each(replies, fn(reply) {
    let assert Ok(answer) = process.receive(reply, 3000)
    assert is_unavailable(answer)
  })
  assert registry.lookup(fixture.guild_id(found)) == Error(Nil)
  slow_members(api, found, 0)
  list.each(sessions, fn(session) {
    let reply = join_in_background(found, session)
    let assert Ok(answer) = process.receive(reply, 5000)
    assert fake.is_joined(answer)
  })
  fixture.stop(found)
}

pub fn unknown_guild_answers_joins_as_gone_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  fixture.publish(api, found)
  guild_api.override(
    api,
    "guild_collection",
    Some("members"),
    guild_api.Status(404, "{\"code\":\"UNKNOWN_GUILD\"}"),
  )
  let sessions = list.map([1, 2], fake.start)
  list.each(sessions, fn(session) {
    let request = fake.request(session, guild.Focused, guild.AsMember)
    let reply = join_in_background(found, session)
    let assert Ok(answer) = process.receive(reply, 3000)
    assert answer == guild.NotMember(request.attempt)
  })
  assert registry.lookup(fixture.guild_id(found)) == Error(Nil)
  fixture.stop(found)
}

pub fn server_error_on_load_stays_unavailable_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  guild_api.override(
    api,
    "guild_collection",
    Some("roles"),
    guild_api.Status(500, "{}"),
  )
  let session = fake.start(1)
  let assert Ok(answer) =
    process.receive(join_in_background(found, session), 10_000)
  assert is_unavailable(answer)
  fixture.stop(found)
}

pub fn hung_load_times_out_and_the_next_join_starts_fresh_test() {
  isolation.set_durations(fn(d) {
    Durations(..d, guild: GuildDurations(..d.guild, fetch_timeout_ms: 400))
  })
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  guild_api.override(api, "guild_collection", Some("members"), guild_api.Hang)
  let session = fake.start(1)
  let started = clock.monotonic_ms()
  let assert Ok(answer) =
    process.receive(join_in_background(found, session), 5000)
  assert is_unavailable(answer)
  assert clock.monotonic_ms() - started < 3000
  slow_members(api, found, 0)
  let assert Ok(again) =
    process.receive(join_in_background(found, session), 5000)
  assert fake.is_joined(again)
  fixture.stop(found)
}

pub fn guild_killed_while_joins_wait_on_the_load_restarts_on_retry_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  fixture.publish(api, found)
  slow_members(api, found, 300)
  let owner = fake.start(1)
  let member = fake.start(2)
  let first = join_in_background(found, owner)
  let second = join_in_background(found, member)
  let assert Ok(answer) = process.receive(first, 5000)
  assert fake.is_joined(answer)
  let assert Ok(answer) = process.receive(second, 5000)
  assert fake.is_joined(answer)
  let assert Ok(handle) = registry.lookup(fixture.guild_id(found))
  process.kill(handle.pid)
  assert wait.until(2000, fn() {
    registry.lookup(fixture.guild_id(found)) == Error(Nil)
  })
  let assert Ok(again) =
    process.receive(join_in_background(found, member), 5000)
  assert fake.is_joined(again)
  let assert Ok(fresh) = registry.lookup(fixture.guild_id(found))
  assert fresh.pid != handle.pid
  fixture.stop(found)
}
