// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, GuildDurations}
import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/guild/counts_cache
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
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

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn message(channel: Int, message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn online(handle: Handle, user: Int) -> Int {
  let assert Ok(user_id) = id.user_id(user)
  let reply = process.new_subject()
  guild.send(handle.ref, guild.CountsQuery(user_id, reply))
  let assert Ok(found) = process.receive(reply, 2000)
  found.online_count
}

fn join_through_shard(found: fixture.Guild, session: fake.FakeSession) -> Bool {
  let reply = process.new_subject()
  let assert Ok(Nil) =
    manager.join(
      node.self(),
      fixture.guild_id(found),
      fake.request(session, guild.Focused, guild.AsMember),
      reply,
    )
  let assert Ok(answer) = process.receive(reply, 5000)
  fake.is_joined(answer)
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

pub fn killed_guild_drops_its_cached_counts_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let session = fake.start(1)
  assert fake.is_joined(fake.join(session, handle))
  let guild_id = fixture.guild_id(found)
  assert wait.until(3000, fn() { result.is_ok(counts_cache.counts(guild_id)) })
  process.kill(handle.pid)
  assert wait.until(3000, fn() { counts_cache.counts(guild_id) == Error(Nil) })
  assert wait.until(3000, fn() { read_model.get(guild_id) == Error(Nil) })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  assert wait.until(3000, fn() { counts_cache.counts(guild_id) == Ok(#(1, 0)) })
  fixture.stop(found)
}

pub fn crashed_guild_serves_only_the_sessions_that_rejoin_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  assert online(handle, 1) == 2
  process.send_abnormal_exit(handle.pid, "injected")
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  let _ = fake.drain(owner, 20)
  let _ = fake.drain(member, 0)
  assert join_through_shard(found, member)
  let assert Ok(again) = registry.lookup(fixture.guild_id(found))
  assert again.pid != handle.pid
  assert online(again, 2) == 1
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 1))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  assert !list.contains(fake.events(owner, 100), "MESSAGE_CREATE")
  fake.leave(owner, again)
  fixture.sync(again)
  assert join_through_shard(found, owner)
  assert online(again, 1) == 2
  fixture.stop(found)
}

pub fn guild_started_without_sessions_stops_when_idle_test() {
  idle_grace(150, 30_000)
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  assert wait.until(3000, fn() {
    registry.lookup(fixture.guild_id(found)) == Error(Nil)
  })
  assert counts_cache.counts(fixture.guild_id(found)) == Error(Nil)
}

pub fn joined_guild_ignores_the_idle_start_timer_test() {
  idle_grace(200, 150)
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let session = fake.start(1)
  assert fake.is_joined(fake.join(session, handle))
  process.sleep(500)
  assert process.is_alive(handle.pid)
  fake.leave(session, handle)
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
}
