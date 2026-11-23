// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn counts(handle: inbox.Handle, user: Int) -> guild.GuildCounts {
  let assert Ok(user_id) = id.user_id(user)
  let reply = process.new_subject()
  guild.send(handle.ref, guild.CountsQuery(user_id, reply))
  let assert Ok(found) = process.receive(reply, 2000)
  found
}

fn message(channel: Int, message_id: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

pub fn paged_members_are_all_loaded_test() {
  use api <- guild_api.with_api
  guild_api.set_page_size(api, 2)
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
    |> fixture.with_member(4, [])
    |> fixture.with_member(5, [])
  let handle = fixture.started(api, found)
  assert counts(handle, 1).member_count == 5
  assert list.length(guild_api.collection_calls(api, "members")) == 3
  let late = fake.start(5)
  assert fake.is_joined(fake.join(late, handle))
  fixture.stop(found)
}

pub fn failed_fetch_is_retried_on_the_next_lookup_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  guild_api.put_data(api, found.id, "roles", json.object([]))
  let assert Error(shard.LookupUnavailable(_)) =
    manager.start_or_lookup(node.self(), fixture.guild_id(found), 5000)
  assert registry.lookup(fixture.guild_id(found)) == Error(Nil)
  let handle = fixture.started(api, found)
  assert registry.lookup(fixture.guild_id(found)) == Ok(handle)
  fixture.stop(found)
}

pub fn concurrent_lookups_share_one_fetch_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  guild_api.override(
    api,
    "guild_collection",
    Some("guild"),
    guild_api.Delayed(
      200,
      guild_api.Data(json.object([#("guild", fixture.guild_json(found))])),
    ),
  )
  let replies = process.new_subject()
  list.each([1, 2, 3, 4], fn(_) {
    process.spawn(fn() {
      process.send(
        replies,
        manager.start_or_lookup(node.self(), fixture.guild_id(found), 5000),
      )
    })
  })
  let results =
    list.map([1, 2, 3, 4], fn(_) {
      let assert Ok(result) = process.receive(replies, 5000)
      result
    })
  let assert [Ok(first), ..] = results
  assert list.all(results, fn(result) { result == Ok(first) })
  assert list.length(guild_api.collection_calls(api, "guild")) == 1
  fixture.stop(found)
}

pub fn crashed_guild_is_restarted_on_lookup_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let session = fake.start(1)
  assert fake.is_joined(fake.join(session, handle))
  process.kill(handle.pid)
  process.sleep(50)
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  assert process.is_alive(again.pid)
  assert registry.lookup(fixture.guild_id(found)) == Ok(again)
  fixture.stop(found)
}

pub fn join_through_the_shard_starts_the_guild_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  fixture.publish(api, found)
  let session = fake.start(1)
  let reply = process.new_subject()
  let assert Ok(Nil) =
    manager.join(
      node.self(),
      fixture.guild_id(found),
      fake.request(session, guild.Focused, guild.AsMember),
      reply,
    )
  let assert Ok(answer) = process.receive(reply, 5000)
  assert fake.is_joined(answer)
  fixture.stop(found)
}

pub fn resumed_session_survives_the_old_process_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let old = fake.start(1)
  assert fake.is_joined(fake.join(old, handle))
  let new = fake.start(1)
  let moved = fake.FakeSession(..new, ref: guild_ref_with_id(new, old))
  assert fake.is_joined(fake.join(moved, handle))
  fake.stop(old)
  process.sleep(50)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  let assert Some(_) = fake.await_event(moved, "MESSAGE_CREATE", 1000)
  assert counts(handle, 1).online_count == 1
  fake.leave(old, handle)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  let assert Some(_) = fake.await_event(moved, "MESSAGE_CREATE", 1000)
  fixture.stop(found)
}

fn guild_ref_with_id(
  new: fake.FakeSession,
  old: fake.FakeSession,
) -> session.SessionRef {
  session.SessionRef(..new.ref, id: old.ref.id)
}

pub fn reload_drops_removed_members_and_announces_the_update_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  let reloaded =
    fixture.Guild(..found, members: [#(1, fixture.member(1, []))])
    |> fixture.with_property("name", json.string("Renamed"))
  fixture.publish(api, reloaded)
  assert manager.reload_local([found.id], 5000) == 1
  let assert Some(update) = fake.await_event(owner, "GUILD_UPDATE", 1000)
  assert fake.text_at(update, ["name"]) == Some("Renamed")
  assert counts(handle, 1).member_count == 1
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 3))
  let assert Some(_) = fake.await_event(owner, "MESSAGE_CREATE", 1000)
  assert !list.contains(fake.events(member, 100), "MESSAGE_CREATE")
  assert counts(handle, 1).online_count == 1
  fixture.stop(found)
}
