// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/guild/inbox
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn counts(handle: inbox.Handle, user: Int) -> guild.GuildCounts {
  let assert Ok(user_id) = id.user_id(user)
  let reply = process.new_subject()
  guild.send(handle.ref, guild.CountsQuery(user_id, reply))
  let assert Ok(found) = process.receive(reply, 2000)
  found
}

pub fn stranger_is_not_a_member_until_added_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let stranger = fake.start(2)
  assert fake.join(stranger, handle) == guild.NotMember(1)
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(2, []))
  let assert Some(added) = fake.await_event(owner, "GUILD_MEMBER_ADD", 1000)
  assert fake.text_at(added, ["user", "id"]) == Some("2")
  assert fake.is_joined(fake.join(stranger, handle))
  assert counts(handle, 1).member_count == 2
  fixture.stop(found)
}

pub fn removed_member_loses_the_guild_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([#("user", fixture.user(2))]),
  )
  let assert Some(_) = fake.await_event(owner, "GUILD_MEMBER_REMOVE", 1000)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 99, 1))
  let assert Some(_) = fake.await_event(owner, "MESSAGE_CREATE", 1000)
  assert !list.contains(fake.events(member, 100), "MESSAGE_CREATE")
  assert counts(handle, 1).member_count == 1
  assert fake.join(member, handle) == guild.NotMember(1)
  fixture.stop(found)
}

pub fn dead_session_goes_offline_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  assert counts(handle, 2).online_count == 2
  fake.stop(member)
  process.sleep(50)
  assert counts(handle, 1).online_count == 1
  fixture.stop(found)
}

pub fn leave_stops_delivery_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  fake.leave(member, handle)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 99, 1))
  let assert Some(_) = fake.await_event(owner, "MESSAGE_CREATE", 1000)
  assert !list.contains(fake.events(member, 100), "MESSAGE_CREATE")
  assert counts(handle, 1).online_count == 1
  fixture.stop(found)
}

pub fn leave_from_a_stale_pid_is_ignored_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  guild.send(handle.ref, guild.Leave(member.ref.id, process.self()))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 99, 1))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  fixture.stop(found)
}

pub fn message(channel: Int, message_id: Int, author: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hello")),
    #("author", fixture.user(author)),
  ])
}

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

pub fn leave_cancels_a_queued_join_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let reply = process.new_subject()
  guild.send(
    handle.ref,
    guild.Join(fake.request(member, guild.Focused, guild.AsMember), reply),
  )
  fake.leave(member, handle)
  let _ = process.receive(reply, 300)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 99, 1))
  assert !list.contains(fake.events(member, 200), "MESSAGE_CREATE")
  assert counts(handle, 1).online_count == 0
  fixture.stop(found)
}
