// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/guild_api
import support/guild_fixture.{RoleAllow, RoleDeny} as fixture
import support/guild_session as fake

const role = 50

const open_channel = 10

const secret_channel = 20

fn secret_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(open_channel)
  |> fixture.with_channel(secret_channel, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
  |> fixture.with_member(2, [])
}

fn channel_events(
  session: fake.FakeSession,
  wait_ms: Int,
) -> List(#(String, String)) {
  fake.dispatches(session, wait_ms)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "CHANNEL_CREATE" | "CHANNEL_DELETE" | "CHANNEL_UPDATE" ->
        case fake.text_at(pair.1, ["id"]) {
          Some(channel) -> Ok(#(pair.0, channel))
          None -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
}

fn member_update(guild: Int, user: Int, roles: List(Int)) -> json.Json {
  json.object([
    #("guild_id", json.string(int_text(guild))),
    #("user", fixture.user(user)),
    #("nick", json.null()),
    #("roles", json.array(roles, fn(found) { json.string(int_text(found)) })),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

pub fn join_hides_denied_channel_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert list.sort(fake.channel_ids(fake.join(owner, handle)), string.compare)
    == ["10", "20"]
  assert fake.channel_ids(fake.join(member, handle)) == ["10"]
  fixture.stop(found)
}

pub fn role_grant_and_revoke_creates_and_deletes_channel_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    member_update(found.id, 2, [role]),
  )
  assert channel_events(member, 150) == [#("CHANNEL_CREATE", "20")]
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    member_update(found.id, 2, []),
  )
  assert channel_events(member, 150) == [#("CHANNEL_DELETE", "20")]
  fixture.stop(found)
}

pub fn everyone_role_update_hides_and_restores_channels_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "GUILD_ROLE_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("role", fixture.role(found.id, fixture.send_messages, 0)),
    ]),
  )
  assert channel_events(member, 150) == [#("CHANNEL_DELETE", "10")]
  fixture.dispatch(
    handle,
    "GUILD_ROLE_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("role", fixture.role(found.id, fixture.everyone_bits(), 0)),
    ]),
  )
  assert channel_events(member, 150) == [#("CHANNEL_CREATE", "10")]
  fixture.stop(found)
}

pub fn custom_role_update_reaches_role_holders_only_test() {
  use api <- guild_api.with_api
  let found =
    secret_guild()
    |> fixture.with_member(3, [role])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert list.contains(fake.channel_ids(fake.join(holder, handle)), "20")
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, secret_channel, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
    ]),
  )
  assert channel_events(holder, 150) == [#("CHANNEL_DELETE", "20")]
  assert channel_events(member, 0) == []
  fixture.stop(found)
}

pub fn channel_overwrite_update_hides_from_member_only_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, open_channel, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
    ]),
  )
  assert channel_events(owner, 150) == [#("CHANNEL_UPDATE", "10")]
  assert channel_events(member, 0) == [#("CHANNEL_DELETE", "10")]
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, open_channel, 0, None, []),
  )
  assert channel_events(owner, 150) == [#("CHANNEL_UPDATE", "10")]
  assert channel_events(member, 0)
    == [#("CHANNEL_CREATE", "10"), #("CHANNEL_UPDATE", "10")]
  fixture.stop(found)
}

pub fn hidden_channel_create_and_messages_skip_member_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  fixture.dispatch(
    handle,
    "CHANNEL_CREATE",
    fixture.channel(found.id, 30, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
    ]),
  )
  fixture.dispatch(handle, "MESSAGE_CREATE", message(30))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_channel))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(open_channel))
  assert message_channels(owner)
    == [
      #("CHANNEL_CREATE", "30"),
      #("MESSAGE_CREATE", "30"),
      #("MESSAGE_CREATE", "20"),
      #("MESSAGE_CREATE", "10"),
    ]
  assert message_channels(member) == [#("MESSAGE_CREATE", "10")]
  fixture.stop(found)
}

pub fn channel_delete_reaches_former_viewers_test() {
  use api <- guild_api.with_api
  let found = secret_guild() |> fixture.with_member(3, [role])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  fixture.dispatch(
    handle,
    "CHANNEL_DELETE",
    fixture.channel(found.id, secret_channel, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
      RoleAllow(role, fixture.view_channel),
    ]),
  )
  assert channel_events(holder, 150) == [#("CHANNEL_DELETE", "20")]
  assert channel_events(member, 0) == []
  fixture.stop(found)
}

fn message_channels(session: fake.FakeSession) -> List(#(String, String)) {
  fake.dispatches(session, 150)
  |> list.filter_map(fn(pair) {
    let key = case pair.0 {
      "MESSAGE_CREATE" -> ["channel_id"]
      _ -> ["id"]
    }
    case pair.0, fake.text_at(pair.1, key) {
      "MESSAGE_CREATE", Some(channel) | "CHANNEL_CREATE", Some(channel) ->
        Ok(#(pair.0, channel))
      _, _ -> Error(Nil)
    }
  })
}

fn message(channel: Int) -> json.Json {
  json.object([
    #("id", json.string(int_text(channel * 1000))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

pub fn deleted_role_takes_its_channels_away_test() {
  use api <- guild_api.with_api
  let found = secret_guild() |> fixture.with_member(3, [role])
  let handle = fixture.started(api, found)
  let holder = fake.start(3)
  assert list.contains(fake.channel_ids(fake.join(holder, handle)), "20")
  let _ = fake.drain(holder, 50)
  fixture.dispatch(
    handle,
    "GUILD_ROLE_DELETE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("role_id", json.string(int_text(role))),
    ]),
  )
  assert channel_events(holder, 150) == [#("CHANNEL_DELETE", "20")]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_channel))
  assert message_channels(holder) == []
  let again = fake.start(3)
  assert fake.channel_ids(fake.join(again, handle)) == ["10"]
  fixture.stop(found)
}

pub fn ownership_transfer_moves_hidden_channels_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(owner, 50)
  let _ = fake.drain(member, 0)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    json.object([
      #("id", json.string(int_text(found.id))),
      #("owner_id", json.string("2")),
    ]),
  )
  assert channel_events(member, 150) == [#("CHANNEL_CREATE", "20")]
  assert channel_events(owner, 0) == [#("CHANNEL_DELETE", "20")]
  fixture.stop(found)
}

pub fn created_visible_channel_arrives_once_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "CHANNEL_CREATE",
    fixture.channel(found.id, 40, 0, None, []),
  )
  assert channel_events(member, 150) == [#("CHANNEL_CREATE", "40")]
  fixture.stop(found)
}

pub fn reaction_skips_the_reacting_session_test() {
  use api <- guild_api.with_api
  let found = secret_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  let second = fake.start(2)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  let _ = fake.drain(first, 50)
  let _ = fake.drain(second, 0)
  fixture.dispatch(
    handle,
    "MESSAGE_REACTION_ADD",
    json.object([
      #("channel_id", json.string("10")),
      #("message_id", json.string("500")),
      #("user_id", json.string("2")),
      #("session_id", json.string(session_text(first))),
      #("emoji", json.object([#("name", json.string("x"))])),
    ]),
  )
  let assert Some(reaction) =
    fake.await_event(second, "MESSAGE_REACTION_ADD", 500)
  assert fake.text_at(reaction, ["session_id"]) == None
  assert !list.contains(fake.events(first, 100), "MESSAGE_REACTION_ADD")
  fixture.stop(found)
}

fn session_text(found: fake.FakeSession) -> String {
  session.session_id_to_string(found.ref.id)
}
