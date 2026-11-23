// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/guild/inbox
import fluxer_gateway/voice/guild_state
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture.{MemberDeny, RoleAllow, RoleDeny} as fixture
import support/guild_session as fake
import support/guild_voice as voice
import support/voice_ids
import support/wait

const role = 50

const text_channel = 10

const secret_channel = 20

const voice_channel = 40

const secret_thread = 200

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn guarded_voice_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(text_channel)
  |> fixture.with_channel(secret_channel, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
  |> fixture.with_channel(voice_channel, 2, None, [
    RoleDeny(found.id, fixture.connect),
    RoleAllow(role, fixture.connect),
  ])
  |> fixture.with_member(2, [role])
  |> fixture.with_member(3, [role])
}

fn member_update(guild: Int, user: Int, roles: List(Int)) -> Json {
  json.object([
    #("guild_id", json.string(int_text(guild))),
    #("user", fixture.user(user)),
    #("nick", json.null()),
    #("roles", json.array(roles, fn(found) { json.string(int_text(found)) })),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn thread(guild: Int, thread_id: Int, parent: Int) -> Json {
  json.object([
    #("id", json.string(int_text(thread_id))),
    #("guild_id", json.string(int_text(guild))),
    #("parent_id", json.string(int_text(parent))),
    #("type", json.int(11)),
    #("name", json.string("t" <> int_text(thread_id))),
    #("owner_id", json.string("1")),
    #("last_message_id", json.null()),
    #(
      "thread_metadata",
      json.object([
        #("archived", json.bool(False)),
        #("locked", json.bool(False)),
        #("auto_archive_duration", json.int(1440)),
      ]),
    ),
  ])
}

fn many(handle: inbox.Handle, events: List(#(String, Json))) -> Nil {
  process.send(
    handle.control,
    inbox.DispatchMany(
      list.map(events, fn(pair) { #(pair.0, fixture.to_dynamic(pair.1)) }),
    ),
  )
}

fn message_ids(found: fake.FakeSession) -> List(String) {
  fake.dispatches(found, 150)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "MESSAGE_CREATE" -> option.to_result(fake.text_at(pair.1, ["id"]), Nil)
      _ -> Error(Nil)
    }
  })
}

pub fn removed_role_disconnects_voice_but_keeps_other_holders_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  let second = fake.start(3)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  voice.connect(api, handle, first, voice_channel, "conn-2")
  voice.connect(api, handle, second, voice_channel, "conn-3")
  let _ = fake.drain(second, 50)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    member_update(found.id, 2, []),
  )
  assert wait.until(3000, fn() { voice.forced(api, 2) })
  fixture.sync(handle)
  assert !voice.forced(api, 3)
  fixture.stop(found)
}

pub fn deleted_role_disconnects_every_holder_from_voice_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  let second = fake.start(3)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  voice.connect(api, handle, first, voice_channel, "conn-2")
  voice.connect(api, handle, second, voice_channel, "conn-3")
  fixture.dispatch(
    handle,
    "GUILD_ROLE_DELETE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("role_id", json.string(int_text(role))),
    ]),
  )
  assert wait.until(3000, fn() { voice.forced(api, 2) && voice.forced(api, 3) })
  fixture.stop(found)
}

pub fn overwrite_denying_connect_moves_only_the_target_out_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  let second = fake.start(3)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  voice.connect(api, handle, first, voice_channel, "conn-2")
  voice.connect(api, handle, second, voice_channel, "conn-3")
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, voice_channel, 2, None, [
      RoleDeny(found.id, fixture.connect),
      RoleAllow(role, fixture.connect),
      MemberDeny(3, fixture.connect),
    ]),
  )
  assert wait.until(3000, fn() { voice.forced(api, 3) })
  fixture.sync(handle)
  assert !voice.forced(api, 2)
  fixture.stop(found)
}

pub fn revoke_and_message_in_one_batch_skip_the_revoked_member_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  let second = fake.start(3)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  let _ = fake.drain(first, 50)
  let _ = fake.drain(second, 0)
  many(handle, [
    #("MESSAGE_CREATE", message(secret_channel, 1)),
    #("GUILD_MEMBER_UPDATE", member_update(found.id, 2, [])),
    #("MESSAGE_CREATE", message(secret_channel, 2)),
    #("GUILD_MEMBER_UPDATE", member_update(found.id, 2, [role])),
    #("MESSAGE_CREATE", message(secret_channel, 3)),
  ])
  assert message_ids(first) == ["1", "3"]
  assert message_ids(second) == ["1", "2", "3"]
  fixture.stop(found)
}

pub fn thread_follows_a_role_revoked_inside_one_batch_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Data(
      json.object([
        #(
          "threads",
          json.preprocessed_array([
            thread(found.id, secret_thread, secret_channel),
          ]),
        ),
        #(
          "thread_members",
          json.preprocessed_array([
            thread_member(secret_thread, 2),
            thread_member(secret_thread, 3),
          ]),
        ),
      ]),
    ),
  )
  let handle = fixture.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let first = fake.start(2)
  let second = fake.start(3)
  assert fake.is_joined(fake.join(first, handle))
  assert fake.is_joined(fake.join(second, handle))
  let _ = fake.drain(first, 50)
  let _ = fake.drain(second, 0)
  many(handle, [
    #("MESSAGE_CREATE", message(secret_thread, 6)),
    #("GUILD_MEMBER_UPDATE", member_update(found.id, 2, [])),
    #("MESSAGE_CREATE", message(secret_thread, 7)),
  ])
  assert message_ids(first) == ["6"]
  assert message_ids(second) == ["6", "7"]
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    member_update(found.id, 2, [role]),
  )
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_thread, 8))
  assert message_ids(first) == ["8"]
  fixture.stop(found)
}

fn thread_member(thread_id: Int, user: Int) -> Json {
  json.object([
    #("id", json.string(int_text(thread_id))),
    #("user_id", json.string(int_text(user))),
    #("join_timestamp", json.string("2026-01-01T00:00:00.000Z")),
    #("flags", json.int(0)),
  ])
}

pub fn hidden_voice_channel_stays_viewable_until_the_disconnect_lands_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  assert fake.is_joined(fake.join(first, handle))
  voice.connect(api, handle, first, voice_channel, "conn-2")
  let _ = fake.drain(first, 50)
  many(handle, [
    #(
      "CHANNEL_UPDATE",
      fixture.channel(found.id, voice_channel, 2, None, [
        RoleDeny(found.id, fixture.connect),
        RoleAllow(role, fixture.connect),
        MemberDeny(2, fixture.view_channel),
      ]),
    ),
    #("MESSAGE_CREATE", message(voice_channel, 9)),
  ])
  assert wait.until(3000, fn() { voice.forced(api, 2) })
  assert message_ids(first) == ["9"]
  let assert Ok(user) = id.user_id(2)
  let assert Ok(channel) = id.channel_id(voice_channel)
  let assert Ok(_) =
    voice.rpc(
      handle,
      guild_state.DisconnectIfInChannel(
        user,
        channel,
        Some(voice_ids.conn("conn-2")),
      ),
    )
  assert !voice.in_voice(handle, 2)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(voice_channel, 10))
  let seen = fake.dispatches(first, 150)
  assert list.any(seen, fn(pair) {
    pair.0 == "CHANNEL_DELETE"
    && fake.text_at(pair.1, ["id"]) == Some(int_text(voice_channel))
  })
  assert !list.any(seen, fn(pair) { pair.0 == "MESSAGE_CREATE" })
  fixture.stop(found)
}

pub fn member_overwrite_hides_text_channel_messages_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  assert fake.is_joined(fake.join(first, handle))
  let _ = fake.drain(first, 50)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, text_channel, 0, None, [
      MemberDeny(2, fixture.view_channel),
    ]),
  )
  fixture.dispatch(handle, "MESSAGE_CREATE", message(text_channel, 9))
  assert message_ids(first) == []
  fixture.stop(found)
}

pub fn member_overwrite_hides_voice_channel_text_test() {
  use api <- guild_api.with_api
  let found = guarded_voice_guild()
  let handle = fixture.started(api, found)
  let first = fake.start(2)
  assert fake.is_joined(fake.join(first, handle))
  let _ = fake.drain(first, 50)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, voice_channel, 2, None, [
      RoleDeny(found.id, fixture.connect),
      RoleAllow(role, fixture.connect),
      MemberDeny(2, fixture.view_channel),
    ]),
  )
  fixture.dispatch(handle, "MESSAGE_CREATE", message(voice_channel, 9))
  assert message_ids(first) == []
  fixture.stop(found)
}
