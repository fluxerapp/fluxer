// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/guild_api
import support/guild_fixture.{RoleDeny} as fixture
import support/guild_session as fake

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn crowd() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_text(10)
  |> fixture.with_channel(20, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
  ])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
  |> fixture.with_member(4, [])
}

fn request_members(
  found: fake.FakeSession,
  handle,
  fields: List(#(String, Json)),
) -> Option(Dynamic) {
  fake.op(
    found,
    handle,
    session.RequestGuildMembers(fixture.to_dynamic(json.object(fields))),
  )
  fake.await_event(found, "GUILD_MEMBERS_CHUNK", 500)
}

fn member_ids(chunk: Dynamic) -> List(String) {
  decode.run(
    chunk,
    decode.at(
      ["members"],
      decode.list(decode.at(["user", "id"], decode.string)),
    ),
  )
  |> option.from_result
  |> option.unwrap([])
  |> list.sort(string.compare)
}

pub fn members_by_id_carry_the_nonce_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let assert Some(chunk) =
    request_members(member, handle, [
      #("guild_id", json.string(int_text(found.id))),
      #("user_ids", json.array(["4", "3", "99"], json.string)),
      #("nonce", json.string("abc")),
    ])
  assert member_ids(chunk) == ["3", "4"]
  assert fake.text_at(chunk, ["nonce"]) == Some("abc")
  assert decode.run(chunk, decode.at(["chunk_count"], decode.int)) == Ok(1)
  fixture.stop(found)
}

pub fn members_by_query_match_name_prefixes_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let assert Some(chunk) =
    request_members(member, handle, [
      #("guild_ids", json.array([int_text(found.id)], json.string)),
      #("query", json.string("USER3")),
      #("limit", json.int(10)),
    ])
  assert member_ids(chunk) == ["3"]
  fixture.stop(found)
}

pub fn full_member_list_needs_moderation_rights_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(owner, 0)
  let full = [
    #("guild_id", json.string(int_text(found.id))),
    #("query", json.string("")),
    #("limit", json.int(0)),
  ]
  assert request_members(member, handle, full) == None
  let assert Some(chunk) = request_members(owner, handle, full)
  assert member_ids(chunk) == ["1", "2", "3", "4"]
  fixture.stop(found)
}

pub fn members_request_for_another_guild_is_ignored_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  assert request_members(member, handle, [
      #("guild_id", json.string("12345")),
      #("user_ids", json.array(["3"], json.string)),
    ])
    == None
  fixture.stop(found)
}

pub fn member_list_sync_then_follows_joins_and_removals_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(owner, 50)
  fake.op(
    owner,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #(
            "member_list_channels",
            json.object([
              #("10", json.preprocessed_array([json.array([0, 99], json.int)])),
            ]),
          ),
        ]),
      ),
    ),
  )
  let assert [#("GUILD_MEMBER_LIST_UPDATE", sync)] = fake.dispatches(owner, 400)
  assert ops(sync) == ["SYNC"]
  assert decode.run(sync, decode.at(["member_count"], decode.int)) == Ok(4)
  assert decode.run(sync, decode.at(["online_count"], decode.int)) == Ok(1)
  let member = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  let assert Some(online) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(online, decode.at(["online_count"], decode.int)) == Ok(2)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([#("user", fixture.user(3))]),
  )
  let assert Some(removed) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(removed, decode.at(["member_count"], decode.int)) == Ok(3)
  assert decode.run(removed, decode.at(["online_count"], decode.int)) == Ok(1)
  fixture.stop(found)
}

pub fn member_list_for_a_hidden_channel_is_refused_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fake.op(
    member,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #(
            "member_list_channels",
            json.object([
              #("20", json.preprocessed_array([json.array([0, 99], json.int)])),
            ]),
          ),
        ]),
      ),
    ),
  )
  assert fake.await_event(member, "GUILD_MEMBER_LIST_UPDATE", 300) == None
  fixture.stop(found)
}

pub fn offline_member_presence_does_not_resync_lists_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  let _ = fake.drain(owner, 50)
  subscribe_list(owner, handle, 10)
  let assert Some(_) = fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  let _ = fake.drain(owner, 300)
  let assert Ok(three) = id.user_id(3)
  guild.send(handle.ref, guild.MemberPresence(fake.online(three)))
  assert fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 400) == None
  let member = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  let assert Some(online) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(online, decode.at(["online_count"], decode.int)) == Ok(2)
  fixture.stop(found)
}

pub fn reconnecting_member_comes_back_online_test() {
  use api <- guild_api.with_api
  let found = crowd()
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(3)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  subscribe_list(owner, handle, 10)
  let assert Some(sync) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(sync, decode.at(["online_count"], decode.int)) == Ok(2)
  fake.stop(member)
  let assert Some(gone) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(gone, decode.at(["online_count"], decode.int)) == Ok(1)
  let again = fake.start(3)
  assert fake.is_joined(fake.join(again, handle))
  let assert Some(back) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(back, decode.at(["online_count"], decode.int)) == Ok(2)
  fixture.stop(found)
}

fn subscribe_list(found: fake.FakeSession, handle, channel: Int) -> Nil {
  fake.op(
    found,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #(
            "member_list_channels",
            json.object([
              #(
                int_text(channel),
                json.preprocessed_array([json.array([0, 99], json.int)]),
              ),
            ]),
          ),
        ]),
      ),
    ),
  )
}

fn ops(update: Dynamic) -> List(String) {
  decode.run(
    update,
    decode.at(["ops"], decode.list(decode.at(["op"], decode.string))),
  )
  |> option.from_result
  |> option.unwrap([])
}

fn split_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(60, 0, 1)
  |> fixture.with_channel(10, 0, None, [RoleDeny(60, fixture.view_channel)])
  |> fixture.with_channel(20, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    fixture.RoleAllow(60, fixture.view_channel),
  ])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [60])
  |> fixture.with_member(4, [])
}

pub fn channel_member_counts_skip_hidden_channels_test() {
  use api <- guild_api.with_api
  let found = split_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let other = fake.start(4)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(other, handle))
  let _ = fake.drain(member, 50)
  fake.op(
    member,
    handle,
    session.RequestChannelMemberCounts(
      fixture.to_dynamic(
        json.object([
          #("guild_id", json.string(int_text(found.id))),
          #("channel_ids", json.array(["20", "10"], json.string)),
          #("nonce", json.string("n1")),
        ]),
      ),
    ),
  )
  let assert Some(reply) =
    fake.await_event(member, "CHANNEL_MEMBER_COUNTS_UPDATE", 1000)
  assert fake.text_at(reply, ["nonce"]) == Some("n1")
  let counts =
    decode.run(
      reply,
      decode.at(
        ["counts"],
        decode.list({
          use channel <- decode.field("channel_id", decode.string)
          use members <- decode.field("member_count", decode.int)
          use online <- decode.field("online_count", decode.int)
          decode.success(#(channel, members, online))
        }),
      ),
    )
  assert counts == Ok([#("10", 3, 2)])
  fixture.stop(found)
}

pub fn mutual_online_counts_only_shared_channels_test() {
  use api <- guild_api.with_api
  let found = split_guild()
  let handle = fixture.started(api, found)
  let sessions = list.map([1, 2, 3, 4], fake.start)
  list.each(sessions, fn(found) {
    assert fake.is_joined(fake.join(found, handle))
  })
  let counts_of = fn(user) {
    let assert Ok(user_id) = id.user_id(user)
    let reply = process.new_subject()
    guild.send(handle.ref, guild.CountsQuery(user_id, reply))
    let assert Ok(found) = process.receive(reply, 1000)
    found.online_count
  }
  assert counts_of(1) == 4
  assert counts_of(2) == 3
  assert counts_of(3) == 2
  fixture.stop(found)
}

fn group_ids(update: Dynamic) -> List(String) {
  decode.run(
    update,
    decode.at(["groups"], decode.list(decode.at(["id"], decode.string))),
  )
  |> option.from_result
  |> option.unwrap([])
}

pub fn hoisted_role_groups_follow_role_changes_test() {
  use api <- guild_api.with_api
  let base = crowd()
  let found =
    fixture.Guild(..base, roles: [
      json.object([
        #("id", json.string("70")),
        #("name", json.string("mods")),
        #("permissions", json.string("0")),
        #("position", json.int(1)),
        #("hoist", json.bool(True)),
      ]),
    ])
    |> fixture.with_member(5, [70])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let moderator = fake.start(5)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(moderator, handle))
  subscribe_list(owner, handle, 10)
  let assert Some(sync) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(sync) == ["70", "online", "offline"]
  let _ = fake.drain(owner, 300)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("user", fixture.user(5)),
      #("roles", json.preprocessed_array([])),
    ]),
  )
  let assert Some(demoted) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(demoted) == ["online", "offline"]
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("user", fixture.user(5)),
      #("roles", json.array(["70"], json.string)),
    ]),
  )
  let assert Some(promoted) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(promoted) == ["70", "online", "offline"]
  let _ = fake.drain(owner, 300)
  fixture.dispatch(
    handle,
    "GUILD_ROLE_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #(
        "role",
        json.object([
          #("id", json.string("70")),
          #("name", json.string("mods")),
          #("permissions", json.string("0")),
          #("position", json.int(1)),
          #("hoist", json.bool(False)),
        ]),
      ),
    ]),
  )
  let assert Some(unhoisted) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(unhoisted) == ["online", "offline"]
  fixture.dispatch(
    handle,
    "GUILD_ROLE_UPDATE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #(
        "role",
        json.object([
          #("id", json.string("70")),
          #("name", json.string("mods")),
          #("permissions", json.string("0")),
          #("position", json.int(1)),
          #("hoist", json.bool(True)),
        ]),
      ),
    ]),
  )
  let assert Some(hoisted) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(hoisted) == ["70", "online", "offline"]
  let _ = fake.drain(owner, 300)
  fixture.dispatch(
    handle,
    "GUILD_ROLE_DELETE",
    json.object([
      #("guild_id", json.string(int_text(found.id))),
      #("role_id", json.string("70")),
    ]),
  )
  let assert Some(deleted) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert group_ids(deleted) == ["online", "offline"]
  fixture.stop(found)
}

pub fn channel_overwrite_change_resyncs_its_member_list_test() {
  use api <- guild_api.with_api
  let base = crowd()
  let found =
    fixture.Guild(..base, channels: [])
    |> fixture.with_channel(10, 0, None, [
      fixture.MemberDeny(4, fixture.view_channel),
    ])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  assert fake.is_joined(fake.join(owner, handle))
  subscribe_list(owner, handle, 10)
  let assert Some(sync) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(sync, decode.at(["member_count"], decode.int)) == Ok(3)
  let _ = fake.drain(owner, 300)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, 10, 0, None, []),
  )
  let assert Some(widened) =
    fake.await_event(owner, "GUILD_MEMBER_LIST_UPDATE", 1000)
  assert decode.run(widened, decode.at(["member_count"], decode.int)) == Ok(4)
  fixture.stop(found)
}
