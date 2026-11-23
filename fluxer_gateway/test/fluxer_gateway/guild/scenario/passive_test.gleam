// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsMember, Background}
import fluxer_gateway/core/session
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import support/guild_api
import support/guild_crowd
import support/guild_fixture.{RoleDeny} as fixture
import support/guild_session as fake

pub const role = 50

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

pub fn big_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(10)
  |> fixture.with_channel(20, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
  ])
  |> fixture.with_member(2, [role])
  |> guild_crowd.with_members(100, 300)
}

pub fn message(
  channel: Int,
  message_id: Int,
  extra: List(#(String, Json)),
) -> Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
    ..extra
  ])
}

pub fn passive_join(found: fake.FakeSession, handle) -> guild.JoinReply {
  fake.join_with(found, handle, fake.request(found, Background, AsMember))
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

fn lazy(found: fake.FakeSession, handle, fields: List(#(String, Json))) -> Nil {
  fake.op(
    found,
    handle,
    session.LazyRequest(fixture.to_dynamic(json.object(fields))),
  )
}

pub fn passive_session_only_gets_mentions_until_active_test() {
  use api <- guild_api.with_api
  let found = big_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(passive_join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1, []))
  fixture.dispatch(
    handle,
    "MESSAGE_CREATE",
    message(10, 2, [#("mentions", json.preprocessed_array([fixture.user(2)]))]),
  )
  fixture.dispatch(
    handle,
    "MESSAGE_CREATE",
    message(10, 3, [
      #("mention_roles", json.array([int_text(role)], json.string)),
    ]),
  )
  fixture.dispatch(
    handle,
    "MESSAGE_CREATE",
    message(10, 4, [#("mention_everyone", json.bool(True))]),
  )
  assert message_ids(member) == ["2", "3", "4"]
  lazy(member, handle, [#("active", json.bool(True))])
  let assert option.Some(_) = fake.await_event(member, "GUILD_SYNC", 1000)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 5, []))
  assert message_ids(member) == ["5"]
  lazy(member, handle, [#("active", json.bool(False))])
  let _ = fake.drain(member, 50)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 6, []))
  assert message_ids(member) == []
  fixture.stop(found)
}

pub fn passive_session_still_gets_channel_and_own_member_events_test() {
  use api <- guild_api.with_api
  let found = big_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(passive_join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "CHANNEL_CREATE",
    fixture.channel(found.id, 40, 0, None, []),
  )
  fixture.dispatch(handle, "GUILD_MEMBER_UPDATE", fixture.member(100, []))
  fixture.dispatch(handle, "GUILD_MEMBER_UPDATE", fixture.member(2, [role]))
  let events = fake.events(member, 200)
  assert list.contains(events, "CHANNEL_CREATE")
  assert list.count(events, fn(name) { name == "GUILD_MEMBER_UPDATE" }) == 1
  fixture.stop(found)
}
