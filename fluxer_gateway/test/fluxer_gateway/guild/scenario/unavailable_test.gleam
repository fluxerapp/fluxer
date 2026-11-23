// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsMember, Focused, JoinRequest}
import fluxer_gateway/core/session
import fluxer_gateway/session/control
import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn features(found: fixture.Guild, names: List(String)) -> Json {
  json.object([
    #("id", json.string(int_text(found.id))),
    #("features", json.array(names, json.string)),
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

fn staff(found: fake.FakeSession) -> guild.JoinRequest {
  JoinRequest(..fake.request(found, Focused, AsMember), staff: True)
}

fn unavailable_controls(found: fake.FakeSession) -> List(Bool) {
  fake.drain(found, 150)
  |> list.filter_map(fn(item) {
    case item {
      fake.Control(control.GuildUnavailable(_, hidden)) -> Ok(hidden)
      _ -> Error(Nil)
    }
  })
}

fn guild_downs(found: fake.FakeSession) -> Int {
  fake.drain(found, 150)
  |> list.count(fn(item) {
    case item {
      fake.Inbox(session.GuildDown(..)) -> True
      _ -> False
    }
  })
}

fn is_unavailable_reply(reply: guild.JoinReply) -> Bool {
  case fake.guild_create(reply) {
    Some(created) ->
      decode.run(created, decode.at(["unavailable"], decode.bool)) == Ok(True)
    _ -> False
  }
}

pub fn unavailable_for_everyone_disconnects_and_comes_back_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE"]),
  )
  assert unavailable_controls(member) == [False]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  assert !list.contains(fake.events(member, 100), "MESSAGE_CREATE")
  let again = fake.join(member, handle)
  assert is_unavailable_reply(again)
  fixture.dispatch(handle, "GUILD_UPDATE", features(found, []))
  assert guild_downs(member) == 1
  let back = fake.join(member, handle)
  assert fake.is_joined(back) && !is_unavailable_reply(back)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  fixture.stop(found)
}

pub fn staff_stay_when_only_staff_may_see_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
    |> fixture.with_member(3, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let employee = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join_with(employee, handle, staff(employee)))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(employee, 0)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE_BUT_STAFF", "UNAVAILABLE_HIDDEN"]),
  )
  assert unavailable_controls(member) == [True]
  assert unavailable_controls(employee) == []
  assert is_unavailable_reply(fake.join(member, handle))
  let staff_reply = fake.join_with(employee, handle, staff(employee))
  assert fake.is_joined(staff_reply) && !is_unavailable_reply(staff_reply)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    features(found, ["UNAVAILABLE_FOR_EVERYONE"]),
  )
  assert unavailable_controls(employee) == [False]
  fixture.stop(found)
}
