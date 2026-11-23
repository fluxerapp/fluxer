// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsVisitor, Background, JoinRequest}
import fluxer_gateway/core/guild_create as created
import fluxer_gateway/core/session
import fluxer_gateway/session/control
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import support/guild_api
import support/guild_fixture.{RoleAllow, RoleDeny} as fixture
import support/guild_session as fake

const role = 50

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn visit_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_property(
    "features",
    json.array(["VISITS_ENABLED"], json.string),
  )
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(10)
  |> fixture.with_channel(20, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
}

fn visit(found: fake.FakeSession) -> guild.JoinRequest {
  JoinRequest(
    ..fake.request(found, Background, AsVisitor(False, False, "verified", None)),
    attempt: 7,
  )
}

fn focus(found: fake.FakeSession, handle, channel: Int) -> Nil {
  fake.op(
    found,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(
        json.object([
          #("focused_channel_id", json.string(int_text(channel))),
        ]),
      ),
    ),
  )
}

fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
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

fn controls(found: fake.FakeSession) -> List(control.Control) {
  fake.drain(found, 150)
  |> list.filter_map(fn(item) {
    case item {
      fake.Control(found) -> Ok(found)
      _ -> Error(Nil)
    }
  })
}

pub fn visitor_sees_public_channels_and_focused_messages_test() {
  use api <- guild_api.with_api
  let found = visit_guild()
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  let reply = fake.join_with(visitor, handle, visit(visitor))
  assert fake.is_joined(reply)
  assert fake.channel_ids(reply) == ["10"]
  let _ = fake.drain(visitor, 50)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  assert message_ids(visitor) == []
  focus(visitor, handle, 10)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(20, 3))
  assert message_ids(visitor) == ["2"]
  focus(visitor, handle, 20)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(20, 4))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 5))
  assert message_ids(visitor) == []
  fixture.stop(found)
}

pub fn visits_are_refused_without_the_feature_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  assert fake.join_with(visitor, handle, visit(visitor))
    == guild.VisitRejected(7)
  fixture.stop(found)
}

pub fn visitor_is_promoted_when_added_as_member_test() {
  use api <- guild_api.with_api
  let found = visit_guild()
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  assert fake.is_joined(fake.join_with(visitor, handle, visit(visitor)))
  let _ = fake.drain(visitor, 50)
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(9, [role]))
  let promoted =
    controls(visitor)
    |> list.filter_map(fn(item) {
      case item {
        control.VisitPromoted(guild_create:, ..) ->
          Ok(created.to_raw(guild_create))
        _ -> Error(Nil)
      }
    })
  let assert [created] = promoted
  let channels =
    fake.ids_at(fake.parse(created), ["channels"], "id")
    |> list.sort(string.compare)
  assert channels == ["10", "20"]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(20, 6))
  assert message_ids(visitor) == ["6"]
  fixture.stop(found)
}

pub fn disabling_visits_evicts_visitors_test() {
  use api <- guild_api.with_api
  let found = visit_guild()
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  assert fake.is_joined(fake.join_with(visitor, handle, visit(visitor)))
  focus(visitor, handle, 10)
  let _ = fake.drain(visitor, 50)
  fixture.dispatch(
    handle,
    "GUILD_UPDATE",
    json.object([
      #("id", json.string(int_text(found.id))),
      #("features", json.preprocessed_array([])),
    ]),
  )
  let rejected =
    controls(visitor)
    |> list.any(fn(item) {
      case item {
        control.VisitRejected(_) -> True
        _ -> False
      }
    })
  assert rejected
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 8))
  assert message_ids(visitor) == []
  fixture.stop(found)
}

pub fn dead_visitors_free_the_per_user_cap_test() {
  use api <- guild_api.with_api
  let found = visit_guild()
  let handle = fixture.started(api, found)
  let sessions = list.repeat(9, 5) |> list.map(fake.start)
  list.each(sessions, fn(visitor) {
    assert fake.is_joined(fake.join_with(visitor, handle, visit(visitor)))
  })
  let extra = fake.start(9)
  assert fake.join_with(extra, handle, visit(extra)) == guild.VisitRejected(7)
  let assert [first, ..] = sessions
  fake.stop(first)
  process.sleep(50)
  assert fake.is_joined(fake.join_with(extra, handle, visit(extra)))
  fixture.stop(found)
}

fn visitor_channel_events(found: fake.FakeSession) -> List(#(String, String)) {
  fake.dispatches(found, 150)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "CHANNEL_CREATE" | "CHANNEL_DELETE" | "CHANNEL_UPDATE" ->
        option.to_result(fake.text_at(pair.1, ["id"]), Nil)
        |> result.map(fn(channel) { #(pair.0, channel) })
      _ -> Error(Nil)
    }
  })
}

pub fn visitor_view_follows_channel_overwrites_test() {
  use api <- guild_api.with_api
  let found = visit_guild()
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  assert fake.is_joined(fake.join_with(visitor, handle, visit(visitor)))
  focus(visitor, handle, 10)
  let _ = fake.drain(visitor, 50)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, 10, 0, None, [
      RoleDeny(found.id, fixture.view_channel),
    ]),
  )
  assert visitor_channel_events(visitor) == [#("CHANNEL_DELETE", "10")]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 30))
  assert message_ids(visitor) == []
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE",
    fixture.channel(found.id, 10, 0, None, []),
  )
  let events = visitor_channel_events(visitor)
  assert list.contains(events, #("CHANNEL_CREATE", "10"))
    || list.contains(events, #("CHANNEL_UPDATE", "10"))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 31))
  assert message_ids(visitor) == []
  fixture.stop(found)
}
