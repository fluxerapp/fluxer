// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/core/guild.{AsVisitor, Background, JoinRequest}
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(json.to_string(json.int(message_id)))),
    #("channel_id", json.string(json.to_string(json.int(channel)))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

fn twin(of: fake.FakeSession) -> fake.FakeSession {
  let found = fake.start(1)
  fake.FakeSession(
    ..found,
    ref: session.SessionRef(..found.ref, id: of.ref.id, user: of.ref.user),
  )
}

fn rebound(found: fake.FakeSession) -> List(Bool) {
  fake.drain(found, 150)
  |> list.filter_map(fn(item) {
    case item {
      fake.Inbox(session.GuildRebound(_, kept)) -> Ok(kept)
      _ -> Error(Nil)
    }
  })
}

fn message_events(found: fake.FakeSession) -> Int {
  fake.events(found, 150)
  |> list.count(fn(event) { event == "MESSAGE_CREATE" })
}

pub fn a_moved_member_is_rebound_in_place_without_a_new_join_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let source = fake.start(1)
  assert fake.is_joined(fake.join(source, handle))
  let _ = fake.drain(source, 100)
  let target = twin(source)
  guild.send(handle.ref, guild.SessionMoved(target.ref, source.ref))
  assert rebound(source) == [True]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  assert message_events(target) == 1
  assert message_events(source) == 0
  fake.stop(source)
  fixture.sync(handle)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  assert message_events(target) == 1
  fake.stop(target)
  fixture.stop(found)
}

pub fn a_move_from_an_unknown_owner_is_refused_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let member = fake.start(1)
  assert fake.is_joined(fake.join(member, handle))
  let stranger = twin(member)
  let target = twin(member)
  guild.send(handle.ref, guild.SessionMoved(target.ref, stranger.ref))
  assert rebound(stranger) == [False]
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  assert message_events(member) == 1
  assert message_events(target) == 0
  fake.stop(member)
  fake.stop(stranger)
  fake.stop(target)
  fixture.stop(found)
}

pub fn a_moved_visitor_is_rebound_in_place_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_property(
      "features",
      json.array(["VISITS_ENABLED"], json.string),
    )
    |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let visitor = fake.start(9)
  let request =
    JoinRequest(
      ..fake.request(
        visitor,
        Background,
        AsVisitor(False, False, "verified", None),
      ),
      attempt: 3,
    )
  assert fake.is_joined(fake.join_with(visitor, handle, request))
  let _ = fake.drain(visitor, 100)
  let target = twin(visitor)
  guild.send(handle.ref, guild.SessionMoved(target.ref, visitor.ref))
  assert rebound(visitor) == [True]
  fake.stop(visitor)
  fixture.sync(handle)
  let again = twin(visitor)
  guild.send(handle.ref, guild.SessionMoved(again.ref, target.ref))
  assert rebound(target) == [True]
  fake.stop(target)
  fake.stop(again)
  fixture.stop(found)
}

pub fn a_guild_frozen_before_its_move_ack_still_tells_the_source_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let guild = id.guild_id_to_int(fixture.guild_id(found))
  let source = fake.start(1)
  assert fake.is_joined(fake.join(source, handle))
  let _ = fake.drain(source, 100)
  let target = twin(source)
  let assert Ok(at) = epoch.from_int(5)
  let move =
    MoveId(
      source: WorkerId(node: atom.create("guild_ack_source"), incarnation: 1),
      serial: 1,
    )
  assert suspend_process(handle.pid)
  guild.send(handle.ref, guild.SessionMoved(target.ref, source.ref))
  let reply = process.new_subject()
  process.spawn(fn() {
    process.send(
      reply,
      guild_adapter.freeze(Key(key.guild, guild), move, 2, at),
    )
  })
  process.sleep(100)
  assert resume_process(handle.pid)
  let assert Ok(Ok(frozen)) = process.receive(reply, 5000)
  assert rebound(source) == [True]
  let assert Ok(Nil) = guild_adapter.thaw(frozen.stub, [])
  fake.stop(source)
  fake.stop(target)
  fixture.stop(found)
}
