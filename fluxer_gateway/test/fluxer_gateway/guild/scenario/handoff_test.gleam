// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/transfer
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

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

fn freeze(
  handle: inbox.Handle,
  owners: List(process.Pid),
) -> transfer.Transfer {
  let reply = process.new_subject()
  process.send(handle.control, inbox.Freeze(owners, reply))
  let assert Ok(Ok(moved)) = process.receive(reply, 2000)
  moved
}

fn moved(handle: inbox.Handle) -> List(#(String, Dynamic)) {
  let reply = process.new_subject()
  process.send(handle.control, inbox.Moved(reply))
  let assert Ok(events) = process.receive(reply, 5000)
  events
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

fn kill_and_await(pid: process.Pid) -> Nil {
  let monitor = process.monitor(pid)
  process.kill(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  Nil
}

fn idle_owner() -> process.Pid {
  process.spawn_unlinked(fn() { process.sleep_forever() })
}

pub fn frozen_guild_keeps_serving_and_hands_over_buffered_events_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let session = fake.start(2)
  assert fake.is_joined(fake.join(session, handle))
  let _ = fake.drain(session, 50)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  let moved_state = freeze(handle, [process.self()])
  assert list.length(moved_state.loaded.members) == 2
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(3, []))
  assert message_ids(session) == ["1", "2"]
  let events = moved(handle)
  assert list.map(events, fn(pair) { pair.0 })
    == ["MESSAGE_CREATE", "GUILD_MEMBER_ADD"]
  let down =
    fake.drain(session, 200)
    |> list.any(fn(item) {
      case item {
        fake.Inbox(session.GuildDown(..)) -> True
        _ -> False
      }
    })
  assert down
  process.sleep(50)
  assert !process.is_alive(handle.pid)
}

pub fn abandoned_freeze_thaws_and_restarts_buffering_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let owner = idle_owner()
  let _ = freeze(handle, [owner])
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 1))
  kill_and_await(owner)
  fixture.sync(handle)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 2))
  let _ = freeze(handle, [process.self()])
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 3))
  let events = moved(handle)
  assert list.length(events) == 1
}

pub fn unfreeze_releases_and_keeps_the_guild_running_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let handle = fixture.started(api, found)
  let session = fake.start(1)
  assert fake.is_joined(fake.join(session, handle))
  let _ = freeze(handle, [process.self()])
  process.send(handle.control, inbox.Unfreeze)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(10, 4))
  assert message_ids(session) == ["4"]
  assert process.is_alive(handle.pid)
  fixture.stop(found)
}

pub fn transferred_guild_replays_before_live_events_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let moved_state = freeze(handle, [process.self()])
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(3, []))
  let events = moved(handle)
  process.sleep(50)
  let ack = process.new_subject()
  let assert Ok(Nil) =
    manager.send_local(
      fixture.guild_id(found),
      shard.StartTransferred(fixture.guild_id(found), moved_state, 5000, ack),
    )
  let assert Ok(True) = process.receive(ack, 5000)
  let target = fixture.started(api, found)
  assert target.pid != handle.pid
  fixture.dispatch(target, "GUILD_MEMBER_ADD", fixture.member(4, []))
  let late = fake.start(4)
  let early = fake.start(3)
  process.send(target.control, inbox.Replay(events))
  assert fake.is_joined(fake.join(early, target))
  assert fake.is_joined(fake.join(late, target))
  fixture.stop(found)
}

pub fn transferred_guild_catches_up_sessions_that_rejoin_late_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let guild = fixture.guild_id(found)
  let handle = fixture.started(api, found)
  let session = fake.start(2)
  let bystander = fake.start(1)
  assert fake.is_joined(fake.join(session, handle))
  let _ = fake.drain(session, 50)
  let moved_state = freeze(handle, [process.self()])
  let events = moved(handle)
  let _ = fake.drain(session, 100)
  let ack = process.new_subject()
  let assert Ok(Nil) =
    manager.send_local(
      guild,
      shard.StartTransferred(guild, moved_state, 5000, ack),
    )
  let assert Ok(True) = process.receive(ack, 5000)
  let target = fixture.started(api, found)
  process.send(target.control, inbox.Replay(events))
  fixture.dispatch(target, "MESSAGE_CREATE", message(10, 7))
  fixture.dispatch(target, "MESSAGE_CREATE", message(10, 8))
  assert fake.is_joined(fake.join(session, target))
  assert message_ids(session) == ["7", "8"]
  assert fake.is_joined(fake.join(bystander, target))
  fixture.dispatch(target, "MESSAGE_CREATE", message(10, 9))
  assert message_ids(session) == ["9"]
  assert message_ids(bystander) == ["9"]
  fixture.stop(found)
}

pub fn a_lookup_waits_for_a_guild_arriving_by_transfer_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let guild = fixture.guild_id(found)
  let handle = fixture.started(api, found)
  let moved_state = freeze(handle, [process.self()])
  let _ = moved(handle)
  let ack = process.new_subject()
  let looked = process.new_subject()
  let assert Ok(Nil) =
    manager.send_local(
      guild,
      shard.StartTransferred(guild, moved_state, 5000, ack),
    )
  let assert Ok(Nil) = manager.send_local(guild, shard.Lookup(guild, looked))
  let assert Ok(Ok(arrived)) = process.receive(looked, 5000)
  let assert Ok(True) = process.receive(ack, 5000)
  assert arrived.pid != handle.pid
  assert fixture.started(api, found).pid == arrived.pid
  fixture.stop(found)
}

pub fn a_cold_start_finds_the_guild_on_any_peer_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10)
  let guild = fixture.guild_id(found)
  let handle = fixture.started(api, found)
  let absent = node.from_atom(atom.create("absent@127.0.0.1"))
  let assert Ok(seen) = shard.running_on(guild, [absent, node.self()], 2000)
  assert seen.pid == handle.pid
  assert shard.running_on(guild, [absent], 2000) == Error(Nil)
  fixture.stop(found)
}
