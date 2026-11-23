// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/transfer
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/guild_api
import support/guild_fixture.{RoleAllow, RoleDeny} as fixture
import support/guild_session as fake
import support/wait

const role = 50

const open_channel = 10

const secret_channel = 20

const open_thread = 100

const marker_ms = -1

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(int_text(message_id))),
    #("channel_id", json.string(int_text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

fn thread(
  guild: Int,
  thread_id: Int,
  parent: Int,
  name: String,
  extra: List(#(String, Json)),
) -> Json {
  json.object(
    list.append(extra, [
      #("id", json.string(int_text(thread_id))),
      #("guild_id", json.string(int_text(guild))),
      #("parent_id", json.string(int_text(parent))),
      #("type", json.int(11)),
      #("name", json.string(name)),
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
    ]),
  )
}

fn created_with(guild: Int, thread_id: Int, users: List(Int)) -> Json {
  thread(guild, thread_id, open_channel, "fresh", [
    #(
      "_fluxer_members",
      json.array(users, fn(user) { thread_member(thread_id, user, 0) }),
    ),
  ])
}

fn thread_member(thread_id: Int, user: Int, flags: Int) -> Json {
  json.object([
    #("id", json.string(int_text(thread_id))),
    #("user_id", json.string(int_text(user))),
    #("join_timestamp", json.string("2026-01-01T00:00:00.000Z")),
    #("flags", json.int(flags)),
  ])
}

fn threaded_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(open_channel)
  |> fixture.with_channel(secret_channel, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [role])
}

fn serve_threads(api: guild_api.FakeApi, found: fixture.Guild) -> Nil {
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Data(
      json.object([
        #(
          "threads",
          json.preprocessed_array([
            thread(found.id, open_thread, open_channel, "open", []),
          ]),
        ),
        #(
          "thread_members",
          json.preprocessed_array([
            thread_member(open_thread, 2, 0),
            thread_member(open_thread, 3, 0),
          ]),
        ),
      ]),
    ),
  )
}

fn thread_ids(value: Dynamic) -> List(String) {
  fake.ids_at(value, ["threads"], "id") |> list.sort(string.compare)
}

fn join_after_threads_load(found: fake.FakeSession, handle: Handle) -> Nil {
  let assert Some(created) = fake.guild_create(fake.join(found, handle))
  case thread_ids(created) {
    ["100"] -> Nil
    _ -> {
      let assert Some(sync) = fake.await_event(found, "THREAD_LIST_SYNC", 7000)
      assert thread_ids(sync) == ["100"]
      Nil
    }
  }
}

fn freeze(handle: Handle) -> transfer.Transfer {
  let reply = process.new_subject()
  process.send(handle.control, inbox.Freeze([process.self()], reply))
  let assert Ok(Ok(moved)) = process.receive(reply, 2000)
  moved
}

fn moved(handle: Handle) -> List(#(String, Dynamic)) {
  let reply = process.new_subject()
  process.send(handle.control, inbox.Moved(reply))
  let assert Ok(events) = process.receive(reply, 5000)
  events
}

fn transfer_to_target(
  api: guild_api.FakeApi,
  found: fixture.Guild,
  source: Handle,
) -> Handle {
  let guild = fixture.guild_id(found)
  let moved_state = freeze(source)
  let events = moved(source)
  assert wait.until(3000, fn() { !process.is_alive(source.pid) })
  let ack = process.new_subject()
  let assert Ok(Nil) =
    manager.send_local(
      guild,
      shard.StartTransferred(guild, moved_state, 5000, ack),
    )
  let assert Ok(True) = process.receive(ack, 5000)
  let target = fixture.started(api, found)
  assert target.pid != source.pid
  process.send(target.control, inbox.Replay(events))
  target
}

fn crash(
  api: guild_api.FakeApi,
  found: fixture.Guild,
  handle: Handle,
) -> Handle {
  process.send_abnormal_exit(handle.pid, "injected")
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  again
}

fn seen(found: fake.FakeSession, handle: Handle) -> List(#(String, Dynamic)) {
  fixture.sync(handle)
  relay.send([found.ref], session.GuildDown(handle.ref.guild, marker_ms))
  collect(found, [])
}

fn collect(
  found: fake.FakeSession,
  acc: List(fake.Seen),
) -> List(#(String, Dynamic)) {
  let assert Ok(item) = process.receive(found.seen, 5000)
  case item {
    fake.Inbox(session.GuildDown(_, retry_after_ms))
      if retry_after_ms == marker_ms
    -> fake.only_dispatches(list.reverse(acc))
    _ -> collect(found, [item, ..acc])
  }
}

fn tagged(
  dispatches: List(#(String, Dynamic)),
  events: List(String),
) -> List(String) {
  list.filter_map(dispatches, fn(pair) {
    case list.contains(events, pair.0) {
      False -> Error(Nil)
      True -> Ok(pair.0 <> " " <> option.unwrap(id_of(pair.0, pair.1), ""))
    }
  })
}

fn id_of(event: String, payload: Dynamic) -> Option(String) {
  case event {
    "THREAD_MEMBER_UPDATE" ->
      option.map(fake.text_at(payload, ["id"]), fn(thread_id) {
        thread_id
        <> "/"
        <> option.unwrap(fake.text_at(payload, ["user_id"]), "")
      })
    _ -> fake.text_at(payload, ["id"])
  }
}

fn message_ids(found: fake.FakeSession, handle: Handle) -> List(String) {
  seen(found, handle)
  |> tagged(["MESSAGE_CREATE"])
}

const thread_events = [
  "THREAD_CREATE", "THREAD_UPDATE", "THREAD_DELETE", "THREAD_MEMBER_UPDATE",
  "THREAD_MEMBERS_UPDATE", "MESSAGE_CREATE",
]

pub fn hand_off_catch_up_replays_thread_events_to_rejoining_sessions_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found)
  let source = fixture.started(api, found)
  let member = fake.start(2)
  join_after_threads_load(member, source)
  let _ = seen(member, source)
  let target = transfer_to_target(api, found, source)
  let twin = fake.start(2)
  let bystander = fake.start(3)
  join_after_threads_load(twin, target)
  join_after_threads_load(bystander, target)
  let _ = seen(twin, target)
  let _ = seen(bystander, target)
  fixture.dispatch(target, "THREAD_CREATE", created_with(found.id, 110, [2]))
  fixture.dispatch(target, "MESSAGE_CREATE", message(open_thread, 901))
  fixture.dispatch(
    target,
    "THREAD_UPDATE",
    thread(found.id, 110, open_channel, "renamed", []),
  )
  fixture.dispatch(
    target,
    "THREAD_MEMBER_UPDATE",
    thread_member(open_thread, 2, 1),
  )
  let expected = [
    "THREAD_CREATE 110",
    "MESSAGE_CREATE 901",
    "THREAD_UPDATE 110",
    "THREAD_MEMBER_UPDATE 100/2",
  ]
  assert tagged(seen(twin, target), thread_events) == expected
  assert tagged(seen(bystander, target), thread_events)
    == ["MESSAGE_CREATE 901"]
  assert fake.is_joined(fake.join(member, target))
  assert tagged(seen(member, target), thread_events) == expected
  fixture.dispatch(target, "MESSAGE_CREATE", message(open_thread, 902))
  assert message_ids(member, target) == ["MESSAGE_CREATE 902"]
  assert message_ids(twin, target) == ["MESSAGE_CREATE 902"]
  assert message_ids(bystander, target) == ["MESSAGE_CREATE 902"]
  fixture.stop(found)
}

fn rejoined_after_transfer(
  api: guild_api.FakeApi,
  found: fixture.Guild,
  events: List(#(String, Json)),
) -> #(List(String), List(String)) {
  let source = fixture.started(api, found)
  let member = fake.start(2)
  join_after_threads_load(member, source)
  let _ = seen(member, source)
  let target = transfer_to_target(api, found, source)
  let twin = fake.start(2)
  join_after_threads_load(twin, target)
  let _ = seen(twin, target)
  list.each(events, fn(pair) { fixture.dispatch(target, pair.0, pair.1) })
  let live = tagged(seen(twin, target), thread_events)
  assert fake.is_joined(fake.join(member, target))
  let caught_up = tagged(seen(member, target), thread_events)
  fixture.stop(found)
  #(live, caught_up)
}

fn users(key: String, ids: List(Int)) -> #(String, Json) {
  #(key, json.array(ids, fn(user) { json.string(int_text(user)) }))
}

fn members_update(fields: List(#(String, Json))) -> #(String, Json) {
  #(
    "THREAD_MEMBERS_UPDATE",
    json.object([#("id", json.string(int_text(open_thread))), ..fields]),
  )
}

pub fn hand_off_catch_up_replays_thread_member_changes_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found)
  let #(live, caught_up) =
    rejoined_after_transfer(api, found, [
      members_update([
        #("member_count", json.int(1)),
        users("removed_member_ids", [2]),
      ]),
      members_update([
        #("member_count", json.int(2)),
        #(
          "added_members",
          json.preprocessed_array([thread_member(open_thread, 2, 0)]),
        ),
      ]),
    ])
  let expected = [
    "THREAD_MEMBERS_UPDATE 100",
    "THREAD_CREATE 100",
    "THREAD_MEMBERS_UPDATE 100",
  ]
  assert live == expected
  assert caught_up == expected
}

pub fn hand_off_catch_up_ends_with_the_delete_of_a_removed_thread_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found)
  let #(live, caught_up) =
    rejoined_after_transfer(api, found, [
      #(
        "THREAD_UPDATE",
        thread(found.id, open_thread, open_channel, "renamed", []),
      ),
      #(
        "THREAD_DELETE",
        json.object([
          #("id", json.string(int_text(open_thread))),
          #("parent_id", json.string(int_text(open_channel))),
          #("type", json.int(11)),
          users("_fluxer_member_ids", [2, 3]),
        ]),
      ),
    ])
  assert live == ["THREAD_UPDATE 100", "THREAD_DELETE 100"]
  assert caught_up == ["THREAD_DELETE 100"]
}

pub fn crashed_guild_catches_up_every_session_that_was_attached_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let owner = fake.start(1)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(owner, handle))
  assert fake.is_joined(fake.join(member, handle))
  let _ = seen(owner, handle)
  let _ = seen(member, handle)
  let again = crash(api, found, handle)
  let stranger = fake.start(2)
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 1))
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 2))
  assert fake.is_joined(fake.join(stranger, again))
  assert message_ids(stranger, again) == []
  assert fake.is_joined(fake.join(member, again))
  assert message_ids(member, again) == ["MESSAGE_CREATE 1", "MESSAGE_CREATE 2"]
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 3))
  assert message_ids(member, again) == ["MESSAGE_CREATE 3"]
  assert message_ids(stranger, again) == ["MESSAGE_CREATE 3"]
  assert message_ids(owner, again) == []
  assert fake.is_joined(fake.join(owner, again))
  assert message_ids(owner, again)
    == ["MESSAGE_CREATE 1", "MESSAGE_CREATE 2", "MESSAGE_CREATE 3"]
  fixture.stop(found)
}

pub fn stopped_guild_restarts_without_catch_up_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = seen(member, handle)
  fixture.stop(found)
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
  let again = fixture.started(api, found)
  assert again.pid != handle.pid
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 1))
  assert fake.is_joined(fake.join(member, again))
  assert message_ids(member, again) == []
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 2))
  assert message_ids(member, again) == ["MESSAGE_CREATE 2"]
  fixture.stop(found)
}

pub fn transferred_guild_that_crashes_before_rejoins_still_catches_up_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(2, [])
  let source = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, source))
  let _ = seen(member, source)
  let target = transfer_to_target(api, found, source)
  fixture.sync(target)
  let again = crash(api, found, target)
  fixture.dispatch(again, "MESSAGE_CREATE", message(10, 5))
  assert fake.is_joined(fake.join(member, again))
  assert message_ids(member, again) == ["MESSAGE_CREATE 5"]
  fixture.stop(found)
}
