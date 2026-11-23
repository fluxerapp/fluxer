// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  Durations, GuildDurations, ThreadsDurations,
}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox.{type Handle}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import support/guild_api.{type FakeApi}
import support/guild_fixture.{RoleAllow, RoleDeny} as fixture
import support/guild_session as fake
import support/isolation
import support/wait

pub const role = 50

pub const open_channel = 10

pub const secret_channel = 20

pub const forum = 30

pub const open_thread = 100

pub const secret_thread = 200

pub const forum_thread = 300

pub fn text(value: Int) -> String {
  int.to_string(value)
}

pub fn fast_timers(
  first first: Int,
  second second: Int,
  later later: Int,
  flush flush: Int,
) -> Nil {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      threads: ThreadsDurations(
        ..durations.threads,
        runtime_first_retry_ms: first,
        runtime_second_retry_ms: second,
        runtime_later_retry_ms: later,
        subscriptions_list_flush_ms: flush,
        subscriptions_sync_jitter_ms: 0,
      ),
    )
  })
}

pub fn forum_timeout(ms: Int) -> Nil {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      guild: GuildDurations(..durations.guild, forum_unreads_api_timeout_ms: ms),
    )
  })
}

pub fn thread(
  guild: Int,
  thread_id: Int,
  parent: Int,
  last: Option(Int),
  extra: List(#(String, Json)),
) -> Json {
  json.object(
    list.append(extra, [
      #("id", json.string(text(thread_id))),
      #("guild_id", json.string(text(guild))),
      #("parent_id", json.string(text(parent))),
      #("type", json.int(11)),
      #("name", json.string("t" <> text(thread_id))),
      #("owner_id", json.string("1")),
      #("last_message_id", case last {
        Some(found) -> json.string(text(found))
        None -> json.null()
      }),
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

pub fn private_thread_fields(
  guild: Int,
  thread_id: Int,
  parent: Int,
) -> List(#(String, Json)) {
  [
    #("id", json.string(text(thread_id))),
    #("guild_id", json.string(text(guild))),
    #("parent_id", json.string(text(parent))),
    #("type", json.int(12)),
    #("name", json.string("p" <> text(thread_id))),
    #("owner_id", json.string("3")),
    #(
      "thread_metadata",
      json.object([
        #("archived", json.bool(False)),
        #("locked", json.bool(False)),
        #("invitable", json.bool(False)),
      ]),
    ),
  ]
}

pub fn member(thread_id: Int, user: Int) -> Json {
  json.object([
    #("id", json.string(text(thread_id))),
    #("user_id", json.string(text(user))),
    #("join_timestamp", json.string("2026-01-01T00:00:00.000Z")),
    #("flags", json.int(0)),
  ])
}

pub fn guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(open_channel)
  |> fixture.with_channel(secret_channel, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
  |> fixture.with_channel(forum, 15, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [role])
  |> fixture.with_member(4, [role])
}

pub fn standard_threads(found: fixture.Guild) -> List(Json) {
  [
    thread(found.id, open_thread, open_channel, None, []),
    thread(found.id, secret_thread, secret_channel, None, []),
    thread(found.id, forum_thread, forum, Some(500), []),
  ]
}

pub fn standard_members() -> List(Json) {
  [member(open_thread, 2), member(open_thread, 3), member(secret_thread, 3)]
}

pub fn flip_data(threads: List(Json), members: List(Json)) -> Json {
  json.object([
    #("threads", json.preprocessed_array(threads)),
    #("thread_members", json.preprocessed_array(members)),
  ])
}

pub fn serve(api: FakeApi, delay: Int, data: Json) -> Nil {
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Delayed(delay, guild_api.Data(data)),
  )
}

pub fn serve_standard(api: FakeApi, found: fixture.Guild, delay: Int) -> Nil {
  serve(api, delay, flip_data(standard_threads(found), standard_members()))
}

pub fn calls_of(api: FakeApi, kind: String) -> List(guild_api.Call) {
  guild_api.calls(api) |> list.filter(fn(call) { call.kind == kind })
}

pub fn flip_calls(api: FakeApi) -> Int {
  list.length(calls_of(api, "guild_thread_flip_data"))
}

pub fn subscribe(
  found: fake.FakeSession,
  handle: Handle,
  threads: Option(Bool),
  lists: Option(List(Int)),
) -> Nil {
  let fields =
    list.flatten([
      case threads {
        Some(flag) -> [#("threads", json.bool(flag))]
        None -> []
      },
      case lists {
        Some(ids) -> [
          #("thread_member_lists", json.array(ids, fn(id) { json.int(id) })),
        ]
        None -> []
      },
    ])
  fake.op(
    found,
    handle,
    session.LazyRequest(fixture.to_dynamic(json.object(fields))),
  )
}

pub fn dispatch(handle: Handle, event: String, payload: Json) -> Nil {
  fixture.dispatch(handle, event, payload)
}

pub fn members_update(
  guild: Int,
  thread_id: Int,
  added: List(Int),
  removed: List(Int),
) -> Json {
  json.object([
    #("id", json.string(text(thread_id))),
    #("guild_id", json.string(text(guild))),
    #("member_count", json.int(list.length(added))),
    #(
      "added_members",
      json.preprocessed_array(
        list.map(added, fn(user) { member(thread_id, user) }),
      ),
    ),
    #(
      "removed_member_ids",
      json.array(removed, fn(user) { json.string(text(user)) }),
    ),
  ])
}

pub fn message(channel: Int, message_id: Int) -> Json {
  json.object([
    #("id", json.string(text(message_id))),
    #("channel_id", json.string(text(channel))),
    #("content", json.string("hi")),
    #("author", fixture.user(1)),
  ])
}

pub fn member_update(
  guild: Int,
  user: Int,
  roles: List(Int),
  nick: String,
) -> Json {
  json.object([
    #("guild_id", json.string(text(guild))),
    #("user", fixture.user(user)),
    #("nick", json.string(nick)),
    #("roles", json.array(roles, fn(role) { json.string(text(role)) })),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

pub fn named(seen: List(#(String, Dynamic)), event: String) -> List(Dynamic) {
  list.filter_map(seen, fn(pair) {
    case pair.0 == event {
      True -> Ok(pair.1)
      False -> Error(Nil)
    }
  })
}

pub fn thread_ids(value: Dynamic) -> List(String) {
  fake.ids_at(value, ["threads"], "id") |> list.sort(string.compare)
}

pub fn list_user_ids(value: Dynamic) -> List(String) {
  fake.ids_at(value, ["members"], "user_id") |> list.sort(string.compare)
}

pub fn added_user_ids(value: Dynamic) -> List(String) {
  fake.ids_at(value, ["added_members"], "user_id") |> list.sort(string.compare)
}

pub fn int_at(value: Dynamic, path: List(String)) -> Option(Int) {
  decode.run(value, decode.at(path, decode.int)) |> option.from_result
}

pub fn thread_entry(value: Dynamic, thread_id: Int) -> Option(Dynamic) {
  decode.run(value, decode.at(["threads"], decode.list(decode.dynamic)))
  |> result.unwrap([])
  |> list.find(fn(entry) {
    fake.text_at(entry, ["id"]) == Some(text(thread_id))
  })
  |> option.from_result
}

pub fn started(api: FakeApi, found: fixture.Guild) -> Handle {
  let handle = fixture.started(api, found)
  fixture.sync(handle)
  handle
}

pub fn threads_loaded(handle: Handle) -> Bool {
  wait.until(5000, fn() { thread_bits(handle, 2, open_thread) > 0 })
}

fn thread_bits(handle: Handle, user: Int, thread: Int) -> Int {
  let reply = process.new_subject()
  let assert Ok(user) = id.user_id(user)
  let assert Ok(thread) = id.channel_id(thread)
  process.send(
    handle.control,
    inbox.AskThreadBits(user, thread, clock.monotonic_ms() + 5000, reply),
  )
  case process.receive(reply, 5000) {
    Ok(Ok(bits)) -> bits
    _ -> 0
  }
}
