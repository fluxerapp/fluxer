// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{AsMember, Focused, JoinRequest}
import fluxer_gateway/core/session
import fluxer_gateway/telemetry/metrics
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import support/guild_api
import support/guild_fixture.{RoleAllow, RoleDeny} as fixture
import support/guild_session as fake

const role = 50

const open_channel = 10

const secret_channel = 20

const forum = 30

const open_thread = 100

const secret_thread = 200

const forum_thread = 300

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn thread(guild: Int, thread_id: Int, parent: Int, last: Option(Int)) -> Json {
  thread_fields(guild, thread_id, parent, last, [])
}

fn thread_fields(
  guild: Int,
  thread_id: Int,
  parent: Int,
  last: Option(Int),
  extra: List(#(String, Json)),
) -> Json {
  json.object(
    list.append(extra, [
      #("id", json.string(int_text(thread_id))),
      #("guild_id", json.string(int_text(guild))),
      #("parent_id", json.string(int_text(parent))),
      #("type", json.int(11)),
      #("name", json.string("t" <> int_text(thread_id))),
      #("owner_id", json.string("1")),
      #("last_message_id", case last {
        Some(found) -> json.string(int_text(found))
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

fn thread_member(thread_id: Int, user: Int) -> Json {
  json.object([
    #("id", json.string(int_text(thread_id))),
    #("user_id", json.string(int_text(user))),
    #("join_timestamp", json.string("2026-01-01T00:00:00.000Z")),
    #("flags", json.int(0)),
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
  |> fixture.with_channel(forum, 15, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [role])
}

fn serve_threads(api: guild_api.FakeApi, found: fixture.Guild, delay: Int) {
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Delayed(
      delay,
      guild_api.Data(
        json.object([
          #(
            "threads",
            json.preprocessed_array([
              thread(found.id, open_thread, open_channel, None),
              thread(found.id, secret_thread, secret_channel, None),
              thread(found.id, forum_thread, forum, Some(500)),
            ]),
          ),
          #(
            "thread_members",
            json.preprocessed_array([
              thread_member(open_thread, 2),
              thread_member(open_thread, 3),
              thread_member(secret_thread, 3),
            ]),
          ),
        ]),
      ),
    ),
  )
}

fn non_viewer(found: fake.FakeSession) -> guild.JoinRequest {
  JoinRequest(
    ..fake.request(found, Focused, AsMember),
    flags: session.no_flags(),
  )
}

fn thread_ids(value: Dynamic) -> List(String) {
  fake.ids_at(value, ["threads"], "id") |> list.sort(string.compare)
}

pub fn loaded_threads_reach_guild_create_by_membership_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let member = fake.start(2)
  let holder = fake.start(3)
  let legacy = fake.start(3)
  let assert Some(created) = fake.guild_create(fake.join(member, handle))
  assert thread_ids(created) == ["100"]
  let assert Some(created) = fake.guild_create(fake.join(holder, handle))
  assert thread_ids(created) == ["100", "200"]
  let assert Some(created) =
    fake.guild_create(fake.join_with(legacy, handle, non_viewer(legacy)))
  assert decode.run(created, decode.at(["threads"], decode.dynamic))
    |> option.from_result
    == None
  fixture.stop(found)
}

pub fn thread_create_reaches_viewers_of_the_parent_only_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  let holder = fake.start(3)
  let legacy = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  assert fake.is_joined(fake.join_with(legacy, handle, non_viewer(legacy)))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  let _ = fake.drain(legacy, 0)
  fake.op(
    holder,
    handle,
    session.LazyRequest(
      fixture.to_dynamic(json.object([#("threads", json.bool(True))])),
    ),
  )
  fixture.dispatch(
    handle,
    "THREAD_CREATE",
    with_initial(found.id, 210, secret_channel, [2]),
  )
  fixture.dispatch(
    handle,
    "THREAD_CREATE",
    with_initial(found.id, 110, open_channel, [2]),
  )
  assert created_ids(holder) == ["210", "110"]
  assert created_ids(member) == ["110"]
  assert created_ids(legacy) == []
  fixture.stop(found)
}

pub fn thread_create_reaches_the_starter_author_added_as_a_member_test() {
  use api <- guild_api.with_api
  let found = threaded_guild() |> fixture.with_member(4, [role])
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let author = fake.start(2)
  let creator = fake.start(3)
  let bystander = fake.start(4)
  assert fake.is_joined(fake.join(author, handle))
  assert fake.is_joined(fake.join(creator, handle))
  assert fake.is_joined(fake.join(bystander, handle))
  let _ = fake.drain(author, 50)
  let _ = fake.drain(creator, 0)
  let _ = fake.drain(bystander, 0)
  fixture.dispatch(
    handle,
    "THREAD_CREATE",
    thread_fields(found.id, 120, open_channel, None, [
      #(
        "_fluxer_members",
        json.preprocessed_array([
          thread_member(120, 3),
          thread_member(120, 2),
        ]),
      ),
    ]),
  )
  assert created_member_users(author) == [Some("2")]
  assert created_member_users(creator) == [Some("3")]
  assert created_ids(bystander) == []
  fixture.stop(found)
}

fn created_member_users(found: fake.FakeSession) -> List(Option(String)) {
  fake.dispatches(found, 150)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "THREAD_CREATE" -> Ok(fake.text_at(pair.1, ["member", "user_id"]))
      _ -> Error(Nil)
    }
  })
}

fn with_initial(
  guild: Int,
  thread_id: Int,
  parent: Int,
  users: List(Int),
) -> Json {
  thread_fields(guild, thread_id, parent, None, [
    #(
      "_fluxer_members",
      json.array(users, fn(user) {
        json.object([#("user_id", json.string(int_text(user)))])
      }),
    ),
  ])
}

fn created_ids(found: fake.FakeSession) -> List(String) {
  fake.dispatches(found, 150)
  |> list.filter_map(fn(pair) {
    case pair.0 {
      "THREAD_CREATE" -> option.to_result(fake.text_at(pair.1, ["id"]), Nil)
      _ -> Error(Nil)
    }
  })
}

pub fn thread_messages_follow_parent_visibility_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_thread, 900))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(open_thread, 901))
  assert message_ids(holder) == ["900", "901"]
  assert message_ids(member) == ["901"]
  fixture.stop(found)
}

pub fn thread_messages_count_as_thread_deliveries_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  let labels = [#("event", "message_create"), #("variant", "thread")]
  let delivered = fn() {
    metrics.value(
      metrics.read_all(),
      "fluxer_gateway_thread_dispatch_total",
      labels,
    )
  }
  let before = delivered()
  fixture.dispatch(handle, "MESSAGE_CREATE", message(secret_thread, 902))
  fixture.dispatch(handle, "MESSAGE_CREATE", message(open_thread, 903))
  assert message_ids(holder) == ["902", "903"]
  assert message_ids(member) == ["903"]
  assert delivered() == before + 3
  fixture.stop(found)
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

pub fn sessions_joined_during_load_get_a_list_sync_test_() {
  use <- fixture.slow(20)
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 400)
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  let assert Some(created) = fake.guild_create(fake.join(member, handle))
  assert thread_ids(created) == []
  let assert Some(sync) = fake.await_event(member, "THREAD_LIST_SYNC", 7000)
  assert thread_ids(sync) == ["100"]
  fixture.stop(found)
}

fn unreads(
  found: fake.FakeSession,
  handle,
  guild: Int,
  channel: Int,
  threads: List(#(Int, Option(Int))),
) -> Option(Dynamic) {
  fake.op(
    found,
    handle,
    session.RequestForumUnreads(
      fixture.to_dynamic(
        json.object([
          #("guild_id", json.string(int_text(guild))),
          #("channel_id", json.string(int_text(channel))),
          #(
            "threads",
            json.array(threads, fn(pair) {
              json.object([
                #("thread_id", json.string(int_text(pair.0))),
                #("ack_message_id", case pair.1 {
                  Some(ack) -> json.string(int_text(ack))
                  None -> json.null()
                }),
              ])
            }),
          ),
        ]),
      ),
    ),
  )
  fake.await_event(found, "FORUM_UNREADS", 1000)
}

fn unread_entries(value: Dynamic) -> List(#(String, String)) {
  decode.run(
    value,
    decode.at(
      ["threads"],
      decode.list({
        use thread_id <- decode.field("thread_id", decode.string)
        use count <- decode.optional_field(
          "count",
          "",
          decode.map(decode.int, int_text),
        )
        use missing <- decode.optional_field("missing", False, decode.bool)
        decode.success(
          #(thread_id, case missing {
            True -> "missing"
            False -> count
          }),
        )
      }),
    ),
  )
  |> option.from_result
  |> option.unwrap([])
}

pub fn forum_unreads_answer_locally_and_from_the_api_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  guild_api.override(
    api,
    "forum_unreads",
    None,
    guild_api.Data(
      json.object([
        #(
          "threads",
          json.preprocessed_array([
            json.object([
              #("thread_id", json.string(int_text(forum_thread))),
              #("count", json.int(40)),
            ]),
            json.object([
              #("thread_id", json.string("999")),
              #("count", json.int(2)),
            ]),
          ]),
        ),
      ]),
    ),
  )
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let assert Some(local) =
    unreads(member, handle, found.id, forum, [
      #(forum_thread, Some(500)),
      #(open_thread, Some(1)),
    ])
  assert unread_entries(local) == [#("300", "0")]
  let assert Some(missing) =
    unreads(member, handle, found.id, forum, [#(forum_thread, None)])
  assert unread_entries(missing) == [#("300", "missing")]
  let assert Some(remote) =
    unreads(member, handle, found.id, forum, [#(forum_thread, Some(400))])
  assert unread_entries(remote) == [#("300", "25")]
  assert unreads(member, handle, found.id, open_channel, [
      #(open_thread, Some(1)),
    ])
    == None
  fixture.stop(found)
}

pub fn private_thread_follows_membership_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  fixture.dispatch(
    handle,
    "THREAD_CREATE",
    json.object([
      #("_fluxer_members", json.preprocessed_array([thread_member(400, 3)])),
      ..private_fields(found.id)
    ]),
  )
  fixture.dispatch(handle, "MESSAGE_CREATE", message(400, 950))
  let _ = fake.drain(member, 100)
  assert message_ids(holder) == ["950"]
  fixture.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    json.object([
      #("id", json.string("400")),
      #("guild_id", json.string(int_text(found.id))),
      #("member_count", json.int(2)),
      #("added_members", json.preprocessed_array([thread_member(400, 2)])),
    ]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_CREATE", 500)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(400, 951))
  assert message_ids(member) == ["951"]
  fixture.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    json.object([
      #("id", json.string("400")),
      #("guild_id", json.string(int_text(found.id))),
      #("member_count", json.int(1)),
      #("removed_member_ids", json.array(["2"], json.string)),
    ]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_MEMBERS_UPDATE", 500)
  fixture.dispatch(handle, "MESSAGE_CREATE", message(400, 952))
  assert message_ids(member) == []
  assert message_ids(holder) == ["951", "952"]
  fixture.stop(found)
}

fn private_fields(guild: Int) -> List(#(String, Json)) {
  [
    #("id", json.string("400")),
    #("guild_id", json.string(int_text(guild))),
    #("parent_id", json.string(int_text(open_channel))),
    #("type", json.int(12)),
    #("name", json.string("private")),
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

pub fn thread_member_update_skips_added_users_without_access_test() {
  use api <- guild_api.with_api
  let found = threaded_guild()
  serve_threads(api, found, 0)
  let handle = fixture.started(api, found)
  process.sleep(200)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  fixture.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    json.object([
      #("id", json.string(int_text(secret_thread))),
      #("guild_id", json.string(int_text(found.id))),
      #("member_count", json.int(2)),
      #(
        "added_members",
        json.preprocessed_array([thread_member(secret_thread, 2)]),
      ),
    ]),
  )
  let assert Some(_) = fake.await_event(holder, "THREAD_MEMBERS_UPDATE", 500)
  let seen = fake.events(member, 100)
  assert !list.contains(seen, "THREAD_MEMBERS_UPDATE")
  assert !list.contains(seen, "THREAD_CREATE")
  fixture.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    json.object([
      #("id", json.string(int_text(open_thread))),
      #("guild_id", json.string(int_text(found.id))),
      #("member_count", json.int(1)),
      #("removed_member_ids", json.array(["2"], json.string)),
    ]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_MEMBERS_UPDATE", 500)
  fixture.stop(found)
}
