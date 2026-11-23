// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import support/guild_api.{type FakeApi}
import support/guild_fixture.{MemberDeny} as fixture
import support/guild_session as fake

fn request(
  found: fake.FakeSession,
  handle: Handle,
  guild: Int,
  channel: Int,
  threads: List(#(Int, Option(Int))),
) -> Nil {
  fake.op(
    found,
    handle,
    session.RequestForumUnreads(
      fixture.to_dynamic(
        json.object([
          #("guild_id", json.string(kit.text(guild))),
          #("channel_id", json.string(kit.text(channel))),
          #(
            "threads",
            json.array(threads, fn(pair) {
              json.object([
                #("thread_id", json.string(kit.text(pair.0))),
                #("ack_message_id", case pair.1 {
                  Some(ack) -> json.string(kit.text(ack))
                  None -> json.null()
                }),
              ])
            }),
          ),
        ]),
      ),
    ),
  )
}

fn entries(value: Dynamic) -> List(#(String, String)) {
  decode.run(
    value,
    decode.at(
      ["threads"],
      decode.list({
        use thread_id <- decode.field("thread_id", decode.string)
        use count <- decode.optional_field(
          "count",
          "",
          decode.map(decode.int, int.to_string),
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
  |> result.unwrap([])
}

fn answer_counts(api: FakeApi, delay: Int, counts: List(#(Int, Int))) -> Nil {
  guild_api.override(
    api,
    "forum_unreads",
    None,
    guild_api.Delayed(
      delay,
      guild_api.Data(
        json.object([
          #(
            "threads",
            json.array(counts, fn(pair) {
              json.object([
                #("thread_id", json.string(kit.text(pair.0))),
                #("count", json.int(pair.1)),
              ])
            }),
          ),
        ]),
      ),
    ),
  )
}

fn unread_calls(api: FakeApi) -> List(Dynamic) {
  kit.calls_of(api, "forum_unreads") |> list.map(fn(call) { call.body })
}

fn requested_ids(body: Dynamic) -> List(String) {
  fake.ids_at(body, ["threads"], "thread_id")
}

fn ready(api: FakeApi, found: fixture.Guild) -> Handle {
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  assert kit.threads_loaded(handle)
  handle
}

pub fn new_messages_turn_a_local_answer_into_an_api_call_test() {
  use api <- guild_api.with_api
  let found = kit.guild()
  answer_counts(api, 0, [#(kit.forum_thread, 1)])
  let handle = ready(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  request(member, handle, found.id, kit.forum, [#(kit.forum_thread, Some(500))])
  let assert Some(local) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(local) == [#("300", "0")]
  assert unread_calls(api) == []
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.forum_thread, 600))
  request(member, handle, found.id, kit.forum, [#(kit.forum_thread, Some(500))])
  let assert Some(remote) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(remote) == [#("300", "1")]
  assert list.map(unread_calls(api), requested_ids) == [["300"]]
  request(member, handle, found.id, kit.forum, [#(kit.forum_thread, Some(600))])
  let assert Some(caught_up) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(caught_up) == [#("300", "0")]
  assert list.length(unread_calls(api)) == 1
  fixture.stop(found)
}

pub fn a_hanging_api_still_gets_the_local_answer_out_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.forum_timeout(200)
  let found = kit.guild()
  guild_api.override(api, "forum_unreads", None, guild_api.Hang)
  let handle = ready(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  request(member, handle, found.id, kit.forum, [
    #(kit.forum_thread, Some(500)),
    #(999, Some(1)),
  ])
  let assert Some(answer) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(answer) == [#("300", "0")]
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.open_channel, 601))
  fixture.sync(handle)
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 1000)
  fixture.stop(found)
}

pub fn overlapping_requests_each_get_their_own_answer_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  answer_counts(api, 300, [#(901, 3), #(902, 40)])
  let handle = ready(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  request(member, handle, found.id, kit.forum, [#(901, Some(1))])
  request(member, handle, found.id, kit.forum, [#(902, Some(1))])
  request(member, handle, found.id, kit.forum, [#(kit.forum_thread, None)])
  let answers =
    kit.named(fake.dispatches(member, 800), "FORUM_UNREADS")
    |> list.map(entries)
  assert answers == [[#("300", "missing")], [#("901", "3")], [#("902", "25")]]
    || answers == [[#("300", "missing")], [#("902", "25")], [#("901", "3")]]
  fixture.stop(found)
}

pub fn requests_are_capped_deduplicated_and_kept_to_the_forum_test() {
  use api <- guild_api.with_api
  let found = kit.guild()
  answer_counts(api, 0, [])
  let handle = ready(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  let many =
    list.flatten([
      [#(kit.open_thread, Some(1)), #(1000, Some(1)), #(1000, Some(2))],
      list.map(list.repeat(Nil, 50) |> list.index_map(fn(_, at) { at }), fn(at) {
        #(2000 + at, Some(1))
      }),
    ])
  request(member, handle, found.id, kit.forum, many)
  let assert Some(_) = fake.await_event(member, "FORUM_UNREADS", 3000)
  let assert [body] = unread_calls(api)
  let ids = requested_ids(body)
  assert list.length(ids) == 39
  assert list.first(ids) == Ok("1000")
  assert !list.contains(ids, "100")
  fixture.stop(found)
}

pub fn only_sessions_that_can_see_the_forum_get_answers_test() {
  use api <- guild_api.with_api
  let found =
    kit.guild()
    |> fixture.with_channel(31, 15, None, [MemberDeny(2, fixture.view_channel)])
  answer_counts(api, 0, [#(950, 2)])
  let handle = ready(api, found)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  request(member, handle, found.id, 31, [#(950, Some(1))])
  request(holder, handle, found.id, 31, [#(950, Some(1))])
  request(member, handle, found.id, kit.open_channel, [#(950, Some(1))])
  let assert Some(answer) = fake.await_event(holder, "FORUM_UNREADS", 3000)
  assert entries(answer) == [#("950", "2")]
  assert kit.named(fake.dispatches(member, 300), "FORUM_UNREADS") == []
  let assert [body] = unread_calls(api)
  assert fake.text_at(body, ["user_id"]) == Some("3")
  fixture.stop(found)
}

pub fn requests_during_a_load_fall_back_to_the_api_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  answer_counts(api, 0, [#(kit.forum_thread, 4)])
  kit.serve_standard(api, found, 1500)
  let handle = kit.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  request(member, handle, found.id, kit.forum, [#(kit.forum_thread, Some(500))])
  let assert Some(answer) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(answer) == [#("300", "4")]
  fixture.stop(found)
}
