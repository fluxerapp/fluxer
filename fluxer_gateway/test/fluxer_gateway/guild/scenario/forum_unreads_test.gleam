// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake
import support/push_crash
import support/wait

const lookup_module = "fluxer_gateway@guild@requests@forum_unreads"

fn request(
  found: fake.FakeSession,
  handle: Handle,
  guild: Int,
  threads: List(#(Int, Int)),
) -> Nil {
  fake.op(
    found,
    handle,
    session.RequestForumUnreads(
      fixture.to_dynamic(
        json.object([
          #("guild_id", json.string(kit.text(guild))),
          #("channel_id", json.string(kit.text(kit.forum))),
          #(
            "threads",
            json.array(threads, fn(pair) {
              json.object([
                #("thread_id", json.string(kit.text(pair.0))),
                #("ack_message_id", json.string(kit.text(pair.1))),
              ])
            }),
          ),
        ]),
      ),
    ),
  )
}

fn entries(value: Dynamic) -> List(#(String, Int)) {
  decode.run(
    value,
    decode.at(
      ["threads"],
      decode.list({
        use thread_id <- decode.field("thread_id", decode.string)
        use count <- decode.optional_field("count", -1, decode.int)
        decode.success(#(thread_id, count))
      }),
    ),
  )
  |> result.unwrap([])
}

fn ready(api: FakeApi, found: fixture.Guild) -> Handle {
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  handle
}

fn lookups() -> List(process.Pid) {
  push_crash.running(lookup_module, "fetch")
}

fn joined_member(
  api: FakeApi,
  found: fixture.Guild,
) -> #(Handle, fake.FakeSession) {
  let handle = ready(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  #(handle, member)
}

pub fn a_crashed_remote_lookup_still_answers_with_the_local_entries_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.forum_timeout(20_000)
  let found = kit.guild()
  guild_api.override(api, "forum_unreads", None, guild_api.Hang)
  let #(handle, member) = joined_member(api, found)
  request(member, handle, found.id, [#(kit.forum_thread, 500), #(999, 1)])
  assert wait.until(3000, fn() { lookups() != [] })
  list.each(lookups(), process.kill)
  let assert Some(answer) = fake.await_event(member, "FORUM_UNREADS", 2000)
  assert entries(answer) == [#(kit.text(kit.forum_thread), 0)]
  fixture.stop(found)
}

pub fn every_request_with_a_crashed_lookup_gets_one_answer_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.forum_timeout(20_000)
  let found = kit.guild()
  guild_api.override(api, "forum_unreads", None, guild_api.Hang)
  let #(handle, member) = joined_member(api, found)
  int.range(0, 3, Nil, fn(_, _) {
    request(member, handle, found.id, [#(kit.forum_thread, 500), #(999, 1)])
  })
  assert wait.until(3000, fn() { list.length(lookups()) == 3 })
  list.each(lookups(), process.kill)
  let answers =
    fake.dispatches(member, 1500)
    |> kit.named("FORUM_UNREADS")
  assert list.length(answers) == 3
  assert list.all(answers, fn(answer) {
    entries(answer) == [#(kit.text(kit.forum_thread), 0)]
  })
  assert lookups() == []
  fixture.stop(found)
}

pub fn a_hanging_lookup_answers_at_its_deadline_and_leaves_no_worker_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.forum_timeout(300)
  let found = kit.guild()
  guild_api.override(api, "forum_unreads", None, guild_api.Hang)
  let #(handle, member) = joined_member(api, found)
  request(member, handle, found.id, [#(kit.forum_thread, 500), #(999, 1)])
  let assert Some(answer) = fake.await_event(member, "FORUM_UNREADS", 3000)
  assert entries(answer) == [#(kit.text(kit.forum_thread), 0)]
  assert wait.until(2000, fn() { lookups() == [] })
  fixture.stop(found)
}
