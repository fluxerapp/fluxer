// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake
import support/wait

fn ready(api: FakeApi, found: fixture.Guild) -> Handle {
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  handle
}

fn joined(handle: Handle, user: Int) -> fake.FakeSession {
  let found = fake.start(user)
  assert fake.is_joined(fake.join(found, handle))
  let _ = fake.drain(found, 30)
  found
}

fn lists(found: fake.FakeSession, wait_ms: Int) -> List(List(String)) {
  kit.named(fake.dispatches(found, wait_ms), "THREAD_MEMBER_LIST_UPDATE")
  |> list.map(kit.list_user_ids)
}

fn nick_of(update: Dynamic, user: Int) -> option.Option(String) {
  dynamic_list(update, ["members"])
  |> list.find_map(fn(entry) {
    case fake.text_at(entry, ["user_id"]) == Some(kit.text(user)) {
      True -> option.to_result(fake.text_at(entry, ["member", "nick"]), Nil)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

fn channel_ids(value: Dynamic) -> List(String) {
  decode.run(value, decode.at(["channel_ids"], decode.list(decode.string)))
  |> result.unwrap([])
}

fn dynamic_list(value: Dynamic, path: List(String)) -> List(Dynamic) {
  decode.run(value, decode.at(path, decode.list(decode.dynamic)))
  |> result.unwrap([])
}

pub fn member_lists_follow_membership_bursts_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let holder = joined(handle, 3)
  kit.subscribe(holder, handle, None, Some([kit.open_thread]))
  assert lists(holder, 100) == [["2", "3"]]
  list.each(list.repeat(Nil, 40), fn(_) {
    kit.dispatch(
      handle,
      "THREAD_MEMBERS_UPDATE",
      kit.members_update(found.id, kit.open_thread, [4], [2]),
    )
    kit.dispatch(
      handle,
      "THREAD_MEMBERS_UPDATE",
      kit.members_update(found.id, kit.open_thread, [2], [4]),
    )
  })
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, kit.open_thread, [4], [2]),
  )
  let updates = lists(holder, 300)
  assert updates != []
  assert list.length(updates) < 20
  assert list.last(updates) == Ok(["3", "4"])
  fixture.stop(found)
}

pub fn member_updates_refresh_the_lists_that_show_them_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let holder = joined(handle, 3)
  kit.subscribe(holder, handle, None, Some([kit.open_thread]))
  assert lists(holder, 100) == [["2", "3"]]
  kit.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    kit.member_update(found.id, 2, [], "renamed"),
  )
  let assert Some(update) =
    fake.await_event(holder, "THREAD_MEMBER_LIST_UPDATE", 500)
  assert nick_of(update, 2) == Some("renamed")
  kit.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    kit.member_update(found.id, 4, [kit.role], "elsewhere"),
  )
  assert lists(holder, 200) == []
  kit.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([
      #("guild_id", json.string(kit.text(found.id))),
      #("user", fixture.user(2)),
    ]),
  )
  assert lists(holder, 300) == [["3"]]
  fixture.stop(found)
}

pub fn a_revoked_role_stops_secret_thread_traffic_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let holder = joined(handle, 3)
  let other = joined(handle, 4)
  kit.subscribe(holder, handle, Some(True), Some([kit.secret_thread]))
  let _ = fake.drain(holder, 200)
  kit.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    kit.member_update(found.id, 3, [], "demoted"),
  )
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, kit.secret_thread, [4], []),
  )
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.secret_thread, 990))
  kit.dispatch(
    handle,
    "THREAD_UPDATE",
    kit.thread(found.id, kit.secret_thread, kit.secret_channel, None, [
      #("name", json.string("renamed")),
    ]),
  )
  let seen = fake.dispatches(holder, 400) |> list.map(fn(pair) { pair.0 })
  assert !list.contains(seen, "THREAD_MEMBER_LIST_UPDATE")
  assert !list.contains(seen, "THREAD_MEMBERS_UPDATE")
  assert !list.contains(seen, "MESSAGE_CREATE")
  assert !list.contains(seen, "THREAD_UPDATE")
  let other_seen = fake.dispatches(other, 100) |> list.map(fn(pair) { pair.0 })
  assert list.contains(other_seen, "THREAD_CREATE")
  fixture.stop(found)
}

pub fn a_removed_member_without_access_learns_nothing_new_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  kit.serve(
    api,
    0,
    kit.flip_data(kit.standard_threads(found), [
      kit.member(kit.secret_thread, 2),
      kit.member(kit.secret_thread, 3),
    ]),
  )
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let stale = joined(handle, 2)
  let holder = joined(handle, 3)
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, kit.secret_thread, [4], [2]),
  )
  let assert Some(update) =
    fake.await_event(holder, "THREAD_MEMBERS_UPDATE", 500)
  assert kit.added_user_ids(update) == ["4"]
  let leaked =
    kit.named(fake.dispatches(stale, 300), "THREAD_MEMBERS_UPDATE")
    |> list.flat_map(kit.added_user_ids)
  assert leaked == []
  fixture.stop(found)
}

pub fn a_removed_private_thread_member_is_still_told_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let member = joined(handle, 2)
  let holder = joined(handle, 3)
  kit.dispatch(
    handle,
    "THREAD_CREATE",
    json.object([
      #(
        "_fluxer_members",
        json.preprocessed_array([kit.member(400, 3), kit.member(400, 2)]),
      ),
      ..kit.private_thread_fields(found.id, 400, kit.open_channel)
    ]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_CREATE", 500)
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, 400, [], [2]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_MEMBERS_UPDATE", 500)
  let assert Some(_) = fake.await_event(holder, "THREAD_MEMBERS_UPDATE", 500)
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(400, 991))
  assert kit.named(fake.dispatches(member, 200), "MESSAGE_CREATE") == []
  fixture.stop(found)
}

pub fn private_thread_member_lists_drop_removed_members_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let member = joined(handle, 2)
  let holder = joined(handle, 3)
  kit.dispatch(
    handle,
    "THREAD_CREATE",
    json.object([
      #(
        "_fluxer_members",
        json.preprocessed_array([kit.member(400, 3), kit.member(400, 2)]),
      ),
      ..kit.private_thread_fields(found.id, 400, kit.open_channel)
    ]),
  )
  let assert Some(_) = fake.await_event(member, "THREAD_CREATE", 500)
  let assert Some(_) = fake.await_event(holder, "THREAD_CREATE", 500)
  kit.subscribe(member, handle, None, Some([400]))
  kit.subscribe(holder, handle, None, Some([400]))
  assert lists(member, 100) == [["2", "3"]]
  assert lists(holder, 0) == [["2", "3"]]
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, 400, [], [2]),
  )
  kit.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    kit.member_update(found.id, 3, [kit.role], "still here"),
  )
  assert lists(holder, 300) == [["3"]]
  assert lists(member, 0) == []
  fixture.stop(found)
}

pub fn gaining_a_role_syncs_the_threads_it_reveals_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  let found = kit.guild()
  let handle = ready(api, found)
  let member = joined(handle, 2)
  kit.subscribe(member, handle, Some(True), None)
  let _ = fake.drain(member, 200)
  kit.dispatch(
    handle,
    "GUILD_MEMBER_UPDATE",
    kit.member_update(found.id, 2, [kit.role], "promoted"),
  )
  let assert Some(sync) = fake.await_event(member, "THREAD_LIST_SYNC", 1000)
  assert kit.thread_ids(sync) == ["200"]
  assert channel_ids(sync) == ["20"]
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.secret_thread, 995))
  let assert Some(_) = fake.await_event(member, "MESSAGE_CREATE", 500)
  fixture.stop(found)
}

pub fn paged_thread_members_load_and_failures_retry_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 100, second: 100, later: 100, flush: 20)
  let found = kit.guild()
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Data(
      json.object([
        #("threads", json.preprocessed_array(kit.standard_threads(found))),
      ]),
    ),
  )
  guild_api.override(
    api,
    "list_thread_members",
    None,
    guild_api.Status(500, "{\"code\":\"BROKEN\"}"),
  )
  let handle = kit.started(api, found)
  let member = joined(handle, 2)
  assert wait.until(2000, fn() { kit.flip_calls(api) >= 2 })
  guild_api.override(
    api,
    "list_thread_members",
    None,
    guild_api.Data(
      json.object([
        #("members", json.preprocessed_array(kit.standard_members())),
        #("has_more", json.bool(False)),
      ]),
    ),
  )
  let assert Some(sync) = fake.await_event(member, "THREAD_LIST_SYNC", 2000)
  assert kit.thread_ids(sync) == ["100"]
  let assert [first, ..] = kit.calls_of(api, "list_thread_members")
  assert decode.run(
      first.body,
      decode.at(["thread_ids"], decode.list(decode.string)),
    )
    == Ok(["100", "200", "300"])
  fixture.stop(found)
}

pub fn member_lists_requested_during_a_load_arrive_after_it_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 800)
  let handle = kit.started(api, found)
  let member = joined(handle, 2)
  let holder = joined(handle, 3)
  kit.subscribe(
    holder,
    handle,
    None,
    Some([kit.open_thread, kit.secret_thread]),
  )
  kit.subscribe(member, handle, None, Some([kit.secret_thread, 999]))
  let updates =
    kit.named(fake.dispatches(holder, 1500), "THREAD_MEMBER_LIST_UPDATE")
    |> list.map(fn(update) {
      #(fake.text_at(update, ["thread_id"]), kit.list_user_ids(update))
    })
    |> list.sort(fn(a, b) { compare_text(a.0, b.0) })
  assert updates == [#(Some("100"), ["2", "3"]), #(Some("200"), ["3"])]
  assert lists(member, 0) == []
  kit.dispatch(
    handle,
    "THREAD_MEMBERS_UPDATE",
    kit.members_update(found.id, kit.secret_thread, [4], []),
  )
  assert lists(holder, 300) == [["3", "4"]]
  assert lists(member, 0) == []
  fixture.stop(found)
}

fn compare_text(a: option.Option(String), b: option.Option(String)) {
  string.compare(option.unwrap(a, ""), option.unwrap(b, ""))
}
