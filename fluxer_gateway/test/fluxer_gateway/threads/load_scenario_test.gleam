// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/inbox
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/wait

pub fn failed_loads_back_off_and_then_sync_viewers_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 150, second: 600, later: 900, flush: 20)
  let found = kit.guild()
  guild_api.override(
    api,
    "guild_thread_flip_data",
    None,
    guild_api.Status(500, "{\"code\":\"BROKEN\"}"),
  )
  let handle = kit.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  assert wait.until(1000, fn() { kit.flip_calls(api) >= 1 })
  assert wait.until(2000, fn() { kit.flip_calls(api) >= 2 })
  process.sleep(300)
  assert kit.flip_calls(api) == 2
  assert wait.until(2000, fn() { kit.flip_calls(api) >= 3 })
  kit.serve_standard(api, found, 0)
  process.sleep(400)
  assert kit.flip_calls(api) == 3
  let assert Some(sync) = fake.await_event(member, "THREAD_LIST_SYNC", 3000)
  assert kit.thread_ids(sync) == ["100"]
  assert kit.flip_calls(api) == 4
  process.sleep(1200)
  assert kit.flip_calls(api) == 4
  fixture.stop(found)
}

pub fn load_queue_overflow_drops_the_stale_load_and_refetches_test_() {
  use <- fixture.slow(60)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve(
    api,
    3000,
    kit.flip_data(kit.standard_threads(found), kit.standard_members()),
  )
  let handle = kit.started(api, found)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 1 })
  let fresh =
    list.append(kit.standard_threads(found), [
      kit.thread(found.id, 110, kit.open_channel, None, []),
    ])
  kit.serve(
    api,
    1500,
    kit.flip_data(fresh, [kit.member(110, 2), ..kit.standard_members()]),
  )
  let noise =
    fixture.to_dynamic(
      json.object([
        #("id", json.string("100")),
        #("user_id", json.string("9")),
        #("flags", json.int(0)),
      ]),
    )
  list.each(list.repeat(Nil, 10_001), fn(_) {
    process.send(handle.control, inbox.Dispatch("THREAD_MEMBER_UPDATE", noise))
  })
  fixture.sync(handle)
  assert wait.until(3000, fn() { kit.flip_calls(api) == 2 })
  kit.dispatch(
    handle,
    "THREAD_CREATE",
    kit.thread(found.id, 120, kit.open_channel, None, [
      #("_fluxer_members", json.preprocessed_array([kit.member(120, 2)])),
    ]),
  )
  process.sleep(2500)
  fixture.sync(handle)
  let member = fake.start(2)
  let assert Some(created) = fake.guild_create(fake.join(member, handle))
  assert kit.thread_ids(created) == ["100", "110", "120"]
  assert kit.flip_calls(api) == 2
  fixture.stop(found)
}

pub fn replayed_messages_are_counted_once_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve(
    api,
    800,
    kit.flip_data(
      [
        kit.thread(found.id, kit.open_thread, kit.open_channel, Some(500), [
          #("message_count", json.int(3)),
          #("total_message_sent", json.int(3)),
        ]),
      ],
      [kit.member(kit.open_thread, 2)],
    ),
  )
  let handle = kit.started(api, found)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 1 })
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.open_thread, 400))
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.open_thread, 600))
  kit.dispatch(
    handle,
    "MESSAGE_DELETE",
    json.object([
      #("id", json.string("450")),
      #("channel_id", json.string("100")),
    ]),
  )
  process.sleep(1200)
  fixture.sync(handle)
  let member = fake.start(2)
  let assert Some(created) = fake.guild_create(fake.join(member, handle))
  let assert Some(entry) = kit.thread_entry(created, kit.open_thread)
  assert kit.int_at(entry, ["message_count"]) == Some(4)
  assert kit.int_at(entry, ["total_message_sent"]) == Some(4)
  assert fake.text_at(entry, ["last_message_id"]) == Some("600")
  fixture.stop(found)
}

pub fn thread_messages_during_a_load_respect_parent_access_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 1500)
  let handle = kit.started(api, found)
  let member = fake.start(2)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  assert fake.is_joined(fake.join(holder, handle))
  kit.subscribe(member, handle, Some(True), None)
  kit.subscribe(holder, handle, Some(True), None)
  let _ = fake.drain(member, 50)
  let _ = fake.drain(holder, 0)
  assert kit.flip_calls(api) == 1
  kit.dispatch(handle, "MESSAGE_CREATE", kit.message(kit.secret_thread, 970))
  kit.dispatch(
    handle,
    "MESSAGE_CREATE",
    json.object([
      #("id", json.string("971")),
      #("channel_id", json.string("200")),
      #("content", json.string("hi")),
      #("author", fixture.user(1)),
      #(
        "_fluxer_thread",
        kit.thread(found.id, kit.secret_thread, kit.secret_channel, None, []),
      ),
    ]),
  )
  let leaked = kit.named(fake.dispatches(member, 300), "MESSAGE_CREATE")
  assert leaked == []
  fixture.stop(found)
}

pub fn a_crashed_guild_stops_counting_its_threads_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let before = metrics.held()
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  assert wait.until(2000, fn() { metrics.held() == before + 3 })
  process.kill(handle.pid)
  assert wait.until(2000, fn() { metrics.held() == before })
}

pub fn a_crashed_fetch_worker_is_retried_test_() {
  use <- fixture.slow(60)
  use api <- guild_api.with_api
  kit.fast_timers(first: 100, second: 100, later: 100, flush: 20)
  let found = kit.guild()
  guild_api.override(api, "guild_thread_flip_data", None, guild_api.Hang)
  let handle = kit.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  assert wait.until(2000, fn() { fetch_workers() != [] })
  kit.serve_standard(api, found, 0)
  list.each(fetch_workers(), process.kill)
  let assert Some(sync) = fake.await_event(member, "THREAD_LIST_SYNC", 15_000)
  assert kit.thread_ids(sync) == ["100"]
  fixture.stop(found)
}

@external(erlang, "erlang", "processes")
fn processes() -> List(process.Pid)

@external(erlang, "erlang", "process_info")
fn process_info(pid: process.Pid, item: Item) -> dynamic.Dynamic

type Item {
  CurrentStacktrace
}

fn fetch_workers() -> List(process.Pid) {
  list.filter(processes(), fn(pid) {
    let stack = string.inspect(process_info(pid, CurrentStacktrace))
    string.contains(stack, "fluxer_gateway@threads@load")
    && !string.contains(stack, "load_scenario_test")
  })
}
