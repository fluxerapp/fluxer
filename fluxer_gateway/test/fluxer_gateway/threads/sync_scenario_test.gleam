// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/threads/scenario/kit
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

pub fn subscribing_during_a_load_never_syncs_an_empty_list_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 800)
  let handle = kit.started(api, found)
  let holder = fake.start(3)
  assert fake.is_joined(fake.join(holder, handle))
  kit.subscribe(holder, handle, Some(True), None)
  let syncs = kit.named(fake.dispatches(holder, 1500), "THREAD_LIST_SYNC")
  assert list.map(syncs, kit.thread_ids) == [["100", "200", "300"]]
  fixture.stop(found)
}

pub fn a_sync_burst_is_rate_limited_and_reaches_everyone_once_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let sessions =
    list.map(list.repeat(3, 30), fn(user) {
      let found = fake.start(user)
      assert fake.is_joined(fake.join(found, handle))
      found
    })
  let #(leaving, staying) = list.split(sessions, 5)
  list.each(sessions, fn(found) {
    kit.subscribe(found, handle, Some(True), None)
  })
  list.each(leaving, fn(found) { fake.leave(found, handle) })
  process.sleep(1500)
  let counts =
    list.map(staying, fn(found) {
      kit.named(fake.dispatches(found, 20), "THREAD_LIST_SYNC")
      |> list.map(kit.thread_ids)
    })
  assert list.all(counts, fn(syncs) { syncs == [["100", "200", "300"]] })
  list.each(leaving, fn(found) {
    let seen = kit.named(fake.dispatches(found, 0), "THREAD_LIST_SYNC")
    assert list.length(seen) <= 1
  })
  fixture.stop(found)
}

pub fn a_burst_spreads_over_the_token_rate_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let sessions =
    list.map(list.repeat(2, 40), fn(user) {
      let found = fake.start(user)
      assert fake.is_joined(fake.join(found, handle))
      found
    })
  let start = now_ms()
  list.each(sessions, fn(found) {
    kit.subscribe(found, handle, Some(True), None)
  })
  let arrivals =
    list.map(sessions, fn(found) {
      let assert Some(_) = fake.await_event(found, "THREAD_LIST_SYNC", 5000)
      now_ms() - start
    })
  let last = list.fold(arrivals, 0, fn(acc, at) { int_max(acc, at) })
  assert last >= 800
  assert last < 3000
  fixture.stop(found)
}

pub fn resubscribing_while_a_sync_is_pending_sends_one_sync_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  let _ = fake.drain(member, 50)
  kit.subscribe(member, handle, Some(True), None)
  kit.subscribe(member, handle, Some(False), None)
  kit.subscribe(member, handle, Some(True), None)
  let syncs = kit.named(fake.dispatches(member, 500), "THREAD_LIST_SYNC")
  assert list.length(syncs) == 1
  fixture.stop(found)
}

pub fn a_session_that_leaves_mid_queue_does_not_stall_the_rest_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  let sessions =
    list.map(list.repeat(3, 25), fn(user) {
      let found = fake.start(user)
      assert fake.is_joined(fake.join(found, handle))
      found
    })
  list.each(sessions, fn(found) {
    kit.subscribe(found, handle, Some(True), None)
  })
  let assert [
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    _,
    a,
    b,
    ..rest
  ] = sessions
  fake.stop(a)
  fake.leave(a, handle)
  fake.leave(b, handle)
  list.each(rest, fn(found) {
    let assert Some(_) = fake.await_event(found, "THREAD_LIST_SYNC", 2000)
    Nil
  })
  fixture.stop(found)
}

fn int_max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: Unit) -> Int

type Unit {
  Millisecond
}

fn now_ms() -> Int {
  monotonic(Millisecond)
}
