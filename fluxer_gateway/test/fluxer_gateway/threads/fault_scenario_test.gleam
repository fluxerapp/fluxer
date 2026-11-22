// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/handoff/transfer
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/threads/metrics
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake
import support/wait

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

fn hand_over(api: FakeApi, found: fixture.Guild, handle: Handle) -> Handle {
  let state = freeze(handle)
  let events = moved(handle)
  assert wait.until(2000, fn() { !process.is_alive(handle.pid) })
  let ack = process.new_subject()
  let assert Ok(Nil) =
    manager.send_local(
      fixture.guild_id(found),
      shard.StartTransferred(fixture.guild_id(found), state, 5000, ack),
    )
  let assert Ok(True) = process.receive(ack, 5000)
  let target = fixture.started(api, found)
  process.send(target.control, inbox.Replay(events))
  fixture.sync(target)
  target
}

fn joined_threads(handle: Handle) -> List(String) {
  let member = fake.start(2)
  let assert Some(created) = fake.guild_create(fake.join(member, handle))
  kit.thread_ids(created)
}

pub fn a_hand_off_during_a_load_refetches_on_the_target_and_drops_the_old_answer_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve(
    api,
    1200,
    kit.flip_data(
      [kit.thread(found.id, kit.open_thread, kit.open_channel, None, [])],
      [kit.member(kit.open_thread, 2)],
    ),
  )
  let handle = kit.started(api, found)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 1 })
  kit.serve(
    api,
    0,
    kit.flip_data(
      [
        kit.thread(found.id, kit.open_thread, kit.open_channel, None, []),
        kit.thread(found.id, 110, kit.open_channel, None, []),
      ],
      [kit.member(kit.open_thread, 2), kit.member(110, 2)],
    ),
  )
  let target = hand_over(api, found, handle)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 2 })
  process.sleep(1500)
  fixture.sync(target)
  assert joined_threads(target) == ["100", "110"]
  assert kit.flip_calls(api) == 2
  fixture.stop(found)
}

pub fn a_guild_killed_during_a_load_loads_once_after_its_restart_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let before = metrics.held()
  let found = kit.guild()
  kit.serve_standard(api, found, 800)
  let handle = kit.started(api, found)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 1 })
  process.kill(handle.pid)
  let again = kit.started(api, found)
  assert again.pid != handle.pid
  assert wait.until(2000, fn() { kit.flip_calls(api) == 2 })
  process.sleep(1500)
  fixture.sync(again)
  assert kit.flip_calls(api) == 2
  assert metrics.held() == before + 3
  assert joined_threads(again) == ["100"]
  fixture.stop(found)
  assert wait.until(2000, fn() { metrics.held() == before })
}

pub fn subscribers_of_a_killed_guild_get_one_sync_after_rejoining_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  let member = fake.start(3)
  assert fake.is_joined(fake.join(member, handle))
  kit.subscribe(member, handle, Some(True), Some([kit.secret_thread]))
  let first = kit.named(fake.dispatches(member, 500), "THREAD_LIST_SYNC")
  assert list.length(first) == 1
  process.kill(handle.pid)
  let again = kit.started(api, found)
  assert fake.is_joined(fake.join(member, again))
  kit.subscribe(member, again, Some(True), Some([kit.secret_thread]))
  let seen = fake.dispatches(member, 800)
  let syncs = kit.named(seen, "THREAD_LIST_SYNC")
  assert list.length(syncs) == 1
  let assert [sync] = syncs
  assert kit.thread_ids(sync) == ["100", "200", "300"]
  let lists = kit.named(seen, "THREAD_MEMBER_LIST_UPDATE")
  assert lists != []
  assert list.all(lists, fn(update) { kit.list_user_ids(update) == ["3"] })
  fixture.stop(found)
}
