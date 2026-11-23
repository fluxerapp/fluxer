// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, ThreadsDurations}
import fluxer_gateway/guild/handoff/transfer
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/threads/scenario/kit
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import support/guild_api.{type FakeApi}
import support/guild_fixture as fixture
import support/guild_session as fake
import support/isolation
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
  kit.dispatch(
    handle,
    "THREAD_CREATE",
    kit.thread(found.id, 130, kit.open_channel, None, [
      #("_fluxer_members", json.preprocessed_array([kit.member(130, 2)])),
    ]),
  )
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

pub fn a_fresh_handoff_keeps_rows_and_replays_buffered_events_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  assert kit.threads_loaded(handle)
  assert kit.flip_calls(api) == 1
  let target = hand_over(api, found, handle)
  process.sleep(100)
  assert joined_threads(target) == ["100", "130"]
  assert kit.flip_calls(api) == 1
  fixture.stop(found)
}

pub fn a_stale_handoff_refetches_test_() {
  use <- fixture.slow(30)
  use api <- guild_api.with_api
  kit.fast_timers(first: 50, second: 50, later: 50, flush: 20)
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      threads: ThreadsDurations(
        ..durations.threads,
        store_handoff_max_age_ms: 0,
      ),
    )
  })
  let found = kit.guild()
  kit.serve_standard(api, found, 0)
  let handle = kit.started(api, found)
  process.sleep(200)
  fixture.sync(handle)
  kit.serve(
    api,
    0,
    kit.flip_data(
      list.append(kit.standard_threads(found), [
        kit.thread(found.id, 130, kit.open_channel, None, []),
      ]),
      [kit.member(130, 2), ..kit.standard_members()],
    ),
  )
  let target = hand_over(api, found, handle)
  assert wait.until(2000, fn() { kit.flip_calls(api) == 2 })
  process.sleep(200)
  assert joined_threads(target) == ["100", "130"]
  fixture.stop(found)
}
