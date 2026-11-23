// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session
import fluxer_gateway/guild/handoff/catch_up
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result

fn recorded(count: Int) -> #(session.SessionId, catch_up.CatchUp) {
  let target = session.new_session_id()
  let waiting =
    int.range(
      from: 1,
      to: count + 1,
      with: catch_up.new([target], 0, 60_000),
      run: fn(acc, index) {
        catch_up.record(acc, "MESSAGE_CREATE", dynamic.int(index), 1)
      },
    )
  #(target, waiting)
}

fn indexes(taken: catch_up.Taken) -> List(Int) {
  list.filter_map(taken.events, fn(pair) {
    decode.run(pair.1, decode.int) |> result.replace_error(Nil)
  })
}

pub fn catch_up_within_the_cap_replays_every_event_in_order_test() {
  let #(target, waiting) = recorded(5)
  let taken = catch_up.take(waiting, target, 2)
  assert taken.complete
  assert indexes(taken) == [1, 2, 3, 4, 5]
  assert !catch_up.is_waiting(taken.catch_up)
}

pub fn overflowing_catch_up_keeps_the_newest_events_test() {
  let #(target, waiting) = recorded(2500)
  let taken = catch_up.take(waiting, target, 2)
  assert !taken.complete
  let kept = indexes(taken)
  assert list.length(kept) == 1000
  assert list.first(kept) == Ok(1501)
  assert list.last(kept) == Ok(2500)
  assert kept
    == int.range(from: 2500, to: 1500, with: [], run: fn(acc, index) {
      [index, ..acc]
    })
}

pub fn catch_up_at_exactly_the_cap_is_complete_test() {
  let #(target, waiting) = recorded(1000)
  let taken = catch_up.take(waiting, target, 2)
  assert taken.complete
  assert list.length(taken.events) == 1000
}

pub fn expired_catch_up_replays_nothing_test() {
  let target = session.new_session_id()
  let waiting =
    catch_up.new([target], 0, 100)
    |> catch_up.record("MESSAGE_CREATE", dynamic.int(1), 50)
  let taken = catch_up.take(waiting, target, 101)
  assert taken.events == []
  assert !catch_up.is_waiting(taken.catch_up)
}

pub fn other_sessions_keep_waiting_after_one_takes_test() {
  let first = session.new_session_id()
  let second = session.new_session_id()
  let waiting =
    catch_up.new([first, second], 0, 60_000)
    |> catch_up.record("MESSAGE_CREATE", dynamic.int(7), 1)
  let taken = catch_up.take(waiting, first, 2)
  assert list.length(taken.events) == 1
  assert catch_up.waiting(taken.catch_up) == [second]
  let again = catch_up.take(taken.catch_up, first, 3)
  assert again.events == []
}
