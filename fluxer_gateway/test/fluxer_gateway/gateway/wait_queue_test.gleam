// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/wait_queue.{type WaitQueue}
import gleam/int
import gleam/list

fn drain(queue: WaitQueue(Int, String)) -> List(#(Int, String)) {
  drain_loop(queue, [])
}

fn drain_loop(
  queue: WaitQueue(Int, String),
  acc: List(#(Int, String)),
) -> List(#(Int, String)) {
  case wait_queue.pop(queue) {
    Ok(#(owner, waiter, rest)) -> drain_loop(rest, [#(owner, waiter), ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

fn numbers(count: Int) -> List(Int) {
  int.range(from: count, to: 0, with: [], run: list.prepend)
}

pub fn fifo_order_test() {
  let queue =
    wait_queue.new()
    |> wait_queue.push(1, "a")
    |> wait_queue.push(2, "b")
    |> wait_queue.push(1, "c")
  assert wait_queue.length(queue) == 3
  assert drain(queue) == [#(1, "a"), #(2, "b"), #(1, "c")]
}

pub fn cancel_skips_waiter_test() {
  let queue =
    wait_queue.new()
    |> wait_queue.push(1, "a")
    |> wait_queue.push(2, "b")
    |> wait_queue.push(3, "c")
    |> wait_queue.cancel(2, "b")
  assert wait_queue.length(queue) == 2
  assert !wait_queue.has_owner(queue, 2)
  assert drain(queue) == [#(1, "a"), #(3, "c")]
}

pub fn cancel_unknown_is_noop_test() {
  let queue =
    wait_queue.new()
    |> wait_queue.push(1, "a")
    |> wait_queue.cancel(1, "z")
    |> wait_queue.cancel(9, "a")
  assert wait_queue.length(queue) == 1
  assert drain(queue) == [#(1, "a")]
}

pub fn drop_owner_removes_all_of_its_waiters_test() {
  let queue =
    wait_queue.new()
    |> wait_queue.push(1, "a")
    |> wait_queue.push(2, "b")
    |> wait_queue.push(1, "c")
    |> wait_queue.push(1, "c")
    |> wait_queue.drop_owner(1)
  assert wait_queue.length(queue) == 1
  assert !wait_queue.has_owner(queue, 1)
  assert drain(queue) == [#(2, "b")]
}

pub fn owner_kept_while_any_waiter_pending_test() {
  let queue =
    wait_queue.new()
    |> wait_queue.push(1, "a")
    |> wait_queue.push(1, "b")
  let assert Ok(#(1, "a", queue)) = wait_queue.pop(queue)
  assert wait_queue.has_owner(queue, 1)
  let assert Ok(#(1, "b", queue)) = wait_queue.pop(queue)
  assert !wait_queue.has_owner(queue, 1)
  assert wait_queue.length(queue) == 0
  assert wait_queue.pop(queue) == Error(Nil)
}

pub fn compaction_keeps_live_order_test() {
  let count = 5000
  let queue =
    numbers(count)
    |> list.fold(wait_queue.new(), fn(queue, n) {
      wait_queue.push(queue, n, int.to_string(n))
    })
  let queue =
    numbers(count)
    |> list.filter(fn(n) { n % 5 != 0 })
    |> list.fold(queue, fn(queue, n) {
      wait_queue.cancel(queue, n, int.to_string(n))
    })
  let expected =
    numbers(count)
    |> list.filter(fn(n) { n % 5 == 0 })
    |> list.map(fn(n) { #(n, int.to_string(n)) })
  assert wait_queue.length(queue) == count / 5
  assert drain(queue) == expected
}

pub fn large_reconnect_storm_test() {
  let count = 100_000
  let queue =
    numbers(count)
    |> list.fold(wait_queue.new(), fn(queue, n) {
      wait_queue.push(queue, n, "w")
    })
  assert wait_queue.length(queue) == count
  assert list.length(drain(queue)) == count
}
