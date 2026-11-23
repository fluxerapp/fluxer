// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/threads/sync_queue.{type SyncQueue}
import gleam/int
import gleam/list

fn drain(queue: SyncQueue, acc: List(String)) -> List(String) {
  case sync_queue.pop(queue) {
    Ok(#(session_id, rest)) -> drain(rest, [session_id, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

pub fn empty_queue_pops_nothing_test() {
  assert sync_queue.pop(sync_queue.new()) == Error(Nil)
}

pub fn pops_in_push_order_test() {
  let queue =
    sync_queue.new()
    |> sync_queue.push("a")
    |> sync_queue.push("b")
  let assert Ok(#("a", queue)) = sync_queue.pop(queue)
  let queue = sync_queue.push(queue, "c")
  assert drain(queue, []) == ["b", "c"]
}

pub fn mass_resync_keeps_fifo_order_test() {
  let ids =
    int.range(from: 50_000, to: 0, with: [], run: fn(acc, n) {
      [int.to_string(n), ..acc]
    })
  let queue = list.fold(ids, sync_queue.new(), sync_queue.push)
  assert drain(queue, []) == ids
}
