// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/list
import gleam/result

pub opaque type WaitQueue(owner, waiter) {
  WaitQueue(
    front: List(#(owner, waiter)),
    back: List(#(owner, waiter)),
    pending: Dict(owner, Dict(waiter, Int)),
    queued: Int,
    size: Int,
  )
}

const compact_slack = 1024

pub fn new() -> WaitQueue(owner, waiter) {
  WaitQueue(front: [], back: [], pending: dict.new(), queued: 0, size: 0)
}

pub fn length(queue: WaitQueue(owner, waiter)) -> Int {
  queue.queued
}

pub fn has_owner(queue: WaitQueue(owner, waiter), owner: owner) -> Bool {
  dict.has_key(queue.pending, owner)
}

pub fn push(
  queue: WaitQueue(owner, waiter),
  owner: owner,
  waiter: waiter,
) -> WaitQueue(owner, waiter) {
  let waiters = dict.get(queue.pending, owner) |> result.unwrap(dict.new())
  let count = dict.get(waiters, waiter) |> result.unwrap(0)
  WaitQueue(
    ..queue,
    back: [#(owner, waiter), ..queue.back],
    pending: dict.insert(
      queue.pending,
      owner,
      dict.insert(waiters, waiter, count + 1),
    ),
    queued: queue.queued + 1,
    size: queue.size + 1,
  )
}

pub fn cancel(
  queue: WaitQueue(owner, waiter),
  owner: owner,
  waiter: waiter,
) -> WaitQueue(owner, waiter) {
  let found = {
    use waiters <- result.try(dict.get(queue.pending, owner))
    use count <- result.map(dict.get(waiters, waiter))
    #(count, put_waiters(queue.pending, owner, dict.delete(waiters, waiter)))
  }
  case found {
    Error(Nil) -> queue
    Ok(#(count, pending)) ->
      compact(WaitQueue(..queue, pending:, queued: queue.queued - count))
  }
}

pub fn drop_owner(
  queue: WaitQueue(owner, waiter),
  owner: owner,
) -> WaitQueue(owner, waiter) {
  case dict.get(queue.pending, owner) {
    Error(Nil) -> queue
    Ok(waiters) -> {
      let count = dict.fold(waiters, 0, fn(total, _, count) { total + count })
      compact(
        WaitQueue(
          ..queue,
          pending: dict.delete(queue.pending, owner),
          queued: queue.queued - count,
        ),
      )
    }
  }
}

pub fn pop(
  queue: WaitQueue(owner, waiter),
) -> Result(#(owner, waiter, WaitQueue(owner, waiter)), Nil) {
  case queue.front, queue.back {
    [], [] -> Error(Nil)
    [], back -> pop(WaitQueue(..queue, front: list.reverse(back), back: []))
    [#(owner, waiter), ..rest], _ -> {
      let queue = WaitQueue(..queue, front: rest, size: queue.size - 1)
      case take(queue.pending, owner, waiter) {
        Ok(pending) ->
          Ok(#(
            owner,
            waiter,
            WaitQueue(..queue, pending:, queued: queue.queued - 1),
          ))
        Error(Nil) -> pop(queue)
      }
    }
  }
}

fn take(
  pending: Dict(owner, Dict(waiter, Int)),
  owner: owner,
  waiter: waiter,
) -> Result(Dict(owner, Dict(waiter, Int)), Nil) {
  use waiters <- result.try(dict.get(pending, owner))
  use count <- result.map(dict.get(waiters, waiter))
  let waiters = case count > 1 {
    True -> dict.insert(waiters, waiter, count - 1)
    False -> dict.delete(waiters, waiter)
  }
  put_waiters(pending, owner, waiters)
}

fn put_waiters(
  pending: Dict(owner, Dict(waiter, Int)),
  owner: owner,
  waiters: Dict(waiter, Int),
) -> Dict(owner, Dict(waiter, Int)) {
  case dict.is_empty(waiters) {
    True -> dict.delete(pending, owner)
    False -> dict.insert(pending, owner, waiters)
  }
}

fn compact(queue: WaitQueue(owner, waiter)) -> WaitQueue(owner, waiter) {
  case queue.size > 2 * queue.queued + compact_slack {
    False -> queue
    True -> {
      let #(kept, _) =
        list.append(queue.front, list.reverse(queue.back))
        |> list.fold(#([], queue.pending), fn(acc, entry) {
          let #(kept, budget) = acc
          case take(budget, entry.0, entry.1) {
            Ok(budget) -> #([entry, ..kept], budget)
            Error(Nil) -> acc
          }
        })
      WaitQueue(
        ..queue,
        front: list.reverse(kept),
        back: [],
        size: list.length(kept),
      )
    }
  }
}
