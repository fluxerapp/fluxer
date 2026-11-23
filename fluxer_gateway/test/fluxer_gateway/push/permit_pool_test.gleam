// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/permit/pool
import gleam/list
import gleam/option.{None, Some}

fn config() -> pool.Config {
  pool.Config(capacity: 2, max_waiters: 2, max_wait_ms: Some(60_000))
}

pub fn grants_up_to_capacity_then_queues_then_drops_test() {
  let p = pool.new(config())
  let assert #(p, pool.Granted(1, "a")) = pool.claim(p, 1, "a", 0)
  let assert #(p, pool.Granted(2, "b")) = pool.claim(p, 2, "b", 0)
  let assert #(p, pool.Queued(3, "c")) = pool.claim(p, 3, "c", 0)
  let assert #(p, pool.Queued(4, "d")) = pool.claim(p, 4, "d", 0)
  let assert #(p, pool.Dropped(5, "e", pool.QueueFull)) =
    pool.claim(p, 5, "e", 0)
  let assert 2 = pool.in_use(p)
}

pub fn release_grants_the_oldest_waiter_test() {
  let p = pool.new(config())
  let assert #(p, pool.Granted(first, _)) = pool.claim(p, 1, "a", 0)
  let assert #(p, pool.Granted(_, _)) = pool.claim(p, 2, "b", 0)
  let assert #(p, pool.Queued(_, _)) = pool.claim(p, 3, "c", 1)
  let assert #(p, [pool.Granted(_, "c")]) = pool.release(p, first)
  let assert 2 = pool.in_use(p)
}

pub fn holder_down_releases_every_permit_test() {
  let p = pool.new(config())
  let #(p, _) = pool.claim(p, 1, "a", 0)
  let #(p, _) = pool.claim(p, 2, "a", 0)
  let assert #(p, pool.Queued(_, "b")) = pool.claim(p, 3, "b", 0)
  let assert #(p, [pool.Granted(_, "b")]) =
    pool.holder_down(p, fn(holder) { holder == "a" })
  let assert 1 = pool.in_use(p)
}

pub fn waiters_expire_at_the_limit_test() {
  let p =
    pool.new(pool.Config(capacity: 1, max_waiters: 4, max_wait_ms: Some(100)))
  let #(p, _) = pool.claim(p, 1, "a", 0)
  let assert #(p, pool.Queued(_, "b")) = pool.claim(p, 2, "b", 0)
  let assert #(p, []) = pool.expire(p, 99)
  let assert #(_, [pool.Dropped(_, "b", pool.WaitExpired)]) =
    pool.expire(p, 100)
}

pub fn restore_keeps_order_and_grants_test() {
  let #(p, effects) =
    pool.restore(
      pool.Config(capacity: 2, max_waiters: 4, max_wait_ms: None),
      [pool.Holding(3, "a")],
      [pool.Waiter(7, "late", 20), pool.Waiter(5, "early", 10)],
    )
  let assert [pool.Granted(5, "early")] = effects
  let assert [pool.Waiter(7, "late", 20)] = pool.waiting(p)
  let assert 2 = list.length(pool.holdings(p))
}

pub fn release_of_a_token_from_an_earlier_incarnation_frees_nothing_test() {
  let #(p, effects) =
    pool.restore(
      pool.Config(capacity: 1, max_waiters: 4, max_wait_ms: None),
      [],
      [],
    )
  let assert [] = effects
  let assert #(p, pool.Granted(1001, "current")) =
    pool.claim(p, 1001, "current", 0)
  let assert #(p, pool.Queued(1002, "waiting")) =
    pool.claim(p, 1002, "waiting", 0)
  let assert #(p, []) = pool.release(p, 1)
  let assert [pool.Holding(1001, "current")] = pool.holdings(p)
  let assert [pool.Waiter(1002, "waiting", 0)] = pool.waiting(p)
}

pub fn release_of_a_queued_token_only_removes_that_waiter_test() {
  let p = pool.new(config())
  let #(p, _) = pool.claim(p, 10, "a", 0)
  let #(p, _) = pool.claim(p, 20, "b", 0)
  let #(p, _) = pool.claim(p, 30, "c", 0)
  let #(p, _) = pool.claim(p, 40, "d", 0)
  let assert #(p, []) = pool.release(p, 30)
  let assert [pool.Waiter(40, "d", 0)] = pool.waiting(p)
  let assert 2 = pool.in_use(p)
}
