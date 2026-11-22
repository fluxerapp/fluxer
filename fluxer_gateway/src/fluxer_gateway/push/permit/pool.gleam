// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Eq}

pub type DropReason {
  QueueFull
  WaitExpired
}

pub type Effect(h) {
  Granted(token: Int, holder: h)
  Queued(token: Int, holder: h)
  Dropped(token: Int, holder: h, reason: DropReason)
}

pub type Waiter(h) {
  Waiter(token: Int, holder: h, enqueued_at_ms: Int)
}

pub type Holding(h) {
  Holding(token: Int, holder: h)
}

pub type Config {
  Config(capacity: Int, max_waiters: Int, max_wait_ms: Option(Int))
}

pub opaque type Pool(h) {
  Pool(
    config: Config,
    holders: Dict(Int, h),
    waiters: List(Waiter(h)),
    waiting: Int,
  )
}

pub fn new(config: Config) -> Pool(h) {
  Pool(config:, holders: dict.new(), waiters: [], waiting: 0)
}

pub fn in_use(pool: Pool(h)) -> Int {
  dict.size(pool.holders)
}

pub fn knows(pool: Pool(h), token: Int) -> Bool {
  dict.has_key(pool.holders, token)
  || list.any(pool.waiters, fn(waiter) { waiter.token == token })
}

pub fn waiting(pool: Pool(h)) -> List(Waiter(h)) {
  pool.waiters
}

pub fn holdings(pool: Pool(h)) -> List(Holding(h)) {
  dict.to_list(pool.holders)
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(pair) { Holding(pair.0, pair.1) })
}

pub fn claim(
  pool: Pool(h),
  token: Int,
  holder: h,
  now_ms: Int,
) -> #(Pool(h), Effect(h)) {
  case in_use(pool) < pool.config.capacity && pool.waiters == [] {
    True -> #(
      Pool(..pool, holders: dict.insert(pool.holders, token, holder)),
      Granted(token, holder),
    )
    False ->
      case pool.waiting < pool.config.max_waiters {
        True -> #(
          Pool(
            ..pool,
            waiters: list.append(pool.waiters, [Waiter(token, holder, now_ms)]),
            waiting: pool.waiting + 1,
          ),
          Queued(token, holder),
        )
        False -> #(pool, Dropped(token, holder, QueueFull))
      }
  }
}

pub fn release(pool: Pool(h), token: Int) -> #(Pool(h), List(Effect(h))) {
  case dict.has_key(pool.holders, token) {
    True ->
      grant_waiters(Pool(..pool, holders: dict.delete(pool.holders, token)))
    False -> {
      let kept = list.filter(pool.waiters, fn(waiter) { waiter.token != token })
      #(Pool(..pool, waiters: kept, waiting: list.length(kept)), [])
    }
  }
}

pub fn holder_down(
  pool: Pool(h),
  is_holder: fn(h) -> Bool,
) -> #(Pool(h), List(Effect(h))) {
  let holders = dict.filter(pool.holders, fn(_, holder) { !is_holder(holder) })
  let waiters =
    list.filter(pool.waiters, fn(waiter) { !is_holder(waiter.holder) })
  grant_waiters(Pool(..pool, holders:, waiters:, waiting: list.length(waiters)))
}

pub fn expire(pool: Pool(h), now_ms: Int) -> #(Pool(h), List(Effect(h))) {
  case pool.config.max_wait_ms {
    None -> #(pool, [])
    Some(limit) -> {
      let #(expired, kept) =
        list.partition(pool.waiters, fn(waiter) {
          now_ms - waiter.enqueued_at_ms >= limit
        })
      let drops =
        list.map(expired, fn(waiter) {
          Dropped(waiter.token, waiter.holder, WaitExpired)
        })
      #(Pool(..pool, waiters: kept, waiting: list.length(kept)), drops)
    }
  }
}

fn grant_waiters(pool: Pool(h)) -> #(Pool(h), List(Effect(h))) {
  grant_loop(pool, [])
}

fn grant_loop(
  pool: Pool(h),
  acc: List(Effect(h)),
) -> #(Pool(h), List(Effect(h))) {
  case in_use(pool) < pool.config.capacity, pool.waiters {
    True, [first, ..rest] ->
      grant_loop(
        Pool(
          ..pool,
          holders: dict.insert(pool.holders, first.token, first.holder),
          waiters: rest,
          waiting: pool.waiting - 1,
        ),
        [Granted(first.token, first.holder), ..acc],
      )
    _, _ -> #(pool, list.reverse(acc))
  }
}

pub fn restore(
  config: Config,
  holdings: List(Holding(h)),
  waiters: List(Waiter(h)),
) -> #(Pool(h), List(Effect(h))) {
  let holders =
    list.fold(holdings, dict.new(), fn(acc, holding) {
      dict.insert(acc, holding.token, holding.holder)
    })
  let ordered =
    list.sort(waiters, fn(a, b) {
      case int.compare(a.enqueued_at_ms, b.enqueued_at_ms) {
        Eq -> int.compare(a.token, b.token)
        other -> other
      }
    })
  grant_waiters(Pool(
    config:,
    holders:,
    waiters: ordered,
    waiting: list.length(ordered),
  ))
}
