// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/erlang/process

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: Atom) -> Int

fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}

pub fn until(timeout_ms: Int, check: fn() -> Bool) -> Bool {
  poll(check, now_ms() + timeout_ms, 10)
}

pub fn until_every(
  timeout_ms: Int,
  interval_ms: Int,
  check: fn() -> Bool,
) -> Bool {
  poll(check, now_ms() + timeout_ms, interval_ms)
}

fn poll(check: fn() -> Bool, deadline: Int, interval_ms: Int) -> Bool {
  case check() {
    True -> True
    False ->
      case now_ms() > deadline {
        True -> False
        False -> {
          process.sleep(interval_ms)
          poll(check, deadline, interval_ms)
        }
      }
  }
}
