// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int

pub opaque type Deadline {
  Deadline(at_monotonic_ms: Int)
}

type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

pub fn monotonic_ms() -> Int {
  monotonic_time(Millisecond)
}

pub fn system_ms() -> Int {
  system_time(Millisecond)
}

pub fn deadline_in(remaining_ms: Int) -> Deadline {
  Deadline(monotonic_ms() + remaining_ms)
}

pub fn remaining_ms(deadline: Deadline) -> Int {
  int.max(deadline.at_monotonic_ms - monotonic_ms(), 0)
}

pub fn is_expired(deadline: Deadline) -> Bool {
  remaining_ms(deadline) == 0
}

pub fn earliest(a: Deadline, b: Deadline) -> Deadline {
  case a.at_monotonic_ms <= b.at_monotonic_ms {
    True -> a
    False -> b
  }
}
