// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/order.{type Order}

pub opaque type Epoch {
  Epoch(value: Int)
}

pub fn from_int(value: Int) -> Result(Epoch, Nil) {
  case value >= 0 {
    True -> Ok(Epoch(value))
    False -> Error(Nil)
  }
}

pub fn to_int(epoch: Epoch) -> Int {
  epoch.value
}

pub fn zero() -> Epoch {
  Epoch(0)
}

pub fn next(epoch: Epoch) -> Epoch {
  Epoch(epoch.value + 1)
}

pub fn floor_at(system_time_ms: Int) -> Epoch {
  Epoch(int.max(system_time_ms, 0) * 1024)
}

pub fn compare(a: Epoch, b: Epoch) -> Order {
  int.compare(a.value, b.value)
}

pub fn max(a: Epoch, b: Epoch) -> Epoch {
  case a.value >= b.value {
    True -> a
    False -> b
  }
}
