// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int

const max_exponent = 32

@external(erlang, "rand", "uniform")
fn uniform(range: Int) -> Int

pub fn delay_ms(
  attempt attempt: Int,
  base_ms base_ms: Int,
  max_ms max_ms: Int,
  jitter_ms jitter_ms: Int,
) -> Int {
  let capped =
    int.min(int.bitwise_shift_left(base_ms, exponent(attempt)), max_ms)
  case jitter_ms > 0 {
    False -> capped
    True -> {
      let jitter = uniform(jitter_ms + 1) - 1
      int.max(1, int.min(max_ms, capped - jitter_ms / 2 + jitter))
    }
  }
}

fn exponent(attempt: Int) -> Int {
  case attempt >= 0 {
    True -> int.min(attempt, max_exponent)
    False -> 0
  }
}
