// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list

pub opaque type Rng {
  Rng(state: Int)
}

const mask64 = 0xFFFFFFFFFFFFFFFF

const multiplier = 6_364_136_223_846_793_005

const increment = 1_442_695_040_888_963_407

pub fn seed(value: Int) -> Rng {
  let #(_, rng) = next(Rng(int.bitwise_and(value, mask64)))
  rng
}

fn next(rng: Rng) -> #(Int, Rng) {
  let state = int.bitwise_and(rng.state * multiplier + increment, mask64)
  #(int.bitwise_shift_right(state, 32), Rng(state))
}

pub fn uniform(rng: Rng, low: Int, high: Int) -> #(Int, Rng) {
  case high <= low {
    True -> #(low, rng)
    False -> {
      let #(value, rng) = next(rng)
      #(low + value % { high - low + 1 }, rng)
    }
  }
}

pub fn jitter(rng: Rng, value: Int, percent: Int) -> #(Int, Rng) {
  let spread = value * percent / 100
  uniform(rng, value - spread, value + spread)
}

pub fn pick(rng: Rng, items: List(a), count: Int) -> #(List(a), Rng) {
  pick_loop(rng, items, count, [])
}

fn pick_loop(
  rng: Rng,
  items: List(a),
  count: Int,
  acc: List(a),
) -> #(List(a), Rng) {
  case count <= 0, items {
    True, _ | _, [] -> #(acc, rng)
    False, _ -> {
      let #(index, rng) = uniform(rng, 0, list.length(items) - 1)
      let #(before, after) = list.split(items, index)
      case after {
        [chosen, ..rest] ->
          pick_loop(rng, list.append(before, rest), count - 1, [chosen, ..acc])
        [] -> #(acc, rng)
      }
    }
  }
}
