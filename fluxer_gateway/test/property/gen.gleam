// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub opaque type Rng {
  Rng(state: Int)
}

const mask = 18_446_744_073_709_551_615

pub fn new(seed: Int) -> Rng {
  Rng(int.bitwise_and(seed * 2_654_435_761 + 40_503, mask))
}

fn step(rng: Rng) -> #(Int, Rng) {
  let next =
    int.bitwise_and(
      rng.state * 6_364_136_223_846_793_005 + 1_442_695_040_888_963_407,
      mask,
    )
  let mixed = int.bitwise_exclusive_or(next, int.bitwise_shift_right(next, 29))
  #(int.bitwise_shift_right(mixed, 16), Rng(next))
}

pub fn int(rng: Rng, below: Int) -> #(Int, Rng) {
  let #(value, rng) = step(rng)
  case below <= 0 {
    True -> #(0, rng)
    False -> #(value % below, rng)
  }
}

pub fn range(rng: Rng, from: Int, to: Int) -> #(Int, Rng) {
  let #(value, rng) = int(rng, to - from + 1)
  #(from + value, rng)
}

pub fn bool(rng: Rng) -> #(Bool, Rng) {
  let #(value, rng) = int(rng, 2)
  #(value == 1, rng)
}

pub fn chance(rng: Rng, percent: Int) -> #(Bool, Rng) {
  let #(value, rng) = int(rng, 100)
  #(value < percent, rng)
}

pub fn pick(rng: Rng, items: List(a), fallback: a) -> #(a, Rng) {
  let #(index, rng) = int(rng, list.length(items))
  #(list.drop(items, index) |> list.first |> result.unwrap(fallback), rng)
}

pub fn list(
  rng: Rng,
  count: Int,
  item: fn(Rng) -> #(a, Rng),
) -> #(List(a), Rng) {
  list_loop(rng, count, item, [])
}

fn list_loop(
  rng: Rng,
  count: Int,
  item: fn(Rng) -> #(a, Rng),
  acc: List(a),
) -> #(List(a), Rng) {
  case count <= 0 {
    True -> #(list.reverse(acc), rng)
    False -> {
      let #(value, rng) = item(rng)
      list_loop(rng, count - 1, item, [value, ..acc])
    }
  }
}

pub fn shuffle(rng: Rng, items: List(a)) -> #(List(a), Rng) {
  let #(keyed, rng) =
    list.fold(items, #([], rng), fn(state, value) {
      let #(acc, rng) = state
      let #(key, rng) = step(rng)
      #([#(key, value), ..acc], rng)
    })
  #(
    keyed
      |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
      |> list.map(fn(pair) { pair.1 }),
    rng,
  )
}

pub fn base_seed() -> Int {
  case envoy.get("FLUXER_PROPERTY_SEED") |> result.try(int.parse) {
    Ok(seed) -> seed
    Error(Nil) -> int.random(1_000_000_000)
  }
}

pub fn cases(default: Int) -> Int {
  case envoy.get("FLUXER_PROPERTY_CASES") |> result.try(int.parse) {
    Ok(count) -> count
    Error(Nil) -> default
  }
}

pub fn check(
  name: String,
  count: Int,
  property: fn(Rng) -> Result(Nil, String),
) -> Nil {
  let seed = base_seed()
  run(name, seed, 0, cases(count), property)
}

fn run(
  name: String,
  seed: Int,
  index: Int,
  count: Int,
  property: fn(Rng) -> Result(Nil, String),
) -> Nil {
  case index >= count {
    True -> Nil
    False ->
      case property(new(seed + index)) {
        Ok(Nil) -> run(name, seed, index + 1, count, property)
        Error(reason) -> {
          let message =
            name
            <> " failed with FLUXER_PROPERTY_SEED="
            <> int.to_string(seed + index)
            <> " FLUXER_PROPERTY_CASES=1: "
            <> reason
          io.println_error(message)
          panic as message
        }
      }
  }
}

pub fn expect(holds: Bool, reason: fn() -> String) -> Result(Nil, String) {
  case holds {
    True -> Ok(Nil)
    False -> Error(reason())
  }
}

pub fn span(from: Int, to: Int) -> List(Int) {
  span_loop(from, to, [])
}

fn span_loop(from: Int, to: Int, acc: List(Int)) -> List(Int) {
  case to < from {
    True -> acc
    False -> span_loop(from, to - 1, [to, ..acc])
  }
}
