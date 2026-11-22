// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/member_list/ranges
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import property/gen.{type Rng}

fn indices(items: List(#(Int, Int))) -> Set(Int) {
  list.fold(items, set.new(), fn(acc, item) {
    set.union(acc, set.from_list(gen.span(item.0, item.1)))
  })
}

fn valid(item: #(Int, Int)) -> Bool {
  item.0 >= 0 && item.0 <= item.1 && item.1 <= 100_000 && item.1 - item.0 <= 99
}

fn well_formed(items: List(#(Int, Int))) -> Bool {
  list.all(items, valid)
  && list.window_by_2(items) |> list.all(fn(pair) { pair.0.1 < pair.1.0 })
}

fn range(rng: Rng) -> #(#(Int, Int), Rng) {
  let #(start, rng) = gen.int(rng, 320)
  let #(span, rng) = gen.pick(rng, [0, 1, 5, 40, 98, 99, 100, 150], 0)
  let #(broken, rng) = gen.chance(rng, 10)
  case broken {
    True -> {
      let #(kind, rng) = gen.int(rng, 3)
      case kind {
        0 -> #(#(start + 5, start), rng)
        1 -> #(#(-1 - start, start), rng)
        _ -> #(#(100_000 - span + 50, 100_000 + 50), rng)
      }
    }
    False -> #(#(start, start + span), rng)
  }
}

fn request(rng: Rng) -> #(List(#(Int, Int)), Rng) {
  let #(count, rng) = gen.int(rng, 13)
  gen.list(rng, count, range)
}

fn ranges_case(rng: Rng) -> Result(Nil, String) {
  let #(raw_new, rng) = request(rng)
  let #(raw_old, rng) = request(rng)
  let #(same, _) = gen.chance(rng, 15)
  let raw_old = case same {
    True -> raw_new
    False -> raw_old
  }
  let wanted = list.filter(raw_new, valid) |> list.take(10)
  let new = ranges.normalize(ranges.parse_request(raw_new))
  let old = ranges.from_list(list.filter(raw_old, valid))
  let new_items = ranges.to_list(new)
  let old_items = ranges.to_list(old)
  let describe = fn() {
    "new " <> string.inspect(raw_new) <> " old " <> string.inspect(raw_old)
  }
  use _ <- result.try(
    gen.expect(indices(new_items) == indices(wanted), fn() {
      "normalize lost or added rows: " <> describe()
    }),
  )
  use _ <- result.try(
    gen.expect(well_formed(new_items) && well_formed(old_items), fn() {
      "normalize gave overlapping or invalid ranges "
      <> string.inspect(new_items)
      <> " for "
      <> describe()
    }),
  )
  use _ <- result.try(
    gen.expect(list.length(new_items) <= 10, fn() {
      "a client request became more than 10 ranges: " <> describe()
    }),
  )
  let delta = ranges.to_list(ranges.delta(new:, old:))
  use _ <- result.try(
    gen.expect(
      indices(delta) == set.difference(indices(new_items), indices(old_items)),
      fn() { "delta is not new minus old: " <> describe() },
    ),
  )
  let subset = set.is_subset(indices(new_items), indices(old_items))
  use _ <- result.try(
    gen.expect(ranges.is_subset(new, of: old) == subset, fn() {
      "is_subset disagrees with the row sets: " <> describe()
    }),
  )
  let sync = ranges.to_list(ranges.to_sync(new:, old:))
  let want = case new_items == [] || new_items == old_items, subset {
    True, _ -> indices(new_items)
    False, True -> set.new()
    False, False -> set.difference(indices(new_items), indices(old_items))
  }
  use _ <- result.try(
    gen.expect(indices(sync) == want, fn() {
      "to_sync covers " <> string.inspect(sync) <> " for " <> describe()
    }),
  )
  gen.expect(well_formed(sync) && list.length(sync) <= 20, fn() {
    "to_sync gave a range list the native core refuses: "
    <> string.inspect(sync)
    <> " for "
    <> describe()
  })
}

pub fn ranges_follow_the_row_set_model_test() {
  gen.check("member list ranges", 3000, ranges_case)
}

pub fn narrowing_across_a_chunk_boundary_needs_no_sync_test() {
  let old = ranges.normalize(ranges.parse_request([#(0, 99), #(100, 199)]))
  let new = ranges.normalize(ranges.parse_request([#(50, 149)]))
  assert ranges.is_subset(new, of: old)
  assert ranges.to_list(ranges.to_sync(new:, old:)) == []
}
