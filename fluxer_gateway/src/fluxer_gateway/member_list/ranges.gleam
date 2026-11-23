// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list
import gleam/order

pub opaque type Ranges {
  Ranges(items: List(#(Int, Int)))
}

const max_end = 100_000

const max_span = 99

const max_ranges = 10

pub fn parse_request(items: List(#(Int, Int))) -> Ranges {
  items
  |> list.filter(valid)
  |> list.take(max_ranges)
  |> Ranges
}

pub fn normalize(ranges: Ranges) -> Ranges {
  Ranges(normalized(ranges.items))
}

pub fn from_list(items: List(#(Int, Int))) -> Ranges {
  Ranges(normalized(items))
}

pub fn empty() -> Ranges {
  Ranges([])
}

pub fn to_list(ranges: Ranges) -> List(#(Int, Int)) {
  ranges.items
}

pub fn delta(new new: Ranges, old old: Ranges) -> Ranges {
  old.items
  |> list.fold(new.items, subtract)
  |> normalized
  |> Ranges
}

pub fn is_subset(ranges: Ranges, of outer: Ranges) -> Bool {
  delta(new: ranges, old: outer).items == []
}

pub fn to_sync(new new: Ranges, old old: Ranges) -> Ranges {
  case new.items == [] || new.items == old.items {
    True -> new
    False -> delta(new: new, old: old)
  }
}

fn valid(item: #(Int, Int)) -> Bool {
  let #(start, end) = item
  start >= 0 && start <= end && end <= max_end && end - start <= max_span
}

fn compare(a: #(Int, Int), b: #(Int, Int)) -> order.Order {
  int.compare(a.0, b.0) |> order.break_tie(int.compare(a.1, b.1))
}

fn normalized(items: List(#(Int, Int))) -> List(#(Int, Int)) {
  items
  |> list.filter(valid)
  |> list.sort(compare)
  |> merge
  |> list.flat_map(split)
}

fn merge(sorted: List(#(Int, Int))) -> List(#(Int, Int)) {
  case sorted {
    [#(s1, e1), #(s2, e2), ..rest] if s2 <= e1 + 1 ->
      merge([#(s1, int.max(e1, e2)), ..rest])
    [first, ..rest] -> [first, ..merge(rest)]
    [] -> []
  }
}

fn split(item: #(Int, Int)) -> List(#(Int, Int)) {
  let #(start, end) = item
  case end - start <= max_span {
    True -> [item]
    False -> [#(start, start + max_span), ..split(#(start + max_span + 1, end))]
  }
}

fn subtract(items: List(#(Int, Int)), sub: #(Int, Int)) -> List(#(Int, Int)) {
  list.flat_map(items, fn(item) {
    let #(start, end) = item
    let #(sub_start, sub_end) = sub
    case end < sub_start || start > sub_end {
      True -> [item]
      False -> {
        let left = case start < sub_start {
          True -> [#(start, sub_start - 1)]
          False -> []
        }
        let right = case end > sub_end {
          True -> [#(sub_end + 1, end)]
          False -> []
        }
        list.append(left, right)
      }
    }
  })
}
