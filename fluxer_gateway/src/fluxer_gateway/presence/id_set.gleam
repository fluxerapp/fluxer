// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids.{type PackedIds}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/order.{Eq, Gt, Lt}
import gleam/result

pub opaque type IdSet {
  IdSet(packed: BitArray)
}

const max_id = 9_223_372_036_854_775_807

pub fn empty() -> IdSet {
  IdSet(<<>>)
}

pub fn from_list(ids: List(Int)) -> IdSet {
  ids
  |> list.filter(in_range)
  |> list.sort(int.compare)
  |> pack(<<>>, -1)
  |> IdSet
}

pub fn from_packed(ids: PackedIds) -> IdSet {
  let bits = packed_ids.to_bits(ids)
  case ascending(bits, -1) {
    True -> IdSet(bits)
    False -> from_list(packed_ids.to_list(ids))
  }
}

fn ascending(bits: BitArray, last: Int) -> Bool {
  case bits {
    <<>> -> True
    <<value:size(64), rest:bits>> if value > last && value <= max_id ->
      ascending(rest, value)
    _ -> False
  }
}

fn single(value: Int) -> IdSet {
  from_list([value])
}

pub fn to_packed(set: IdSet) -> PackedIds {
  packed_ids.from_bits(set.packed) |> result.lazy_unwrap(packed_ids.empty)
}

fn in_range(value: Int) -> Bool {
  value >= 0 && value <= max_id
}

fn pack(ids: List(Int), acc: BitArray, last: Int) -> BitArray {
  case ids {
    [] -> acc
    [value, ..rest] if value == last -> pack(rest, acc, last)
    [value, ..rest] -> pack(rest, <<acc:bits, value:size(64)>>, value)
  }
}

pub fn to_list(set: IdSet) -> List(Int) {
  unpack(set.packed, [])
}

fn unpack(packed: BitArray, acc: List(Int)) -> List(Int) {
  case packed {
    <<value:size(64), rest:bits>> -> unpack(rest, [value, ..acc])
    _ -> list.reverse(acc)
  }
}

pub fn size(set: IdSet) -> Int {
  bit_array.byte_size(set.packed) / 8
}

pub fn is_empty(set: IdSet) -> Bool {
  set.packed == <<>>
}

pub fn contains(set: IdSet, value: Int) -> Bool {
  case position(set, value) {
    Ok(_) -> True
    Error(Nil) -> False
  }
}

fn position(set: IdSet, value: Int) -> Result(Int, Nil) {
  case in_range(value) {
    True -> search(set.packed, value, 0, size(set))
    False -> Error(Nil)
  }
}

fn search(
  packed: BitArray,
  value: Int,
  low: Int,
  high: Int,
) -> Result(Int, Nil) {
  case low < high {
    False -> Error(Nil)
    True -> {
      let mid = { low + high } / 2
      let skip = mid * 8
      case packed {
        <<_:bytes-size(skip), found:size(64), _:bits>> if found == value ->
          Ok(mid)
        <<_:bytes-size(skip), found:size(64), _:bits>> if found < value ->
          search(packed, value, mid + 1, high)
        <<_:bytes-size(skip), _:size(64), _:bits>> ->
          search(packed, value, low, mid)
        _ -> Error(Nil)
      }
    }
  }
}

pub fn group_low(set: IdSet, key: fn(Int) -> Int) -> Dict(Int, List(Int)) {
  group_low_loop(set.packed, key, dict.new())
}

fn group_low_loop(
  packed: BitArray,
  key: fn(Int) -> Int,
  groups: Dict(Int, List(Int)),
) -> Dict(Int, List(Int)) {
  case packed {
    <<high:size(32), low:size(32), rest:bytes>> ->
      group_low_loop(
        rest,
        key,
        dict.upsert(groups, key(low), fn(found) {
          [high * 4_294_967_296 + low, ..option.unwrap(found, [])]
        }),
      )
    _ -> groups
  }
}

pub fn insert(set: IdSet, value: Int) -> IdSet {
  union(set, single(value))
}

pub fn remove(set: IdSet, value: Int) -> IdSet {
  case position(set, value) {
    Error(Nil) -> set
    Ok(index) -> {
      let skip = index * 8
      case set.packed {
        <<before:bytes-size(skip), _:size(64), after:bytes>> ->
          IdSet(<<before:bits, after:bits>>)
        _ -> set
      }
    }
  }
}

pub fn union(a: IdSet, b: IdSet) -> IdSet {
  IdSet(merge_union(a.packed, b.packed, <<>>))
}

fn merge_union(a: BitArray, b: BitArray, acc: BitArray) -> BitArray {
  case a, b {
    <<x:size(64), ra:bits>>, <<y:size(64), rb:bits>> ->
      case int.compare(x, y) {
        Lt -> merge_union(ra, b, <<acc:bits, x:size(64)>>)
        Gt -> merge_union(a, rb, <<acc:bits, y:size(64)>>)
        Eq -> merge_union(ra, rb, <<acc:bits, x:size(64)>>)
      }
    _, <<>> -> <<acc:bits, a:bits>>
    _, _ -> <<acc:bits, b:bits>>
  }
}

pub fn subtract(a: IdSet, b: IdSet) -> IdSet {
  IdSet(merge_subtract(a.packed, b.packed, <<>>))
}

fn merge_subtract(a: BitArray, b: BitArray, acc: BitArray) -> BitArray {
  case a, b {
    <<x:size(64), ra:bits>>, <<y:size(64), rb:bits>> ->
      case int.compare(x, y) {
        Lt -> merge_subtract(ra, b, <<acc:bits, x:size(64)>>)
        Gt -> merge_subtract(a, rb, acc)
        Eq -> merge_subtract(ra, rb, acc)
      }
    _, _ -> <<acc:bits, a:bits>>
  }
}

pub fn diff(old old: IdSet, new new: IdSet) -> #(IdSet, IdSet) {
  #(subtract(new, old), subtract(old, new))
}
