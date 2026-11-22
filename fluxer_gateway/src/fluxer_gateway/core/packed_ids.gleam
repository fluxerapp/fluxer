// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/list

pub opaque type PackedIds {
  PackedIds(bytes: BitArray)
}

pub fn empty() -> PackedIds {
  PackedIds(<<>>)
}

pub fn from_bits(bytes: BitArray) -> Result(PackedIds, Nil) {
  case bit_array.byte_size(bytes) % 8 {
    0 -> Ok(PackedIds(bytes))
    _ -> Error(Nil)
  }
}

pub fn from_list(ids: List(Int)) -> PackedIds {
  ids
  |> list.fold(<<>>, fn(acc, id) { <<acc:bits, id:64>> })
  |> PackedIds
}

pub fn to_bits(ids: PackedIds) -> BitArray {
  ids.bytes
}

fn size(ids: PackedIds) -> Int {
  bit_array.byte_size(ids.bytes) / 8
}

pub fn fold(
  ids: PackedIds,
  from initial: acc,
  with step: fn(acc, Int) -> acc,
) -> acc {
  fold_bits(ids.bytes, initial, step)
}

fn fold_bits(bytes: BitArray, acc: acc, step: fn(acc, Int) -> acc) -> acc {
  case bytes {
    <<id:64, rest:bits>> -> fold_bits(rest, step(acc, id), step)
    _ -> acc
  }
}

pub fn to_list(ids: PackedIds) -> List(Int) {
  ids
  |> fold([], fn(acc, id) { [id, ..acc] })
  |> list.reverse
}

fn at(ids: PackedIds, index: Int) -> Result(Int, Nil) {
  case bit_array.slice(ids.bytes, index * 8, 8) {
    Ok(<<id:64>>) -> Ok(id)
    _ -> Error(Nil)
  }
}

pub fn contains_sorted(ids: PackedIds, id: Int) -> Bool {
  search(ids, id, 0, size(ids) - 1)
}

fn search(ids: PackedIds, id: Int, low: Int, high: Int) -> Bool {
  case low > high {
    True -> False
    False -> {
      let middle = { low + high } / 2
      case at(ids, middle) {
        Ok(found) if found == id -> True
        Ok(found) if found < id -> search(ids, id, middle + 1, high)
        Ok(_) -> search(ids, id, low, middle - 1)
        Error(Nil) -> False
      }
    }
  }
}
