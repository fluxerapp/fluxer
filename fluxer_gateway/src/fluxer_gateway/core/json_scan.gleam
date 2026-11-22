// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/result

pub type ScanError {
  Malformed
  Broken
}

pub type Location {
  Found(value_start: Int, value_end: Int, cut_start: Int, cut_end: Int)
  Absent(close: Int, empty: Bool)
}

const dirty_bytes = 262_144

pub fn object(bytes: BitArray) -> Result(List(#(String, BitArray)), Nil) {
  case bit_array.byte_size(bytes) > dirty_bytes {
    True -> object_dirty(bytes)
    False -> object_nif(bytes)
  }
  |> result.replace_error(Nil)
}

pub fn array(bytes: BitArray) -> Result(List(BitArray), Nil) {
  case bit_array.byte_size(bytes) > dirty_bytes {
    True -> array_dirty(bytes)
    False -> array_nif(bytes)
  }
  |> result.replace_error(Nil)
}

pub fn locate(bytes: BitArray, key: String) -> Result(Location, Nil) {
  case bit_array.byte_size(bytes) > dirty_bytes {
    True -> locate_dirty(bytes, key)
    False -> locate_nif(bytes, key)
  }
  |> result.replace_error(Nil)
}

pub fn field(bytes: BitArray, key: String) -> Result(BitArray, Nil) {
  case locate(bytes, key) {
    Ok(Found(value_start:, value_end:, ..)) ->
      bit_array.slice(bytes, value_start, value_end - value_start)
    _ -> Error(Nil)
  }
}

@external(erlang, "fluxer_gateway@nif", "json_scan_object")
fn object_nif(bytes: BitArray) -> Result(List(#(String, BitArray)), ScanError)

@external(erlang, "fluxer_gateway@nif", "json_scan_object_dirty")
fn object_dirty(bytes: BitArray) -> Result(List(#(String, BitArray)), ScanError)

@external(erlang, "fluxer_gateway@nif", "json_scan_array")
fn array_nif(bytes: BitArray) -> Result(List(BitArray), ScanError)

@external(erlang, "fluxer_gateway@nif", "json_scan_array_dirty")
fn array_dirty(bytes: BitArray) -> Result(List(BitArray), ScanError)

@external(erlang, "fluxer_gateway@nif", "json_scan_locate")
fn locate_nif(bytes: BitArray, key: String) -> Result(Location, ScanError)

@external(erlang, "fluxer_gateway@nif", "json_scan_locate_dirty")
fn locate_dirty(bytes: BitArray, key: String) -> Result(Location, ScanError)
