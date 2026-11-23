// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_scan.{Absent, Found}
import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/result

pub opaque type RawJson {
  RawJson(data: BytesTree)
}

pub fn wrap(bytes: BitArray) -> RawJson {
  RawJson(bits_tree(bytes))
}

pub fn wrap_string(text: String) -> RawJson {
  RawJson(string_tree(text))
}

pub fn from_tree(tree: BytesTree) -> RawJson {
  RawJson(tree)
}

pub fn from_json(value: Json) -> RawJson {
  RawJson(string_tree(json.to_string(value)))
}

pub fn concat(parts: List(RawJson)) -> RawJson {
  RawJson(bytes_tree.concat(list.map(parts, to_iodata)))
}

pub fn detach(raw: RawJson) -> RawJson {
  case is_binary(raw.data) {
    True -> RawJson(copy(raw.data))
    False -> flatten(raw)
  }
}

pub fn detach_string(text: String) -> String {
  copy_string(text)
}

pub fn flatten(raw: RawJson) -> RawJson {
  RawJson(bits_tree(to_bits(raw)))
}

pub fn null() -> RawJson {
  wrap(<<"null">>)
}

pub fn to_json(raw: RawJson) -> Json {
  tree_json(raw.data)
}

pub fn to_iodata(raw: RawJson) -> BytesTree {
  raw.data
}

pub fn to_bits(raw: RawJson) -> BitArray {
  iolist_to_binary(raw.data)
}

pub fn byte_size(raw: RawJson) -> Int {
  bytes_tree.byte_size(raw.data)
}

pub fn read(raw: RawJson, decoder: Decoder(a)) -> Result(a, Nil) {
  json.parse_bits(to_bits(raw), decoder) |> result.replace_error(Nil)
}

pub fn string(raw: RawJson) -> Result(String, Nil) {
  let bytes = to_bits(raw)
  case bytes {
    <<"\"", _:bits>> ->
      case unescaped(bytes) {
        Ok(inner) -> bit_array.to_string(inner)
        Error(Nil) -> read(raw, decode.string)
      }
    _ -> Error(Nil)
  }
}

pub fn snowflake(raw: RawJson) -> Result(Int, Nil) {
  let bytes = to_bits(raw)
  case bytes {
    <<"\"", _:bits>> ->
      case unescaped(bytes) {
        Ok(digits) -> id.parse_bits(digits)
        Error(Nil) -> read(raw, decode.string) |> result.try(id.parse)
      }
    _ -> id.parse_bits(bytes)
  }
}

fn unescaped(quoted: BitArray) -> Result(BitArray, Nil) {
  let size = bit_array.byte_size(quoted)
  use inner <- result.try(bit_array.slice(quoted, 1, size - 2))
  case bit_array.slice(quoted, size - 1, 1), has_escape(inner) {
    Ok(<<"\"">>), False -> Ok(inner)
    _, _ -> Error(Nil)
  }
}

fn has_escape(bytes: BitArray) -> Bool {
  case bytes {
    <<>> -> False
    <<"\\", _:bits>> | <<"\"", _:bits>> -> True
    <<byte, _:bits>> if byte < 32 -> True
    <<_, rest:bits>> -> has_escape(rest)
    _ -> True
  }
}

pub fn fields(raw: RawJson) -> Result(List(#(String, RawJson)), Nil) {
  json_scan.object(to_bits(raw))
  |> result.map(list.map(_, fn(pair) { #(pair.0, wrap(pair.1)) }))
}

pub fn items(raw: RawJson) -> Result(List(RawJson), Nil) {
  json_scan.array(to_bits(raw)) |> result.map(list.map(_, wrap))
}

pub fn field(raw: RawJson, key: String) -> Result(RawJson, Nil) {
  json_scan.field(to_bits(raw), key) |> result.map(wrap)
}

pub fn array(items: List(RawJson)) -> RawJson {
  [wrap(<<"[">>), ..list.intersperse(items, wrap(<<",">>))]
  |> list.append([wrap(<<"]">>)])
  |> concat
}

pub fn object(fields: List(#(String, RawJson))) -> RawJson {
  let members =
    list.map(fields, fn(pair) { concat([key(pair.0), pair.1]) })
    |> list.intersperse(wrap(<<",">>))
  concat([wrap(<<"{">>), ..list.append(members, [wrap(<<"}">>)])])
}

pub fn with_field(
  raw: RawJson,
  key name: String,
  value value: RawJson,
) -> Result(RawJson, Nil) {
  let bytes = to_bits(raw)
  use location <- result.try(json_scan.locate(bytes, name))
  case location {
    Found(value_start:, value_end:, ..) ->
      splice(bytes, value_start, value_end, [value])
    Absent(close:, empty:) -> {
      let separator = case empty {
        True -> wrap(<<>>)
        False -> wrap(<<",">>)
      }
      splice(bytes, close, close, [separator, key(name), value])
    }
  }
}

pub fn replace_field(
  raw: RawJson,
  key name: String,
  value value: RawJson,
) -> Result(RawJson, Nil) {
  let bytes = to_bits(raw)
  case json_scan.locate(bytes, name) {
    Ok(Found(value_start:, value_end:, ..)) ->
      splice(bytes, value_start, value_end, [value])
    _ -> Error(Nil)
  }
}

pub fn without_field(raw: RawJson, key name: String) -> Result(RawJson, Nil) {
  let bytes = to_bits(raw)
  use location <- result.try(json_scan.locate(bytes, name))
  case location {
    Found(cut_start:, cut_end:, ..) -> splice(bytes, cut_start, cut_end, [])
    Absent(..) -> Ok(raw)
  }
}

fn key(name: String) -> RawJson {
  concat([from_json(json.string(name)), wrap(<<":">>)])
}

fn splice(
  bytes: BitArray,
  from: Int,
  to: Int,
  insert: List(RawJson),
) -> Result(RawJson, Nil) {
  use head <- result.try(bit_array.slice(bytes, 0, from))
  use tail <- result.map(bit_array.slice(
    bytes,
    to,
    bit_array.byte_size(bytes) - to,
  ))
  concat([wrap(head), ..list.append(insert, [wrap(tail)])])
}

@external(erlang, "gleam_stdlib", "identity")
fn bits_tree(bytes: BitArray) -> BytesTree

@external(erlang, "gleam_stdlib", "identity")
fn string_tree(text: String) -> BytesTree

@external(erlang, "gleam_stdlib", "identity")
fn tree_json(tree: BytesTree) -> Json

@external(erlang, "erlang", "iolist_to_binary")
fn iolist_to_binary(tree: BytesTree) -> BitArray

@external(erlang, "erlang", "is_binary")
fn is_binary(tree: BytesTree) -> Bool

@external(erlang, "binary", "copy")
fn copy(tree: BytesTree) -> BytesTree

@external(erlang, "binary", "copy")
fn copy_string(text: String) -> String
