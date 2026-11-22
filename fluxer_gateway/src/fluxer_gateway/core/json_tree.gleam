// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/string

pub type JsonTree {
  Null
  Bool(Bool)
  Int(Int)
  Float(Float)
  String(String)
  Array(List(JsonTree))
  Object(List(#(String, JsonTree)))
  Raw(RawJson)
}

pub fn from_raw(raw: RawJson) -> JsonTree {
  Raw(raw)
}

pub fn from_term(data: Dynamic) -> JsonTree {
  case dynamic.classify(data) {
    "String" ->
      case bit_array.to_string(coerce(data)) {
        Ok(text) -> String(text)
        Error(Nil) -> Null
      }
    "Int" -> Int(coerce(data))
    "Float" -> Float(coerce(data))
    "Bool" -> Bool(coerce(data))
    "List" -> Array(list.map(coerce(data), from_term))
    "Dict" -> object_term(map_to_list(coerce(data)), [])
    _ -> Null
  }
}

fn object_term(
  pairs: List(#(Dynamic, Dynamic)),
  acc: List(#(String, JsonTree)),
) -> JsonTree {
  case pairs {
    [] -> Object(list.reverse(acc))
    [#(key, item), ..rest] ->
      case dynamic.classify(key) {
        "String" ->
          case bit_array.to_string(coerce(key)) {
            Ok(name) -> object_term(rest, [#(name, from_term(item)), ..acc])
            Error(Nil) -> Null
          }
        _ -> Null
      }
  }
}

@external(erlang, "gleam_stdlib", "identity")
fn coerce(data: Dynamic) -> a

@external(erlang, "maps", "to_list")
fn map_to_list(map: Dynamic) -> List(#(Dynamic, Dynamic))

pub fn expand(tree: JsonTree) -> Result(JsonTree, Nil) {
  case tree {
    Raw(raw) -> classify(raw_json.to_bits(raw))
    other -> Ok(other)
  }
}

fn classify(bytes: BitArray) -> Result(JsonTree, Nil) {
  case bytes {
    <<" ", rest:bits>>
    | <<"\n", rest:bits>>
    | <<"\r", rest:bits>>
    | <<"\t", rest:bits>> -> classify(rest)
    <<"{", _:bits>> ->
      raw_json.fields(raw_json.wrap(bytes))
      |> result.map(fn(fields) {
        Object(list.map(fields, fn(pair) { #(pair.0, Raw(pair.1)) }))
      })
    <<"[", _:bits>> ->
      raw_json.items(raw_json.wrap(bytes))
      |> result.map(list.map(_, Raw))
      |> result.map(Array)
    <<"\"", _:bits>> -> scalar(bytes, decode.map(decode.string, String))
    <<"true", _:bits>> -> scalar(bytes, decode.success(Bool(True)))
    <<"false", _:bits>> -> scalar(bytes, decode.success(Bool(False)))
    <<"null", _:bits>> -> scalar(bytes, decode.success(Null))
    _ ->
      scalar(
        bytes,
        decode.one_of(decode.map(decode.int, Int), [
          decode.map(decode.float, Float),
        ]),
      )
  }
}

fn scalar(
  bytes: BitArray,
  decoder: decode.Decoder(JsonTree),
) -> Result(JsonTree, Nil) {
  json.parse_bits(bytes, decoder) |> result.replace_error(Nil)
}

pub fn deep(tree: JsonTree) -> JsonTree {
  case expand(tree) {
    Ok(Array(items)) -> Array(list.map(items, deep))
    Ok(Object(fields)) ->
      Object(
        list.fold(fields, [], fn(acc, pair) {
          list.key_set(acc, pair.0, deep(pair.1))
        }),
      )
    Ok(scalar) -> scalar
    Error(Nil) -> Null
  }
}

pub fn field(tree: JsonTree, key: String) -> Result(JsonTree, Nil) {
  case tree {
    Raw(raw) -> raw_json.field(raw, key) |> result.map(Raw)
    Object(fields) -> list.key_find(fields, key)
    _ -> Error(Nil)
  }
}

pub fn path(tree: JsonTree, keys: List(String)) -> Result(JsonTree, Nil) {
  list.try_fold(keys, tree, field)
}

pub fn items(tree: JsonTree) -> Result(List(JsonTree), Nil) {
  case expand(tree) {
    Ok(Array(items)) -> Ok(items)
    _ -> Error(Nil)
  }
}

pub fn fields(tree: JsonTree) -> Result(List(#(String, JsonTree)), Nil) {
  case expand(tree) {
    Ok(Object(fields)) -> Ok(fields)
    _ -> Error(Nil)
  }
}

pub fn set(
  tree: JsonTree,
  key: String,
  value: JsonTree,
) -> Result(JsonTree, Nil) {
  case tree {
    Raw(raw) -> raw_json.with_field(raw, key, to_raw(value)) |> result.map(Raw)
    Object(fields) -> Ok(Object(list.key_set(fields, key, value)))
    _ -> Error(Nil)
  }
}

pub fn remove(tree: JsonTree, key: String) -> Result(JsonTree, Nil) {
  case tree {
    Raw(raw) -> raw_json.without_field(raw, key) |> result.map(Raw)
    Object(fields) ->
      Ok(Object(list.filter(fields, fn(pair) { pair.0 != key })))
    _ -> Error(Nil)
  }
}

pub fn update(
  tree: JsonTree,
  key: String,
  change: fn(JsonTree) -> Result(JsonTree, Nil),
) -> Result(JsonTree, Nil) {
  use found <- result.try(field(tree, key))
  use changed <- result.try(change(found))
  set(tree, key, changed)
}

pub fn filter(
  tree: JsonTree,
  keeping: fn(JsonTree) -> Bool,
) -> Result(JsonTree, Nil) {
  items(tree) |> result.map(fn(all) { Array(list.filter(all, keeping)) })
}

pub fn string(tree: JsonTree) -> Result(String, Nil) {
  case expand(tree) {
    Ok(String(text)) -> Ok(text)
    _ -> Error(Nil)
  }
}

pub fn int(tree: JsonTree) -> Result(Int, Nil) {
  case expand(tree) {
    Ok(Int(value)) -> Ok(value)
    _ -> Error(Nil)
  }
}

pub fn loose_int(tree: JsonTree) -> Result(Int, Nil) {
  case expand(tree) {
    Ok(Int(value)) -> Ok(value)
    Ok(String(text)) -> id.parse_decimal(text)
    _ -> Error(Nil)
  }
}

pub fn bool(tree: JsonTree) -> Result(Bool, Nil) {
  case expand(tree) {
    Ok(Bool(flag)) -> Ok(flag)
    _ -> Error(Nil)
  }
}

pub fn snowflake(tree: JsonTree) -> Result(Int, Nil) {
  case expand(tree) {
    Ok(String(text)) -> id.parse(text)
    Ok(Int(value)) -> id.check(value) |> result.replace_error(Nil)
    _ -> Error(Nil)
  }
}

pub fn snowflake_field(tree: JsonTree, key: String) -> Result(Int, Nil) {
  field(tree, key) |> result.try(snowflake)
}

pub fn to_json(tree: JsonTree) -> json.Json {
  case tree {
    Raw(raw) -> raw_json.to_json(raw)
    Null -> json.null()
    Bool(flag) -> json.bool(flag)
    Int(value) -> json.int(value)
    Float(value) -> json.float(value)
    String(text) -> json.string(text)
    Array(items) -> json.preprocessed_array(list.map(items, to_json))
    Object(fields) ->
      json.object(list.map(fields, fn(pair) { #(pair.0, to_json(pair.1)) }))
  }
}

pub fn to_raw(tree: JsonTree) -> RawJson {
  case tree {
    Raw(raw) -> raw
    Null -> raw_json.null()
    Bool(True) -> raw_json.wrap(<<"true">>)
    Bool(False) -> raw_json.wrap(<<"false">>)
    Int(value) -> raw_json.from_json(json.int(value))
    Float(value) -> raw_json.from_json(json.float(value))
    String(text) -> raw_json.from_json(json.string(text))
    Array(items) -> raw_json.array(list.map(items, to_raw))
    Object(fields) ->
      raw_json.object(list.map(fields, fn(pair) { #(pair.0, to_raw(pair.1)) }))
  }
}

pub fn canonical(tree: JsonTree) -> Result(BitArray, Nil) {
  canonical_raw(tree) |> result.map(raw_json.to_bits)
}

fn canonical_raw(tree: JsonTree) -> Result(RawJson, Nil) {
  case tree {
    Raw(raw) ->
      case plain_string(raw_json.to_bits(raw)) {
        True -> Ok(raw)
        False -> expand(tree) |> result.try(canonical_raw)
      }
    Array(items) ->
      list.try_map(items, canonical_raw) |> result.map(raw_json.array)
    Object(fields) ->
      fields
      |> dict.from_list
      |> dict.to_list
      |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
      |> list.try_map(fn(pair) {
        canonical_raw(pair.1) |> result.map(fn(value) { #(pair.0, value) })
      })
      |> result.map(raw_json.object)
    scalar -> Ok(to_raw(scalar))
  }
}

fn plain_string(bytes: BitArray) -> Bool {
  case bytes {
    <<"\"", rest:bits>> -> plain_body(rest)
    _ -> False
  }
}

fn plain_body(bytes: BitArray) -> Bool {
  case bytes {
    <<"\"">> -> True
    <<"\"", _:bits>> -> False
    <<"\\", _:bits>> -> False
    <<byte, _:bits>> if byte < 32 -> False
    <<_, rest:bits>> -> plain_body(rest)
    _ -> False
  }
}
