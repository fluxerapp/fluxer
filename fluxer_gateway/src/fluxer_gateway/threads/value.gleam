// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Object =
  List(#(String, JsonTree))

pub fn object_from_tree(tree: JsonTree) -> Object {
  case json_tree.deep(tree) {
    json_tree.Object(fields) -> fields
    _ -> []
  }
}

pub fn expand_object(raw: RawJson) -> Object {
  object_from_tree(json_tree.from_raw(raw))
}

pub fn expand(raw: RawJson) -> JsonTree {
  json_tree.deep(json_tree.from_raw(raw))
}

pub fn to_json(object: Object) -> Json {
  json_tree.to_json(json_tree.Object(object))
}

pub fn to_raw(object: Object) -> RawJson {
  json_tree.to_raw(json_tree.Object(object))
}

pub fn get(object: Object, key: String) -> Result(JsonTree, Nil) {
  list.key_find(object, key)
}

pub fn has(object: Object, key: String) -> Bool {
  result.is_ok(list.key_find(object, key))
}

pub fn insert(object: Object, key: String, item: JsonTree) -> Object {
  list.key_set(object, key, item)
}

pub fn delete(object: Object, key: String) -> Object {
  list.filter(object, fn(pair) { pair.0 != key })
}

pub fn merge(object: Object, update: Object) -> Object {
  list.fold(update, object, fn(acc, pair) { list.key_set(acc, pair.0, pair.1) })
}

pub fn filter_keys(object: Object, keeping: fn(String) -> Bool) -> Object {
  list.filter(object, fn(pair) { keeping(pair.0) })
}

pub fn without(object: Object, keys: List(String)) -> Object {
  filter_keys(object, fn(key) { !list.contains(keys, key) })
}

pub fn with(object: Object, keys: List(String)) -> Object {
  filter_keys(object, list.contains(keys, _))
}

pub fn field(object: Object, key: String) -> Option(JsonTree) {
  list.key_find(object, key) |> option.from_result
}

pub fn object_field(object: Object, key: String) -> Option(Object) {
  case list.key_find(object, key) {
    Ok(json_tree.Object(inner)) -> Some(inner)
    _ -> None
  }
}

pub fn objects(tree: JsonTree) -> List(Object) {
  case tree {
    json_tree.Array(items) ->
      list.filter_map(items, fn(item) {
        case item {
          json_tree.Object(object) -> Ok(object)
          _ -> Error(Nil)
        }
      })
    _ -> []
  }
}

pub fn object_list(object: Object, key: String) -> List(Object) {
  list.key_find(object, key) |> result.map(objects) |> result.unwrap([])
}

pub fn items(object: Object, key: String) -> List(JsonTree) {
  case list.key_find(object, key) {
    Ok(json_tree.Array(values)) -> values
    _ -> []
  }
}

pub fn snowflake(tree: JsonTree) -> Option(Int) {
  json_tree.snowflake(tree) |> option.from_result
}

pub fn snowflake_field(object: Object, key: String) -> Option(Int) {
  list.key_find(object, key)
  |> result.try(json_tree.snowflake)
  |> option.from_result
}

pub fn snowflake_list(object: Object, key: String) -> List(Int) {
  list.filter_map(items(object, key), json_tree.snowflake)
}

pub fn int_field(object: Object, key: String) -> Option(Int) {
  list.key_find(object, key)
  |> result.try(json_tree.loose_int)
  |> option.from_result
}

pub fn string_field(object: Object, key: String) -> Option(String) {
  case list.key_find(object, key) {
    Ok(json_tree.String(text)) -> Some(text)
    _ -> None
  }
}

pub fn is_true(object: Object, key: String) -> Bool {
  list.key_find(object, key) == Ok(json_tree.Bool(True))
}

pub fn id_string(number: Int) -> JsonTree {
  json_tree.String(id.write(number))
}

pub fn as_object(tree: JsonTree) -> Object {
  case tree {
    json_tree.Object(fields) -> fields
    _ -> []
  }
}
