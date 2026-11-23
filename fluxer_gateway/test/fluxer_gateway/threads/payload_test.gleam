// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/threads/payload
import fluxer_gateway/threads/runtime
import fluxer_gateway/threads/value.{type Object}
import gleam/list
import gleam/option.{None}
import gleeunit/should

fn object(pairs: List(#(String, JsonTree))) -> Object {
  pairs
}

fn should_match(found: Object, expected: Object) -> Nil {
  json_tree.canonical(json_tree.Object(found))
  |> should.equal(json_tree.canonical(json_tree.Object(expected)))
}

fn to_tree(object: Object) -> JsonTree {
  value.to_raw(object) |> json_tree.from_raw |> expand_all
}

fn expand_all(tree: JsonTree) -> JsonTree {
  case json_tree.expand(tree) {
    Ok(json_tree.Object(fields)) ->
      json_tree.Object(
        list.map(fields, fn(pair) { #(pair.0, expand_all(pair.1)) }),
      )
    Ok(json_tree.Array(items)) -> json_tree.Array(list.map(items, expand_all))
    Ok(other) -> other
    Error(Nil) -> tree
  }
}

pub fn mask_role_strips_thread_bits_test() {
  let role = object([#("permissions", json_tree.String("274877907968"))])
  payload.mask_role(role)
  |> should_match(object([#("permissions", json_tree.String("1024"))]))
}

pub fn mask_message_drops_thread_marks_test() {
  let message =
    object([
      #("thread", json_tree.Object(object([]))),
      #("flags", json_tree.Int(0x121)),
      #(
        "referenced_message",
        json_tree.Object(object([#("type", json_tree.Int(18))])),
      ),
    ])
  payload.mask("MESSAGE_CREATE", to_tree(message))
  |> value.object_from_tree
  |> should_match(
    object([
      #("flags", json_tree.Int(1)),
      #("referenced_message", json_tree.Null),
    ]),
  )
}

pub fn strip_channel_surface_test() {
  let forum =
    object([
      #("id", json_tree.String("1")),
      #("available_tags", json_tree.Array([])),
      #("flags", json_tree.Int(2)),
    ])
  payload.mask_channel(forum)
  |> should_match(object([#("id", json_tree.String("1"))]))
}

pub fn strip_internal_test() {
  object([
    #("_fluxer_members", json_tree.Array([])),
    #("__thread_scoped", json_tree.String("1")),
    #("id", json_tree.String("1")),
  ])
  |> payload.strip_internal
  |> should_match(object([#("id", json_tree.String("1"))]))
}

pub fn starter_source_test() {
  let starter =
    object([
      #("type", json_tree.Int(21)),
      #("referenced_message", json_tree.Object(object([]))),
      #(
        "message_reference",
        json_tree.Object(
          object([
            #("channel_id", json_tree.String("5")),
            #("message_id", json_tree.String("9")),
          ]),
        ),
      ),
    ])
  case id.channel_id(5) {
    Ok(parent) ->
      payload.starter_source("MESSAGE_CREATE", to_tree(starter))
      |> should.equal(payload.ParentSource(parent, 9))
    Error(_) -> should.fail()
  }
  payload.hide_unreadable_source(
    "MESSAGE_CREATE",
    to_tree(starter),
    fn(_) { permissions.none() },
    None,
  )
  |> value.object_from_tree
  |> value.get("referenced_message")
  |> should.equal(Ok(json_tree.Null))
}

pub fn backoff_test() {
  runtime.retry_delay(0) |> should.equal(0)
  runtime.retry_delay(1) |> should.equal(30_000)
  runtime.retry_delay(9) |> should.equal(600_000)
}

pub fn mask_leaves_plain_messages_untouched_test() {
  let message =
    json_tree.from_raw(raw_json.wrap_string(
      "{\"id\":\"1\",\"flags\":4,\"content\":\"hi\",\"referenced_message\":null}",
    ))
    |> expand_all
  assert payload.mask("MESSAGE_CREATE", message) == message
  assert payload.mask("TYPING_START", message) == message
}

pub fn mask_raw_channel_matches_object_mask_test() {
  let wire =
    "{\"id\":\"3\",\"available_tags\":[],\"permission_overwrites\":[{\"id\":\"9\",\"allow\":\"274877907968\",\"deny\":\"0\"}]}"
  let masked =
    json_tree.from_raw(raw_json.wrap_string(wire))
    |> payload.mask_channel_tree
    |> json_tree.to_raw
    |> json_tree.from_raw
    |> value.object_from_tree
  let expected =
    json_tree.from_raw(raw_json.wrap_string(wire))
    |> value.object_from_tree
    |> payload.mask_channel
  masked |> should_match(expected)
}
