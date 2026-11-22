// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/json

fn bits(raw: raw_json.RawJson) -> BitArray {
  raw_json.to_bits(raw)
}

pub fn wrap_keeps_the_binary_test() {
  let bytes = <<"{\"a\":1}">>
  assert bits(raw_json.wrap(bytes)) == bytes
  assert raw_json.byte_size(raw_json.wrap(bytes)) == 7
  assert bits(raw_json.wrap_string("[]")) == <<"[]">>
}

pub fn iodata_flattens_on_demand_test() {
  let raw = raw_json.concat([raw_json.wrap(<<"[1,">>), raw_json.wrap(<<"2]">>)])
  assert raw_json.byte_size(raw) == 5
  assert bits(raw) == <<"[1,2]">>
  assert bits(raw_json.flatten(raw)) == <<"[1,2]">>
  assert bits(raw_json.detach(raw)) == <<"[1,2]">>
  assert bits(raw_json.detach(raw_json.wrap(<<"[3]">>))) == <<"[3]">>
  assert bits(raw_json.from_tree(bytes_tree.from_string("null"))) == <<"null">>
  assert json.to_string(json.array([raw_json.to_json(raw)], fn(x) { x }))
    == "[[1,2]]"
}

pub fn from_json_encodes_once_test() {
  let raw = raw_json.from_json(json.object([#("a", json.int(1))]))
  assert bits(raw) == <<"{\"a\":1}">>
  assert raw_json.read(raw, decode.at(["a"], decode.int)) == Ok(1)
}

pub fn fields_items_and_field_slice_test() {
  let raw = raw_json.wrap(<<"{\"a\":[1,{\"b\":2}],\"c\":\"d\"}">>)
  let assert Ok([#("a", a), #("c", c)]) = raw_json.fields(raw)
  assert bits(c) == <<"\"d\"">>
  let assert Ok([one, two]) = raw_json.items(a)
  assert bits(one) == <<"1">>
  assert bits(two) == <<"{\"b\":2}">>
  let assert Ok(found) = raw_json.field(raw, "c")
  assert bits(found) == <<"\"d\"">>
  assert raw_json.field(raw, "z") == Error(Nil)
}

pub fn with_field_replaces_in_place_or_appends_test() {
  let raw = raw_json.wrap(<<"{\"a\":1,\"b\":2}">>)
  let assert Ok(replaced) =
    raw_json.with_field(raw, "a", raw_json.wrap(<<"9">>))
  assert bits(replaced) == <<"{\"a\":9,\"b\":2}">>
  let assert Ok(added) =
    raw_json.with_field(raw, "q\"", raw_json.wrap(<<"[]">>))
  assert bits(added) == <<"{\"a\":1,\"b\":2,\"q\\\"\":[]}">>
  let assert Ok(first) =
    raw_json.with_field(raw_json.wrap(<<"{ }">>), "z", raw_json.null())
  assert bits(first) == <<"{ \"z\":null}">>
  assert raw_json.with_field(raw_json.wrap(<<"[]">>), "z", raw_json.null())
    == Error(Nil)
}

pub fn replace_field_needs_the_key_test() {
  let raw = raw_json.wrap(<<"{\"a\":1}">>)
  let assert Ok(replaced) =
    raw_json.replace_field(raw, "a", raw_json.wrap(<<"true">>))
  assert bits(replaced) == <<"{\"a\":true}">>
  assert raw_json.replace_field(raw, "b", raw_json.null()) == Error(Nil)
}

pub fn without_field_cuts_one_member_test() {
  let raw = raw_json.wrap(<<"{\"a\":1, \"b\":[2], \"c\":3}">>)
  let assert Ok(middle) = raw_json.without_field(raw, "b")
  assert bits(middle) == <<"{\"a\":1, \"c\":3}">>
  let assert Ok(last) = raw_json.without_field(raw, "c")
  assert bits(last) == <<"{\"a\":1, \"b\":[2]}">>
  let assert Ok(same) = raw_json.without_field(raw, "z")
  assert bits(same) == bits(raw)
  let assert Ok(empty) =
    raw_json.without_field(raw_json.wrap(<<"{\"a\":1}">>), "a")
  assert bits(empty) == <<"{}">>
}

pub fn array_and_object_build_iodata_test() {
  let items = [raw_json.wrap(<<"1">>), raw_json.wrap(<<"{}">>)]
  assert bits(raw_json.array(items)) == <<"[1,{}]">>
  assert bits(raw_json.array([])) == <<"[]">>
  assert bits(
      raw_json.object([#("a", raw_json.null()), #("b", raw_json.array(items))]),
    )
    == <<"{\"a\":null,\"b\":[1,{}]}">>
  assert bits(raw_json.object([])) == <<"{}">>
}
