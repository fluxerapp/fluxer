// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree.{
  Array, Bool, Float, Int, Null, Object, Raw, String,
}
import fluxer_gateway/core/raw_json
import gleam/dynamic
import gleam/list

fn tree(text: String) -> json_tree.JsonTree {
  json_tree.from_raw(raw_json.wrap_string(text))
}

fn text(value: json_tree.JsonTree) -> BitArray {
  raw_json.to_bits(json_tree.to_raw(value))
}

pub fn expand_classifies_one_level_test() {
  let assert Ok(Object([#("a", Raw(a)), #("b", Raw(_))])) =
    json_tree.expand(tree("{\"a\":[1,2],\"b\":{\"c\":3}}"))
  assert raw_json.to_bits(a) == <<"[1,2]">>
  assert json_tree.expand(tree(" \"x\\\"y\" ")) == Ok(String("x\"y"))
  assert json_tree.expand(tree("true")) == Ok(Bool(True))
  assert json_tree.expand(tree("false")) == Ok(Bool(False))
  assert json_tree.expand(tree("null")) == Ok(Null)
  assert json_tree.expand(tree("-12")) == Ok(Int(-12))
  assert json_tree.expand(tree("1.5")) == Ok(Float(1.5))
  assert json_tree.expand(tree("nope")) == Error(Nil)
  assert json_tree.expand(Int(3)) == Ok(Int(3))
}

pub fn field_and_path_stay_lazy_test() {
  let value = tree("{\"a\":{\"b\":[1,\"2\"]},\"id\":\"77\"}")
  let assert Ok(Raw(b)) = json_tree.path(value, ["a", "b"])
  assert raw_json.to_bits(b) == <<"[1,\"2\"]">>
  let assert Ok([one, two]) = json_tree.items(Raw(b))
  assert json_tree.int(one) == Ok(1)
  assert json_tree.snowflake(two) == Ok(2)
  assert json_tree.field(value, "id")
    |> fn(found) {
      case found {
        Ok(id) -> json_tree.snowflake(id)
        Error(Nil) -> Error(Nil)
      }
    }
    == Ok(77)
  assert json_tree.field(value, "missing") == Error(Nil)
  assert json_tree.field(Int(1), "a") == Error(Nil)
}

pub fn set_remove_and_update_splice_bytes_test() {
  let value = tree("{\"a\":1,\"roles\":[\"1\",\"2\",\"3\"]}")
  let assert Ok(set) = json_tree.set(value, "b", String("x"))
  assert text(set) == <<"{\"a\":1,\"roles\":[\"1\",\"2\",\"3\"],\"b\":\"x\"}">>
  let assert Ok(removed) = json_tree.remove(value, "a")
  assert text(removed) == <<"{\"roles\":[\"1\",\"2\",\"3\"]}">>
  let assert Ok(updated) =
    json_tree.update(
      value,
      "roles",
      json_tree.filter(_, fn(item) { json_tree.snowflake(item) != Ok(2) }),
    )
  assert text(updated) == <<"{\"a\":1,\"roles\":[\"1\",\"3\"]}">>
  assert json_tree.update(value, "nope", Ok) == Error(Nil)
}

pub fn built_trees_encode_in_order_test() {
  let value =
    Object([
      #("z", Null),
      #("a", Array([Int(1), Float(2.5), Bool(True), String("s")])),
      #("raw", Raw(raw_json.wrap(<<"{\"k\":[]}">>))),
    ])
  assert text(value)
    == <<"{\"z\":null,\"a\":[1,2.5,true,\"s\"],\"raw\":{\"k\":[]}}">>
  let assert Ok(set) = json_tree.set(value, "z", Bool(False))
  assert json_tree.field(set, "z") == Ok(Bool(False))
  let assert Ok(removed) = json_tree.remove(value, "a")
  assert text(removed) == <<"{\"z\":null,\"raw\":{\"k\":[]}}">>
  assert json_tree.string(String("s")) == Ok("s")
  assert json_tree.bool(Bool(True)) == Ok(True)
  assert json_tree.string(Int(1)) == Error(Nil)
}

@external(erlang, "json", "decode")
fn otp_decode(bytes: BitArray) -> dynamic.Dynamic

pub fn from_term_classifies_decoded_terms_test() {
  let decoded =
    otp_decode(<<"{\"a\":[1,2.5,\"x\",true,null],\"b\":{\"c\":false}}">>)
  let assert Object(fields) = json_tree.from_term(decoded)
  assert list.key_find(fields, "a")
    == Ok(Array([Int(1), Float(2.5), String("x"), Bool(True), Null]))
  assert list.key_find(fields, "b") == Ok(Object([#("c", Bool(False))]))
  assert json_tree.from_term(dynamic.int(7)) == Int(7)
  assert json_tree.from_term(dynamic.nil()) == Null
}

pub fn from_term_encodes_like_the_decoded_map_test() {
  let bytes = <<"{\"id\":\"12\",\"n\":[1,{\"k\":\"v\"}],\"f\":1.5}">>
  let tree = json_tree.from_term(otp_decode(bytes))
  let again = json_tree.from_term(otp_decode(text(tree)))
  assert again == tree
  assert raw_json.to_bits(raw_json.from_json(json_tree.to_json(tree)))
    == text(tree)
}

pub fn snowflake_field_reads_positive_ids_test() {
  let data = tree("{\"id\":\"1310000000000000001\",\"zero\":\"0\",\"n\":7}")
  assert json_tree.snowflake_field(data, "id") == Ok(1_310_000_000_000_000_001)
  assert json_tree.snowflake_field(data, "zero") == Error(Nil)
  assert json_tree.snowflake_field(data, "n") == Ok(7)
  assert json_tree.snowflake_field(data, "missing") == Error(Nil)
}
