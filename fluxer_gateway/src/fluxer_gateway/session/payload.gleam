// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/util/decode as edge
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/result

pub fn read(raw: RawJson, decoder: Decoder(a)) -> Result(a, Nil) {
  raw
  |> raw_json.to_bits
  |> json.parse_bits(decoder)
  |> result.replace_error(Nil)
}

pub fn id() -> Decoder(Option(Int)) {
  decode.one_of(edge.snowflake() |> decode.map(positive), [
    decode.success(None),
  ])
}

fn positive(value: Int) -> Option(Int) {
  case value > 0 {
    True -> Some(value)
    False -> None
  }
}

pub fn id_field(name: String) -> Decoder(Option(Int)) {
  decode.optional_field(name, None, id(), decode.success)
}

pub fn nested_id(outer: String) -> Decoder(Option(Int)) {
  decode.optional_field(
    outer,
    None,
    decode.one_of(id_field("id"), [decode.success(None)]),
    decode.success,
  )
}

pub fn int_field(name: String) -> Decoder(Option(Int)) {
  decode.optional_field(
    name,
    None,
    decode.one_of(decode.int |> decode.map(Some), [decode.success(None)]),
    decode.success,
  )
}

pub fn bool_field(name: String) -> Decoder(Bool) {
  decode.optional_field(
    name,
    False,
    decode.one_of(decode.bool, [decode.success(False)]),
    decode.success,
  )
}

pub fn raw() -> Decoder(RawJson) {
  decode.dynamic |> decode.map(dynamic_to_raw)
}

pub fn raw_field(name: String) -> Decoder(Option(RawJson)) {
  decode.optional_field(name, None, decode.optional(raw()), decode.success)
}

pub fn dynamic_to_raw(value: Dynamic) -> RawJson {
  value |> encode_term |> raw_json.from_json
}

pub fn raw_to_dynamic(raw: RawJson) -> Result(Dynamic, Nil) {
  read(raw, decode.dynamic)
}

@external(erlang, "json", "encode")
fn encode_term(value: Dynamic) -> Json

pub fn with_field(raw: RawJson, key: String, value: Json) -> RawJson {
  let field =
    raw_json.concat([
      raw_json.from_json(json.string(key)),
      raw_json.wrap(<<":">>),
      raw_json.from_json(value),
    ])
  case raw_json.to_bits(raw) {
    <<"{}">> ->
      raw_json.concat([raw_json.wrap(<<"{">>), field, raw_json.wrap(<<"}">>)])
    <<"{", rest:bits>> ->
      raw_json.concat([
        raw_json.wrap(<<"{">>),
        field,
        raw_json.wrap(<<",">>),
        raw_json.wrap(rest),
      ])
    _ -> raw
  }
}

pub fn mark_available(body: RawJson) -> RawJson {
  raw_json.with_field(body, "unavailable", raw_json.wrap(<<"false">>))
  |> result.unwrap(body)
}

pub fn text(value: String) -> RawJson {
  raw_json.from_json(json.string(value))
}
