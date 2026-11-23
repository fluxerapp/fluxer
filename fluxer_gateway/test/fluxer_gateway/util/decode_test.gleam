// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/id
import fluxer_gateway/util/decode as edge
import gleam/dynamic
import gleam/dynamic/decode
import gleam/option.{None, Some}

pub fn snowflake_accepts_positive_integer_test() {
  assert decode.run(dynamic.int(42), edge.snowflake()) == Ok(42)
}

pub fn snowflake_accepts_decimal_string_test() {
  assert decode.run(dynamic.string("1234567890123"), edge.snowflake())
    == Ok(1_234_567_890_123)
}

pub fn snowflake_rejects_zero_and_negative_test() {
  let assert Error(_) = decode.run(dynamic.int(0), edge.snowflake())
  let assert Error(_) = decode.run(dynamic.int(-5), edge.snowflake())
  let assert Error(_) = decode.run(dynamic.string("0"), edge.snowflake())
}

pub fn snowflake_rejects_leading_zero_and_junk_test() {
  let assert Error(_) = decode.run(dynamic.string("012"), edge.snowflake())
  let assert Error(_) = decode.run(dynamic.string("12a"), edge.snowflake())
  let assert Error(_) = decode.run(dynamic.string(""), edge.snowflake())
  let assert Error(_) = decode.run(dynamic.string("+12"), edge.snowflake())
}

pub fn snowflake_rejects_above_max_test() {
  let assert Error(_) =
    decode.run(dynamic.string("9223372036854775808"), edge.snowflake())
  assert decode.run(dynamic.string("9223372036854775807"), edge.snowflake())
    == Ok(9_223_372_036_854_775_807)
}

pub fn guild_id_decodes_test() {
  let assert Ok(guild) = decode.run(dynamic.string("77"), edge.guild_id())
  assert id.guild_id_to_int(guild) == 77
}

pub fn optional_snowflake_test() {
  assert decode.run(dynamic.nil(), edge.optional_snowflake()) == Ok(None)
  assert decode.run(dynamic.string("9"), edge.optional_snowflake())
    == Ok(Some(9))
}

pub fn snowflake_list_test() {
  let items = dynamic.list([dynamic.int(1), dynamic.string("2")])
  assert decode.run(items, edge.snowflake_list()) == Ok([1, 2])
}

pub fn validate_snowflake_reasons_test() {
  assert edge.validate_snowflake(dynamic.nil())
    == Error(error.ValidationNullSnowflake)
  assert edge.validate_snowflake(dynamic.string("x"))
    == Error(error.ValidationInvalidSnowflake)
  assert edge.validate_snowflake(dynamic.int(5)) == Ok(5)
}

pub fn validate_snowflake_list_reasons_test() {
  assert edge.validate_snowflake_list(dynamic.int(1))
    == Error(error.ValidationExpectedList)
  assert edge.validate_snowflake_list(
      dynamic.list([dynamic.int(1), dynamic.string("bad")]),
    )
    == Error(error.ValidationInvalidSnowflakeList)
}

pub fn decode_json_test() {
  let assert Ok(value) = edge.decode_json(<<"{\"a\":\"5\"}":utf8>>)
  assert decode.run(value, decode.at(["a"], edge.snowflake())) == Ok(5)
  assert edge.decode_json(<<"{nope":utf8>>) == Error(Nil)
}
