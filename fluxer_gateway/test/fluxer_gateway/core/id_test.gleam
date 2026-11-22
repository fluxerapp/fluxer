// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import gleam/json

pub fn parse_accepts_nineteen_digits_up_to_max_test() {
  assert id.parse("9223372036854775807") == Ok(9_223_372_036_854_775_807)
  assert id.parse_bits(<<"1310000000000000001">>)
    == Ok(1_310_000_000_000_000_001)
}

pub fn parse_rejects_what_is_not_a_snowflake_test() {
  assert id.parse("9223372036854775808") == Error(Nil)
  assert id.parse("10000000000000000000") == Error(Nil)
  assert id.parse("0") == Error(Nil)
  assert id.parse("012") == Error(Nil)
  assert id.parse("-5") == Error(Nil)
  assert id.parse("+5") == Error(Nil)
  assert id.parse("12a") == Error(Nil)
  assert id.parse("") == Error(Nil)
  assert id.parse_bits(<<"\"12\"">>) == Error(Nil)
}

pub fn typed_parse_keeps_the_reason_test() {
  assert id.parse_guild_id("x") == Error(id.NotDecimal("x"))
  assert id.parse_user_id("9223372036854775808")
    == Error(id.OutOfRange(9_223_372_036_854_775_808))
}

pub fn write_is_the_inverse_of_parse_test() {
  assert id.write(1_310_000_000_000_000_001) == "1310000000000000001"
  assert id.parse(id.write(9_223_372_036_854_775_807))
    == Ok(9_223_372_036_854_775_807)
  assert json.to_string(id.json(42)) == "\"42\""
}

pub fn to_string_uses_the_writer_test() {
  let assert Ok(guild) = id.guild_id(77)
  assert id.guild_id_to_string(guild) == "77"
}
