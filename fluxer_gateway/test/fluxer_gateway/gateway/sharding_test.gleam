// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/gateway/sharding
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}

fn guild(value: Int) {
  let assert Ok(guild) = id.guild_id(value)
  guild
}

fn parse(text: String) {
  let assert Ok(outcome) = json.parse(text, sharding.decoder())
  outcome
}

fn absent() {
  let decoder =
    decode.optional_field("shard", Ok(None), sharding.decoder(), decode.success)
  let assert Ok(outcome) = json.parse("{}", decoder)
  outcome
}

pub fn single_shard_keeps_everything_test() {
  assert sharding.guild_in_shard(guild(123_456_789_012_345_678), #(0, 1))
}

pub fn shard_uses_timestamp_bits_test() {
  let value = 5 * 4_194_304
  assert sharding.guild_in_shard(guild(value), #(1, 4))
  assert !sharding.guild_in_shard(guild(value), #(0, 4))
}

pub fn shard_json_test() {
  assert json.to_string(sharding.shard_to_json(#(2, 8))) == "[2,8]"
}

pub fn parse_test() {
  assert absent() == Ok(None)
  assert parse("null") == Ok(None)
  assert parse("[0,1]") == Ok(Some(#(0, 1)))
  assert parse("[3,16384]") == Ok(Some(#(3, 16_384)))
}

pub fn parse_rejects_test() {
  assert parse("[1,1]") == Error(sharding.InvalidShard)
  assert parse("[0,16385]") == Error(sharding.InvalidShard)
  assert parse("[-1,2]") == Error(sharding.InvalidShard)
  assert parse("[0]") == Error(sharding.InvalidShard)
  assert parse("\"0,1\"") == Error(sharding.InvalidShard)
  assert parse("[\"0\",1]") == Error(sharding.InvalidShard)
}
