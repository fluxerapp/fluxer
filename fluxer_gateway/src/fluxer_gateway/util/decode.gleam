// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{
  type ChannelId, type GuildId, type IdError, type RoleId, type UserId,
}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json
import gleam/option.{type Option, None}
import gleam/result

pub fn snowflake() -> Decoder(Int) {
  decode.one_of(decode.int |> decode.then(checked_int), or: [
    decode.string |> decode.then(parsed_string),
  ])
}

pub fn guild_id() -> Decoder(GuildId) {
  typed(id.guild_id, "GuildId")
}

pub fn user_id() -> Decoder(UserId) {
  typed(id.user_id, "UserId")
}

pub fn channel_id() -> Decoder(ChannelId) {
  typed(id.channel_id, "ChannelId")
}

pub fn role_id() -> Decoder(RoleId) {
  typed(id.role_id, "RoleId")
}

pub fn optional_snowflake() -> Decoder(Option(Int)) {
  decode.optional(snowflake())
}

pub fn snowflake_list() -> Decoder(List(Int)) {
  decode.list(snowflake())
}

pub fn decode_json(bytes: BitArray) -> Result(Dynamic, Nil) {
  json.parse_bits(bytes, decode.dynamic)
  |> result.replace_error(Nil)
}

pub fn events(value: Dynamic) -> Result(List(#(String, Dynamic)), Nil) {
  decode.run(
    value,
    decode.list({
      use name <- decode.field("event", decode.string)
      use payload <- decode.field("data", decode.dynamic)
      decode.success(#(name, payload))
    }),
  )
  |> result.replace_error(Nil)
}

pub fn validate_snowflake(value: Dynamic) -> Result(Int, GatewayError) {
  case decode.run(value, decode.optional(decode.dynamic)) {
    Ok(None) -> Error(error.ValidationNullSnowflake)
    _ ->
      decode.run(value, snowflake())
      |> result.replace_error(error.ValidationInvalidSnowflake)
  }
}

pub fn validate_snowflake_list(
  value: Dynamic,
) -> Result(List(Int), GatewayError) {
  case decode.run(value, decode.list(decode.dynamic)) {
    Error(_) -> Error(error.ValidationExpectedList)
    Ok(_) ->
      decode.run(value, snowflake_list())
      |> result.replace_error(error.ValidationInvalidSnowflakeList)
  }
}

fn checked_int(value: Int) -> Decoder(Int) {
  case id.check(value) {
    Ok(valid) -> decode.success(valid)
    Error(_) -> decode.failure(0, "Snowflake")
  }
}

fn parsed_string(text: String) -> Decoder(Int) {
  case id.parse(text) {
    Ok(valid) -> decode.success(valid)
    Error(_) -> decode.failure(0, "Snowflake")
  }
}

fn typed(make: fn(Int) -> Result(a, IdError), name: String) -> Decoder(a) {
  use value <- decode.then(snowflake())
  case make(value) {
    Ok(typed_id) -> decode.success(typed_id)
    Error(_) ->
      case make(1) {
        Ok(placeholder) -> decode.failure(placeholder, name)
        Error(_) -> typed(make, name)
      }
  }
}
