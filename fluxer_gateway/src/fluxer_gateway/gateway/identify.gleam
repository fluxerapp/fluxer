// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{
  type ClientProperties, type Identify, type IdentifyFlags, ClientProperties,
  Identify, IdentifyFlags,
}
import fluxer_gateway/gateway/sharding
import fluxer_gateway/voice/geo
import gleam/bit_array
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

const max_token_bytes = 256

const max_ignored_events = 256

const flag_debounce_message_reactions = 0x2

const flag_channel_threads = 0x4

const flag_guild_visits = 0x8

pub type IdentifyError {
  InvalidPayload
  InvalidToken
  InvalidShard
}

pub fn decoder() -> Decoder(Result(Identify, IdentifyError)) {
  decode.one_of(fields(), [decode.success(Error(InvalidPayload))])
}

type Slot(a) {
  Missing
  Invalid
  Present(a)
}

type Properties {
  Properties(
    client: ClientProperties,
    e2ee_capable: Bool,
    mobile: Bool,
    latitude: Option(String),
    longitude: Option(String),
  )
}

fn slot(
  name: String,
  inner: Decoder(a),
  next: fn(Slot(a)) -> Decoder(b),
) -> Decoder(b) {
  decode.optional_field(
    name,
    Missing,
    decode.one_of(decode.map(inner, Present), [decode.success(Invalid)]),
    next,
  )
}

fn fields() -> Decoder(Result(Identify, IdentifyError)) {
  use token <- slot("token", decode.string)
  use properties <- slot("properties", properties())
  use presence <- decode.optional_field(
    "presence",
    None,
    decode.optional(decode.dynamic),
  )
  use presence_mobile <- decode.optional_field(
    "presence",
    None,
    decode.optional(mobile_flag()),
  )
  use ignored_events <- decode.optional_field(
    "ignored_events",
    Ok([]),
    ignored_events(),
  )
  use flags <- decode.optional_field("flags", Ok(0), flags())
  use initial_guild_id <- decode.optional_field(
    "initial_guild_id",
    None,
    initial_guild_id(),
  )
  use shard <- decode.optional_field("shard", Ok(None), sharding.decoder())
  decode.success({
    use token <- result.try(required(token))
    use properties <- result.try(required(properties))
    use token <- result.try(validate_token(token))
    use properties <- result.try(valid_properties(properties))
    use ignored_events <- result.try(ignored_events)
    use flags <- result.try(flags)
    use shard <- result.try(shard |> result.replace_error(InvalidShard))
    Ok(Identify(
      token:,
      properties: properties.client,
      shard:,
      presence:,
      flags: flags_from_int(flags),
      ignored_events:,
      initial_guild_id:,
      e2ee_capable: properties.e2ee_capable,
      mobile: option.unwrap(presence_mobile, properties.mobile),
      latitude: properties.latitude,
      longitude: properties.longitude,
    ))
  })
}

fn required(value: Slot(a)) -> Result(Slot(a), IdentifyError) {
  case value {
    Missing -> Error(InvalidPayload)
    Invalid | Present(_) -> Ok(value)
  }
}

fn valid_properties(
  value: Slot(Properties),
) -> Result(Properties, IdentifyError) {
  case value {
    Present(properties) -> Ok(properties)
    Missing | Invalid -> Error(InvalidPayload)
  }
}

fn validate_token(raw: Slot(String)) -> Result(String, IdentifyError) {
  case raw {
    Present(token) ->
      case is_valid_token(token) {
        True -> Ok(token)
        False -> Error(InvalidToken)
      }
    Missing | Invalid -> Error(InvalidToken)
  }
}

fn is_valid_token(token: String) -> Bool {
  let bytes = bit_array.from_string(token)
  bit_array.byte_size(bytes) <= max_token_bytes && has_visible_byte(bytes)
}

fn has_visible_byte(bytes: BitArray) -> Bool {
  case bytes {
    <<byte, _:bytes>> if byte > 32 -> True
    <<_, rest:bytes>> -> has_visible_byte(rest)
    _ -> False
  }
}

fn properties() -> Decoder(Properties) {
  use os <- decode.field("os", decode.string)
  use browser <- decode.field("browser", decode.string)
  use device <- decode.field("device", decode.string)
  use e2ee_capable <- decode.optional_field("e2ee_capable", False, is_true())
  use mobile <- decode.optional_field("mobile", False, is_true())
  use latitude <- decode.optional_field("latitude", None, geo.coordinate())
  use longitude <- decode.optional_field("longitude", None, geo.coordinate())
  decode.success(Properties(
    client: ClientProperties(os:, browser:, device:),
    e2ee_capable:,
    mobile:,
    latitude:,
    longitude:,
  ))
}

fn is_true() -> Decoder(Bool) {
  decode.one_of(decode.bool, [decode.success(False)])
}

fn mobile_flag() -> Decoder(Bool) {
  decode.optional_field("mobile", False, is_true(), decode.success)
  |> decode.one_of([decode.success(False)])
}

fn ignored_events() -> Decoder(Result(List(String), IdentifyError)) {
  decode.optional(decode.list(decode.string))
  |> decode.map(fn(events) {
    case events {
      None -> Ok([])
      Some(events) ->
        case list.length(events) <= max_ignored_events {
          True ->
            events
            |> list.map(string.uppercase)
            |> list.sort(string.compare)
            |> list.unique
            |> Ok
          False -> Error(InvalidPayload)
        }
    }
  })
  |> decode.one_of([decode.success(Error(InvalidPayload))])
}

fn flags() -> Decoder(Result(Int, IdentifyError)) {
  decode.int
  |> decode.map(fn(flags) {
    case flags >= 0 {
      True -> Ok(flags)
      False -> Error(InvalidPayload)
    }
  })
  |> decode.one_of([decode.success(Error(InvalidPayload))])
}

fn initial_guild_id() -> Decoder(Option(id.GuildId)) {
  decode.string
  |> decode.map(fn(text) { id.parse_guild_id(text) |> option.from_result })
  |> decode.one_of([decode.success(None)])
}

fn flags_from_int(flags: Int) -> IdentifyFlags {
  IdentifyFlags(
    debounce_message_reactions: has_flag(flags, flag_debounce_message_reactions),
    channel_threads: has_flag(flags, flag_channel_threads),
    guild_visits: has_flag(flags, flag_guild_visits),
  )
}

fn has_flag(flags: Int, flag: Int) -> Bool {
  int.bitwise_and(flags, flag) != 0
}
