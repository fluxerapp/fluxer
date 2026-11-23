// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/user
import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}

const default_presence = <<
  "{\"afk\":false,\"mobile\":false,\"status\":\"offline\"}":utf8,
>>

pub fn encode(
  source: Member,
  found: Option(Presence),
  connected: Bool,
) -> #(BitArray, BitArray) {
  let page_bytes = raw_json.to_bits(member.wire(source))
  let presence_bytes = case found {
    Some(shown) ->
      case connected && presence.is_online(presence.status(shown)) {
        True -> raw_json.to_bits(presence.wire(shown))
        False -> default_presence
      }
    None -> default_presence
  }
  #(list_bytes(page_bytes, presence_bytes), page_bytes)
}

pub fn encode_raw(
  source: Member,
  shown: Option(RawJson),
) -> #(BitArray, BitArray) {
  let page_bytes = raw_json.to_bits(member.wire(source))
  let presence_bytes = case shown {
    Some(value) -> raw_json.to_bits(value)
    None -> default_presence
  }
  #(list_bytes(page_bytes, presence_bytes), page_bytes)
}

pub fn display_name(source: Member) -> BitArray {
  case member.nick(source) {
    Some(nick) if nick != "" -> bit_array.from_string(nick)
    _ ->
      case user.name(member.user(source)) {
        Some(name) -> bit_array.from_string(name)
        None -> user_name(user.wire(member.user(source)))
      }
  }
}

fn list_bytes(member_bytes: BitArray, presence_bytes: BitArray) -> BitArray {
  let presence_field =
    bit_array.concat([<<"\"presence\":":utf8>>, presence_bytes])
  let body = case member_bytes {
    <<"{}":utf8>> ->
      bit_array.concat([<<"{":utf8>>, presence_field, <<"}":utf8>>])
    _ ->
      case strip_closing(member_bytes) {
        Ok(open) ->
          bit_array.concat([open, <<",":utf8>>, presence_field, <<"}":utf8>>])
        Error(Nil) -> member_bytes
      }
  }
  bit_array.concat([<<"{\"member\":":utf8>>, body, <<"}":utf8>>])
}

fn strip_closing(bytes: BitArray) -> Result(BitArray, Nil) {
  let size = bit_array.byte_size(bytes)
  case bit_array.slice(bytes, size - 1, 1) {
    Ok(<<"}":utf8>>) -> bit_array.slice(bytes, 0, size - 1)
    _ -> Error(Nil)
  }
}

fn user_name(wire: RawJson) -> BitArray {
  let name =
    decode.one_of(decode.string, or: [
      decode.map(decode.int, int.to_string),
      decode.success(""),
    ])
  let decoder = {
    use global <- decode.optional_field("global_name", "", name)
    use username <- decode.optional_field("username", "", name)
    decode.success(case global {
      "" -> username
      _ -> global
    })
  }
  case raw_json.read(wire, decoder) {
    Ok(found) -> bit_array.from_string(found)
    Error(_) -> <<>>
  }
}
