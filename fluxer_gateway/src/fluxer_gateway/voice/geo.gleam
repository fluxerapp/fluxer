// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/option.{type Option, None, Some}

pub opaque type Geo {
  Known(latitude: String, longitude: String)
  Unknown
}

pub fn unknown() -> Geo {
  Unknown
}

pub fn new(latitude: Option(String), longitude: Option(String)) -> Geo {
  case latitude, longitude {
    Some(latitude), Some(longitude) -> Known(latitude:, longitude:)
    _, _ -> Unknown
  }
}

pub fn decoder() -> Decoder(Geo) {
  let located = {
    use latitude <- decode.optional_field("latitude", None, coordinate())
    use longitude <- decode.optional_field("longitude", None, coordinate())
    decode.success(new(latitude, longitude))
  }
  decode.one_of(located, [decode.success(Unknown)])
}

pub fn is_known(geo: Geo) -> Bool {
  geo != Unknown
}

pub fn or(geo: Geo, fallback: Geo) -> Geo {
  case geo {
    Known(..) -> geo
    Unknown -> fallback
  }
}

pub fn coordinates(geo: Geo) -> Option(#(String, String)) {
  case geo {
    Known(latitude:, longitude:) -> Some(#(latitude, longitude))
    Unknown -> None
  }
}

pub fn coordinate() -> Decoder(Option(String)) {
  decode.one_of(decode.map(decode.string, Some), [
    decode.map(decode.int, fn(number) { Some(int.to_string(number)) }),
    decode.map(decode.float, fn(number) { Some(float_to_short(number)) }),
    decode.success(None),
  ])
}

@external(erlang, "erlang", "float_to_binary")
fn float_to_binary(value: Float, options: List(ShortOption)) -> String

type ShortOption {
  Short
}

fn float_to_short(value: Float) -> String {
  float_to_binary(value, [Short])
}
