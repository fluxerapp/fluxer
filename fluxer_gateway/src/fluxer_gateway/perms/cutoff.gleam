// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/iso8601
import gleam/dynamic/decode.{type Decoder}

pub type Cutoff {
  Missing
  At(ms: Int)
  Malformed
}

fn from_text(text: String) -> Cutoff {
  case iso8601.parse_ms(text) {
    Ok(ms) -> At(ms)
    Error(Nil) -> Malformed
  }
}

pub fn decoder() -> Decoder(Cutoff) {
  decode.one_of(decode.map(decode.int, At), or: [
    decode.map(decode.string, from_text),
    decode.success(Missing),
  ])
}
