// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nif
import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import gleam/list
import gleam/string

const prefix = "fluxer_gateway_nif_"

@external(erlang, "fluxer_gateway@nif", "gateway_nif_metrics")
fn gateway_nif_metrics() -> List(#(String, Int))

fn read() -> List(#(String, Int)) {
  case nif.is_loaded() {
    True -> gateway_nif_metrics()
    False -> []
  }
}

pub fn samples() -> List(Sample) {
  read()
  |> list.map(fn(entry) {
    let #(key, value) = entry
    Sample(prefix <> sanitise(key), [], value)
  })
}

fn sanitise(key: String) -> String {
  sanitise_bits(<<key:utf8>>, "")
}

fn sanitise_bits(bytes: BitArray, acc: String) -> String {
  case bytes {
    <<code:utf8_codepoint, rest:bits>> -> {
      let char = string.from_utf_codepoints([code])
      case is_name_char(char) {
        True -> sanitise_bits(rest, acc <> char)
        False -> sanitise_bits(rest, acc <> "_")
      }
    }
    _ -> acc
  }
}

fn is_name_char(grapheme: String) -> Bool {
  case <<grapheme:utf8>> {
    <<code>> ->
      { code >= 48 && code <= 57 }
      || { code >= 65 && code <= 90 }
      || { code >= 97 && code <= 122 }
      || code == 95
    _ -> False
  }
}
