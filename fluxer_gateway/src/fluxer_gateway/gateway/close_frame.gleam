// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/string

const max_reason_bytes = 123

pub fn encode(code: Int, reason: String) -> BitArray {
  let payload = <<code:size(16), fit(reason):utf8>>
  <<0x88, bit_array.byte_size(payload), payload:bits>>
}

fn fit(reason: String) -> String {
  case string.byte_size(reason) <= max_reason_bytes {
    True -> reason
    False -> fit(string.drop_end(reason, 1))
  }
}
