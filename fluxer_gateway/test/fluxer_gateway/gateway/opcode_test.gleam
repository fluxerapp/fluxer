// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/opcode
import gleam/list

pub fn round_trip_test() {
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 28]
  |> list.each(fn(code) {
    assert opcode.to_int(opcode.from_int(code)) == Ok(code)
  })
}

pub fn unknown_codes_test() {
  assert opcode.from_int(17) == opcode.Unknown
  assert opcode.from_int(-1) == opcode.Unknown
  assert opcode.to_int(opcode.Unknown) == Error(Nil)
}
