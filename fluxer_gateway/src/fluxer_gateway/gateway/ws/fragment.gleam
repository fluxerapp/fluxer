// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/ws/frame.{type Opcode}
import gleam/bit_array

pub const max_frames = 1024

pub opaque type Fragment {
  Fragment(opcode: Opcode, payload: BitArray, frames: Int)
}

pub type Extended {
  Partial(Fragment)
  Complete(opcode: Opcode, payload: BitArray)
  TooBig
}

pub fn start(opcode: Opcode, payload: BitArray) -> Fragment {
  Fragment(opcode:, payload:, frames: 1)
}

pub fn extend(
  fragment: Fragment,
  fin: Bool,
  payload: BitArray,
  max_size: Int,
) -> Extended {
  let frames = fragment.frames + 1
  let joined = <<fragment.payload:bits, payload:bits>>
  let too_big = frames > max_frames || bit_array.byte_size(joined) > max_size
  case too_big, fin {
    True, _ -> TooBig
    False, True -> Complete(opcode: fragment.opcode, payload: joined)
    False, False -> Partial(Fragment(..fragment, payload: joined, frames:))
  }
}
