// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int

pub const oldest_protocol = 1

pub const current_protocol = 3

pub const sync_answer_protocol = 2

pub const fresh_answer_protocol = 3

pub type ProtocolRange {
  ProtocolRange(oldest: Int, current: Int)
}

pub fn own_range() -> ProtocolRange {
  ProtocolRange(oldest: oldest_protocol, current: current_protocol)
}

pub fn negotiate(
  ours: ProtocolRange,
  theirs: ProtocolRange,
) -> Result(Int, Nil) {
  let version = int.min(ours.current, theirs.current)
  case version >= ours.oldest && version >= theirs.oldest {
    True -> Ok(version)
    False -> Error(Nil)
  }
}
