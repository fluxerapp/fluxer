// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}

pub type Dispatch {
  Dispatch(to: List(SessionRef), event: String, payload: RawJson)
}

pub type Timer {
  KeepTimer
  ArmTimer
  CancelTimer
}

pub const event_name = "GUILD_MEMBER_LIST_UPDATE"

@internal
pub fn later(first: Timer, next: Timer) -> Timer {
  case next {
    KeepTimer -> first
    ArmTimer | CancelTimer -> next
  }
}
