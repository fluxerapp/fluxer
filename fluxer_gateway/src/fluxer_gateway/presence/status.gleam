// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{
  type Status, Dnd, Idle, Invisible, Offline, Online,
}
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn settings_invisible(raw: Option(String)) -> Option(Bool) {
  case raw {
    Some("online") | Some("idle") | Some("dnd") -> Some(False)
    Some("invisible") | Some("offline") -> Some(True)
    _ -> None
  }
}

pub fn to_wire(status: Status) -> String {
  case status {
    Online -> "online"
    Idle -> "idle"
    Dnd -> "dnd"
    Invisible | Offline -> "offline"
  }
}

pub fn is_visible(status: Status) -> Bool {
  case status {
    Online | Idle | Dnd -> True
    Invisible | Offline -> False
  }
}

pub fn precedence(statuses: List(Status)) -> Status {
  list.fold(statuses, Offline, fn(best, status) { stronger(status, best) })
}

pub fn stronger(status: Status, best: Status) -> Status {
  case rank(status) > rank(best) {
    True -> status
    False -> best
  }
}

fn rank(status: Status) -> Int {
  case status {
    Dnd -> 4
    Online -> 3
    Idle -> 2
    Invisible -> 1
    Offline -> 0
  }
}
