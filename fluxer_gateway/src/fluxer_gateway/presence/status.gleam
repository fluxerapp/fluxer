// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{
  type Status, Dnd, Idle, Invisible, Offline, Online,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn known(raw: String) -> Result(Status, Nil) {
  case raw {
    "" | "online" -> Ok(Online)
    "idle" -> Ok(Idle)
    "dnd" -> Ok(Dnd)
    "invisible" -> Ok(Invisible)
    "offline" -> Ok(Offline)
    _ -> Error(Nil)
  }
}

pub fn parse_op3(raw: String) -> Result(Status, Nil) {
  case known(raw) {
    Ok(Offline) -> Ok(Invisible)
    other -> other
  }
}

pub fn parse_identify(raw: Option(String)) -> Option(Status) {
  case raw {
    Some("online") -> None
    Some(value) -> option.from_result(known(value))
    None -> None
  }
}

pub fn parse_stored(raw: Option(String)) -> Status {
  raw
  |> option.to_result(Nil)
  |> result.try(known)
  |> result.unwrap(Online)
}

pub fn initial(identify: Option(Status), stored: Status) -> Status {
  option.unwrap(identify, stored)
}

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

pub fn to_session_wire(status: Status) -> String {
  presence.status_to_wire(status)
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
