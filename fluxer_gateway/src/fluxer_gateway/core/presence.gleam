// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/option.{type Option, None}

pub type Status {
  Online
  Idle
  Dnd
  Invisible
  Offline
}

pub opaque type Presence {
  Presence(
    user: UserId,
    status: Status,
    mobile: Bool,
    afk: Bool,
    custom_status: Option(RawJson),
    wire: RawJson,
  )
}

pub fn status_from_wire(text: String) -> Result(Status, Nil) {
  case text {
    "" | "online" -> Ok(Online)
    "idle" -> Ok(Idle)
    "dnd" -> Ok(Dnd)
    "invisible" -> Ok(Invisible)
    "offline" -> Ok(Offline)
    _ -> Error(Nil)
  }
}

pub fn status_to_wire(status: Status) -> String {
  case status {
    Online -> "online"
    Idle -> "idle"
    Dnd -> "dnd"
    Invisible -> "invisible"
    Offline -> "offline"
  }
}

pub fn visible_status(status: Status) -> Status {
  case status {
    Invisible -> Offline
    other -> other
  }
}

pub fn is_online(status: Status) -> Bool {
  case visible_status(status) {
    Offline -> False
    _ -> True
  }
}

pub fn new(
  user user: UserId,
  status status: Status,
  mobile mobile: Bool,
  afk afk: Bool,
  custom_status custom_status: Option(RawJson),
  wire wire: RawJson,
) -> Presence {
  Presence(user:, status:, mobile:, afk:, custom_status:, wire:)
}

pub fn offline(user: UserId, wire: RawJson) -> Presence {
  Presence(
    user:,
    status: Offline,
    mobile: False,
    afk: False,
    custom_status: None,
    wire:,
  )
}

pub fn user(presence: Presence) -> UserId {
  presence.user
}

pub fn status(presence: Presence) -> Status {
  presence.status
}

pub fn mobile(presence: Presence) -> Bool {
  presence.mobile
}

pub fn afk(presence: Presence) -> Bool {
  presence.afk
}

pub fn custom_status(presence: Presence) -> Option(RawJson) {
  presence.custom_status
}

pub fn wire(presence: Presence) -> RawJson {
  presence.wire
}
