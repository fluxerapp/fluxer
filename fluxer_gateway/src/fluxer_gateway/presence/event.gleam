// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{type DispatchFacts}
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/presence/display.{type Display}
import fluxer_gateway/presence/payload
import fluxer_gateway/presence/version.{type Version}
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Kind {
  StatusChanged
  UserUpdated
}

pub type PresenceEvent {
  PresenceEvent(
    kind: Kind,
    user: Int,
    version: Version,
    display: Display,
    custom_status: BitArray,
    user_wire: BitArray,
    wire: BitArray,
    facts: Option(DispatchFacts),
  )
}

pub fn status_changed(
  user user: Int,
  version version: Version,
  display shown: Display,
  custom_status custom_status: BitArray,
  user_wire user_wire: BitArray,
) -> PresenceEvent {
  let wire = payload.status_wire(shown, custom_status, user_wire)
  PresenceEvent(
    kind: StatusChanged,
    user:,
    version:,
    display: shown,
    custom_status:,
    user_wire:,
    wire:,
    facts: Some(scan(wire)),
  )
}

pub fn as_user_update(event: PresenceEvent) -> PresenceEvent {
  PresenceEvent(..event, kind: UserUpdated)
}

pub fn merge(older: PresenceEvent, newer: PresenceEvent) -> PresenceEvent {
  let latest = case version.compare(newer.version, older.version) {
    version.Stale -> older
    version.Newer | version.OtherEpoch -> newer
  }
  case older.kind, newer.kind {
    UserUpdated, _ | _, UserUpdated -> as_user_update(latest)
    StatusChanged, StatusChanged -> latest
  }
}

pub fn user_id(event: PresenceEvent) -> Result(UserId, Nil) {
  id.user_id(event.user) |> result.replace_error(Nil)
}

pub fn wire_json(event: PresenceEvent) -> RawJson {
  raw_json.wrap(event.wire)
}

pub fn facts(event: PresenceEvent) -> DispatchFacts {
  option.lazy_unwrap(event.facts, fn() { scan(event.wire) })
}

pub fn scan(wire: BitArray) -> DispatchFacts {
  dispatch_facts.scan("PRESENCE_UPDATE", raw_json.wrap(wire))
}

pub fn to_presence(event: PresenceEvent, user: UserId) -> Presence {
  let shown = display.external(event.display)
  let custom_status = case event.custom_status {
    <<"null":utf8>> -> None
    bytes -> Some(raw_json.wrap(bytes))
  }
  presence.new(
    user:,
    status: display.status(shown),
    mobile: display.mobile(shown),
    afk: display.afk(shown),
    custom_status:,
    wire: raw_json.wrap(event.wire),
  )
}

pub type Row {
  Row(
    version: Version,
    display: Display,
    wire: BitArray,
    user_wire: BitArray,
    custom_status: BitArray,
    published_ms: Int,
  )
}

pub fn row(event: PresenceEvent, published_ms published_ms: Int) -> Row {
  Row(
    version: event.version,
    display: event.display,
    wire: event.wire,
    user_wire: event.user_wire,
    custom_status: event.custom_status,
    published_ms:,
  )
}

pub fn from_row(user: Int, row: Row) -> PresenceEvent {
  PresenceEvent(
    kind: StatusChanged,
    user:,
    version: row.version,
    display: row.display,
    custom_status: row.custom_status,
    user_wire: row.user_wire,
    wire: row.wire,
    facts: None,
  )
}
