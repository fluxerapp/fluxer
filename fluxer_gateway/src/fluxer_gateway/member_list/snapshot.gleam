// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/native
import gleam/list

pub opaque type Snapshot {
  Snapshot(
    members: List(#(Int, BitArray)),
    statuses: List(#(Int, presence.Status)),
    connected: List(Int),
    updates: native.Updates,
  )
}

pub fn new() -> Snapshot {
  Snapshot(members: [], statuses: [], connected: [], updates: native.UpdatesOn)
}

pub fn add_member(snapshot: Snapshot, source: Member) -> Snapshot {
  let entry = #(
    id.user_id_to_int(member.user_id(source)),
    item.display_name(source),
  )
  Snapshot(..snapshot, members: [entry, ..snapshot.members])
}

pub fn add_presence(snapshot: Snapshot, found: Presence) -> Snapshot {
  let entry = #(id.user_id_to_int(presence.user(found)), presence.status(found))
  Snapshot(..snapshot, statuses: [entry, ..snapshot.statuses])
}

pub fn add_connected(snapshot: Snapshot, user: UserId) -> Snapshot {
  Snapshot(..snapshot, connected: [
    id.user_id_to_int(user),
    ..snapshot.connected
  ])
}

pub fn set_updates(snapshot: Snapshot, on: Bool) -> Snapshot {
  let updates = case on {
    True -> native.UpdatesOn
    False -> native.UpdatesOff
  }
  Snapshot(..snapshot, updates:)
}

@internal
pub fn to_native(snapshot: Snapshot) -> native.MlSnapshot {
  native.Snapshot(
    members: list.reverse(snapshot.members),
    statuses: list.reverse(snapshot.statuses),
    connected: list.reverse(snapshot.connected),
    updates: snapshot.updates,
  )
}
