// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type RoleId}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/option.{type Option}

pub opaque type Role {
  Role(
    id: RoleId,
    name: String,
    permissions: Permissions,
    position: RankPosition,
    hoist: Hoist,
    raw: RawJson,
  )
}

pub type Hoist {
  NotHoisted
  Hoisted(hoist_position: Option(Int))
}

pub type RankPosition {
  Position(Int)
  UndefinedPosition
}

pub fn new(
  id id: RoleId,
  name name: String,
  permissions permissions: Permissions,
  position position: RankPosition,
  hoist hoist: Hoist,
  raw raw: RawJson,
) -> Role {
  Role(
    id:,
    name: raw_json.detach_string(name),
    permissions:,
    position:,
    hoist:,
    raw:,
  )
}

pub fn id(role: Role) -> RoleId {
  role.id
}

pub fn name(role: Role) -> String {
  role.name
}

pub fn permissions(role: Role) -> Permissions {
  role.permissions
}

pub fn position(role: Role) -> RankPosition {
  role.position
}

pub fn hoist(role: Role) -> Hoist {
  role.hoist
}

pub fn raw(role: Role) -> RawJson {
  role.raw
}
