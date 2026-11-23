// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import gleam/int
import gleam/json.{type Json}
import gleam/list

pub opaque type Permissions {
  Permissions(bits: Int)
}

pub type Permission {
  KickMembers
  BanMembers
  Administrator
  ManageChannels
  ViewAuditLog
  Stream
  ViewChannel
  ReadMessageHistory
  Connect
  Speak
  ManageRoles
}

pub type PermissionsError {
  Negative(value: Int)
  NotDecimal(text: String)
}

const mask64 = 0xFFFF_FFFF_FFFF_FFFF

fn bit(permission: Permission) -> Int {
  case permission {
    KickMembers -> 2
    BanMembers -> 4
    Administrator -> 8
    ManageChannels -> 16
    ViewAuditLog -> 128
    Stream -> 512
    ViewChannel -> 1024
    ReadMessageHistory -> 65_536
    Connect -> 1_048_576
    Speak -> 2_097_152
    ManageRoles -> 268_435_456
  }
}

pub fn none() -> Permissions {
  Permissions(0)
}

pub fn from_int(bits: Int) -> Result(Permissions, PermissionsError) {
  case bits < 0 {
    True -> Error(Negative(bits))
    False -> Ok(Permissions(int.bitwise_and(bits, mask64)))
  }
}

pub fn from_wire(text: String) -> Result(Permissions, PermissionsError) {
  case id.parse_decimal(text) {
    Ok(bits) -> from_int(bits)
    Error(Nil) -> Error(NotDecimal(text))
  }
}

pub fn bits_json(bits: Int) -> Json {
  json.string(int.to_string(bits))
}

pub fn to_int(permissions: Permissions) -> Int {
  permissions.bits
}

pub fn of(permissions: List(Permission)) -> Permissions {
  list.fold(permissions, none(), grant)
}

pub fn has(permissions: Permissions, permission: Permission) -> Bool {
  int.bitwise_and(permissions.bits, bit(permission)) != 0
}

fn grant(permissions: Permissions, permission: Permission) -> Permissions {
  Permissions(int.bitwise_or(permissions.bits, bit(permission)))
}

pub fn apply(
  base: Permissions,
  allow allow: Permissions,
  deny deny: Permissions,
) -> Permissions {
  let kept =
    int.bitwise_and(base.bits, int.bitwise_exclusive_or(mask64, deny.bits))
  Permissions(int.bitwise_or(kept, allow.bits))
}
