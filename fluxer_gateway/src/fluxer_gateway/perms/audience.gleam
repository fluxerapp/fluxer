// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type MessageId, type UserId}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/snowflake
import fluxer_gateway/perms/native
import fluxer_gateway/perms/view
import gleam/bit_array
import gleam/option.{type Option, None}

pub type Need {
  View
  ViewMembers
  MessageAccess(message: MessageId)
  ChannelBits(required: Permissions)
}

pub opaque type Group {
  Group(native.Group)
}

pub opaque type Audience {
  Audience(native.Audience)
}

pub fn user(group: Group) -> Option(UserId) {
  let Group(found) = group
  case found {
    native.UserGroup(user) -> id.user_id(user) |> option.from_result
    native.RoleSetGroup(_) -> None
  }
}

pub fn is_exception(group: Group) -> Bool {
  let Group(found) = group
  case found {
    native.UserGroup(_) -> True
    native.RoleSetGroup(_) -> False
  }
}

pub fn contains(audience: Audience, group: Group) -> Bool {
  let Audience(found) = audience
  let Group(member) = group
  case member {
    native.RoleSetGroup(role_set) -> view.bit_at(found.role_sets, role_set)
    native.UserGroup(user) -> packed_ids.contains_sorted(found.users, user)
  }
}

pub fn epoch(audience: Audience) -> Int {
  let Audience(found) = audience
  found.epoch
}

@internal
pub fn to_native_need(need: Need) -> native.Need {
  case need {
    View -> native.View
    ViewMembers -> native.ViewMembers
    MessageAccess(message) ->
      native.MessageAccess(snowflake.message_timestamp_ms(message))
    ChannelBits(required) -> native.ChannelBits(permissions.to_int(required))
  }
}

@internal
pub fn group_from_native(group: native.Group) -> Group {
  Group(group)
}

@internal
pub fn renumbered(group: Group, table: BitArray) -> Group {
  case group {
    Group(native.RoleSetGroup(set)) if set >= 0 ->
      case bit_array.slice(table, set * 4, 4) {
        Ok(<<old:32>>) -> Group(native.RoleSetGroup(old))
        _ -> group
      }
    _ -> group
  }
}

@internal
pub fn from_native(audience: native.Audience) -> Audience {
  Audience(audience)
}
