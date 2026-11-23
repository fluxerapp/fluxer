// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type RoleId, type UserId}
import fluxer_gateway/core/member
import fluxer_gateway/core/permissions
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/members.{type MembersTable}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order

fn position(row: Role) -> Int {
  case role.position(row) {
    role.Position(value) -> value
    role.UndefinedPosition -> 0
  }
}

fn member_roles(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
) -> Option(List(Role)) {
  members.get(table, user)
  |> option.map(fn(row) {
    list.filter_map(member.roles(row), fn(role_id) {
      data.find_role(guild_data, role_id) |> option.to_result(Nil)
    })
  })
}

fn max_position(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
) -> Int {
  member_roles(guild_data, table, user)
  |> option.map(list.fold(_, -1, fn(acc, row) { int.max(acc, position(row)) }))
  |> option.unwrap(-1)
}

fn highest_role(rows: List(Role)) -> Option(Role) {
  list.fold(rows, None, fn(acc, row) {
    case acc {
      None -> Some(row)
      Some(best) ->
        case int.compare(position(row), position(best)) {
          order.Gt -> Some(row)
          order.Lt -> acc
          order.Eq ->
            case id.compare_role_id(role.id(row), role.id(best)) {
              order.Lt -> Some(row)
              _ -> acc
            }
        }
    }
  })
}

fn rank_manages(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
  user_max: Int,
  role_position: Int,
  target: RoleId,
) -> Bool {
  case user_max > role_position, user_max == role_position {
    True, _ -> True
    False, True ->
      case member_roles(guild_data, table, user) |> option.then(highest_role) {
        Some(top) -> id.role_id_to_int(role.id(top)) < id.role_id_to_int(target)
        None -> False
      }
    False, False -> False
  }
}

pub fn can_manage_roles(
  guild_data: GuildData,
  table: MembersTable,
  guild_permissions: permissions.Permissions,
  user: UserId,
  target: RoleId,
) -> Bool {
  data.owner_is(guild_data, user)
  || {
    permissions.has(guild_permissions, permissions.ManageRoles)
    && can_manage_role(guild_data, table, user, target)
  }
}

pub fn can_manage_role(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
  target: RoleId,
) -> Bool {
  case data.find_role(guild_data, target) {
    None -> False
    Some(row) ->
      rank_manages(
        guild_data,
        table,
        user,
        max_position(guild_data, table, user),
        position(row),
        target,
      )
  }
}

fn rank(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
) -> #(Int, Int) {
  case member_roles(guild_data, table, user) |> option.then(highest_role) {
    None -> #(-1, 0)
    Some(top) -> #(position(top), 0 - id.role_id_to_int(role.id(top)))
  }
}

pub fn check_target_member(
  guild_data: GuildData,
  table: MembersTable,
  user: UserId,
  target: UserId,
) -> Bool {
  let owner = core_guild.owner(guild_data.facts)
  case owner == Some(user), owner == Some(target) {
    True, _ -> True
    False, True -> False
    False, False -> {
      let #(user_position, user_tie) = rank(guild_data, table, user)
      let #(target_position, target_tie) = rank(guild_data, table, target)
      user_position > target_position
      || { user_position == target_position && user_tie > target_tie }
    }
  }
}
