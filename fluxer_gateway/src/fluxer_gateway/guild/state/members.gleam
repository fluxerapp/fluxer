// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/member as core_member
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members.{type MembersTable}
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}

pub type Change {
  Added(user: UserId)
  Updated(user: UserId)
  Removed(user: UserId)
  Unchanged
}

pub fn add(
  guild_data: GuildData,
  table: MembersTable,
  event: Dynamic,
) -> #(GuildData, Change) {
  case data_decode.decode_member(event) {
    Ok(row) -> {
      let user = core_member.user_id(row)
      let known = members.has(table, user)
      members.put(table, row)
      case known {
        True -> #(guild_data, Updated(user))
        False -> #(
          data.set_member_count(guild_data, members.count(table)),
          Added(user),
        )
      }
    }
    Error(_) -> #(guild_data, Unchanged)
  }
}

pub fn update(
  guild_data: GuildData,
  table: MembersTable,
  event: Dynamic,
) -> #(GuildData, Change) {
  case data_decode.decode_member(event) {
    Ok(row) -> {
      let user = core_member.user_id(row)
      case members.get(table, user) {
        Some(_) -> {
          members.put(table, row)
          #(guild_data, Updated(user))
        }
        None -> #(guild_data, Unchanged)
      }
    }
    Error(_) -> #(guild_data, Unchanged)
  }
}

pub fn remove(
  guild_data: GuildData,
  table: MembersTable,
  event: Dynamic,
) -> #(GuildData, Change) {
  case data_decode.user_id_of(event, ["user", "id"]) {
    Ok(user) -> {
      members.remove(table, user)
      #(data.set_member_count(guild_data, members.count(table)), Removed(user))
    }
    Error(Nil) -> #(guild_data, Unchanged)
  }
}
