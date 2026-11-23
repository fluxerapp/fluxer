// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/id.{type RoleId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role
import fluxer_gateway/guild/data.{type GuildData, GuildData}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option
import gleam/result

pub fn create_or_update(guild_data: GuildData, event: Dynamic) -> GuildData {
  case decode.run(event, decode.at(["role"], data_decode.role_decoder())) {
    Ok(row) -> data.put_role(guild_data, row)
    Error(_) -> guild_data
  }
}

pub fn update_bulk(guild_data: GuildData, event: Dynamic) -> GuildData {
  let rows =
    decode.run(
      event,
      decode.at(["roles"], data_decode.lenient_list(data_decode.role_decoder())),
    )
    |> result.unwrap([])
  list.fold(rows, guild_data, fn(acc, row) {
    case option.is_some(data.find_role(acc, role.id(row))) {
      True -> data.put_role(acc, row)
      False -> acc
    }
  })
}

pub fn delete(
  guild_data: GuildData,
  members_table: MembersTable,
  event: Dynamic,
) -> GuildData {
  case decode.run(event, decode.at(["role_id"], id_decode.role_id())) {
    Error(_) -> guild_data
    Ok(target) -> {
      strip_from_members(members_table, target)
      let next = data.remove_role(guild_data, target)
      GuildData(
        ..next,
        channels: list.map(next.channels, fn(row) {
          strip_from_channel(row, target)
        }),
      )
    }
  }
}

fn strip_from_members(members_table: MembersTable, target: RoleId) -> Nil {
  let target_int = id.role_id_to_int(target)
  members.fold(members_table, Nil, fn(_, row) {
    case list.contains(member.roles(row), target) {
      False -> Nil
      True -> {
        let wire =
          without_items(member.wire(row), "roles", fn(item) {
            json_tree.snowflake(item) == Ok(target_int)
          })
        members.put(
          members_table,
          member.new(
            user: member.user(row),
            nick: member.nick(row),
            roles: list.filter(member.roles(row), fn(role_id) {
              role_id != target
            }),
            joined_at_ms: member.joined_at_ms(row),
            timed_out_until_ms: member.timed_out_until_ms(row),
            wire:,
          ),
        )
      }
    }
  })
}

fn strip_from_channel(row: channel.Channel, target: RoleId) -> channel.Channel {
  let gone = channel.RoleTarget(target)
  let has =
    list.any(channel.overwrites(row), fn(overwrite) {
      channel.overwrite_target(overwrite) == gone
    })
  case has {
    False -> row
    True ->
      row
      |> channel.set_overwrites(
        list.filter(channel.overwrites(row), fn(overwrite) {
          channel.overwrite_target(overwrite) != gone
        }),
      )
      |> channel.set_raw(
        without_items(channel.raw(row), "permission_overwrites", fn(item) {
          json_tree.field(item, "id") |> result.try(json_tree.snowflake)
          == Ok(id.role_id_to_int(target))
        }),
      )
  }
}

fn without_items(
  raw: RawJson,
  key: String,
  dropped: fn(JsonTree) -> Bool,
) -> RawJson {
  json_tree.from_raw(raw)
  |> json_tree.update(key, json_tree.filter(_, fn(item) { !dropped(item) }))
  |> result.map(fn(tree) { raw_json.flatten(json_tree.to_raw(tree)) })
  |> result.unwrap(raw)
}
