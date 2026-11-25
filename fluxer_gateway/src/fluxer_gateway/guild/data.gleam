// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/guild.{type GuildFacts} as core_guild
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type RoleId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role.{type Role}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result

const disabled_member_list_updates = 64

pub type GuildData {
  GuildData(
    facts: GuildFacts,
    properties: Dict(String, RawJson),
    roles: List(Role),
    channels: List(Channel),
    channel_patches: Dict(Int, Dict(String, RawJson)),
    emojis: RawJson,
    stickers: RawJson,
  )
}

pub type Loaded {
  Loaded(data: GuildData, members: List(Member))
}

pub fn guild_id(data: GuildData) -> GuildId {
  core_guild.id(data.facts)
}

pub fn find_role(data: GuildData, wanted: RoleId) -> Option(Role) {
  data.roles
  |> list.find(fn(candidate) { role.id(candidate) == wanted })
  |> option.from_result
}

pub fn everyone_permissions(data: GuildData) -> Option(Permissions) {
  find_role(data, id.everyone_role(guild_id(data)))
  |> option.map(role.permissions)
}

pub fn find_channel(data: GuildData, wanted: ChannelId) -> Option(Channel) {
  data.channels
  |> list.find(fn(candidate) { channel.id(candidate) == wanted })
  |> option.from_result
}

pub fn channel_wire(data: GuildData, row: Channel) -> RawJson {
  case dict.get(data.channel_patches, id.channel_id_to_int(channel.id(row))) {
    Error(Nil) -> channel.raw(row)
    Ok(fields) ->
      dict.fold(fields, channel.raw(row), fn(raw, key, value) {
        raw_json.with_field(raw, key, value) |> result.unwrap(raw)
      })
  }
}

pub fn set_channel_field(
  data: GuildData,
  target: ChannelId,
  key: String,
  value: RawJson,
) -> GuildData {
  let key_int = id.channel_id_to_int(target)
  let fields =
    dict.get(data.channel_patches, key_int) |> result.unwrap(dict.new())
  GuildData(
    ..data,
    channel_patches: dict.insert(
      data.channel_patches,
      key_int,
      dict.insert(fields, key, value),
    ),
  )
}

pub fn drop_channel_patches(data: GuildData, target: ChannelId) -> GuildData {
  GuildData(
    ..data,
    channel_patches: dict.delete(
      data.channel_patches,
      id.channel_id_to_int(target),
    ),
  )
}

pub fn roles_json(data: GuildData) -> Json {
  json.array(data.roles, fn(row) { raw_json.to_json(role.raw(row)) })
}

pub fn channels_json(data: GuildData, channels: List(Channel)) -> Json {
  json.array(channels, fn(row) { raw_json.to_json(channel_wire(data, row)) })
}

pub fn properties_json(data: GuildData, extra: List(#(String, Json))) -> Json {
  let keys = list.map(extra, fn(pair) { pair.0 })
  data.properties
  |> dict.to_list
  |> list.filter(fn(pair) { !list.contains(keys, pair.0) })
  |> list.map(fn(pair) { #(pair.0, raw_json.to_json(pair.1)) })
  |> list.append(extra)
  |> json.object
}

pub fn property(data: GuildData, key: String) -> Option(RawJson) {
  dict.get(data.properties, key) |> option.from_result
}

pub fn decoded_property(
  data: GuildData,
  key: String,
  decoder: decode.Decoder(a),
) -> Option(a) {
  property(data, key)
  |> option.then(fn(raw) { raw_json.read(raw, decoder) |> option.from_result })
}

pub fn updates_enabled(data: GuildData) -> Bool {
  !operation_disabled(data, disabled_member_list_updates)
}

pub fn operation_disabled(data: GuildData, bit: Int) -> Bool {
  let bits =
    decoded_property(data, "disabled_operations", operations())
    |> option.unwrap(0)
  int.bitwise_and(bits, bit) == bit
}

fn operations() -> decode.Decoder(Int) {
  decode.one_of(decode.int, [
    decode.map(decode.string, fn(text) {
      id.parse_decimal(text) |> result.unwrap(0)
    }),
    decode.success(0),
  ])
}

pub fn set_member_count(data: GuildData, count: Int) -> GuildData {
  GuildData(..data, facts: core_guild.set_member_count(data.facts, count))
}

pub fn put_role(data: GuildData, row: Role) -> GuildData {
  let found = list.any(data.roles, fn(old) { role.id(old) == role.id(row) })
  case found {
    True ->
      GuildData(
        ..data,
        roles: list.map(data.roles, fn(old) {
          case role.id(old) == role.id(row) {
            True -> row
            False -> old
          }
        }),
      )
    False -> GuildData(..data, roles: [row, ..data.roles])
  }
}

pub fn put_channel(data: GuildData, row: Channel) -> GuildData {
  let found =
    list.any(data.channels, fn(old) { channel.id(old) == channel.id(row) })
  case found {
    True ->
      GuildData(
        ..data,
        channels: list.map(data.channels, fn(old) {
          case channel.id(old) == channel.id(row) {
            True -> row
            False -> old
          }
        }),
      )
    False -> GuildData(..data, channels: [row, ..data.channels])
  }
}

pub fn remove_channel(data: GuildData, target: ChannelId) -> GuildData {
  GuildData(
    ..drop_channel_patches(data, target),
    channels: list.filter(data.channels, fn(old) { channel.id(old) != target }),
  )
}

pub fn remove_role(data: GuildData, target: RoleId) -> GuildData {
  GuildData(
    ..data,
    roles: list.filter(data.roles, fn(old) { role.id(old) != target }),
  )
}

pub fn has_channel(data: GuildData, target: ChannelId) -> Bool {
  list.any(data.channels, fn(row) { channel.id(row) == target })
}

pub fn owner_is(data: GuildData, user: id.UserId) -> Bool {
  core_guild.owner(data.facts) == Some(user)
}
