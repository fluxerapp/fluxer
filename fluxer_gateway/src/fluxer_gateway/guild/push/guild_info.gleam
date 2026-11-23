// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{
  type Channel, type ChannelKind, type Nsfw, type Overwrite,
}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/push/format
import fluxer_gateway/util/decode as edge
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type PushChannel {
  PushChannel(
    id: Int,
    name: Option(String),
    kind: ChannelKind,
    parent: Option(Int),
    nsfw: Nsfw,
    overwrites: Option(List(Overwrite)),
  )
}

type NsfwOverride {
  NoOverride
  UnsetOverride
  Override(Bool)
}

pub type GuildInfo {
  GuildInfo(
    name: Option(String),
    default_level: Int,
    disabled_operations: Int,
    channels: Dict(Int, PushChannel),
  )
}

pub fn empty() -> GuildInfo {
  GuildInfo(
    name: None,
    default_level: 0,
    disabled_operations: 0,
    channels: dict.new(),
  )
}

pub fn from_channels(
  name: Option(String),
  default_message_notifications: Int,
  disabled_operations: Int,
  channels: List(Channel),
) -> GuildInfo {
  GuildInfo(
    name:,
    default_level: default_message_notifications,
    disabled_operations:,
    channels: list.fold(channels, dict.new(), fn(acc, item) {
      let push_channel =
        PushChannel(
          id: id.channel_id_to_int(channel.id(item)),
          name: raw_name(channel.raw(item)),
          kind: channel.kind(item),
          parent: option.map(channel.parent(item), id.channel_id_to_int),
          nsfw: channel.nsfw(item),
          overwrites: Some(channel.overwrites(item)),
        )
      dict.insert(acc, push_channel.id, push_channel)
    }),
  )
}

fn raw_name(raw: raw_json.RawJson) -> Option(String) {
  raw_json.field(raw, "name")
  |> result.try(raw_json.string)
  |> option.from_result
}

pub fn push_disabled(info: GuildInfo) -> Bool {
  int.bitwise_and(info.disabled_operations, 1) != 0
}

pub fn lookup_channel(info: GuildInfo, channel_id: Int) -> Option(PushChannel) {
  dict.get(info.channels, channel_id) |> option.from_result
}

pub fn apply_event(info: GuildInfo, event: String, data: Dynamic) -> GuildInfo {
  case event {
    "GUILD_UPDATE" -> {
      let name =
        decode.run(data, decode.at(["name"], decode.string))
        |> option.from_result
      let level =
        decode.run(
          data,
          decode.at(["default_message_notifications"], decode.int),
        )
        |> result.unwrap(info.default_level)
      let disabled =
        decode.run(data, decode.at(["disabled_operations"], decode.int))
        |> result.unwrap(info.disabled_operations)
      GuildInfo(
        ..info,
        name: option.or(name, info.name),
        default_level: level,
        disabled_operations: disabled,
      )
    }
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" ->
      decode.run(data, channel_decoder())
      |> result.map(fn(found) {
        GuildInfo(..info, channels: dict.insert(info.channels, found.id, found))
      })
      |> result.unwrap(info)
    "CHANNEL_UPDATE_BULK" ->
      decode.run(data, decode.at(["channels"], decode.list(decode.dynamic)))
      |> result.unwrap([])
      |> list.filter_map(decode.run(_, channel_decoder()))
      |> list.fold(info, fn(acc, found) {
        case dict.has_key(acc.channels, found.id) {
          True ->
            GuildInfo(
              ..acc,
              channels: dict.insert(acc.channels, found.id, found),
            )
          False -> acc
        }
      })
    "CHANNEL_DELETE" ->
      decode.run(data, decode.at(["id"], edge.snowflake()))
      |> result.map(fn(channel_id) {
        GuildInfo(..info, channels: dict.delete(info.channels, channel_id))
      })
      |> result.unwrap(info)
    _ -> info
  }
}

fn channel_decoder() -> decode.Decoder(PushChannel) {
  use channel_id <- decode.field("id", edge.snowflake())
  use name <- decode.optional_field(
    "name",
    None,
    decode.one_of(decode.map(decode.string, Some), [decode.success(None)]),
  )
  use kind <- decode.optional_field(
    "type",
    0,
    decode.one_of(decode.int, [decode.success(0)]),
  )
  use parent <- decode.optional_field(
    "parent_id",
    None,
    decode.one_of(decode.map(edge.snowflake(), Some), [decode.success(None)]),
  )
  use override <- decode.optional_field(
    "nsfw_override",
    NoOverride,
    decode.one_of(decode.map(decode.bool, Override), [
      decode.success(UnsetOverride),
    ]),
  )
  use legacy <- decode.optional_field(
    "nsfw",
    None,
    decode.one_of(decode.map(decode.bool, Some), [decode.success(None)]),
  )
  use overwrites <- decode.optional_field(
    "permission_overwrites",
    [],
    data_decode.overwrites(),
  )
  let nsfw = case override, legacy {
    Override(True), _ | NoOverride, Some(True) -> channel.AdultOnly
    Override(False), _ | NoOverride, Some(False) -> channel.SafeForWork
    UnsetOverride, _ | NoOverride, None -> channel.InheritNsfw
  }
  decode.success(PushChannel(
    id: channel_id,
    name:,
    kind: channel.kind_from_int(kind),
    parent:,
    nsfw:,
    overwrites: Some(overwrites),
  ))
}

pub fn effective_nsfw(
  info: GuildInfo,
  channel_id: Int,
  thread_parent: Option(Int),
  guild_nsfw: Bool,
) -> Bool {
  let resolved = case dict.has_key(info.channels, channel_id), thread_parent {
    True, _ -> channel_id
    False, Some(parent) -> parent
    False, None -> channel_id
  }
  case dict.get(info.channels, resolved) {
    Error(Nil) -> guild_nsfw
    Ok(found) ->
      explicit_nsfw(found.nsfw)
      |> option.lazy_or(fn() { inherited_nsfw(info, found) })
      |> option.unwrap(guild_nsfw)
  }
}

fn inherited_nsfw(info: GuildInfo, found: PushChannel) -> Option(Bool) {
  case found.kind, found.parent {
    channel.Category, _ | _, None -> None
    _, Some(parent_id) ->
      dict.get(info.channels, parent_id)
      |> option.from_result
      |> option.then(fn(parent) { explicit_nsfw(parent.nsfw) })
  }
}

fn explicit_nsfw(nsfw: Nsfw) -> Option(Bool) {
  case nsfw {
    channel.AdultOnly -> Some(True)
    channel.SafeForWork -> Some(False)
    channel.InheritNsfw -> None
  }
}

pub fn channel_names_for_context(
  info: GuildInfo,
  everyone_view: fn(Int) -> Bool,
  mentioned: List(Int),
) -> Dict(Int, String) {
  list.fold(mentioned, dict.new(), fn(acc, channel_id) {
    case dict.get(info.channels, channel_id) {
      Ok(found) ->
        case
          found.kind,
          found.name,
          format.is_copyable_channel_type(channel.kind_to_int(found.kind))
        {
          channel.Forum, _, _ | channel.Media, _, _ -> acc
          _, Some(name), True if name != "" ->
            case everyone_view(channel_id) {
              True -> dict.insert(acc, channel_id, name)
              False -> acc
            }
          _, _, _ -> acc
        }
      Error(Nil) -> acc
    }
  })
}

pub fn everyone_can_view(
  info: GuildInfo,
  guild: GuildId,
  roles: List(Role),
  channel_id: Int,
) -> Bool {
  let everyone = id.everyone_role(guild)
  let base =
    list.find(roles, fn(item) { role.id(item) == everyone })
    |> result.map(role.permissions)
    |> result.unwrap(permissions.none())
  case permissions.has(base, permissions.Administrator) {
    True -> True
    False ->
      case dict.get(info.channels, channel_id) {
        Ok(PushChannel(overwrites: Some(overwrites), ..)) -> {
          let applied =
            list.fold(overwrites, base, fn(acc, overwrite) {
              case channel.overwrite_target(overwrite) {
                channel.RoleTarget(target) if target == everyone ->
                  permissions.apply(
                    acc,
                    allow: channel.overwrite_allow(overwrite),
                    deny: channel.overwrite_deny(overwrite),
                  )
                _ -> acc
              }
            })
          permissions.has(applied, permissions.ViewChannel)
        }
        _ -> False
      }
  }
}
