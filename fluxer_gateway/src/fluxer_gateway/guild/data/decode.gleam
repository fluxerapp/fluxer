// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel, type Overwrite}
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/json_scan
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/core/user.{type User}
import fluxer_gateway/guild/data.{type GuildData, type Loaded, GuildData, Loaded}
import fluxer_gateway/util/decode as id_decode
import fluxer_gateway/util/iso8601
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type DecodeError {
  DecodeError(what: String)
}

type NsfwOverride {
  NoOverride
  UnsetOverride
  Override(Bool)
}

type IoData

@external(erlang, "json", "encode")
fn json_encode(value: Dynamic) -> IoData

@external(erlang, "erlang", "iolist_to_binary")
fn iodata_to_bits(data: IoData) -> BitArray

pub fn raw(value: Dynamic) -> RawJson {
  raw_json.wrap(iodata_to_bits(json_encode(value)))
}

pub fn flexible_int() -> Decoder(Int) {
  decode.one_of(decode.int, [
    decode.then(decode.string, fn(text) {
      case id.parse_decimal(text) {
        Ok(value) -> decode.success(value)
        Error(Nil) -> decode.failure(0, "Int")
      }
    }),
  ])
}

fn lenient_limit() -> Decoder(Int) {
  decode.one_of(decode.int, [decode.success(-1)])
}

fn lenient_bool() -> Decoder(Bool) {
  decode.one_of(decode.bool, [decode.success(False)])
}

fn permissions_decoder() -> Decoder(Permissions) {
  decode.then(flexible_int(), fn(bits) {
    case permissions.from_int(bits) {
      Ok(value) -> decode.success(value)
      Error(_) -> decode.success(permissions.none())
    }
  })
}

type Slices =
  Dict(String, RawJson)

fn slices(raw: RawJson) -> Result(Slices, Nil) {
  raw_json.fields(raw) |> result.map(dict.from_list)
}

fn slice_bool(fields: Slices, key: String) -> Bool {
  case dict.get(fields, key) {
    Ok(value) -> raw_json.to_bits(value) == <<"true">>
    Error(Nil) -> False
  }
}

fn slice_text(fields: Slices, key: String) -> Option(String) {
  dict.get(fields, key) |> result.try(raw_json.string) |> option.from_result
}

fn slice_ms(fields: Slices, key: String) -> Option(Int) {
  slice_text(fields, key)
  |> option.then(fn(text) { iso8601.parse_ms(text) |> option.from_result })
}

fn user_of(fields: Slices, wire: RawJson) -> Result(User, Nil) {
  use user_int <- result.try(
    dict.get(fields, "id") |> result.try(raw_json.snowflake),
  )
  use user_id <- result.map(id.user_id(user_int) |> result.replace_error(Nil))
  let flags =
    dict.get(fields, "flags")
    |> result.try(raw_json.read(_, flexible_int()))
    |> result.unwrap(0)
  let flags = case slice_bool(fields, "is_staff") {
    True -> int.bitwise_or(flags, 1)
    False -> flags
  }
  let #(global_name, global_text) = name_text(fields, "global_name")
  let #(username, username_text) = name_text(fields, "username")
  user.new(
    id: user_id,
    bot: slice_bool(fields, "bot"),
    flags:,
    restricted: slice_bool(fields, "profile_restricted"),
    wire:,
  )
  |> user.with_name(global_name:, username:)
  |> user.with_search_names([global_text, username_text])
}

fn member_of(wire: RawJson) -> Result(Member, Nil) {
  use fields <- result.try(slices(wire))
  use user_wire <- result.try(dict.get(fields, "user"))
  use user_fields <- result.try(slices(user_wire))
  use user <- result.map(user_of(user_fields, user_wire))
  member.new(
    user:,
    nick: slice_text(fields, "nick"),
    roles: dict.get(fields, "roles")
      |> result.try(raw_json.items)
      |> result.unwrap([])
      |> list.filter_map(fn(item) {
        raw_json.snowflake(item)
        |> result.try(fn(value) {
          id.role_id(value) |> result.replace_error(Nil)
        })
      }),
    joined_at_ms: slice_ms(fields, "joined_at"),
    timed_out_until_ms: slice_ms(fields, "communication_disabled_until"),
    wire:,
  )
}

fn name_text(fields: Slices, key: String) -> #(String, Option(String)) {
  case dict.get(fields, key) {
    Error(Nil) -> #("", None)
    Ok(value) ->
      case raw_json.string(value) {
        Ok(text) -> #(text, Some(text))
        Error(Nil) ->
          case raw_json.read(value, decode.int) {
            Ok(number) -> #(id.write(number), None)
            Error(Nil) -> #("", None)
          }
      }
  }
}

pub fn role_decoder() -> Decoder(Role) {
  role_fields(raw)
}

fn role_fields(wire: fn(Dynamic) -> RawJson) -> Decoder(Role) {
  use whole <- decode.then(decode.dynamic)
  use role_id <- decode.field("id", id_decode.role_id())
  use name <- decode.optional_field(
    "name",
    "",
    decode.one_of(decode.string, [decode.success("")]),
  )
  use permissions <- decode.optional_field(
    "permissions",
    permissions.none(),
    decode.one_of(permissions_decoder(), [decode.success(permissions.none())]),
  )
  use position <- decode.optional_field(
    "position",
    role.UndefinedPosition,
    decode.one_of(flexible_int() |> decode.map(role.Position), [
      decode.success(role.UndefinedPosition),
    ]),
  )
  use hoist <- decode.optional_field("hoist", False, lenient_bool())
  use hoist_position <- decode.optional_field(
    "hoist_position",
    None,
    decode.one_of(decode.optional(flexible_int()), [decode.success(None)]),
  )
  let hoist = case hoist {
    True -> role.Hoisted(hoist_position)
    False -> role.NotHoisted
  }
  decode.success(role.new(
    id: role_id,
    name:,
    permissions:,
    position:,
    hoist:,
    raw: wire(whole),
  ))
}

fn overwrite_decoder() -> Decoder(Result(Overwrite, Nil)) {
  use target_id <- decode.field("id", id_decode.snowflake())
  use kind <- decode.optional_field("type", 0, flexible_int())
  use allow <- decode.optional_field(
    "allow",
    permissions.none(),
    permissions_decoder(),
  )
  use deny <- decode.optional_field(
    "deny",
    permissions.none(),
    permissions_decoder(),
  )
  let target = case kind {
    1 -> id.user_id(target_id) |> result.map(channel.MemberTarget)
    _ -> id.role_id(target_id) |> result.map(channel.RoleTarget)
  }
  target
  |> result.map(fn(target) { channel.overwrite(target:, allow:, deny:) })
  |> result.replace_error(Nil)
  |> decode.success
}

pub fn overwrites() -> Decoder(List(Overwrite)) {
  lenient_list(overwrite_decoder()) |> decode.map(result.values)
}

pub fn channel_decoder() -> Decoder(Channel) {
  channel_fields(raw)
}

fn channel_fields(wire: fn(Dynamic) -> RawJson) -> Decoder(Channel) {
  use whole <- decode.then(decode.dynamic)
  use channel_id <- decode.field("id", id_decode.channel_id())
  use kind <- decode.optional_field("type", 0, flexible_int())
  use parent <- decode.optional_field(
    "parent_id",
    None,
    decode.one_of(id_decode.optional_snowflake(), [decode.success(None)]),
  )
  use position <- decode.optional_field(
    "position",
    0,
    decode.one_of(flexible_int(), [decode.success(0)]),
  )
  use overwrite_list <- decode.optional_field(
    "permission_overwrites",
    [],
    overwrites(),
  )
  use override <- decode.optional_field(
    "nsfw_override",
    NoOverride,
    decode.one_of(decode.map(decode.bool, Override), [
      decode.success(UnsetOverride),
    ]),
  )
  use nsfw <- decode.optional_field(
    "nsfw",
    None,
    decode.one_of(decode.optional(decode.bool), [decode.success(None)]),
  )
  use name <- decode.optional_field(
    "name",
    None,
    decode.one_of(decode.optional(decode.string), [decode.success(None)]),
  )
  use user_limit <- decode.optional_field("user_limit", 0, lenient_limit())
  use connection_limit <- decode.optional_field(
    "voice_connection_limit",
    5,
    lenient_limit(),
  )
  use rtc_p2p <- decode.optional_field("rtc_p2p", False, lenient_bool())
  use last_message <- decode.optional_field(
    "last_message_id",
    None,
    decode.one_of(decode.map(id_decode.snowflake(), Some), [
      decode.success(None),
    ]),
  )
  let parent =
    parent
    |> option.then(fn(value) { id.channel_id(value) |> option.from_result })
  let row =
    channel.new(
      id: channel_id,
      kind: channel.kind_from_int(kind),
      parent:,
      position:,
      overwrites: overwrite_list,
      raw: wire(whole),
    )
    |> channel.set_named(option.is_some(name))
    |> channel.set_voice(channel.voice_limits(
      user_limit:,
      connection_limit:,
      rtc_p2p:,
    ))
    |> channel.set_last_message(last_message)
  let row = case override, nsfw {
    Override(True), _ -> channel.set_nsfw(row, channel.AdultOnly)
    Override(False), _ -> channel.set_nsfw(row, channel.SafeForWork)
    UnsetOverride, _ | NoOverride, None -> row
    NoOverride, Some(True) -> channel.set_nsfw(row, channel.AdultOnly)
    NoOverride, Some(False) -> channel.set_nsfw(row, channel.SafeForWork)
  }
  decode.success(row)
}

pub fn lenient_list(item: Decoder(a)) -> Decoder(List(a)) {
  decode.one_of(
    decode.list(decode.dynamic)
      |> decode.map(fn(items) {
        list.filter_map(items, fn(value) {
          decode.run(value, item) |> result.replace_error(Nil)
        })
      }),
    [decode.success([])],
  )
}

pub fn decode_member(value: Dynamic) -> Result(Member, DecodeError) {
  member_of(raw(value))
  |> result.replace_error(DecodeError("member"))
}

pub fn decode_channel(value: Dynamic) -> Result(Channel, DecodeError) {
  decode.run(value, channel_decoder())
  |> result.replace_error(DecodeError("channel"))
}

pub fn decode_user(value: Dynamic) -> Result(User, DecodeError) {
  let wire = raw(value)
  slices(wire)
  |> result.try(user_of(_, wire))
  |> result.replace_error(DecodeError("user"))
}

pub fn user_id_of(value: Dynamic, path: List(String)) -> Result(UserId, Nil) {
  decode.run(value, decode.at(path, id_decode.user_id()))
  |> result.replace_error(Nil)
}

fn properties(value: Dynamic) -> Dict(String, RawJson) {
  decode.run(value, decode.dict(decode.string, decode.dynamic))
  |> result.map(dict.map_values(_, fn(_key, field) { raw(field) }))
  |> result.unwrap(dict.new())
}

fn facts(
  guild: GuildId,
  properties: Dict(String, RawJson),
  member_count: Int,
) -> core_guild.GuildFacts {
  core_guild.facts(
    id: guild,
    owner: property(properties, "owner_id", id_decode.user_id())
      |> option.from_result,
    nsfw_level: property(properties, "nsfw_level", flexible_int())
      |> result.unwrap(0)
      |> core_guild.nsfw_level_from_int,
    features: property(properties, "features", lenient_list(decode.string))
      |> result.unwrap([]),
    member_count:,
  )
}

fn property(
  properties: Dict(String, RawJson),
  key: String,
  decoder: Decoder(a),
) -> Result(a, Nil) {
  dict.get(properties, key) |> result.try(raw_json.read(_, decoder))
}

pub fn raw_list(value: Result(Dynamic, Nil)) -> RawJson {
  value
  |> result.try(fn(items) {
    decode.run(items, decode.list(decode.dynamic))
    |> result.replace(raw(items))
    |> result.replace_error(Nil)
  })
  |> result.unwrap(raw_json.wrap(<<"[]">>))
}

pub type Collection {
  Whole(BitArray)
  Items(List(BitArray))
  Members(List(Member))
}

pub fn members(rows: List(BitArray)) -> List(Member) {
  list.filter_map(rows, fn(bytes) {
    raw_json.wrap(bytes) |> raw_json.detach |> member_of
  })
}

pub fn collections(
  guild: GuildId,
  collected: Dict(String, Collection),
) -> Result(Loaded, DecodeError) {
  use guild_bytes <- result.try(case dict.get(collected, "guild") {
    Ok(Whole(bytes)) -> Ok(bytes)
    _ -> Error(DecodeError("guild"))
  })
  let properties =
    raw_json.wrap(guild_bytes)
    |> raw_json.detach
    |> raw_json.fields
    |> result.map(fn(fields) { list.reverse(fields) |> dict.from_list })
    |> result.unwrap(dict.new())
  let members = case dict.get(collected, "members") {
    Ok(Members(decoded)) -> decoded
    _ -> collection(collected, "members", member_of)
  }
  let roles =
    collection(collected, "roles", fn(wire) {
      raw_json.read(wire, role_fields(fn(_) { wire }))
    })
  let channels =
    collection(collected, "channels", fn(wire) {
      raw_json.read(wire, channel_fields(fn(_) { wire }))
    })
  Ok(Loaded(
    data: GuildData(
      facts: facts(guild, properties, list.length(members)),
      properties:,
      roles:,
      channels:,
      channel_patches: dict.new(),
      emojis: list_wire(dict.get(collected, "emojis")),
      stickers: list_wire(dict.get(collected, "stickers")),
    ),
    members:,
  ))
}

fn items(found: Result(Collection, Nil)) -> List(BitArray) {
  case found {
    Ok(Whole(bytes)) -> json_scan.array(bytes) |> result.unwrap([])
    Ok(Items(rows)) -> rows
    Ok(Members(_)) | Error(Nil) -> []
  }
}

fn collection(
  collected: Dict(String, Collection),
  key: String,
  item: fn(RawJson) -> Result(a, Nil),
) -> List(a) {
  dict.get(collected, key)
  |> items
  |> list.filter_map(fn(bytes) {
    raw_json.wrap(bytes) |> raw_json.detach |> item
  })
}

fn list_wire(found: Result(Collection, Nil)) -> RawJson {
  case found {
    Ok(Whole(bytes)) ->
      case json_scan.array(bytes) {
        Ok(_) -> raw_json.wrap(bytes) |> raw_json.detach
        Error(Nil) -> raw_json.wrap(<<"[]">>)
      }
    Ok(Items(rows)) ->
      list.map(rows, raw_json.wrap) |> raw_json.array |> raw_json.flatten
    Ok(Members(_)) | Error(Nil) -> raw_json.wrap(<<"[]">>)
  }
}

pub fn rewrite_facts(current: GuildData, guild_value: Dynamic) -> GuildData {
  let merged = dict.merge(current.properties, properties(guild_value))
  GuildData(
    ..current,
    properties: merged,
    facts: facts(
        core_guild.id(current.facts),
        merged,
        core_guild.member_count(current.facts),
      )
      |> core_guild.set_unavailable(core_guild.is_unavailable(current.facts)),
  )
}
