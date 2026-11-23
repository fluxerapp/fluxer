// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/user
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/util/decode as edge
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None}
import gleam/result

const partial_user_fields = [
  "id", "username", "discriminator", "global_name", "avatar", "avatar_color",
  "bot", "system", "flags", "mention_flags",
]

pub fn refresh_author(
  table: MembersTable,
  guild: GuildId,
  event_name: String,
  event: Dynamic,
) -> Option(Dynamic) {
  case event_name {
    "MESSAGE_CREATE" | "MESSAGE_UPDATE" ->
      decode.run(event, decode.at(["author"], decode.dynamic))
      |> option.from_result
      |> option.then(fn(author) {
        refresh(table, guild, author, fn() { data_decode.raw(author) })
      })
    _ -> None
  }
}

pub fn refresh_user(
  table: MembersTable,
  guild: GuildId,
  user_wire: BitArray,
) -> Option(Dynamic) {
  edge.decode_json(user_wire)
  |> option.from_result
  |> option.then(refresh(table, guild, _, fn() { raw_json.wrap(user_wire) }))
}

fn refresh(
  table: MembersTable,
  guild: GuildId,
  author: Dynamic,
  author_wire: fn() -> RawJson,
) -> Option(Dynamic) {
  use user_id <- option.then(
    data_decode.user_id_of(author, ["id"]) |> option.from_result,
  )
  use row <- option.then(members.get(table, user_id))
  let current =
    raw_json.read(user.wire(member.user(row)), decode.dynamic)
    |> result.unwrap(dynamic.nil())
  case differs(current, author), data_decode.decode_user(author) {
    True, Ok(fresh) -> {
      let previous = member.wire(row)
      let wire =
        raw_json.with_field(previous, "user", author_wire())
        |> result.map(raw_json.flatten)
        |> result.unwrap(previous)
      members.put(
        table,
        member.new(
          user: fresh,
          nick: member.nick(row),
          roles: member.roles(row),
          joined_at_ms: member.joined_at_ms(row),
          timed_out_until_ms: member.timed_out_until_ms(row),
          wire:,
        ),
      )
      raw_json.with_field(
        wire,
        "guild_id",
        raw_json.from_json(id.json(id.guild_id_to_int(guild))),
      )
      |> result.try(raw_json.read(_, decode.dynamic))
      |> option.from_result
    }
    _, _ -> None
  }
}

fn differs(current: Dynamic, fresh: Dynamic) -> Bool {
  let current = normalize(current)
  let fresh = normalize(fresh)
  list.any(partial_user_fields, fn(field) {
    case list.key_find(fresh, field) {
      Error(Nil) -> False
      Ok(value) -> list.key_find(current, field) != Ok(value)
    }
  })
}

type Field {
  IntField(Int)
  Other(Dynamic)
  Null
}

fn normalize(value: Dynamic) -> List(#(String, Field)) {
  let restricted =
    decode.run(value, decode.at(["profile_restricted"], decode.bool))
    |> result.unwrap(False)
  list.filter_map(partial_user_fields, fn(field) {
    case decode.run(value, decode.at([field], decode.dynamic)) {
      Error(_) -> Error(Nil)
      Ok(raw) -> {
        let normalized = case field {
          "id" | "flags" | "mention_flags" | "avatar_color" ->
            case decode.run(raw, data_decode.flexible_int()) {
              Ok(number) -> IntField(number)
              Error(_) -> Other(raw)
            }
          _ -> Other(raw)
        }
        case restricted, field {
          True, "global_name" | True, "avatar" -> Ok(#(field, Null))
          _, _ -> Ok(#(field, normalized))
        }
      }
    }
  })
}
