// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/json_scan.{Found}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/bit_array
import gleam/json
import gleam/list
import gleam/result

pub type GuildCreate {
  Available(guild: GuildId, body: Body)
  Unavailable(guild: GuildId, hidden: Bool)
}

pub opaque type Body {
  Body(open: RawJson, members: List(Member), close: RawJson)
}

pub opaque type Member {
  Member(wire: RawJson, ready: Result(ReadyMember, Nil))
}

type ReadyMember {
  ReadyMember(user: Int, wire: RawJson, partial_user: RawJson)
}

pub type ReadyGuild {
  ReadyGuild(wire: RawJson, users: List(#(Int, RawJson)))
}

const partial_user_fields = [
  "id", "username", "discriminator", "global_name", "avatar", "avatar_color",
  "bot", "system", "flags", "mention_flags",
]

pub fn available(
  guild: GuildId,
  before_members before_members: RawJson,
  members members: List(RawJson),
  after_members after_members: RawJson,
) -> GuildCreate {
  Available(
    guild:,
    body: Body(
      open: raw_json.concat([
        before_members,
        raw_json.wrap(<<",\"members\":[">>),
      ]),
      members: list.map(members, member),
      close: raw_json.concat([raw_json.wrap(<<"]">>), after_members]),
    ),
  )
}

pub fn from_wire(guild: GuildId, wire: RawJson) -> GuildCreate {
  let bytes = raw_json.to_bits(wire)
  case json_scan.field(bytes, "unavailable") {
    Ok(<<"true">>) ->
      Unavailable(
        guild:,
        hidden: json_scan.field(bytes, "unavailable_hidden") == Ok(<<"true">>),
      )
    _ ->
      Available(
        guild:,
        body: split_members(bytes)
          |> result.unwrap(Body(open: wire, members: [], close: empty())),
      )
  }
}

pub fn guild(created: GuildCreate) -> GuildId {
  created.guild
}

pub fn to_raw(created: GuildCreate) -> RawJson {
  case created {
    Available(_, body) -> wire(body)
    Unavailable(guild, hidden) -> stub(guild, hidden)
  }
}

pub fn wire(body: Body) -> RawJson {
  join(body, fn(item) { item.wire })
}

pub fn ready(body: Body) -> ReadyGuild {
  let wire =
    join(body, fn(item) {
      case item.ready {
        Ok(found) -> found.wire
        Error(Nil) -> item.wire
      }
    })
  let users =
    list.filter_map(body.members, fn(item) {
      item.ready
      |> result.map(fn(found) { #(found.user, found.partial_user) })
    })
  ReadyGuild(wire:, users:)
}

pub fn bot_wire(body: Body) -> RawJson {
  let ReadyGuild(wire:, ..) = ready(body)
  case raw_json.to_bits(wire) {
    <<"{", rest:bits>> ->
      raw_json.concat([
        raw_json.wrap(<<"{\"unavailable\":false,">>),
        raw_json.wrap(rest),
      ])
    _ -> wire
  }
}

fn join(body: Body, pick: fn(Member) -> RawJson) -> RawJson {
  let members =
    body.members
    |> list.map(pick)
    |> list.intersperse(raw_json.wrap(<<",">>))
  raw_json.concat([body.open, ..list.append(members, [body.close])])
}

fn stub(guild: GuildId, hidden: Bool) -> RawJson {
  let fields = [
    #("id", id.json(id.guild_id_to_int(guild))),
    #("unavailable", json.bool(True)),
  ]
  case hidden {
    True -> [#("unavailable_hidden", json.bool(True)), ..fields]
    False -> fields
  }
  |> json.object
  |> raw_json.from_json
}

fn empty() -> RawJson {
  raw_json.wrap(<<>>)
}

fn split_members(bytes: BitArray) -> Result(Body, Nil) {
  use location <- result.try(json_scan.locate(bytes, "members"))
  case location {
    Found(value_start:, value_end:, ..) -> {
      use array <- result.try(bit_array.slice(
        bytes,
        value_start,
        value_end - value_start,
      ))
      use items <- result.try(json_scan.array(array))
      use open <- result.try(bit_array.slice(bytes, 0, value_start + 1))
      use close <- result.map(bit_array.slice(
        bytes,
        value_end - 1,
        bit_array.byte_size(bytes) - value_end + 1,
      ))
      Body(
        open: raw_json.wrap(open),
        members: list.map(items, fn(item) { member(raw_json.wrap(item)) }),
        close: raw_json.wrap(close),
      )
    }
    _ -> Error(Nil)
  }
}

fn member(wire: RawJson) -> Member {
  Member(wire:, ready: ready_member(wire))
}

fn ready_member(wire: RawJson) -> Result(ReadyMember, Nil) {
  use user <- result.try(raw_json.field(wire, "user"))
  use fields <- result.try(raw_json.fields(user))
  use #(user_id, id_json, partial) <- result.try(partial(fields))
  use stripped <- result.map(raw_json.replace_field(
    wire,
    key: "user",
    value: raw_json.concat([
      raw_json.wrap(<<"{\"id\":">>),
      id_json,
      raw_json.wrap(<<"}">>),
    ]),
  ))
  ReadyMember(user: user_id, wire: stripped, partial_user: partial)
}

pub fn partial_user(
  fields: List(#(String, RawJson)),
) -> Result(#(Int, RawJson), Nil) {
  partial(fields)
  |> result.map(fn(found) { #(found.0, found.2) })
}

fn partial(
  fields: List(#(String, RawJson)),
) -> Result(#(Int, RawJson, RawJson), Nil) {
  use id_bytes <- result.try(list.key_find(fields, "id"))
  use #(user_id, id_json) <- result.map(user_id(raw_json.to_bits(id_bytes)))
  let restricted =
    result.map(list.key_find(fields, "profile_restricted"), raw_json.to_bits)
    == Ok(<<"true">>)
  #(user_id, id_json, partial_fields(fields, id_json, restricted))
}

fn user_id(bytes: BitArray) -> Result(#(Int, RawJson), Nil) {
  let size = bit_array.byte_size(bytes)
  case bytes {
    <<"\"", _:bits>> -> {
      use digits <- result.try(bit_array.slice(bytes, 1, size - 2))
      use value <- result.map(id.parse_bits(digits))
      #(value, raw_json.wrap(bytes))
    }
    _ -> {
      use value <- result.map(id.parse_bits(bytes))
      #(
        value,
        raw_json.concat([
          raw_json.wrap(<<"\"">>),
          raw_json.wrap(bytes),
          raw_json.wrap(<<"\"">>),
        ]),
      )
    }
  }
}

fn partial_fields(
  fields: List(#(String, RawJson)),
  id_json: RawJson,
  restricted: Bool,
) -> RawJson {
  partial_user_fields
  |> list.filter_map(fn(key) {
    case key, restricted {
      "id", _ -> Ok(#(key, id_json))
      "global_name", True | "avatar", True -> Ok(#(key, raw_json.null()))
      _, _ ->
        list.key_find(fields, key) |> result.map(fn(value) { #(key, value) })
    }
  })
  |> raw_json.object
}
