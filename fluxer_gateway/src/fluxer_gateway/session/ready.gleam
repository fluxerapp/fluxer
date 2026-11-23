// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild_create.{
  type GuildCreate, Available, ReadyGuild, Unavailable,
}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/gateway/sharding
import fluxer_gateway/session/payload
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub type ReadyData {
  ReadyData(
    fields: List(#(String, RawJson)),
    relationships: List(RawJson),
    users: Dict(Int, RawJson),
    sessions: RawJson,
  )
}

pub type Build {
  Build(
    data: ReadyData,
    session_id: String,
    version: Int,
    shard: Option(#(Int, Int)),
    bot: Bool,
    staff: Bool,
    guilds: List(GuildId),
    collected: List(GuildCreate),
    presences: List(Presence),
    timings: Option(Json),
  )
}

pub type Built {
  Built(payload: RawJson, guild_members: List(#(Int, List(Int))))
}

const replaced_fields = [
  "guilds", "sessions", "presences", "users", "version", "session_id",
  "relationships", "shard", "_timings_gw",
]

pub fn empty() -> ReadyData {
  ReadyData(
    fields: [],
    relationships: [],
    users: dict.new(),
    sessions: no_sessions(),
  )
}

pub fn no_sessions() -> RawJson {
  raw_json.wrap(<<"[]":utf8>>)
}

pub fn unavailable_guild(guild: GuildId, hidden: Bool) -> RawJson {
  let base = [
    #("id", id.json(id.guild_id_to_int(guild))),
    #("unavailable", json.bool(True)),
  ]
  let fields = case hidden {
    True -> list.append(base, [#("unavailable_hidden", json.bool(True))])
    False -> base
  }
  raw_json.from_json(json.object(fields))
}

pub fn deleted_guild(guild: GuildId) -> RawJson {
  raw_json.from_json(json.object([#("id", id.json(id.guild_id_to_int(guild)))]))
}

pub fn build(build: Build) -> RawJson {
  assemble(build).payload
}

pub fn assemble(build: Build) -> Built {
  let chronological = list.reverse(build.collected)
  let #(guilds, member_users, guild_members) = case build.bot {
    True -> #(
      list.map(build.guilds, fn(guild) { unavailable_guild(guild, False) }),
      dict.new(),
      [],
    )
    False -> full_guilds(chronological, build.guilds)
  }
  let users = case build.bot {
    True -> []
    False ->
      build.data.users
      |> dict.merge(member_users)
      |> dict.values
  }
  let presences = case build.bot {
    True -> []
    False -> dedup_presences(build.presences)
  }
  let base =
    list.filter(build.data.fields, fn(entry) {
      !list.contains(replaced_fields, entry.0)
      && { build.staff || entry.0 != "_timings" }
    })
  let fields =
    list.flatten([
      base,
      [
        #("relationships", raw_json.array(build.data.relationships)),
        #("guilds", raw_json.array(guilds)),
        #("sessions", build.data.sessions),
        #("presences", raw_json.array(list.map(presences, presence.wire))),
        #("users", raw_json.array(users)),
        #("version", raw_json.from_json(json.int(build.version))),
        #("session_id", raw_json.from_json(json.string(build.session_id))),
      ],
      case build.shard {
        Some(shard) -> [
          #("shard", raw_json.from_json(sharding.shard_to_json(shard))),
        ]
        None -> []
      },
      case build.staff, build.timings {
        True, Some(timings) -> [#("_timings_gw", raw_json.from_json(timings))]
        _, _ -> []
      },
    ])
  Built(payload: raw_json.object(fields), guild_members:)
}

fn dedup_presences(presences: List(Presence)) -> List(Presence) {
  presences
  |> list.fold(dict.new(), fn(acc, p) {
    dict.insert(acc, id.user_id_to_int(presence.user(p)), p)
  })
  |> dict.values
}

fn full_guilds(
  chronological: List(GuildCreate),
  all_guilds: List(GuildId),
) -> #(List(RawJson), Dict(Int, RawJson), List(#(Int, List(Int)))) {
  let #(wires, users, guild_members) =
    list.fold(chronological, #([], dict.new(), []), fn(acc, collected) {
      let #(out, users, guild_members) = acc
      case collected {
        Unavailable(guild, hidden) -> #(
          [unavailable_guild(guild, hidden), ..out],
          users,
          guild_members,
        )
        Available(guild, body) -> {
          let ReadyGuild(wire:, users: found) = guild_create.ready(body)
          #(
            [wire, ..out],
            list.fold(found, users, fn(users, entry) {
              dict.insert(users, entry.0, entry.1)
            }),
            [
              #(
                id.guild_id_to_int(guild),
                list.map(found, fn(entry) { entry.0 }),
              ),
              ..guild_members
            ],
          )
        }
      }
    })
  let seen =
    chronological
    |> list.map(fn(collected) {
      id.guild_id_to_int(guild_create.guild(collected))
    })
    |> set.from_list
  let missing =
    all_guilds
    |> list.filter(fn(guild) { !set.contains(seen, id.guild_id_to_int(guild)) })
    |> list.map(fn(guild) { unavailable_guild(guild, False) })
  #(list.append(list.reverse(wires), missing), users, guild_members)
}

pub fn user_decoder() -> Decoder(#(Int, RawJson)) {
  use fields <- decode.then(decode.dict(decode.string, payload.raw()))
  case guild_create.partial_user(dict.to_list(fields)) {
    Ok(found) -> decode.success(found)
    Error(Nil) -> decode.failure(#(0, raw_json.null()), "user id")
  }
}
