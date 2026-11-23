// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/requests/members_filter
import fluxer_gateway/guild/requests/members_search
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/presence/client
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

const max_user_ids = 100

const max_nonce_length = 32

const max_query_limit = 100

const chunk_size = 1000

const request_guild_members_opcode = 8

pub type Request {
  Request(
    guild_ids: List(Int),
    query: String,
    limit: Int,
    user_ids: List(Int),
    presences: Bool,
    nonce: Option(String),
  )
}

pub fn parse(data: JsonTree) -> Result(Request, Nil) {
  let guild_ids = case
    json_tree.field(data, "guild_ids") |> result.try(json_tree.items)
  {
    Ok([_, ..] as items) -> guild_id_list(items)
    _ ->
      json_tree.snowflake_field(data, "guild_id")
      |> result.map(fn(guild) { [guild] })
  }
  let user_items =
    json_tree.field(data, "user_ids")
    |> result.try(json_tree.items)
    |> result.unwrap([])
  case guild_ids, list.length(user_items) > max_user_ids {
    Ok(guilds), False ->
      Ok(
        Request(
          guild_ids: guilds,
          query: text(data, "query") |> option.unwrap(""),
          limit: case
            json_tree.field(data, "limit") |> result.try(json_tree.int)
          {
            Ok(limit) if limit >= 0 -> int.min(limit, max_query_limit)
            _ -> 0
          },
          user_ids: list.filter_map(user_items, json_tree.snowflake),
          presences: json_tree.field(data, "presences")
          |> result.try(json_tree.bool)
            == Ok(True),
          nonce: case guilds {
            [_] -> nonce(text(data, "nonce"))
            _ -> None
          },
        ),
      )
    _, _ -> Error(Nil)
  }
}

fn guild_id_list(items: List(JsonTree)) -> Result(List(Int), Nil) {
  list.try_map(items, json_tree.snowflake)
  |> result.map(list.unique)
}

fn text(data: JsonTree, key: String) -> Option(String) {
  json_tree.field(data, key)
  |> result.try(json_tree.string)
  |> option.from_result
}

fn nonce(text: Option(String)) -> Option(String) {
  use found <- option.then(text)
  case string.byte_size(found) <= max_nonce_length {
    True -> Some(found)
    False -> None
  }
}

pub fn handle(state: GuildState, ref: SessionRef, data: Dynamic) -> GuildState {
  let guild = id.guild_id_to_int(guild_state.guild_id(state))
  case
    parse(json_tree.from_term(data)),
    table.get(guild_state.sessions(state), ref.id)
  {
    Ok(request), Some(found) ->
      case list.contains(request.guild_ids, guild) {
        False -> state
        True -> {
          let bot = entry.is_bot(found)
          case bot && list.length(request.guild_ids) > 1 {
            True -> state
            False -> admitted(state, ref, request, bot)
          }
        }
      }
    _, _ -> state
  }
}

fn admitted(
  state: GuildState,
  ref: SessionRef,
  request: Request,
  bot: Bool,
) -> GuildState {
  let now = clock.system_ms()
  case members_filter.check_guild(state, now) {
    Error(Nil) -> state
    Ok(state) -> {
      let full =
        members_filter.is_full_list(
          request.query,
          request.limit,
          request.user_ids,
        )
      case members_filter.check_bot_full_list(state, full, bot, ref.user, now) {
        Error(retry_ms) -> {
          rate_limited(state, ref, request, retry_ms)
          state
        }
        Ok(state) ->
          case members_filter.may_list(state, ref.user, bot, full) {
            False -> state
            True -> {
              respond(state, ref, request)
              state
            }
          }
      }
    }
  }
}

fn rate_limited(
  state: GuildState,
  ref: SessionRef,
  request: Request,
  retry_ms: Int,
) -> Nil {
  let guild = #("guild_id", id.guild_json(guild_state.guild_id(state)))
  let meta = case request.nonce {
    Some(found) -> [guild, #("nonce", json.string(found))]
    None -> [guild]
  }
  json.object([
    #("opcode", json.int(request_guild_members_opcode)),
    #("retry_after", json.float(int.to_float(retry_ms) /. 1000.0)),
    #("meta", json.object(meta)),
  ])
  |> raw_json.from_json
  |> send.deliver(state, [ref], "RATE_LIMITED", _)
}

fn respond(state: GuildState, ref: SessionRef, request: Request) -> Nil {
  let found =
    members_search.fetch(state, request.query, request.limit, request.user_ids)
  let presences = case request.presences {
    True -> online_presences(found)
    False -> dict.new()
  }
  let parts = chunks(found)
  let total = list.length(parts)
  list.index_map(parts, fn(part, index) {
    send.deliver(
      state,
      [ref],
      "GUILD_MEMBERS_CHUNK",
      chunk_payload(state, part, presences, index, total, request.nonce),
    )
  })
  Nil
}

fn online_presences(found: List(Member)) -> dict.Dict(Int, Presence) {
  case found {
    [] -> dict.new()
    _ ->
      found
      |> list.map(fn(item) { id.user_id_to_int(member.user_id(item)) })
      |> list.unique
      |> list.sort(int.compare)
      |> packed_ids.from_list
      |> client.lookup_many
      |> list.filter(fn(item) { presence.is_online(presence.status(item)) })
      |> list.map(fn(item) { #(id.user_id_to_int(presence.user(item)), item) })
      |> dict.from_list
  }
}

fn chunks(found: List(Member)) -> List(List(Member)) {
  case list.sized_chunk(found, chunk_size) {
    [] -> [[]]
    parts -> parts
  }
}

fn chunk_payload(
  state: GuildState,
  part: List(Member),
  presences: dict.Dict(Int, Presence),
  index: Int,
  total: Int,
  request_nonce: Option(String),
) -> raw_json.RawJson {
  let found_presences =
    list.filter_map(part, fn(item) {
      dict.get(presences, id.user_id_to_int(member.user_id(item)))
    })
  let base = [
    #(
      "members",
      json.preprocessed_array(
        list.map(part, fn(item) { raw_json.to_json(member.wire(item)) }),
      ),
    ),
    #("chunk_index", json.int(index)),
    #("chunk_count", json.int(total)),
    #("guild_id", id.guild_json(guild_state.guild_id(state))),
  ]
  let with_presences: List(#(String, Json)) = case found_presences {
    [] -> base
    _ ->
      list.append(base, [
        #(
          "presences",
          json.preprocessed_array(
            list.map(found_presences, fn(item) {
              raw_json.to_json(presence.wire(item))
            }),
          ),
        ),
      ])
  }
  case request_nonce {
    Some(found) -> list.append(with_presences, [#("nonce", json.string(found))])
    None -> with_presences
  }
  |> json.object
  |> raw_json.from_json
}
