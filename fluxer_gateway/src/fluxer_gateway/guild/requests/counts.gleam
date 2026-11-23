// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/member_list
import fluxer_gateway/perms
import fluxer_gateway/threads/store as thread_store
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

const max_channel_ids = 25

const max_nonce_bytes = 64

pub fn channel_counts(
  state: GuildState,
  ref: SessionRef,
  data: Dynamic,
) -> GuildState {
  let parsed = json_tree.from_term(data)
  let guild = guild_state.guild_id(state)
  let found = case
    json_tree.snowflake_field(parsed, "guild_id")
    == Ok(id.guild_id_to_int(guild)),
    table.get(guild_state.sessions(state), ref.id),
    channel_ids(parsed)
  {
    True, Some(_), [_, ..] as requested -> entries(state, ref, requested)
    _, _, _ -> []
  }
  reply(ref, "CHANNEL_MEMBER_COUNTS_UPDATE", found, nonce(parsed))
  state
}

fn entries(
  state: GuildState,
  ref: SessionRef,
  requested: List(Int),
) -> List(Json) {
  let reader = guild_state.perms_reader(state)
  let store = guild_state.threads(state)
  let allowed =
    list.filter_map(requested, fn(channel_int) {
      use channel_id <- result.try(
        id.channel_id(channel_int) |> result.replace_error(Nil),
      )
      case
        perms.can_view(reader, ref.user, channel_id) == Ok(True)
        && perms.can_view_members(reader, ref.user, channel_id) == Ok(True)
        && thread_store.thread(store, channel_id) == None
      {
        True -> Ok(channel_id)
        False -> Error(Nil)
      }
    })
  let guild_id = id.guild_json(guild_state.guild_id(state))
  case allowed {
    [] -> []
    _ ->
      member_list.channel_counts(guild_state.member_list(state), allowed)
      |> result.map(
        list.map(_, fn(found) {
          let #(channel_id, member_count, online_count) = found
          json.object([
            #("guild_id", guild_id),
            #("channel_id", id.channel_json(channel_id)),
            #("member_count", json.int(member_count)),
            #("online_count", json.int(online_count)),
          ])
        }),
      )
      |> result.unwrap([])
  }
}

fn channel_ids(parsed: JsonTree) -> List(Int) {
  let raw = case
    json_tree.field(parsed, "channel_ids") |> result.try(json_tree.items)
  {
    Ok(items) -> items
    Error(Nil) -> result.values([json_tree.field(parsed, "channel_id")])
  }
  list.filter_map(raw, json_tree.snowflake)
  |> list.unique
  |> list.sort(int.compare)
  |> list.take(max_channel_ids)
}

fn nonce(parsed: JsonTree) -> Option(String) {
  use text <- option.then(
    json_tree.field(parsed, "nonce")
    |> result.try(json_tree.string)
    |> option.from_result,
  )
  case string.byte_size(text) {
    size if size > 0 && size <= max_nonce_bytes -> Some(text)
    _ -> None
  }
}

fn reply(
  ref: SessionRef,
  event_name: String,
  entries: List(Json),
  found_nonce: Option(String),
) -> Nil {
  let base = [#("counts", json.preprocessed_array(entries))]
  let fields = case found_nonce {
    Some(text) -> list.append(base, [#("nonce", json.string(text))])
    None -> base
  }
  relay.send(
    [ref],
    session.Dispatch(event_name, raw_json.from_json(json.object(fields))),
  )
}
