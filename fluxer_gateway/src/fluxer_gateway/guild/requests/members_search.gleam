// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/user
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/member_list
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

const full_list_limit = 100_000

const default_query_limit = 25

pub fn resolve_limit(query: String, limit: Int) -> Int {
  case query, limit {
    "", 0 -> full_list_limit
    _, 0 -> default_query_limit
    _, _ -> limit
  }
}

pub fn fetch(
  state: GuildState,
  query: String,
  limit: Int,
  user_ids: List(Int),
) -> List(Member) {
  case user_ids {
    [_, ..] ->
      user_ids
      |> list.unique
      |> list.sort(int.compare)
      |> list.filter_map(lookup(state, _))
    [] -> {
      let wanted = resolve_limit(query, limit)
      case query {
        "" -> listed(state, wanted)
        _ ->
          search(
            guild_state.members_table(state),
            string.lowercase(query),
            wanted,
          )
      }
    }
  }
}

fn lookup(state: GuildState, user_int: Int) -> Result(Member, Nil) {
  use user_id <- result.try(id.user_id(user_int) |> result.replace_error(Nil))
  guild_state.member(state, user_id) |> option.to_result(Nil)
}

fn listed(state: GuildState, limit: Int) -> List(Member) {
  case member_list.page_ids(guild_state.member_list(state), 0, limit) {
    Ok(ids) -> packed_ids.to_list(ids) |> list.filter_map(lookup(state, _))
    Error(_) -> []
  }
}

pub fn search(table: MembersTable, query: String, limit: Int) -> List(Member) {
  case limit > 0 {
    False -> []
    True ->
      members.fold_until(table, from: #([], 0), with: fn(acc, found) {
        case matches(found, query) {
          False -> list.Continue(acc)
          True -> {
            let #(kept, count) = acc
            let next = #([found, ..kept], count + 1)
            case count + 1 >= limit {
              True -> list.Stop(next)
              False -> list.Continue(next)
            }
          }
        }
      }).0
      |> list.reverse
  }
}

fn matches(found: Member, query: String) -> Bool {
  case member.nick(found) {
    Some(nick) -> string.starts_with(string.lowercase(nick), query)
    None -> False
  }
  || list.any(user.search_names(member.user(found)), string.starts_with(
    _,
    query,
  ))
}
