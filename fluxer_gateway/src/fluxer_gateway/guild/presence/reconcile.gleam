// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/presence/row.{type Display}
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/push/engaged.{type EngagementChange}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/presence/client
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/set

pub fn interval_ms() -> Int {
  durations.guild().reconcile_interval_ms
}

fn confirm_delay_ms() -> Int {
  durations.guild().reconcile_confirm_delay_ms
}

pub opaque type Mismatches {
  Mismatches(found: List(Mismatch))
}

type Mismatch {
  Mismatch(user: Int, fetched: Presence, seen: Display)
}

pub fn start(state: GuildState, deliver: fn(Mismatches) -> Nil) -> Nil {
  let sessions = guild_state.sessions(state)
  let users = table.connected_list(sessions) |> list.sort(int.compare)
  case users {
    [] -> Nil
    _ -> {
      let seen =
        users
        |> list.map(fn(user) { #(user, shown_display(sessions, user)) })
        |> dict.from_list
      process.spawn_unlinked(fn() {
        deliver(Mismatches(confirmed(users, seen)))
      })
      Nil
    }
  }
}

fn confirmed(users: List(Int), seen: Dict(Int, Display)) -> List(Mismatch) {
  case observe(users, seen) {
    [] -> []
    first -> {
      process.sleep(confirm_delay_ms())
      let keys = set.from_list(list.map(first, mismatch_key))
      observe(list.map(first, fn(item) { item.user }), seen)
      |> list.filter(fn(item) { set.contains(keys, mismatch_key(item)) })
    }
  }
}

fn mismatch_key(item: Mismatch) -> #(Int, Display, Display) {
  #(item.user, row.presence_display(item.fetched), item.seen)
}

fn observe(users: List(Int), seen: Dict(Int, Display)) -> List(Mismatch) {
  let #(found, unknown) =
    users
    |> packed_ids.from_list
    |> client.lookup_known
  let fetched =
    found
    |> list.map(fn(found) { #(id.user_id_to_int(presence.user(found)), found) })
    |> dict.from_list
  let unknown = set.from_list(unknown)
  list.filter_map(users, fn(user) {
    let shown = dict.get(seen, user) |> result.unwrap(row.offline_display())
    case dict.get(fetched, user), id.user_id(user) {
      Ok(found), _ ->
        case row.presence_display(found) == shown {
          True -> Error(Nil)
          False -> Ok(Mismatch(user, found, shown))
        }
      Error(Nil), Ok(user_id) ->
        case shown == row.offline_display() || set.contains(unknown, user) {
          True -> Error(Nil)
          False -> Ok(Mismatch(user, offline(user_id), shown))
        }
      Error(Nil), Error(_) -> Error(Nil)
    }
  })
}

fn shown_display(sessions: table.Sessions, user: Int) -> Display {
  table.presence_row(sessions, user)
  |> option.map(row.display)
  |> option.unwrap(row.offline_display())
}

fn offline(user: id.UserId) -> Presence {
  presence.offline(
    user,
    raw_json.from_json(json.object([#("status", json.string("offline"))])),
  )
}

pub fn apply(
  state: GuildState,
  mismatches: Mismatches,
) -> #(GuildState, List(EngagementChange)) {
  list.fold(mismatches.found, #(state, []), fn(acc, item) {
    let #(state, changes) = acc
    let sessions = guild_state.sessions(state)
    let current = shown_display(sessions, item.user)
    case table.is_connected(sessions, item.user) && current == item.seen {
      True -> {
        let #(state, more) = rows.apply(state, item.fetched, rows.Immediate)
        #(state, list.append(more, changes))
      }
      False -> acc
    }
  })
}
