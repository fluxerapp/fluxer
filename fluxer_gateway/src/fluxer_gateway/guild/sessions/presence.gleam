// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/push/engaged.{type EngagementChange}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/presence/client
import gleam/int
import gleam/list

pub fn watch(
  state: GuildState,
  users: List(Int),
) -> #(GuildState, List(EngagementChange)) {
  let sessions = guild_state.sessions(state)
  case unwatched(state, users) {
    [] -> #(state, [])
    fresh -> {
      let state =
        guild_state.set_sessions(state, table.add_interest(sessions, fresh))
      let packed = packed_ids.from_list(fresh)
      client.register_guild_interest(
        guild_state.guild_id(state),
        packed,
        guild_state.self_ref(state).inbox,
      )
      client.lookup_many(packed)
      |> list.fold(#(state, []), fn(acc, found) {
        let #(state, changes) = acc
        let #(state, more) = rows.apply(state, found, rows.Deferred)
        #(state, list.append(more, changes))
      })
    }
  }
}

pub fn watch_and_resend(state: GuildState, users: List(Int)) -> GuildState {
  case unwatched(state, users) {
    [] -> state
    fresh -> {
      let sessions = guild_state.sessions(state)
      let state =
        guild_state.set_sessions(state, table.add_interest(sessions, fresh))
      client.register_guild_interest_and_resend(
        guild_state.guild_id(state),
        packed_ids.from_list(fresh),
        guild_state.self_ref(state).inbox,
      )
      state
    }
  }
}

fn unwatched(state: GuildState, users: List(Int)) -> List(Int) {
  let sessions = guild_state.sessions(state)
  users
  |> list.unique
  |> list.filter(fn(user) { !table.is_interested(sessions, user) })
  |> list.sort(int.compare)
}

pub fn release(state: GuildState, users: List(Int)) -> GuildState {
  let sessions = guild_state.sessions(state)
  unwatch(
    state,
    list.filter(users, fn(user) {
      !table.is_connected(sessions, user)
      && !table.has_subscribers(sessions, user)
    }),
  )
}

pub fn unwatch(state: GuildState, users: List(Int)) -> GuildState {
  let sessions = guild_state.sessions(state)
  let watched =
    users
    |> list.unique
    |> list.filter(fn(user) { table.is_interested(sessions, user) })
    |> list.sort(int.compare)
  case watched {
    [] -> state
    _ -> {
      client.unregister_guild_interest(
        guild_state.guild_id(state),
        packed_ids.from_list(watched),
      )
      table.remove_interest(sessions, watched)
      |> list.fold(watched, _, table.delete_row)
      |> guild_state.set_sessions(state, _)
    }
  }
}
