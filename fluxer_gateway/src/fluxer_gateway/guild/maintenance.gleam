// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/guild/counts_cache/shard as counts_shard
import fluxer_gateway/guild/data
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/members
import fluxer_gateway/guild/read_model
import fluxer_gateway/guild/requests/online
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

pub fn count_refresh_interval_ms() -> Int {
  durations.guild().maintenance_count_refresh_interval_ms
}

pub type MaintenanceError {
  MembersTableLost
}

pub fn refresh_counts(
  state: GuildState,
) -> Result(GuildState, MaintenanceError) {
  let table = guild_state.members_table(state)
  case members.is_alive(table) {
    False -> Error(MembersTableLost)
    True -> {
      let count = members.count(table)
      let state = case count == guild_state.member_count(state) {
        True -> state
        False ->
          guild_state.set_guild_data(
            state,
            data.set_member_count(guild_state.guild_data(state), count),
          )
      }
      Ok(update_counts(state))
    }
  }
}

pub fn update_counts(state: GuildState) -> GuildState {
  let state = guild_state.set_public_online(state, online.public_online(state))
  publish_counts(state)
  state
}

pub fn publish_counts(state: GuildState) -> Nil {
  counts_shard.put(
    guild_state.guild_id(state),
    guild_state.member_count(state),
    guild_state.public_online(state),
  )
}

pub fn publish_view(state: GuildState, handle: Handle) -> Nil {
  read_model.put(guild_state.guild_id(state), view(state, handle))
}

pub fn view(state: GuildState, handle: Handle) -> read_model.View {
  read_model.View(
    handle:,
    data: guild_state.guild_data(state),
    members: guild_state.members_table(state),
    reader: guild_state.perms_reader(state),
    counts: guild_state.counts_handle(state),
    public_online: guild_state.public_online(state),
    version: guild_state.version(state),
  )
}

pub fn prune_member_subscriptions(
  state: GuildState,
  event: String,
  payload: Dynamic,
) -> GuildState {
  let event = string.uppercase(event)
  case requires_prune(event) {
    False -> state
    True -> {
      let changed = case event {
        "GUILD_MEMBER_UPDATE" | "GUILD_MEMBER_REMOVE" ->
          data_decode.user_id_of(payload, ["user", "id"])
          |> result.map(id.user_id_to_int)
          |> option.from_result
        _ -> None
      }
      prune(state, changed)
    }
  }
}

pub fn prune_all(state: GuildState) -> GuildState {
  prune(state, None)
}

fn prune(state: GuildState, changed: Option(Int)) -> GuildState {
  let #(state, released) =
    guild_state.sessions(state)
    |> guild_sessions.subscription_lists
    |> list.fold(#(state, []), fn(acc, pair) {
      let #(state, released) = acc
      let #(key, users) = pair
      let sessions = guild_state.sessions(state)
      case guild_sessions.get_key(sessions, key) {
        None -> acc
        Some(found) ->
          case stale_subscriptions(state, found, users, changed) {
            [] -> acc
            stale -> {
              let gone = set.from_list(stale)
              let kept =
                list.filter(users, fn(user) { !set.contains(gone, user) })
              let #(sessions, _, removed) =
                guild_sessions.replace_subscriptions(sessions, key, kept)
              #(
                guild_state.set_sessions(state, sessions),
                list.append(removed, released),
              )
            }
          }
      }
    })
  session_presence.release(state, list.unique(released))
}

fn requires_prune(event: String) -> Bool {
  case event {
    "GUILD_MEMBER_REMOVE"
    | "GUILD_MEMBER_UPDATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE" -> True
    _ -> False
  }
}

fn stale_subscriptions(
  state: GuildState,
  found: Entry,
  users: List(Int),
  changed: Option(Int),
) -> List(Int) {
  let viewer = entry.user_int(found)
  let candidates =
    case changed {
      Some(user) if user != viewer ->
        list.filter(users, fn(member) { member == user })
      _ -> users
    }
    |> list.filter(fn(member) { member != viewer })
  case candidates {
    [] -> []
    _ -> {
      let packed =
        candidates
        |> list.unique
        |> list.sort(int.compare)
        |> packed_ids.from_list
      case
        perms.shares_view_many(
          guild_state.perms_reader(state),
          entry.user(found),
          packed,
        )
      {
        Error(_) -> []
        Ok(shared) -> {
          let kept = set.from_list(packed_ids.to_list(shared))
          list.filter(candidates, fn(member) { !set.contains(kept, member) })
        }
      }
    }
  }
}
