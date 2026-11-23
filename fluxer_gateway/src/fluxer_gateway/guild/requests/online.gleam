// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/perms/query
import gleam/int
import gleam/list
import gleam/option.{None, Some}

fn viewer_count_ttl_ms() -> Int {
  durations.guild().online_viewer_count_ttl_ms
}

pub fn public_online(state: GuildState) -> Int {
  case perms.public_online(guild_state.perms_reader(state), online(state)) {
    Ok(query.OpenAccess) -> guild_state.online_count(state)
    Ok(query.Restricted(count)) -> count
    Ok(query.NoPublicChannel) -> 0
    Error(_) -> 0
  }
}

pub fn mutual_online(state: GuildState, viewer: UserId) -> #(GuildState, Int) {
  let now = clock.monotonic_ms()
  let key = id.user_id_to_int(viewer)
  let sessions = guild_state.sessions(state)
  case table.viewer_count(sessions, key, now) {
    Some(count) -> #(state, count)
    None -> {
      let count = compute_mutual(state, viewer)
      #(
        guild_state.set_sessions(
          state,
          table.put_viewer_count(
            sessions,
            key,
            count,
            now,
            viewer_count_ttl_ms(),
          ),
        ),
        count,
      )
    }
  }
}

fn compute_mutual(state: GuildState, viewer: UserId) -> Int {
  case
    perms.mutual_online(guild_state.perms_reader(state), viewer, online(state))
  {
    Ok(query.SeesEveryone) -> guild_state.online_count(state)
    Ok(query.Mutual(count)) -> count
    Error(_) -> 0
  }
}

fn online(state: GuildState) -> PackedIds {
  table.online_users(guild_state.sessions(state))
  |> list.sort(int.compare)
  |> packed_ids.from_list
}
