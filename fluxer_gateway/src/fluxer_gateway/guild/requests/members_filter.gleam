// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/permissions
import fluxer_gateway/gateway/limits
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result

fn guild_window_ms() -> Int {
  durations.guild().members_filter_guild_window_ms
}

const guild_max_events = 40

fn bot_full_list_window_ms() -> Int {
  durations.guild().members_filter_bot_full_list_window_ms
}

pub fn admit(
  stamps: List(Int),
  now_ms: Int,
  window_ms: Int,
  max_events: Int,
) -> Result(List(Int), Nil) {
  let recent = list.filter(stamps, fn(stamp) { now_ms - stamp < window_ms })
  case list.length(recent) >= max_events {
    True -> Error(Nil)
    False -> Ok([now_ms, ..recent])
  }
}

pub fn check_guild(state: GuildState, now_ms: Int) -> Result(GuildState, Nil) {
  case limits.rate_limits_disabled() {
    True -> Ok(state)
    False -> {
      let sessions = guild_state.sessions(state)
      use stamps <- result.map(admit(
        table.guild_window(sessions),
        now_ms,
        guild_window_ms(),
        guild_max_events,
      ))
      guild_state.set_sessions(state, table.set_guild_window(sessions, stamps))
    }
  }
}

pub fn check_bot_full_list(
  state: GuildState,
  full: Bool,
  bot: Bool,
  user: UserId,
  now_ms: Int,
) -> Result(GuildState, Int) {
  case full && bot && !limits.rate_limits_disabled() {
    False -> Ok(state)
    True -> {
      let key = id.user_id_to_int(user)
      let sessions = guild_state.sessions(state)
      let window_ms = bot_full_list_window_ms()
      case table.bot_full_list(sessions, key) {
        Some(last) if now_ms - last < window_ms ->
          Error(int.max(0, window_ms - { now_ms - last }))
        _ ->
          Ok(guild_state.set_sessions(
            state,
            table.set_bot_full_list(sessions, key, now_ms),
          ))
      }
    }
  }
}

pub fn is_full_list(query: String, limit: Int, user_ids: List(Int)) -> Bool {
  query == "" && limit == 0 && user_ids == []
}

pub fn may_list(
  state: GuildState,
  user: UserId,
  bot: Bool,
  full: Bool,
) -> Bool {
  case full, bot {
    False, _ -> True
    True, True -> True
    True, False ->
      perms.check(
        guild_state.perms_reader(state),
        user,
        perms.GuildScope,
        permissions.of([
          permissions.ManageRoles,
          permissions.KickMembers,
          permissions.BanMembers,
        ]),
      )
      == Ok(True)
  }
}
