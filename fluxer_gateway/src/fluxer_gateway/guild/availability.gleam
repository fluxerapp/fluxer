// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{type GuildFacts} as core_guild
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/session
import fluxer_gateway/guild/names
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/sessions/connect
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/table.{type Table}
import fluxer_gateway/session/control
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn recheck_interval_ms() -> Int {
  durations.guild().availability_recheck_interval_ms
}

pub type Mode {
  Available
  UnavailableForEveryone
  UnavailableForEveryoneButStaff
}

pub fn mode(facts: GuildFacts) -> Mode {
  case
    core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE"),
    core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF")
  {
    True, _ -> UnavailableForEveryone
    False, True -> UnavailableForEveryoneButStaff
    False, False -> Available
  }
}

fn hidden(facts: GuildFacts) -> Bool {
  core_guild.has_feature(facts, "UNAVAILABLE_HIDDEN")
  && mode(facts) != Available
}

fn cache() -> Table(Int, #(Mode, Bool)) {
  table.named(names.unavailability_table)
}

pub fn publish(facts: GuildFacts) -> Mode {
  let current = mode(facts)
  table.insert(cache(), id.guild_id_to_int(core_guild.id(facts)), #(
    current,
    hidden(facts),
  ))
  current
}

fn cached_mode(guild: GuildId) -> Mode {
  case table.lookup(cache(), id.guild_id_to_int(guild)) {
    Ok(#(found, _)) -> found
    Error(Nil) -> Available
  }
}

pub fn transition(old: GuildFacts, state: GuildState) -> GuildState {
  let facts = guild_state.facts(state)
  let before = mode(old)
  let after = publish(facts)
  case before, after {
    Available, UnavailableForEveryone -> disconnect(state, False, hidden(facts))
    Available, UnavailableForEveryoneButStaff ->
      disconnect(state, True, hidden(facts))
    UnavailableForEveryoneButStaff, UnavailableForEveryone ->
      disconnect(state, False, hidden(facts))
    UnavailableForEveryone, Available
    | UnavailableForEveryoneButStaff, Available
    -> rejoin_all(state)
    _, _ -> state
  }
}

pub fn voice_keep(
  old: GuildFacts,
  state: GuildState,
) -> Option(fn(UserId) -> Bool) {
  case mode(old), mode(guild_state.facts(state)) {
    Available, UnavailableForEveryone
    | UnavailableForEveryoneButStaff, UnavailableForEveryone
    -> Some(fn(_user) { False })
    Available, UnavailableForEveryoneButStaff ->
      Some(fn(user) { guild_state.is_staff(state, user) })
    _, _ -> None
  }
}

pub fn recheck(state: GuildState) -> GuildState {
  let facts = guild_state.facts(state)
  let cached = cached_mode(core_guild.id(facts))
  let current = mode(facts)
  let _ = publish(facts)
  case cached != Available && current == Available {
    True -> rejoin_all(state)
    False -> state
  }
}

fn disconnect(state: GuildState, staff_only: Bool, hidden: Bool) -> GuildState {
  let guild = guild_state.guild_id(state)
  guild_state.sessions(state)
  |> guild_sessions.session_refs
  |> list.fold(state, fn(acc, ref) {
    case staff_only && guild_state.is_staff(acc, ref.user) {
      True -> acc
      False -> {
        control.send(ref, control.GuildUnavailable(guild, hidden))
        connect.leave(acc, ref.id)
      }
    }
  })
}

pub fn resync_all(state: GuildState) -> #(GuildState, Int) {
  let guild = guild_state.guild_id(state)
  guild_state.sessions(state)
  |> guild_sessions.session_refs
  |> list.fold(#(state, 0), fn(acc, ref) {
    control.send(ref, control.GuildUnavailable(guild, False))
    #(connect.leave(acc.0, ref.id), acc.1 + 1)
  })
}

fn rejoin_all(state: GuildState) -> GuildState {
  let guild = guild_state.guild_id(state)
  guild_state.sessions(state)
  |> guild_sessions.session_refs
  |> list.each(fn(ref) { session.send(ref, session.GuildDown(guild, 0)) })
  state
}
