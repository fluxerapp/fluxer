// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/core/session.{type SessionId, session_id_to_string}
import fluxer_gateway/guild/dispatch/event.{type EventFacts, type Mentions}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/sessions/guild_create
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub fn should_receive(
  found: Entry,
  event_name: String,
  facts: EventFacts,
  small_guild: Bool,
  roles: fn() -> List(Int),
) -> Bool {
  case event_name {
    "TYPING_START" ->
      case entry.typing(found) {
        Some(flag) -> flag
        None -> effectively_active(found, small_guild)
      }
    _ ->
      bypasses(event_name, facts, found)
      || entry.is_bot(found)
      || effectively_active(found, small_guild)
      || passive_receives(event_name, facts, found, roles)
  }
}

fn effectively_active(found: Entry, small_guild: Bool) -> Bool {
  !entry.is_passive(found) || small_guild
}

fn bypasses(event_name: String, facts: EventFacts, found: Entry) -> Bool {
  case event_name {
    "GUILD_UPDATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE" -> True
    "GUILD_MEMBER_UPDATE" -> facts.user == Some(entry.user_int(found))
    _ -> False
  }
}

fn passive_receives(
  event_name: String,
  facts: EventFacts,
  found: Entry,
  roles: fn() -> List(Int),
) -> Bool {
  case event_name {
    "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE"
    | "GUILD_DELETE"
    | "GUILD_AUDIT_LOG_ENTRY_CREATE"
    | "PASSIVE_UPDATES" -> True
    "MESSAGE_CREATE" ->
      is_mentioned(facts.mentions, found, roles)
      || facts.author == Some(entry.user_int(found))
    "MESSAGE_UPDATE" -> is_mentioned(facts.mentions, found, roles)
    "GUILD_MEMBER_UPDATE" | "GUILD_MEMBER_REMOVE" ->
      facts.user == Some(entry.user_int(found))
    _ -> False
  }
}

fn is_mentioned(
  mentions: Mentions,
  found: Entry,
  roles: fn() -> List(Int),
) -> Bool {
  mentions.everyone
  || mentions.here
  || set.contains(mentions.users, entry.user_int(found))
  || {
    !set.is_empty(mentions.roles)
    && list.any(roles(), set.contains(mentions.roles, _))
  }
}

pub fn set_active(state: GuildState, session: SessionId) -> GuildState {
  update(state, session, entry.set_mode(_, entry.Active))
}

pub fn set_passive(state: GuildState, session: SessionId) -> GuildState {
  update(state, session, fn(found) {
    found
    |> entry.set_mode(entry.Passive)
    |> entry.set_synced(False)
  })
  |> guild_threads.session_passive(session_id_to_string(session))
}

pub fn set_typing(
  state: GuildState,
  session: SessionId,
  flag: Option(Bool),
) -> GuildState {
  update(state, session, entry.set_typing(_, flag))
}

pub fn send_guild_sync(state: GuildState, session: SessionId) -> GuildState {
  case table.get(guild_state.sessions(state), session) {
    None -> state
    Some(found) ->
      case entry.is_synced(found) {
        True -> state
        False -> {
          let #(state, payload) =
            guild_create.build(
              state,
              entry.user(found),
              entry.flags(found),
              entry.is_bot(found),
            )
          relay.send(
            [entry.ref(found)],
            guild_create.sync_message(state, payload),
          )
          update(state, session, entry.set_synced(_, True))
        }
      }
  }
}

fn update(
  state: GuildState,
  session: SessionId,
  with: fn(Entry) -> Entry,
) -> GuildState {
  guild_state.set_sessions(
    state,
    table.update(guild_state.sessions(state), session, with),
  )
}
