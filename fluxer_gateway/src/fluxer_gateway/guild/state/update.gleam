// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/state/channels as channel_events
import fluxer_gateway/guild/state/members as member_events
import fluxer_gateway/guild/state/roles as role_events
import fluxer_gateway/guild/user_data
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

pub type Applied {
  Applied(
    state: GuildState,
    member: member_events.Change,
    self_dispatch: Option(Dynamic),
  )
}

pub fn apply_event(
  state: GuildState,
  event_name: String,
  event: Dynamic,
) -> Applied {
  let table = guild_state.members_table(state)
  let author_update =
    user_data.refresh_author(
      table,
      guild_state.guild_id(state),
      event_name,
      event,
    )
  let current = guild_state.guild_data(state)
  let #(next, change) = case event_name {
    "GUILD_MEMBER_ADD" -> member_events.add(current, table, event)
    "GUILD_MEMBER_UPDATE" -> member_events.update(current, table, event)
    "GUILD_MEMBER_REMOVE" -> member_events.remove(current, table, event)
    _ -> #(
      other_event(current, table, event_name, event),
      member_events.Unchanged,
    )
  }
  let state = case data_changed(event_name, change, current, next) {
    True -> guild_state.set_guild_data(state, next)
    False -> guild_state.replace_guild_data(state, next)
  }
  Applied(state:, member: change, self_dispatch: author_update)
}

fn other_event(
  current: GuildData,
  table: MembersTable,
  event_name: String,
  event: Dynamic,
) -> GuildData {
  case event_name {
    "GUILD_UPDATE" -> channel_events.guild_update(current, event)
    "GUILD_ROLE_CREATE" | "GUILD_ROLE_UPDATE" ->
      role_events.create_or_update(current, event)
    "GUILD_ROLE_UPDATE_BULK" -> role_events.update_bulk(current, event)
    "GUILD_ROLE_DELETE" -> role_events.delete(current, table, event)
    "CHANNEL_CREATE" -> channel_events.create(current, event)
    "CHANNEL_UPDATE" -> channel_events.update(current, event)
    "CHANNEL_UPDATE_BULK" -> channel_events.update_bulk(current, event)
    "CHANNEL_DELETE" -> channel_events.delete(current, event)
    "MESSAGE_CREATE" -> channel_events.message_create(current, event)
    "CHANNEL_PINS_UPDATE" -> channel_events.pins_update(current, event)
    "GUILD_EMOJIS_UPDATE" -> channel_events.emojis_update(current, event)
    "GUILD_STICKERS_UPDATE" -> channel_events.stickers_update(current, event)
    _ -> current
  }
}

fn mutates_guild_data(event_name: String) -> Bool {
  case event_name {
    "GUILD_MEMBER_ADD"
    | "GUILD_MEMBER_UPDATE"
    | "GUILD_MEMBER_REMOVE"
    | "GUILD_ROLE_CREATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE"
    | "GUILD_UPDATE"
    | "GUILD_EMOJIS_UPDATE"
    | "GUILD_STICKERS_UPDATE" -> True
    _ -> False
  }
}

fn data_changed(
  event_name: String,
  change: member_events.Change,
  current: GuildData,
  next: GuildData,
) -> Bool {
  case change {
    member_events.Unchanged -> mutates_guild_data(event_name) || next != current
    _ -> True
  }
}
