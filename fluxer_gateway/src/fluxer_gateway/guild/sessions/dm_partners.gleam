// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type ChannelKind, type Overwrite}
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/role
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/session/control
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const max_partners = 1000

type Scope {
  NoScope
  UserScope(user: Int)
  AllScope
}

pub fn update(
  state: GuildState,
  session_id: SessionId,
  partners: List(Int),
) -> GuildState {
  case table.get(guild_state.sessions(state), session_id) {
    None -> state
    Some(found) -> {
      let user = entry.user_int(found)
      let wanted =
        partners
        |> list.filter(fn(partner) { partner != user })
        |> list.take(max_partners)
      store(state, found, wanted, evaluate(state, found, wanted))
    }
  }
}

type VisibilityInputs {
  VisibilityInputs(
    owner: Option(UserId),
    roles: List(#(Int, Permissions)),
    channels: List(#(Int, ChannelKind, Option(ChannelId), List(Overwrite))),
  )
}

pub fn reevaluate(
  before: GuildState,
  state: GuildState,
  event_name: String,
  data: JsonTree,
) -> GuildState {
  case scope(event_name, data) {
    AllScope ->
      case any_registered(state) && visibility_changed(before, state) {
        True -> reevaluate_scope(state, AllScope)
        False -> state
      }
    target -> reevaluate_scope(state, target)
  }
}

fn any_registered(state: GuildState) -> Bool {
  table.entries(guild_state.sessions(state))
  |> list.any(fn(found) { entry.dm_partners(found) != [] })
}

fn visibility_changed(before: GuildState, state: GuildState) -> Bool {
  visibility_inputs(guild_state.guild_data(before))
  != visibility_inputs(guild_state.guild_data(state))
}

fn visibility_inputs(guild: GuildData) -> VisibilityInputs {
  VisibilityInputs(
    owner: core_guild.owner(guild.facts),
    roles: guild.roles
      |> list.map(fn(row) {
        #(id.role_id_to_int(role.id(row)), role.permissions(row))
      })
      |> list.sort(fn(a, b) { int.compare(a.0, b.0) }),
    channels: guild.channels
      |> list.map(fn(row) {
        #(
          id.channel_id_to_int(channel.id(row)),
          channel.kind(row),
          channel.parent(row),
          channel.overwrites(row),
        )
      })
      |> list.sort(fn(a, b) { int.compare(a.0, b.0) }),
  )
}

pub fn reevaluate_all(state: GuildState) -> GuildState {
  reevaluate_scope(state, AllScope)
}

pub fn reevaluate_user(state: GuildState, user: UserId) -> GuildState {
  reevaluate_scope(state, UserScope(id.user_id_to_int(user)))
}

fn reevaluate_scope(state: GuildState, target: Scope) -> GuildState {
  case target {
    NoScope -> state
    _ ->
      table.entries(guild_state.sessions(state))
      |> list.filter(fn(found) {
        entry.dm_partners(found) != [] && involves(found, target)
      })
      |> list.fold(state, fn(acc, found) {
        let partners = entry.dm_partners(found)
        store(acc, found, partners, evaluate(acc, found, partners))
      })
  }
}

fn involves(found: Entry, target: Scope) -> Bool {
  case target {
    AllScope -> True
    NoScope -> False
    UserScope(user) ->
      entry.user_int(found) == user
      || list.contains(entry.dm_partners(found), user)
  }
}

fn scope(event_name: String, data: JsonTree) -> Scope {
  case event_name {
    "GUILD_MEMBER_ADD" | "GUILD_MEMBER_REMOVE" | "GUILD_MEMBER_UPDATE" ->
      case event.event_user(data) {
        Some(user) -> UserScope(user)
        None -> AllScope
      }
    "GUILD_UPDATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE" -> AllScope
    _ -> NoScope
  }
}

fn evaluate(state: GuildState, found: Entry, partners: List(Int)) -> List(Int) {
  case partners {
    [] -> []
    _ -> {
      let candidates =
        partners
        |> list.unique
        |> list.sort(int.compare)
        |> packed_ids.from_list
      perms.shares_view_many(
        guild_state.perms_reader(state),
        entry.user(found),
        candidates,
      )
      |> result.map(packed_ids.to_list)
      |> result.unwrap([])
    }
  }
}

fn store(
  state: GuildState,
  found: Entry,
  partners: List(Int),
  eligible: List(Int),
) -> GuildState {
  case eligible == entry.dm_eligible(found) {
    True -> Nil
    False ->
      control.send(
        entry.ref(found),
        control.DmPartnerMutual(
          guild_state.guild_id(state),
          packed_ids.from_list(eligible),
        ),
      )
  }
  let updated = entry.set_dm_partners(found, partners:, eligible:)
  guild_state.set_sessions(
    state,
    table.put(guild_state.sessions(state), updated),
  )
}
