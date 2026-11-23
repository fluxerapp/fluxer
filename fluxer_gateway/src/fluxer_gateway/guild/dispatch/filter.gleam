// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/perms/audience.{type Audience, type Group}
import fluxer_gateway/threads/dispatch as thread_dispatch
import fluxer_gateway/threads/value.{type Object} as _
import fluxer_gateway/threads/viewers
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

pub type Recipients {
  Scoped(entries: List(Entry))
  InThread(entries: List(Entry))
}

pub fn recipients(
  state: GuildState,
  event_name: String,
  data: JsonTree,
  scope_data: Object,
  exclude: Option(String),
) -> #(GuildState, Recipients) {
  let threads = guild_state.threads(state)
  let candidates = case exclude {
    None -> table.entries(guild_state.sessions(state))
    Some(key) ->
      table.entries(guild_state.sessions(state))
      |> list.filter(fn(found) { entry.key(found) != key })
  }
  case
    thread_dispatch.scope_sessions(
      threads,
      guild_state.thread_context(state),
      event_name,
      scope_data,
      exclude,
    )
  {
    thread_dispatch.ThreadAudience(sessions) -> #(
      state,
      InThread(kept(candidates, sessions)),
    )
    thread_dispatch.Unscoped -> {
      let #(state, entries) = scoped(state, event_name, data, candidates)
      #(state, Scoped(entries))
    }
    thread_dispatch.ViewersOnly -> {
      let viewing =
        list.filter(candidates, fn(found) {
          viewers.is_viewer(threads, entry.key(found))
        })
      let #(state, entries) = scoped(state, event_name, data, viewing)
      #(state, Scoped(entries))
    }
  }
}

fn kept(candidates: List(Entry), sessions: List(SessionRef)) -> List(Entry) {
  let keys =
    list.map(sessions, fn(ref) { session.session_id_to_string(ref.id) })
    |> set.from_list
  list.filter(candidates, fn(found) { set.contains(keys, entry.key(found)) })
}

fn scoped(
  state: GuildState,
  event_name: String,
  data: JsonTree,
  candidates: List(Entry),
) -> #(GuildState, List(Entry)) {
  case event.scope(event_name, data) {
    event.GuildWide -> #(state, candidates)
    event.GuildPermissionEvent(required) -> #(
      state,
      with_permission(state, candidates, perms.GuildScope, required),
    )
    event.InviteEvent(None) -> #(state, [])
    event.InviteEvent(Some(channel_int)) ->
      case id.channel_id(channel_int) {
        Ok(channel_id) -> #(
          state,
          with_permission(
            state,
            candidates,
            perms.ChannelScope(channel_id),
            permissions.ManageChannels,
          ),
        )
        Error(_) -> #(state, [])
      }
    event.ChannelEvent(None) -> #(state, [])
    event.ChannelEvent(Some(channel_int)) ->
      channel_recipients(state, event_name, data, channel_int, candidates)
  }
}

fn with_permission(
  state: GuildState,
  candidates: List(Entry),
  scope: perms.Scope,
  required: permissions.Permission,
) -> List(Entry) {
  let reader = guild_state.perms_reader(state)
  let bits = permissions.of([required])
  list.filter(candidates, fn(found) {
    perms.check(reader, entry.user(found), scope, bits) == Ok(True)
  })
}

fn channel_recipients(
  state: GuildState,
  event_name: String,
  data: JsonTree,
  channel_int: Int,
  candidates: List(Entry),
) -> #(GuildState, List(Entry)) {
  case id.channel_id(channel_int), need(event_name, data) {
    Ok(channel_id), Ok(wanted) -> {
      in_audience(state, channel_id, wanted, candidates)
    }
    _, _ -> #(state, [])
  }
}

fn need(event_name: String, data: JsonTree) -> Result(audience.Need, Nil) {
  case event.is_message_access(event_name) {
    False -> Ok(audience.View)
    True ->
      event.message_id(data)
      |> option.to_result(Nil)
      |> result.try(fn(message) {
        id.message_id(message) |> result.replace_error(Nil)
      })
      |> result.map(audience.MessageAccess)
  }
}

pub fn in_audience(
  state: GuildState,
  channel_id: ChannelId,
  wanted: audience.Need,
  candidates: List(Entry),
) -> #(GuildState, List(Entry)) {
  let reader = guild_state.perms_reader(state)
  case perms.audience(reader, channel_id, wanted) {
    Error(_) -> #(state, [])
    Ok(found) -> {
      let revision = perms.revision(reader)
      let sessions = guild_state.sessions(state)
      let #(groups, allowed) =
        list.fold(
          candidates,
          #(table.groups(sessions, revision), []),
          fn(acc, item) {
            let #(groups, allowed) = acc
            let #(groups, group) = group_of(reader, groups, item)
            case member_of(found, group) {
              True -> #(groups, [item, ..allowed])
              False -> #(groups, allowed)
            }
          },
        )
      let state =
        guild_state.set_sessions(
          state,
          table.set_groups(sessions, revision, groups),
        )
      #(state, list.reverse(allowed))
    }
  }
}

fn group_of(
  reader: perms.Reader,
  groups: Dict(Int, Option(Group)),
  found: Entry,
) -> #(Dict(Int, Option(Group)), Option(Group)) {
  let key = entry.user_int(found)
  case dict.get(groups, key) {
    Ok(group) -> #(groups, group)
    Error(Nil) -> {
      let group =
        perms.group_of(reader, entry.user(found))
        |> result.unwrap(None)
      #(dict.insert(groups, key, group), group)
    }
  }
}

fn member_of(found: Audience, group: Option(Group)) -> Bool {
  case group {
    Some(group) -> audience.contains(found, group)
    None -> False
  }
}
