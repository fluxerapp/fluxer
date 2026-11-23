// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/sessions/connect
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/guild/visitors
import fluxer_gateway/guild/visitors/snapshot
import fluxer_gateway/guild/visitors/table.{type Visitor}
import fluxer_gateway/perms
import fluxer_gateway/threads/access
import fluxer_gateway/threads/payload as thread_payload
import fluxer_gateway/threads/store
import fluxer_gateway/threads/thread
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

type Delivery {
  Delivery(event: String, to: SessionRef, payload: JsonTree)
}

type Scope {
  GuildScope
  ChannelScope
  FocusedScope
  ThreadScope
  PinsScope
  NoScope
}

pub fn after(
  state: GuildState,
  previous: data.GuildData,
  event_name: String,
  data: JsonTree,
) -> GuildState {
  case event_name {
    "GUILD_MEMBER_ADD" ->
      case member_user(data) {
        Some(user) -> connect.promote_user(state, user)
        None -> state
      }
    _ ->
      case visitors.count(state) {
        0 -> state
        _ -> dispatch(state, previous, event_name, data)
      }
  }
}

fn dispatch(
  state: GuildState,
  previous: data.GuildData,
  event_name: String,
  data: JsonTree,
) -> GuildState {
  case event_name {
    "GUILD_DELETE" -> visitors.evict_all(state, visitors.EvictRemoved)
    _ -> {
      let data =
        guild_threads.strip_internal(data)
        |> with_field(
          "guild_id",
          json_tree.Raw(guild_state.guild_id_json(state)),
        )
      let state = case event_name {
        "GUILD_UPDATE" ->
          state
          |> visitors.evict_unavailable
          |> visitors.evict_disabled
          |> visitors.evict_minors
        _ -> state
      }
      let #(state, view_deliveries) =
        refresh_for(state, previous, event_name, data)
      let deliveries =
        list.append(view_deliveries, event_deliveries(state, event_name, data))
      deliver(state, deliveries)
      state
    }
  }
}

fn is_view_event(event_name: String) -> Bool {
  case event_name {
    "GUILD_UPDATE"
    | "GUILD_ROLE_CREATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE" -> True
    _ -> False
  }
}

fn scope(event_name: String) -> Scope {
  case event_name {
    "GUILD_UPDATE"
    | "GUILD_ROLE_CREATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE"
    | "GUILD_EMOJIS_UPDATE"
    | "GUILD_STICKERS_UPDATE" -> GuildScope
    "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "CHANNEL_DELETE" -> ChannelScope
    "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI" -> FocusedScope
    "THREAD_CREATE" | "THREAD_UPDATE" | "THREAD_DELETE" -> ThreadScope
    "CHANNEL_PINS_UPDATE" -> PinsScope
    _ -> NoScope
  }
}

fn with_field(data: JsonTree, key: String, item: JsonTree) -> JsonTree {
  json_tree.set(data, key, item)
  |> result.unwrap(json_tree.Object([#(key, item)]))
}

fn without(data: JsonTree, keys: List(String)) -> JsonTree {
  case data {
    json_tree.Object(fields) ->
      json_tree.Object(
        list.filter(fields, fn(pair) { !list.contains(keys, pair.0) }),
      )
    other -> other
  }
}

fn visitor_payload(event_name: String, data: JsonTree) -> JsonTree {
  case event_name {
    "MESSAGE_REACTION_ADD" | "MESSAGE_REACTION_REMOVE" ->
      without(data, ["user_id", "member", "session_id"])
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      snapshot.strip_channel(data)
    "CHANNEL_UPDATE_BULK" ->
      case json_tree.field(data, "channels") {
        Ok(json_tree.Array(channels)) ->
          with_field(
            data,
            "channels",
            json_tree.Array(list.map(channels, snapshot.strip_channel)),
          )
        _ -> data
      }
    "THREAD_CREATE" | "THREAD_UPDATE" ->
      without(data, ["member", "member_ids_preview"])
    "THREAD_DELETE" ->
      case data {
        json_tree.Object(fields) ->
          json_tree.Object(
            list.filter(fields, fn(pair) {
              list.contains(["id", "guild_id", "parent_id", "type"], pair.0)
            }),
          )
        other -> other
      }
    _ -> data
  }
}

fn mask_for(found: Visitor, event_name: String, payload: JsonTree) -> JsonTree {
  case found.thread_viewer {
    True -> payload
    False -> thread_payload.mask(event_name, payload)
  }
}

fn refresh_for(
  state: GuildState,
  previous: data.GuildData,
  event_name: String,
  data: JsonTree,
) -> #(GuildState, List(Delivery)) {
  case is_view_event(event_name) {
    False -> #(state, [])
    True -> {
      let before =
        table.visitors(visitors.visitors(state))
        |> list.map(fn(found) { #(table.key(found), found.view) })
        |> dict.from_list
      let state = visitors.refresh_views(state)
      let event_ids = event_channel_ids(event_name, data)
      let changes_channels = case event_name {
        "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_UPDATE_BULK" -> True
        _ -> False
      }
      let deliveries =
        table.visitors(visitors.visitors(state))
        |> list.flat_map(fn(found) {
          view_deliveries(
            state,
            previous:,
            found:,
            before:,
            event_name:,
            source: data,
            event_ids:,
            changes_channels:,
          )
        })
      #(state, deliveries)
    }
  }
}

fn view_deliveries(
  state: GuildState,
  previous previous: data.GuildData,
  found found: Visitor,
  before before: Dict(String, set.Set(Int)),
  event_name event_name: String,
  source source: JsonTree,
  event_ids event_ids: List(Int),
  changes_channels changes_channels: Bool,
) -> List(Delivery) {
  let old = dict.get(before, table.key(found)) |> result.unwrap(found.view)
  let lost = set.to_list(set.difference(old, found.view))
  let gained = set.to_list(set.difference(found.view, old))
  let guild = guild_state.guild_id_json(state)
  let guild_data = guild_state.guild_data(state)
  let deletes =
    list.map(lost, fn(channel_int) {
      Delivery(
        "CHANNEL_DELETE",
        found.ref,
        lost_payload(event_name, source, channel_int, guild, previous),
      )
    })
  let creates =
    gained
    |> list.filter(fn(channel_int) {
      !changes_channels || !list.contains(event_ids, channel_int)
    })
    |> list.filter_map(fn(channel_int) {
      use channel_id <- result.try(
        id.channel_id(channel_int) |> result.replace_error(Nil),
      )
      use row <- result.map(
        data.find_channel(guild_data, channel_id) |> option.to_result(Nil),
      )
      Delivery(
        "CHANNEL_CREATE",
        found.ref,
        json_tree.Raw(data.channel_wire(guild_data, row))
          |> with_field("guild_id", json_tree.Raw(guild))
          |> snapshot.strip_channel
          |> mask_for(found, "CHANNEL_CREATE", _),
      )
    })
  list.append(deletes, creates)
}

fn lost_payload(
  event_name: String,
  data: JsonTree,
  channel_int: Int,
  guild: RawJson,
  guild_data: data.GuildData,
) -> JsonTree {
  case event_name, json_tree.snowflake_field(data, "id") {
    "CHANNEL_DELETE", Ok(deleted) if deleted == channel_int ->
      visitor_payload("CHANNEL_DELETE", data)
    _, _ -> minimal_delete(channel_int, guild, guild_data)
  }
}

fn minimal_delete(
  channel_int: Int,
  guild: RawJson,
  guild_data: data.GuildData,
) -> JsonTree {
  let known =
    id.channel_id(channel_int)
    |> result.replace_error(Nil)
    |> result.try(fn(channel_id) {
      data.find_channel(guild_data, channel_id) |> option.to_result(Nil)
    })
    |> result.map(fn(row) { json_tree.Raw(data.channel_wire(guild_data, row)) })
  let channel_text = case
    result.try(known, json_tree.field(_, "id")) |> result.try(json_tree.expand)
  {
    Ok(json_tree.String(_) as text) -> text
    _ -> json_tree.String(id.write(channel_int))
  }
  let base = [#("id", channel_text), #("guild_id", json_tree.Raw(guild))]
  case result.try(known, json_tree.field(_, "type")) {
    Ok(kind) -> {
      let parent = case result.try(known, json_tree.field(_, "parent_id")) {
        Ok(parent) -> [#("parent_id", parent)]
        Error(Nil) -> []
      }
      json_tree.Object(list.flatten([base, [#("type", kind)], parent]))
    }
    Error(Nil) -> json_tree.Object(base)
  }
}

fn event_channel_ids(event_name: String, data: JsonTree) -> List(Int) {
  case event_name {
    "CHANNEL_UPDATE_BULK" ->
      json_tree.field(data, "channels")
      |> result.try(json_tree.items)
      |> result.unwrap([])
      |> list.filter_map(json_tree.snowflake_field(_, "id"))
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      json_tree.snowflake_field(data, "id")
      |> result.map(list.wrap)
      |> result.unwrap([])
    _ -> []
  }
}

fn event_deliveries(
  state: GuildState,
  event_name: String,
  data: JsonTree,
) -> List(Delivery) {
  let payload = visitor_payload(event_name, data)
  let all = table.visitors(visitors.visitors(state))
  case scope(event_name) {
    NoScope -> []
    GuildScope ->
      list.map(all, fn(found) {
        Delivery(event_name, found.ref, mask_for(found, event_name, payload))
      })
    PinsScope ->
      case json_tree.snowflake_field(data, "channel_id") {
        Ok(channel_int) ->
          all
          |> list.filter(snapshot.channel_visible(state, _, channel_int))
          |> list.map(fn(found) { Delivery(event_name, found.ref, payload) })
        Error(Nil) -> []
      }
    ThreadScope -> {
      let kind = case json_tree.field(data, "type") {
        Ok(json_tree.Int(kind)) -> Some(kind)
        _ -> None
      }
      let parent =
        json_tree.snowflake_field(data, "parent_id") |> option.from_result
      all
      |> list.filter(snapshot.thread_visible(_, kind, parent))
      |> list.map(fn(found) { Delivery(event_name, found.ref, payload) })
    }
    ChannelScope -> channel_deliveries(all, event_name, payload)
    FocusedScope -> focused_deliveries(state, event_name, data, payload)
  }
}

fn channel_deliveries(
  all: List(Visitor),
  event_name: String,
  payload: JsonTree,
) -> List(Delivery) {
  case event_name {
    "CHANNEL_DELETE" -> []
    "CHANNEL_UPDATE_BULK" -> {
      let channels =
        json_tree.field(payload, "channels")
        |> result.try(json_tree.items)
        |> result.unwrap([])
      list.filter_map(all, fn(found) {
        let visible =
          list.filter(channels, fn(item) {
            json_tree.snowflake_field(item, "id")
            |> result.map(snapshot.in_view(found, _))
            |> result.unwrap(False)
          })
        case visible {
          [] -> Error(Nil)
          _ ->
            Ok(Delivery(
              event_name,
              found.ref,
              with_field(payload, "channels", json_tree.Array(visible))
                |> mask_for(found, event_name, _),
            ))
        }
      })
    }
    _ ->
      case json_tree.snowflake_field(payload, "id") {
        Ok(channel_int) ->
          all
          |> list.filter(snapshot.in_view(_, channel_int))
          |> list.map(fn(found) {
            Delivery(
              event_name,
              found.ref,
              mask_for(found, event_name, payload),
            )
          })
        Error(Nil) -> []
      }
  }
}

fn focused_deliveries(
  state: GuildState,
  event_name: String,
  data: JsonTree,
  payload: JsonTree,
) -> List(Delivery) {
  case json_tree.snowflake_field(data, "channel_id") {
    Error(Nil) -> []
    Ok(channel_int) -> {
      let focused =
        table.focused(visitors.visitors(state), channel_int)
        |> list.filter(snapshot.channel_visible(state, _, channel_int))
      case focused != [] && readable(state, event_name, data, channel_int) {
        True -> {
          let visible = hide_unreadable_source(state, event_name, payload)
          list.map(focused, fn(found) {
            Delivery(
              event_name,
              found.ref,
              mask_for(found, event_name, visible),
            )
          })
        }
        False -> []
      }
    }
  }
}

fn readable(
  state: GuildState,
  event_name: String,
  data: JsonTree,
  channel_int: Int,
) -> Bool {
  case event.is_message_access(event_name) {
    False -> True
    True ->
      case event.message_id(data) {
        None -> False
        Some(message) ->
          case id.channel_id(channel_int) {
            Ok(channel_id) ->
              access.message_readable(
                channel_permissions(state, channel_id),
                Some(message),
                cutoff_ms(state),
              )
            Error(_) -> False
          }
      }
  }
}

fn hide_unreadable_source(
  state: GuildState,
  event_name: String,
  payload: JsonTree,
) -> JsonTree {
  case event_name {
    "MESSAGE_CREATE" ->
      thread_payload.hide_unreadable_source(
        event_name,
        payload,
        channel_permissions(state, _),
        cutoff_ms(state),
      )
    _ -> payload
  }
}

fn cutoff_ms(state: GuildState) -> Option(Int) {
  guild_state.thread_context(state).history_cutoff_ms
}

fn channel_permissions(
  state: GuildState,
  channel_id: ChannelId,
) -> Permissions {
  let target = case store.thread(guild_state.threads(state), channel_id) {
    Some(found) -> thread.parent(found)
    None -> Some(channel_id)
  }
  case target, perms.visitor_channels(guild_state.perms_reader(state), True) {
    Some(wanted), Ok(grants) ->
      case list.find(grants, fn(grant) { grant.0 == wanted }) {
        Ok(grant) -> grant.1
        Error(Nil) -> permissions.none()
      }
    _, _ -> permissions.none()
  }
}

fn deliver(state: GuildState, deliveries: List(Delivery)) -> Nil {
  deliveries
  |> list.fold(dict.new(), fn(groups, delivery) {
    dict.upsert(groups, #(delivery.event, delivery.payload), fn(existing) {
      [delivery.to, ..option.unwrap(existing, [])]
    })
  })
  |> dict.each(fn(key, refs) {
    let #(event_name, payload) = key
    send.deliver_tree(state, list.reverse(refs), event_name, payload)
  })
}

fn member_user(data: JsonTree) -> Option(Int) {
  json_tree.path(data, ["user", "id"])
  |> result.try(json_tree.snowflake)
  |> result.lazy_or(fn() { json_tree.snowflake_field(data, "user_id") })
  |> option.from_result
}
