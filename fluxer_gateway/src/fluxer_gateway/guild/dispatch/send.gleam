// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import fluxer_gateway/guild/dispatch/broadcast
import fluxer_gateway/guild/dispatch/event
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/member_list/effects
import fluxer_gateway/perms
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const max_bulk_groups = 1024

fn claim_deadline_ms() -> Int {
  durations.guild().send_claim_deadline_ms
}

fn claim_backoff_ms() -> Int {
  durations.guild().send_claim_backoff_ms
}

pub fn deliver(
  state: GuildState,
  to: List(SessionRef),
  event_name: String,
  payload: RawJson,
) -> Nil {
  case to {
    [] -> Nil
    _ -> {
      guild_metrics.delivered(event_name, list.length(to))
      deliver_message(
        state,
        to,
        session.guild_dispatch(guild_state.guild_id(state), event_name, payload),
      )
    }
  }
}

pub fn deliver_tree(
  state: GuildState,
  to: List(SessionRef),
  event_name: String,
  data: JsonTree,
) -> Nil {
  case to {
    [] -> Nil
    _ -> {
      guild_metrics.delivered(event_name, list.length(to))
      deliver_message(state, to, message(state, event_name, data))
    }
  }
}

pub fn message(
  state: GuildState,
  event_name: String,
  data: JsonTree,
) -> SessionInbox {
  session.GuildDispatch(
    guild: guild_state.guild_id(state),
    event: event_name,
    payload: encode(data),
    facts: event.dispatch_facts(event_name, data),
  )
}

pub fn with_broadcaster(state: GuildState) -> GuildState {
  let sessions = guild_state.sessions(state)
  case table.broadcaster(sessions) {
    Some(found) ->
      case broadcast.is_alive(found) {
        True -> state
        False -> start_broadcaster(state)
      }
    None -> start_broadcaster(state)
  }
}

fn start_broadcaster(state: GuildState) -> GuildState {
  case broadcast.start(process.self()) {
    Ok(found) ->
      guild_state.set_sessions(
        state,
        table.set_broadcaster(guild_state.sessions(state), Some(found)),
      )
    Error(Nil) -> state
  }
}

fn deliver_message(
  state: GuildState,
  to: List(SessionRef),
  message: SessionInbox,
) -> Nil {
  case to {
    [] -> Nil
    _ -> {
      case table.broadcaster(guild_state.sessions(state)) {
        Some(found) ->
          case broadcast.is_alive(found) {
            True ->
              claim(
                found,
                to,
                message,
                clock.monotonic_ms() + claim_deadline_ms(),
              )
            False -> relay.send(to, message)
          }
        None -> relay.send(to, message)
      }
    }
  }
}

fn claim(
  found: broadcast.Broadcaster,
  to: List(SessionRef),
  message: session.SessionInbox,
  deadline: Int,
) -> Nil {
  case broadcast.cast(found, to, message) {
    True -> Nil
    False ->
      case clock.monotonic_ms() < deadline {
        True -> {
          process.sleep(claim_backoff_ms())
          claim(found, to, message, deadline)
        }
        False -> broadcast.force(found, to, message)
      }
  }
}

pub fn list_dispatches(
  state: GuildState,
  dispatches: List(effects.Dispatch),
) -> Nil {
  list.each(dispatches, fn(item) {
    guild_metrics.delivered(item.event, list.length(item.to))
    relay.send(
      item.to,
      session.guild_dispatch(
        guild_state.guild_id(state),
        item.event,
        item.payload,
      ),
    )
  })
}

pub fn standard(
  state: GuildState,
  recipients: List(Entry),
  event_name: String,
  data: JsonTree,
) -> Nil {
  case recipients {
    [] -> Nil
    _ -> {
      let to = list.map(recipients, entry.ref)
      guild_metrics.delivered(event_name, list.length(to))
      deliver_message(state, to, message(state, event_name, data))
    }
  }
}

pub fn encode(data: JsonTree) -> RawJson {
  raw_json.from_json(json_tree.to_json(data))
}

pub fn bulk(
  state: GuildState,
  recipients: List(Entry),
  event_name: String,
  data: JsonTree,
) -> Nil {
  let reader = guild_state.perms_reader(state)
  let channels =
    json_tree.field(data, "channels")
    |> result.try(json_tree.items)
    |> result.unwrap([])
    |> list.map(fn(item) {
      #(
        json_tree.snowflake_field(item, "id") |> option.from_result,
        json_tree.Raw(encode(item)),
      )
    })
  let BulkGroups(groups:, singles:, ..) =
    list.fold(
      recipients,
      BulkGroups(groups: dict.new(), singles: [], memo: dict.new()),
      fn(acc, found) {
        let #(visible, memo) =
          visible_channels(reader, found, channels, acc.memo)
        let acc = BulkGroups(..acc, memo:)
        let ref = entry.ref(found)
        let key = list.map(visible, fn(pair) { pair.0 })
        case visible, dict.get(acc.groups, key) {
          [], _ -> acc
          _, Ok(Group(visible: kept, refs:)) ->
            BulkGroups(
              ..acc,
              groups: dict.insert(acc.groups, key, Group(kept, [ref, ..refs])),
            )
          _, Error(Nil) ->
            case dict.size(acc.groups) >= max_bulk_groups {
              True ->
                BulkGroups(..acc, singles: [
                  Group(visible, [ref]),
                  ..acc.singles
                ])
              False ->
                BulkGroups(
                  ..acc,
                  groups: dict.insert(acc.groups, key, Group(visible, [ref])),
                )
            }
        }
      },
    )
  list.each(list.reverse(singles), fn(single) {
    deliver_tree(
      state,
      single.refs,
      event_name,
      bulk_payload(data, single.visible),
    )
  })
  dict.each(groups, fn(_key, group) {
    deliver_tree(
      state,
      list.reverse(group.refs),
      event_name,
      bulk_payload(data, group.visible),
    )
  })
}

type Group {
  Group(visible: List(#(Int, JsonTree)), refs: List(SessionRef))
}

type BulkGroups {
  BulkGroups(
    groups: Dict(List(Int), Group),
    singles: List(Group),
    memo: Dict(Int, List(#(Int, JsonTree))),
  )
}

fn visible_channels(
  reader: perms.Reader,
  found: Entry,
  channels: List(#(option.Option(Int), JsonTree)),
  memo: Dict(Int, List(#(Int, JsonTree))),
) -> #(List(#(Int, JsonTree)), Dict(Int, List(#(Int, JsonTree)))) {
  let user = entry.user_int(found)
  case dict.get(memo, user) {
    Ok(visible) -> #(visible, memo)
    Error(Nil) -> {
      let visible =
        list.filter_map(channels, fn(pair) {
          use channel_int <- result.try(option.to_result(pair.0, Nil))
          use channel_id <- result.try(
            id.channel_id(channel_int) |> result.replace_error(Nil),
          )
          case perms.can_view(reader, entry.user(found), channel_id) {
            Ok(True) -> Ok(#(channel_int, pair.1))
            _ -> Error(Nil)
          }
        })
      #(visible, dict.insert(memo, user, visible))
    }
  }
}

fn bulk_payload(data: JsonTree, visible: List(#(Int, JsonTree))) -> JsonTree {
  let channels = json_tree.Array(list.map(visible, fn(pair) { pair.1 }))
  json_tree.set(data, "channels", channels)
  |> result.unwrap(json_tree.Object([#("channels", channels)]))
}
