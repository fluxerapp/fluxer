// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/dispatch/decorate
import fluxer_gateway/guild/dispatch/event.{type EventFacts}
import fluxer_gateway/guild/dispatch/filter
import fluxer_gateway/guild/dispatch/model
import fluxer_gateway/guild/dispatch/model_events
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/dispatch/visibility
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/push/dispatch as push_dispatch
import fluxer_gateway/guild/push/thread_scope.{type ThreadScope}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/connect
import fluxer_gateway/guild/sessions/dm_partners
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/sessions/passive
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/state/members as member_change
import fluxer_gateway/guild/state/update
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/guild/visitors/dispatch as visitor_dispatch
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/perms
import fluxer_gateway/perms/audience
import fluxer_gateway/threads/dispatch as thread_dispatch
import fluxer_gateway/threads/metrics as thread_metrics
import fluxer_gateway/threads/value as thread_value
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import logging

const disabled_push_notifications = 1

const broadcast_fan_out = 64

pub type Origin {
  Live
  Replayed
}

pub fn handle_event(
  state: GuildState,
  event_name: String,
  data: Dynamic,
) -> GuildState {
  handle(state, event_name, data, Live)
}

pub fn replay_event(
  state: GuildState,
  event_name: String,
  data: Dynamic,
) -> GuildState {
  handle(state, event_name, data, Replayed)
}

fn handle(
  state: GuildState,
  event_name: String,
  data: Dynamic,
  origin: Origin,
) -> GuildState {
  case event.normalize(event_name) {
    Error(Nil) -> {
      logging.log(
        logging.Warning,
        "guild dispatch dropped unknown event: " <> event_name,
      )
      state
    }
    Ok(name) -> {
      let features = core_guild.features(guild_state.facts(state))
      let full =
        name == "GUILD_UPDATE" || !list.any(features, event.unavailable_feature)
      let began = clock.monotonic_ms()
      let state = run(state, name, data, full, origin)
      guild_metrics.event_handled(name, clock.monotonic_ms() - began)
      state
    }
  }
}

fn run(
  state: GuildState,
  name: String,
  raw: Dynamic,
  full: Bool,
  origin: Origin,
) -> GuildState {
  let data = json_tree.from_term(raw)
  case thread_dispatch.handles(name) {
    True -> {
      let previous = guild_state.guild_data(state)
      let state =
        guild_threads.apply_event(
          state,
          name,
          thread_value.object_from_tree(data),
          full,
        )
      case full {
        True -> visitor_dispatch.after(state, previous, name, data)
        False -> state
      }
    }
    False -> run_regular(state, name, raw, data, full, origin)
  }
}

fn run_regular(
  state: GuildState,
  name: String,
  raw: Dynamic,
  parsed: JsonTree,
  full: Bool,
  origin: Origin,
) -> GuildState {
  let #(exclude, parsed) = decorate.extract_session(state, name, parsed)
  let decorated =
    with_field(
      parsed,
      "guild_id",
      json_tree.Raw(guild_state.guild_id_json(state)),
    )
    |> decorate.member(state, name, _)
  let facts = event.facts(name, decorated)
  let scope_data = guild_threads.scope_object(parsed)
  let #(state, early) = case full && name == "CHANNEL_DELETE" {
    True -> {
      let #(state, found) =
        filter.recipients(state, name, decorated, scope_data, exclude)
      #(state, Some(found))
    }
    False -> #(state, None)
  }
  let updates_before = member_list_updates_enabled(state)
  let before = state
  let prior = model_events.prior(state, name, raw)
  let applied = update.apply_event(state, name, raw)
  case origin, applied.self_dispatch {
    Live, Some(payload) ->
      process.send(
        guild_state.control(state),
        inbox.Dispatch("GUILD_MEMBER_UPDATE", payload),
      )
    _, _ -> Nil
  }
  let state = forget_removed(applied.state, applied.member)
  let #(state, effects) =
    model.apply_perms(
      state,
      name,
      model_events.perm_events(state, name, raw, applied.member, prior),
    )
  let delta = option.map(effects, perms.delta)
  let updates_after = member_list_updates_enabled(state)
  let list_events = model_events.list_events(state, applied.member)
  let list_events = case updates_before == updates_after {
    True -> list_events
    False -> [list_event.updates(updates_after), ..list_events]
  }
  let #(state, list_dispatches) =
    model.apply_member_list(state, list_events, delta)
  let state = case effects {
    Some(found) -> visibility.dispatch(before, state, name, raw, found)
    None -> state
  }
  let state = guild_threads.apply_event(state, name, scope_data, full)
  let state =
    guild_state.set_push(
      state,
      push_dispatch.apply_info(guild_state.push(state), name, raw),
    )
  case full {
    False -> state
    True -> {
      let #(state, found) = case early {
        Some(found) -> #(state, found)
        None -> filter.recipients(state, name, decorated, scope_data, exclude)
      }
      let recipients = case found {
        filter.InThread(entries) | filter.Scoped(entries) ->
          passive_filter(state, name, facts, entries)
      }
      let fan_out = list.length(recipients)
      guild_metrics.fanned_out(fan_out)
      case found {
        filter.InThread(_) -> thread_metrics.delivered_in_thread(name, fan_out)
        filter.Scoped(_) -> Nil
      }
      let state = case fan_out >= broadcast_fan_out {
        True -> send.with_broadcaster(state)
        False -> state
      }
      guild_threads.variants(state, name, decorated, recipients)
      |> list.each(fn(group) {
        case event.is_bulk_update(name) {
          True -> send.bulk(state, group.0, name, group.1)
          False -> send.standard(state, group.0, name, group.1)
        }
      })
      let state = case origin {
        Live -> push(state, name, raw, facts)
        Replayed -> state
      }
      case member_list_updates_enabled(state) {
        True -> send.list_dispatches(state, list_dispatches)
        False -> Nil
      }
      let state =
        visitor_dispatch.after(
          state,
          guild_state.guild_data(before),
          name,
          decorated,
        )
      dm_partners.reevaluate(before, state, name, decorated)
    }
  }
}

pub fn catch_up(
  state: GuildState,
  target: SessionId,
  events: List(#(String, Dynamic)),
) -> GuildState {
  let key = session.session_id_to_string(target)
  let #(state, _) =
    list.fold(events, #(state, thread_dispatch.catch_up()), fn(acc, pair) {
      let #(state, seen) = acc
      case event.normalize(pair.0) {
        Error(Nil) -> acc
        Ok(name) -> {
          let features = core_guild.features(guild_state.facts(state))
          let full =
            name == "GUILD_UPDATE"
            || !list.any(features, event.unavailable_feature)
          let data = json_tree.from_term(pair.1)
          case full, thread_dispatch.handles(name) {
            False, _ -> acc
            True, True -> #(
              state,
              thread_dispatch.catch_up_event(
                guild_state.threads(state),
                guild_state.thread_context(state),
                key,
                name,
                thread_value.object_from_tree(data),
                seen,
              ),
            )
            True, False -> #(deliver_to(state, target, name, data), seen)
          }
        }
      }
    })
  state
}

fn deliver_to(
  state: GuildState,
  session: SessionId,
  name: String,
  parsed: JsonTree,
) -> GuildState {
  let #(exclude, parsed) = decorate.extract_session(state, name, parsed)
  let decorated =
    with_field(
      parsed,
      "guild_id",
      json_tree.Raw(guild_state.guild_id_json(state)),
    )
    |> decorate.member(state, name, _)
  let facts = event.facts(name, decorated)
  let scope_data = guild_threads.scope_object(parsed)
  let #(state, found) =
    filter.recipients(state, name, decorated, scope_data, exclude)
  let recipients =
    case found {
      filter.InThread(entries) | filter.Scoped(entries) ->
        passive_filter(state, name, facts, entries)
    }
    |> list.filter(fn(found) { entry.id(found) == session })
  guild_threads.variants(state, name, decorated, recipients)
  |> list.each(fn(group) {
    case event.is_bulk_update(name) {
      True -> send.bulk(state, group.0, name, group.1)
      False -> send.standard(state, group.0, name, group.1)
    }
  })
  state
}

fn forget_removed(
  state: GuildState,
  change: member_change.Change,
) -> GuildState {
  case change {
    member_change.Removed(user) -> {
      let user = id.user_id_to_int(user)
      let state = connect.remove_user(state, user)
      guild_state.set_sessions(
        state,
        table.delete_row(guild_state.sessions(state), user),
      )
      |> session_presence.unwatch([user])
    }
    _ -> state
  }
}

fn with_field(data: JsonTree, key: String, item: JsonTree) -> JsonTree {
  json_tree.set(data, key, item)
  |> result.unwrap(json_tree.Object([#(key, item)]))
}

fn passive_filter(
  state: GuildState,
  name: String,
  facts: EventFacts,
  recipients: List(Entry),
) -> List(Entry) {
  let small = guild_state.is_small_guild(state)
  let everyone_active = small || !table.has_passive(guild_state.sessions(state))
  use <- bool.guard(name != "TYPING_START" && everyone_active, recipients)
  list.filter(recipients, fn(found) {
    passive.should_receive(found, name, facts, small, fn() {
      roles_of(state, found)
    })
  })
}

fn roles_of(state: GuildState, found: Entry) -> List(Int) {
  guild_state.member(state, entry.user(found))
  |> option.map(fn(owner) { list.map(member.roles(owner), id.role_id_to_int) })
  |> option.unwrap([])
}

fn push(
  state: GuildState,
  name: String,
  raw: Dynamic,
  facts: EventFacts,
) -> GuildState {
  case disabled(state, disabled_push_notifications) {
    True -> state
    False ->
      guild_state.set_push(
        state,
        push_dispatch.on_event(
          guild_state.push(state),
          guild_state.push_inputs(state),
          name,
          raw,
          push_thread(state, name, facts),
        ),
      )
  }
}

fn push_thread(
  state: GuildState,
  name: String,
  facts: EventFacts,
) -> Option(ThreadScope) {
  case name, facts.channel {
    "MESSAGE_CREATE", Some(channel_int) ->
      case id.channel_id(channel_int) {
        Ok(channel_id) -> guild_threads.push_scope(state, channel_id)
        Error(_) -> None
      }
    _, _ -> None
  }
}

pub fn member_list_updates_enabled(state: GuildState) -> Bool {
  model.member_list_updates_enabled(state)
}

fn disabled(state: GuildState, bit: Int) -> Bool {
  data.operation_disabled(guild_state.guild_data(state), bit)
}

pub fn broadcast(
  state: GuildState,
  event_name: String,
  payload: RawJson,
  channel: Option(ChannelId),
) -> Nil {
  let candidates = table.entries(guild_state.sessions(state))
  let allowed = case channel {
    None -> candidates
    Some(channel_id) ->
      filter.in_audience(state, channel_id, audience.View, candidates).1
  }
  let recipients = passive_filter(state, event_name, event.no_facts(), allowed)
  send.deliver(state, list.map(recipients, entry.ref), event_name, payload)
}

pub fn broadcast_move(
  state: GuildState,
  event_name: String,
  payload: RawJson,
  left: RawJson,
  from: ChannelId,
  to: ChannelId,
) -> Nil {
  let candidates = table.entries(guild_state.sessions(state))
  let new_viewers = filter.in_audience(state, to, audience.View, candidates).1
  let old_viewers = filter.in_audience(state, from, audience.View, candidates).1
  let new_keys = set.from_list(list.map(new_viewers, entry.key))
  let old_only =
    list.filter(old_viewers, fn(found) {
      !set.contains(new_keys, entry.key(found))
    })
  let moved = passive_filter(state, event_name, event.no_facts(), new_viewers)
  let stayed = passive_filter(state, event_name, event.no_facts(), old_only)
  send.deliver(state, list.map(moved, entry.ref), event_name, payload)
  send.deliver(state, list.map(stayed, entry.ref), event_name, left)
}
