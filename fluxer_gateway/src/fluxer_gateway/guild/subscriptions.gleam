// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/session.{type ClientOp, type SessionRef}
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/presence/row
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/sessions/passive
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/subscriptions/mutual_channels
import fluxer_gateway/guild/subscriptions/unified.{type GuildSubscription}
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/guild/visitors
import fluxer_gateway/member_list
import fluxer_gateway/perms
import fluxer_gateway/telemetry/metrics as telemetry
import gleam/list
import gleam/option.{None, Some}
import logging

const guild_keys = [
  "active", "sync", "typing", "member_list_channels", "members",
  "focused_channel_id", "threads", "thread_member_lists",
]

pub fn handle(state: GuildState, ref: SessionRef, op: ClientOp) -> GuildState {
  case op {
    session.LazyRequest(data) -> {
      let parsed = json_tree.from_term(data)
      let guild = id.guild_id_to_int(guild_state.guild_id(state))
      case
        unified.for_guild(parsed, guild),
        list.any(guild_keys, fn(key) {
          json_tree.field(parsed, key) != Error(Nil)
        })
      {
        Some(subscription), _ -> apply(state, ref, subscription)
        None, True -> apply(state, ref, unified.parse(parsed))
        None, False -> state
      }
    }
    _ -> state
  }
}

fn apply(
  state: GuildState,
  ref: SessionRef,
  subscription: GuildSubscription,
) -> GuildState {
  let key = session.session_id_to_string(ref.id)
  case table.get_key(guild_state.sessions(state), key) {
    None ->
      case subscription.focus {
        Some(focus) -> visitors.set_focus(state, key, focus)
        None -> state
      }
    Some(found) -> {
      let was_active = !entry.is_passive(found)
      let #(state, changed) = case subscription.active {
        Some(True) -> #(passive.set_active(state, ref.id), !was_active)
        Some(False) -> #(passive.set_passive(state, ref.id), was_active)
        None -> #(state, False)
      }
      let state = case subscription.sync || changed {
        True -> passive.send_guild_sync(state, ref.id)
        False -> state
      }
      let state =
        list.fold(subscription.channels, state, fn(acc, pair) {
          subscribe_list(acc, ref, pair.0, pair.1)
        })
      let state = case subscription.members {
        Some(members) -> update_members(state, ref, members)
        None -> state
      }
      let state = case subscription.typing {
        Some(flag) -> passive.set_typing(state, ref.id, Some(flag))
        None -> state
      }
      case subscription.threads, subscription.thread_member_lists {
        None, None -> state
        threads, lists ->
          guild_threads.update_subscriptions(state, key, threads, lists)
      }
    }
  }
}

fn subscribe_list(
  state: GuildState,
  ref: SessionRef,
  channel_int: Int,
  ranges: List(#(Int, Int)),
) -> GuildState {
  case id.channel_id(channel_int) {
    Ok(channel_id) -> {
      let members = guild_state.member_list(state)
      let outcome = case may_view_members(state, ref, channel_id), ranges {
        True, _ -> {
          let began = clock.monotonic_ms()
          let outcome =
            member_list.subscribe(
              members,
              ref,
              channel_id,
              ranges,
              rows.lookup(state),
            )
          guild_metrics.member_list_subscribed(
            list.length(ranges),
            clock.monotonic_ms() - began,
          )
          outcome
        }
        False, [] ->
          case member_list.subscribed_channel(members, ref.id) {
            Some(current) if current == channel_id -> {
              guild_metrics.member_list_op(guild_metrics.Unsubscribe)
              member_list.unsubscribe(members, ref.id)
            }
            _ -> Ok([])
          }
        False, _ -> {
          guild_metrics.member_list_op(guild_metrics.Denied)
          Ok([])
        }
      }
      case outcome {
        Ok(dispatches) -> send.list_dispatches(state, dispatches)
        Error(error) -> report_list_error(state, ref, channel_id, error)
      }
      state
    }
    Error(_) -> state
  }
}

fn report_list_error(
  state: GuildState,
  ref: SessionRef,
  channel_id: id.ChannelId,
  error: member_list.MemberListError,
) -> Nil {
  let #(kind, detail) = case error {
    member_list.ListBadInput(reason) -> #("bad_input", reason)
    member_list.ListBroken -> #("broken", "member list unavailable")
  }
  telemetry.counter(
    "fluxer_gateway_guild_member_list_errors_total",
    [#("reason", kind)],
    1,
  )
  logging.log(
    logging.Warning,
    "guild "
      <> id.guild_id_to_string(guild_state.guild_id(state))
      <> ": member list subscribe failed for session "
      <> session.session_id_to_string(ref.id)
      <> " channel "
      <> id.channel_id_to_string(channel_id)
      <> ": "
      <> kind
      <> " ("
      <> detail
      <> ")",
  )
}

fn may_view_members(
  state: GuildState,
  ref: SessionRef,
  channel_id: id.ChannelId,
) -> Bool {
  let reader = guild_state.perms_reader(state)
  perms.can_view(reader, ref.user, channel_id) == Ok(True)
  && perms.can_view_members(reader, ref.user, channel_id) == Ok(True)
}

fn update_members(
  state: GuildState,
  ref: SessionRef,
  requested: List(Int),
) -> GuildState {
  let members = mutual_channels.filter(state, ref.user, requested)
  let #(sessions, added, removed) =
    table.replace_subscriptions(
      guild_state.sessions(state),
      session.session_id_to_string(ref.id),
      members,
    )
  let #(known, fresh) = list.partition(added, table.is_interested(sessions, _))
  let state =
    guild_state.set_sessions(
      state,
      list.fold(fresh, sessions, table.delete_row),
    )
  let guild = guild_state.guild_id(state)
  list.each(known, fn(user) {
    case table.presence_row(sessions, user) {
      Some(found) ->
        send.deliver(
          state,
          [ref],
          "PRESENCE_UPDATE",
          row.update_payload(found, guild),
        )
      None -> Nil
    }
  })
  let #(state, changes) = session_presence.watch(state, added)
  rows.apply_engagement(state, changes)
  |> session_presence.release(removed)
}
