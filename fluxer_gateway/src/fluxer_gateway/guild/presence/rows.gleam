// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/dispatch/broadcast
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/presence/row.{type Row}
import fluxer_gateway/guild/push/engaged.{type EngagementChange}
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/member_list
import fluxer_gateway/member_list/effects
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/member_list/item as list_item
import fluxer_gateway/perms
import fluxer_gateway/telemetry/rollout_metrics
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import logging

fn small_flush_delay_ms() -> Int {
  durations.guild().rows_small_flush_delay_ms
}

fn large_flush_delay_ms() -> Int {
  durations.guild().rows_large_flush_delay_ms
}

const large_guild_members = 5000

pub type Timing {
  Immediate
  Deferred
}

pub fn on_presence(
  state: GuildState,
  source: Presence,
) -> #(GuildState, List(EngagementChange)) {
  apply(state, source, Immediate)
}

pub fn apply(
  state: GuildState,
  source: Presence,
  timing: Timing,
) -> #(GuildState, List(EngagementChange)) {
  let user = presence.user(source)
  case guild_state.member(state, user) {
    None -> #(state, [])
    Some(owner) -> store(state, user, row.from_presence(source, owner), timing)
  }
}

fn store(
  state: GuildState,
  user: UserId,
  next: Row,
  timing: Timing,
) -> #(GuildState, List(EngagementChange)) {
  let key = id.user_id_to_int(user)
  let previous = table.presence_row(guild_state.sessions(state), key)
  case option.map(previous, row.same(_, next)) {
    Some(True) -> #(offline_path(state, key, next), [])
    _ -> changed(state, user, previous, next, timing)
  }
}

fn changed(
  state: GuildState,
  user: UserId,
  previous: Option(Row),
  next: Row,
  timing: Timing,
) -> #(GuildState, List(EngagementChange)) {
  let key = id.user_id_to_int(user)
  let sessions = table.put_row(guild_state.sessions(state), key, next)
  let state =
    guild_state.set_sessions(state, sessions)
    |> guild_threads.presence_changed(key)
  let change = case
    table.is_connected(sessions, key)
    && row.list_relevant_change(previous, next)
  {
    True -> list_event.Changed
    False -> list_event.Unchanged
  }
  let list_timing = case timing {
    Immediate -> list_event.Immediate
    Deferred -> list_event.Deferred
  }
  let state =
    apply_list(state, [
      list_event.presence(user, row.status(next), change, list_timing),
    ])
  let state = broadcast_update(state, user, next)
  let state = offline_path(state, key, next)
  engagement(state, [key])
}

fn offline_path(state: GuildState, key: Int, current: Row) -> GuildState {
  case row.is_online(current) {
    True -> state
    False -> {
      let sessions = guild_state.sessions(state)
      case
        table.is_connected(sessions, key)
        || table.has_subscribers(sessions, key)
      {
        True -> state
        False ->
          guild_state.set_sessions(state, table.delete_row(sessions, key))
      }
    }
  }
}

pub fn apply_list(
  state: GuildState,
  events: List(list_event.Event),
) -> GuildState {
  case
    member_list.apply_timed(
      guild_state.member_list(state),
      events,
      None,
      lookup(state),
    )
  {
    Ok(#(dispatches, timer)) -> {
      send.list_dispatches(state, dispatches)
      list_timer(state, timer)
    }
    Error(_) -> {
      logging.log(logging.Warning, "guild member list presence update failed")
      state
    }
  }
}

pub fn flush_list(state: GuildState) -> GuildState {
  apply_list(guild_state.set_list_flush(state, None), [list_event.flush()])
}

pub fn list_timer(state: GuildState, timer: effects.Timer) -> GuildState {
  case timer, guild_state.list_flush(state) {
    effects.ArmTimer, None ->
      guild_state.set_list_flush(
        state,
        Some(process.send_after(
          guild_state.control(state),
          flush_delay_ms(state),
          inbox.FlushMemberList,
        )),
      )
    effects.CancelTimer, Some(pending) -> {
      let _ = process.cancel_timer(pending)
      guild_state.set_list_flush(state, None)
    }
    _, _ -> state
  }
}

fn flush_delay_ms(state: GuildState) -> Int {
  case guild_state.member_count(state) >= large_guild_members {
    True -> large_flush_delay_ms()
    False -> small_flush_delay_ms()
  }
}

pub fn lookup(state: GuildState) -> member_list.Lookup {
  fn(users: List(UserId)) {
    let sessions = guild_state.sessions(state)
    list.filter_map(users, fn(user) {
      case guild_state.member(state, user) {
        None -> Error(Nil)
        Some(owner) -> {
          let key = id.user_id_to_int(user)
          let presence_json = case table.is_connected(sessions, key) {
            True ->
              table.presence_row(sessions, key)
              |> option.then(fn(found) {
                case row.is_online(found) {
                  True -> Some(row.to_raw(found))
                  False -> None
                }
              })
            False -> None
          }
          let #(list_bytes, page_bytes) =
            list_item.encode_raw(owner, presence_json)
          Ok(#(user, list_bytes, page_bytes))
        }
      }
    })
  }
}

fn broadcast_update(state: GuildState, user: UserId, next: Row) -> GuildState {
  let sessions = guild_state.sessions(state)
  let key = id.user_id_to_int(user)
  let candidates =
    table.subscribers(sessions, key)
    |> list.filter_map(fn(session_key) {
      option.to_result(table.get_key(sessions, session_key), Nil)
    })
    |> list.filter(fn(found) { entry.user_int(found) != key })
  case candidates {
    [] -> state
    _ -> {
      let sharing = sharing_users(state, user, candidates)
      let recipients =
        candidates
        |> list.filter(fn(found) {
          set.contains(sharing, entry.user_int(found))
        })
        |> list.map(entry.ref)
      deliver_update(
        state,
        recipients,
        row.update_payload(next, guild_state.guild_id(state)),
      )
    }
  }
}

fn sharing_users(
  state: GuildState,
  user: UserId,
  candidates: List(entry.Entry),
) -> set.Set(Int) {
  let users =
    candidates
    |> list.map(entry.user_int)
    |> list.unique
    |> list.sort(int.compare)
    |> packed_ids.from_list
  perms.shares_view_many(guild_state.perms_reader(state), user, users)
  |> result.map(fn(shared) { set.from_list(packed_ids.to_list(shared)) })
  |> result.unwrap(set.new())
}

fn deliver_update(
  state: GuildState,
  recipients: List(SessionRef),
  payload: RawJson,
) -> GuildState {
  case recipients {
    [] -> state
    _ -> {
      let state = send.with_broadcaster(state)
      let found = table.broadcaster(guild_state.sessions(state))
      let message =
        session.guild_dispatch(
          guild_state.guild_id(state),
          "PRESENCE_UPDATE",
          payload,
        )
      case found {
        Some(broadcaster) ->
          case broadcast.cast(broadcaster, recipients, message) {
            True -> Nil
            False ->
              rollout_metrics.count(
                rollout_metrics.presence_broadcast_drops,
                [],
                1,
              )
          }
        None -> send.deliver(state, recipients, "PRESENCE_UPDATE", payload)
      }
      state
    }
  }
}

pub fn engagement(
  state: GuildState,
  users: List(Int),
) -> #(GuildState, List(EngagementChange)) {
  list.fold(users, #(state, []), fn(acc, key) {
    let #(state, changes) = acc
    let sessions = guild_state.sessions(state)
    let now =
      table.is_connected(sessions, key)
      && {
        table.presence_row(sessions, key)
        |> option.map(row.is_engaged)
        |> option.unwrap(False)
      }
    case now == table.is_engaged(sessions, key), id.user_id(key) {
      False, Ok(user) -> #(
        guild_state.set_sessions(state, table.set_engaged(sessions, key, now)),
        [engaged.EngagementChange(user:, engaged: now), ..changes],
      )
      _, _ -> #(state, changes)
    }
  })
}

pub fn restore_engagement(state: GuildState) -> GuildState {
  guild_state.sessions(state)
  |> table.engaged_users
  |> list.filter_map(fn(key) {
    id.user_id(key)
    |> result.replace_error(Nil)
    |> result.map(fn(user) { engaged.EngagementChange(user:, engaged: True) })
  })
  |> apply_engagement(state, _)
}

pub fn apply_engagement(
  state: GuildState,
  changes: List(EngagementChange),
) -> GuildState {
  case changes {
    [] -> state
    _ ->
      guild_state.set_push(
        state,
        engaged.on_change(guild_state.push(state), list.reverse(changes)),
      )
  }
}
