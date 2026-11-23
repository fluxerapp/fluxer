// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{
  type JoinReply, type JoinRequest, AsMember, AsVisitor, Focused, JoinOverloaded,
  JoinRequest, JoinUnavailable, Joined, NotMember,
}
import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/core/session.{
  type IdentifyFlags, type SessionId, type SessionRef,
}
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/handoff/roster
import fluxer_gateway/guild/health
import fluxer_gateway/guild/presence/rows
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/sessions/guild_create
import fluxer_gateway/guild/sessions/passive_sync
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/guild/visitors
import fluxer_gateway/guild/visitors/table as visitor_table
import fluxer_gateway/member_list
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/session/control
import fluxer_gateway/telemetry/metrics
import gleam/dict
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}

fn too_many_sessions_retry_ms() -> Int {
  durations.guild().connect_too_many_sessions_retry_ms
}

fn no_owner_retry_ms() -> Int {
  durations.guild().connect_no_owner_retry_ms
}

pub fn join(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
) -> GuildState {
  case request.mode {
    AsVisitor(operator:, adult_content_allowed:, tier:, ip_key:) ->
      case guild_state.member(state, request.session.user) {
        Some(_) ->
          join_member(state, JoinRequest(..request, mode: AsMember), reply)
        None ->
          visitors.admit(
            state,
            request,
            reply,
            operator:,
            adult: adult_content_allowed,
            tier:,
            ip_key:,
          )
      }
    AsMember -> join_member(state, request, reply)
  }
}

fn join_member(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
) -> GuildState {
  case process.subject_owner(request.session.inbox) {
    Error(Nil) -> {
      process.send(reply, JoinUnavailable(no_owner_retry_ms(), request.attempt))
      state
    }
    Ok(pid) ->
      case table.get(guild_state.sessions(state), request.session.id) {
        Some(found) -> rejoin(state, request, reply, found, pid)
        None -> admit(state, request, reply, pid)
      }
  }
}

fn rejoin(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  found: Entry,
  pid: Pid,
) -> GuildState {
  case guild_state.member(state, request.session.user) {
    None -> {
      process.send(reply, NotMember(request.attempt))
      remove(state, found)
    }
    Some(_) -> {
      let #(state, updated) = rebind_entry(state, found, request.session, pid)
      let state =
        reply_joined(
          state,
          request,
          reply,
          entry.flags(updated),
          entry.is_bot(updated),
        )
      health.send_current(guild_state.guild_id(state), request.session)
      state
    }
  }
}

fn rebind_entry(
  state: GuildState,
  found: Entry,
  ref: SessionRef,
  pid: Pid,
) -> #(GuildState, Entry) {
  let monitor = case entry.pid(found) == pid {
    True -> entry.monitor(found)
    False -> {
      process.demonitor_process(entry.monitor(found))
      process.monitor(pid)
    }
  }
  let updated = entry.rebind(found, ref:, pid:, monitor:)
  let state =
    guild_state.set_sessions(
      state,
      table.put(guild_state.sessions(state), updated),
    )
    |> guild_threads.session_rebound(ref)
  member_list.rebind(guild_state.member_list(state), ref)
  health.put_session(guild_state.guild_id(state), ref)
  roster.put(guild_state.guild_id(state), ref.id)
  #(state, updated)
}

pub fn moved(
  state: GuildState,
  to: SessionRef,
  from: SessionRef,
) -> #(GuildState, Bool) {
  let owner = process.subject_owner(to.inbox)
  let source = process.subject_owner(from.inbox)
  case owner, source, to.id == from.id {
    Ok(pid), Ok(old), True ->
      case table.get(guild_state.sessions(state), to.id) {
        Some(found) ->
          case entry.pid(found) == old || entry.pid(found) == pid {
            True -> #(rebind_entry(state, found, to, pid).0, True)
            False -> #(state, False)
          }
        None ->
          case visitors.moved(state, to, pid, old) {
            #(state, True) -> {
              member_list.rebind(guild_state.member_list(state), to)
              #(guild_threads.session_rebound(state, to), True)
            }
            unmoved -> unmoved
          }
      }
    _, _, _ -> #(state, False)
  }
}

fn admit(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  pid: Pid,
) -> GuildState {
  let user = request.session.user
  let key = id.user_id_to_int(user)
  let sessions = guild_state.sessions(state)
  case
    table.user_session_count(sessions, key) >= table.max_sessions_per_user,
    guild_state.member(state, user)
  {
    True, _ -> {
      process.send(
        reply,
        JoinOverloaded(too_many_sessions_retry_ms(), request.attempt),
      )
      state
    }
    False, None -> {
      process.send(reply, NotMember(request.attempt))
      state
    }
    False, Some(_) -> register(state, request, reply, pid)
  }
}

fn register(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  pid: Pid,
) -> GuildState {
  let mode = case request.bot || request.class == Focused {
    True -> entry.Active
    False -> entry.Passive
  }
  let state =
    add_session(
      state,
      ref: request.session,
      pid:,
      flags: request.flags,
      bot: request.bot,
      staff: Some(request.staff),
      mode:,
      synced: request.class == Focused,
      own_presence: Some(request.presence),
    )
  let state = reply_joined(state, request, reply, request.flags, request.bot)
  health.send_current(guild_state.guild_id(state), request.session)
  state
}

fn add_session(
  state: GuildState,
  ref ref: SessionRef,
  pid pid: Pid,
  flags flags: IdentifyFlags,
  bot bot: Bool,
  staff staff: Option(Bool),
  mode mode: entry.Mode,
  synced synced: Bool,
  own_presence own_presence: Option(Presence),
) -> GuildState {
  let user = ref.user
  let key = id.user_id_to_int(user)
  let state =
    visitors.remove_session(state, session.session_id_to_string(ref.id))
  let created =
    entry.new(
      ref:,
      pid:,
      monitor: process.monitor(pid),
      flags:,
      bot:,
      staff:,
      mode:,
    )
    |> entry.set_synced(synced)
    |> entry.set_seen(
      messages: passive_sync.baseline(state, user),
      voice: dict.new(),
    )
  let sessions = guild_state.sessions(state)
  let was_connected = table.is_connected(sessions, key)
  let sessions =
    sessions
    |> table.put(created)
    |> table.track_connected(key, 1)
  let state =
    guild_state.set_sessions(state, sessions)
    |> guild_threads.session_joined(ref, flags, bot)
  health.put_session(guild_state.guild_id(state), ref)
  roster.put(guild_state.guild_id(state), ref.id)
  case was_connected {
    True -> state
    False -> became_connected(state, ref, own_presence)
  }
}

fn became_connected(
  state: GuildState,
  ref: SessionRef,
  own_presence: Option(Presence),
) -> GuildState {
  let user = ref.user
  let key = id.user_id_to_int(user)
  let state =
    rows.apply_list(state, [
      list_event.connected(user, list_event.Unchanged),
    ])
  let state = session_presence.watch_and_resend(state, [key])
  let #(state, own) = case own_presence {
    Some(found) -> rows.apply(state, found, rows.Immediate)
    None -> #(state, [])
  }
  let #(state, connection) = rows.engagement(state, [key])
  rows.apply_engagement(state, list.append(connection, own) |> list.reverse)
}

fn reply_joined(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  flags: IdentifyFlags,
  bot: Bool,
) -> GuildState {
  let #(state, payload) = case
    guild_state.is_unavailable_for(state, request.session.user)
  {
    True -> #(state, guild_create.unavailable(state))
    False -> guild_create.build(state, request.session.user, flags, bot)
  }
  process.send(
    reply,
    Joined(guild_state.self_ref(state), payload, request.attempt),
  )
  state
}

pub fn leave(state: GuildState, session_id: SessionId) -> GuildState {
  case table.get(guild_state.sessions(state), session_id) {
    Some(found) -> remove(state, found)
    None ->
      visitors.remove_session(state, session.session_id_to_string(session_id))
  }
}

pub fn leave_owned(
  state: GuildState,
  session_id: SessionId,
  pid: Pid,
) -> GuildState {
  case table.get(guild_state.sessions(state), session_id) {
    Some(found) ->
      case entry.pid(found) == pid {
        True -> remove(state, found)
        False -> state
      }
    None ->
      visitors.remove_owned(
        state,
        session.session_id_to_string(session_id),
        pid,
      )
  }
}

pub fn remove_user(state: GuildState, user: Int) -> GuildState {
  table.entries(guild_state.sessions(state))
  |> list.filter(fn(found) { entry.user_int(found) == user })
  |> list.fold(state, remove)
}

pub fn on_down(state: GuildState, monitor: Monitor) -> GuildState {
  case table.find_by_monitor(guild_state.sessions(state), monitor) {
    Some(found) -> remove(state, found)
    None -> visitors.remove_monitored(state, monitor)
  }
}

fn remove(state: GuildState, found: Entry) -> GuildState {
  process.demonitor_process(entry.monitor(found))
  health.remove_session(guild_state.guild_id(state), entry.id(found))
  roster.remove(guild_state.guild_id(state), entry.id(found))
  let key = entry.user_int(found)
  let #(sessions, _, subscribed) =
    guild_state.sessions(state)
    |> table.delete(entry.id(found))
    |> table.replace_subscriptions(entry.key(found), [])
  let sessions = table.track_connected(sessions, key, -1)
  let state =
    guild_state.set_sessions(state, sessions)
    |> guild_threads.session_left(entry.key(found))
  case
    member_list.unsubscribe(guild_state.member_list(state), entry.id(found))
  {
    Ok(dispatches) -> send.list_dispatches(state, dispatches)
    Error(_) -> Nil
  }
  let state = case table.is_connected(sessions, key) {
    True -> state
    False -> {
      let state =
        rows.apply_list(state, [
          list_event.disconnected(entry.user(found)),
        ])
      let #(state, changes) = rows.engagement(state, [key])
      rows.apply_engagement(state, changes)
    }
  }
  session_presence.release(state, [key, ..subscribed])
}

pub fn promote_user(state: GuildState, user: Int) -> GuildState {
  visitors.of_user(state, user)
  |> list.fold(state, promote)
}

pub fn promoted_flags(found: visitor_table.Visitor) -> IdentifyFlags {
  session.IdentifyFlags(
    ..session.no_flags(),
    guild_visits: True,
    channel_threads: found.thread_viewer,
  )
}

fn promote(state: GuildState, found: visitor_table.Visitor) -> GuildState {
  let ref = found.ref
  let guild = guild_state.guild_id(state)
  let state = visitors.remove_session(state, visitor_table.key(found))
  let sessions = guild_state.sessions(state)
  case
    guild_state.member(state, ref.user),
    guild_state.is_unavailable_for(state, ref.user)
    || table.user_session_count(sessions, id.user_id_to_int(ref.user))
    >= table.max_sessions_per_user
  {
    Some(_), False -> {
      let flags = promoted_flags(found)
      let mode = case found.focus {
        Some(_) -> entry.Active
        None -> entry.Passive
      }
      let state =
        add_session(
          state,
          ref:,
          pid: found.pid,
          flags:,
          bot: False,
          staff: None,
          mode:,
          synced: True,
          own_presence: None,
        )
      let #(state, payload) = guild_create.build(state, ref.user, flags, False)
      control.send(
        ref,
        control.VisitPromoted(guild_state.self_ref(state), payload),
      )
      health.send_current(guild, ref)
      metrics.counter("fluxer_gateway_guild_visitor_converted_total", [], 1)
      state
    }
    _, _ -> {
      control.send(ref, control.VisitRejected(guild))
      state
    }
  }
}
