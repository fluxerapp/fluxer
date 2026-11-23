// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild.{
  type JoinReply, type JoinRequest, Joined, VisitRejected,
} as core_guild
import fluxer_gateway/core/guild_create
import fluxer_gateway/core/session
import fluxer_gateway/guild/sessions as sessions_table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/visitors/snapshot
import fluxer_gateway/guild/visitors/table.{type Visitor, Visitor}
import fluxer_gateway/session/control
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/threads/viewers as thread_viewers
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

type Rejection {
  Disabled
  AgeRestricted
  Unavailable
  Busy
}

pub type Eviction {
  EvictBanned
  EvictDisabled
  EvictAgeRestricted
  EvictUnavailable
  EvictBusy
  EvictRemoved
}

pub fn count(state: GuildState) -> Int {
  table.count(visitors(state))
}

pub fn admit(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  operator operator: Bool,
  adult adult: Bool,
  tier tier: String,
  ip_key ip_key: Option(String),
) -> GuildState {
  let key = session.session_id_to_string(request.session.id)
  let state = remove_session(state, key)
  case static_rejection(state, operator, adult) {
    Some(reason) -> reject(state, request, reply, reason)
    None ->
      case process.subject_owner(request.session.inbox) {
        Error(Nil) -> reject(state, request, reply, Unavailable)
        Ok(pid) -> {
          let thread_viewer =
            thread_viewers.is_viewer_session(request.flags, request.bot)
          let candidate =
            Visitor(
              ref: request.session,
              pid:,
              monitor: process.monitor(pid),
              adult:,
              thread_viewer:,
              operator:,
              tier: table.tier_from_wire(Some(tier)),
              ip_key: table.ip_key(ip_key),
              since_ms: clock.system_ms(),
              focus: None,
              view: snapshot.view(state, adult, thread_viewer),
            )
          within_caps(state, request, reply, candidate)
        }
      }
  }
}

fn static_rejection(
  state: GuildState,
  operator: Bool,
  adult: Bool,
) -> Option(Rejection) {
  let facts = guild_state.facts(state)
  case
    unavailable(facts),
    !operator && !core_guild.has_feature(facts, "VISITS_ENABLED"),
    adult
  {
    True, _, _ -> Some(Unavailable)
    False, True, _ -> Some(Disabled)
    False, False, True -> None
    False, False, False ->
      case is_nsfw(facts) {
        True -> Some(AgeRestricted)
        False -> None
      }
  }
}

fn within_caps(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  candidate: Visitor,
) -> GuildState {
  let config = settings.get()
  let current = visitors(state)
  let by_user = table.user_count(current, table.user_of(candidate))
  let by_ip = table.ip_count(current, candidate.ip_key)
  case
    by_user >= config.guild_visitor_sessions_per_user,
    by_ip >= config.guild_visitor_sessions_per_ip,
    table.count(current) >= cap(state)
  {
    True, _, _ | _, True, _ -> refuse(state, request, reply, candidate)
    False, False, False -> add(state, request, reply, candidate)
    False, False, True ->
      case candidate.tier, table.oldest_unverified(current) {
        table.Verified, Some(victim) ->
          state
          |> evict([victim], EvictBusy)
          |> add(request, reply, candidate)
        _, _ -> refuse(state, request, reply, candidate)
      }
  }
}

fn refuse(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  candidate: Visitor,
) -> GuildState {
  process.demonitor_process(candidate.monitor)
  reject(state, request, reply, Busy)
}

fn reject(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  reason: Rejection,
) -> GuildState {
  metrics.counter(
    "fluxer_gateway_guild_visitor_rejected_total",
    [#("reason", rejection_name(reason))],
    1,
  )
  process.send(reply, VisitRejected(request.attempt))
  state
}

fn add(
  state: GuildState,
  request: JoinRequest,
  reply: Subject(JoinReply),
  candidate: Visitor,
) -> GuildState {
  let state = put_visitors(state, table.put(visitors(state), candidate))
  metrics.counter("fluxer_gateway_guild_visitor_admitted_total", [], 1)
  metrics.counter("fluxer_gateway_guild_visitor_sessions", [], 1)
  process.send(
    reply,
    Joined(
      guild_state.self_ref(state),
      guild_create.from_wire(
        guild_state.guild_id(state),
        snapshot.build(state, candidate),
      ),
      request.attempt,
    ),
  )
  state
}

pub fn remove_session(state: GuildState, key: String) -> GuildState {
  let #(next, removed) = table.remove(visitors(state), key)
  case removed {
    None -> state
    Some(found) -> {
      process.demonitor_process(found.monitor)
      metrics.counter("fluxer_gateway_guild_visitor_sessions", [], -1)
      put_visitors(state, next)
    }
  }
}

pub fn remove_owned(state: GuildState, key: String, pid: Pid) -> GuildState {
  case table.get(visitors(state), key) {
    Some(found) if found.pid == pid -> remove_session(state, key)
    _ -> state
  }
}

pub fn moved(
  state: GuildState,
  to: session.SessionRef,
  pid: Pid,
  old: Pid,
) -> #(GuildState, Bool) {
  let current =
    table.visitors(visitors(state))
    |> list.find(fn(found) { found.ref.id == to.id })
  case current {
    Ok(found) if found.pid == old || found.pid == pid -> {
      let monitor = case found.pid == pid {
        True -> found.monitor
        False -> {
          process.demonitor_process(found.monitor)
          process.monitor(pid)
        }
      }
      let next =
        table.replace(
          visitors(state),
          Visitor(..found, ref: to, pid:, monitor:),
        )
      #(put_visitors(state, next), True)
    }
    _ -> #(state, False)
  }
}

pub fn remove_monitored(state: GuildState, monitor: Monitor) -> GuildState {
  case table.find_by_monitor(visitors(state), monitor) {
    Some(found) -> remove_session(state, table.key(found))
    None -> state
  }
}

pub fn set_focus(
  state: GuildState,
  key: String,
  channel: Option(Int),
) -> GuildState {
  case table.get(visitors(state), key) {
    None -> state
    Some(found) -> {
      let focus = keep_focus(state, found, channel)
      put_visitors(
        state,
        table.replace(visitors(state), Visitor(..found, focus:)),
      )
    }
  }
}

fn keep_focus(
  state: GuildState,
  found: Visitor,
  channel: Option(Int),
) -> Option(Int) {
  use channel_int <- option.then(channel)
  case snapshot.channel_visible(state, found, channel_int) {
    True -> Some(channel_int)
    False -> None
  }
}

pub fn evict_ips(
  state: GuildState,
  ip_keys: List(String),
) -> #(List(Int), GuildState) {
  let victims =
    table.visitors(visitors(state))
    |> list.filter(fn(found) {
      case found.ip_key {
        Some(ip) -> list.contains(ip_keys, ip)
        None -> False
      }
    })
  let users =
    victims
    |> list.map(table.user_of)
    |> list.unique
    |> list.sort(int.compare)
  #(users, evict(state, victims, EvictBanned))
}

pub fn evict_disabled(state: GuildState) -> GuildState {
  case core_guild.has_feature(guild_state.facts(state), "VISITS_ENABLED") {
    True -> state
    False ->
      evict(
        state,
        list.filter(table.visitors(visitors(state)), fn(found) {
          !found.operator
        }),
        EvictDisabled,
      )
  }
}

pub fn evict_minors(state: GuildState) -> GuildState {
  case is_nsfw(guild_state.facts(state)) {
    False -> state
    True ->
      evict(
        state,
        list.filter(table.visitors(visitors(state)), fn(found) { !found.adult }),
        EvictAgeRestricted,
      )
  }
}

pub fn evict_unavailable(state: GuildState) -> GuildState {
  case unavailable(guild_state.facts(state)) {
    False -> state
    True -> evict_all(state, EvictUnavailable)
  }
}

pub fn evict_all(state: GuildState, reason: Eviction) -> GuildState {
  evict(state, table.visitors(visitors(state)), reason)
}

fn evict(
  state: GuildState,
  victims: List(Visitor),
  reason: Eviction,
) -> GuildState {
  case victims {
    [] -> state
    _ -> {
      metrics.counter(
        "fluxer_gateway_guild_visitor_evicted_total",
        [#("reason", eviction_name(reason))],
        list.length(victims),
      )
      let guild = guild_state.guild_id(state)
      list.fold(victims, state, fn(acc, found) {
        control.send(found.ref, control.VisitRejected(guild))
        remove_session(acc, table.key(found))
      })
    }
  }
}

pub fn of_user(state: GuildState, user: Int) -> List(Visitor) {
  table.visitors(visitors(state))
  |> list.filter(fn(found) { table.user_of(found) == user })
}

pub fn refresh_views(state: GuildState) -> GuildState {
  table.visitors(visitors(state))
  |> list.fold(state, fn(acc, found) {
    let viewing =
      Visitor(
        ..found,
        view: snapshot.view(acc, found.adult, found.thread_viewer),
      )
    let refreshed =
      Visitor(..viewing, focus: keep_focus(acc, viewing, found.focus))
    put_visitors(acc, table.replace(visitors(acc), refreshed))
  })
}

pub fn visitors(state: GuildState) -> table.Table {
  sessions_table.visitors(guild_state.sessions(state))
}

fn put_visitors(state: GuildState, next: table.Table) -> GuildState {
  guild_state.set_sessions(
    state,
    sessions_table.set_visitors(guild_state.sessions(state), next),
  )
}

fn cap(state: GuildState) -> Int {
  let config = settings.get()
  let members = int.max(0, guild_state.member_count(state))
  int.min(
    config.guild_visitor_sessions_max,
    int.max(
      config.guild_visitor_sessions_floor,
      config.guild_visitor_sessions_per_member * members,
    ),
  )
}

fn unavailable(facts: core_guild.GuildFacts) -> Bool {
  core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE")
  || core_guild.has_feature(facts, "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF")
}

fn is_nsfw(facts: core_guild.GuildFacts) -> Bool {
  core_guild.nsfw_level(facts) == core_guild.NsfwAgeRestricted
}

fn rejection_name(reason: Rejection) -> String {
  case reason {
    Disabled -> "disabled"
    AgeRestricted -> "age_restricted"
    Unavailable -> "unavailable"
    Busy -> "busy"
  }
}

fn eviction_name(reason: Eviction) -> String {
  case reason {
    EvictBanned -> "banned"
    EvictDisabled -> "disabled"
    EvictAgeRestricted -> "age_restricted"
    EvictUnavailable -> "unavailable"
    EvictBusy -> "busy"
    EvictRemoved -> "removed"
  }
}
