// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/gateway/counter_table
import fluxer_gateway/gateway/limits
import fluxer_gateway/gateway/opcode.{type Opcode}
import gleam/list
import gleam/option.{type Option, None, Some}

fn voice_signal_window_ms() -> Int {
  durations.gateway().rate_limit_voice_signal_window_ms
}

fn forum_unreads_window_ms() -> Int {
  durations.gateway().rate_limit_forum_unreads_window_ms
}

fn presence_window_ms() -> Int {
  durations.gateway().rate_limit_presence_window_ms
}

const shared_ip_table = "gateway_shared_ip_rate"

const shared_user_table = "gateway_shared_user_rate"

pub fn shared_cleanup_interval_ms() -> Int {
  durations.gateway().rate_limit_shared_cleanup_interval_ms
}

fn shared_window_ms() -> Int {
  durations.gateway().rate_limit_shared_window_ms
}

const shared_ip_max_events = 6000

const shared_user_max_events = 600

fn connection_window_ms() -> Int {
  durations.gateway().rate_limit_connection_window_ms
}

const connection_max_events = 600

pub opaque type RateLimits {
  RateLimits(
    events: List(Int),
    presence: List(Int),
    forum_unreads: List(Int),
    voice_signal: List(Int),
  )
}

pub type Verdict {
  Allowed(RateLimits)
  ConnectionLimited
  OpcodeLimited(RateLimits)
}

pub fn new() -> RateLimits {
  RateLimits(events: [], presence: [], forum_unreads: [], voice_signal: [])
}

pub fn create_tables() -> Nil {
  counter_table.create(shared_ip_table)
  counter_table.create(shared_user_table)
}

pub fn prune() -> Nil {
  let cutoff = clock.system_ms() / shared_window_ms() - 1
  counter_table.prune_buckets_before(shared_ip_table, cutoff)
  counter_table.prune_buckets_before(shared_user_table, cutoff)
}

pub fn check(
  rates: RateLimits,
  op: Opcode,
  peer_ip: String,
  session: Option(SessionId),
) -> Verdict {
  case limits.rate_limits_disabled() {
    True -> Allowed(rates)
    False ->
      case shared_ip_ok(peer_ip) && shared_user_ok(session) {
        False -> ConnectionLimited
        True -> check_connection(rates, op, clock.system_ms())
      }
  }
}

pub fn check_connection(rates: RateLimits, op: Opcode, now: Int) -> Verdict {
  case
    window(rates.events, now, connection_window_ms(), connection_max_events)
  {
    Error(Nil) -> ConnectionLimited
    Ok(events) -> check_opcode(RateLimits(..rates, events:), op, now)
  }
}

fn check_opcode(rates: RateLimits, op: Opcode, now: Int) -> Verdict {
  case op {
    opcode.PresenceUpdate ->
      case window(rates.presence, now, presence_window_ms(), 5) {
        Ok(presence) -> Allowed(RateLimits(..rates, presence:))
        Error(Nil) -> OpcodeLimited(rates)
      }
    opcode.RequestForumUnreads ->
      case window(rates.forum_unreads, now, forum_unreads_window_ms(), 5) {
        Ok(forum_unreads) -> Allowed(RateLimits(..rates, forum_unreads:))
        Error(Nil) -> OpcodeLimited(rates)
      }
    opcode.VoiceSignal ->
      case window(rates.voice_signal, now, voice_signal_window_ms(), 240) {
        Ok(voice_signal) -> Allowed(RateLimits(..rates, voice_signal:))
        Error(Nil) -> OpcodeLimited(rates)
      }
    _ -> Allowed(rates)
  }
}

pub fn window(
  events: List(Int),
  now: Int,
  window_ms: Int,
  max_events: Int,
) -> Result(List(Int), Nil) {
  let in_window = list.filter(events, fn(at) { now - at < window_ms })
  case list.length(in_window) >= max_events {
    True -> Error(Nil)
    False -> Ok([now, ..in_window])
  }
}

fn shared_ip_ok(peer_ip: String) -> Bool {
  case peer_ip {
    "unknown" -> True
    _ -> shared_ok(shared_ip_table, peer_ip, shared_ip_max_events)
  }
}

fn shared_user_ok(session: Option(SessionId)) -> Bool {
  case session {
    None -> True
    Some(key) -> shared_ok(shared_user_table, key, shared_user_max_events)
  }
}

fn shared_ok(table: String, key: key, max_events: Int) -> Bool {
  let bucket = clock.system_ms() / shared_window_ms()
  case counter_table.bump(table, #(key, bucket)) {
    Ok(count) -> count <= max_events
    Error(Nil) -> True
  }
}
