// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/gateway/counter_table
import fluxer_gateway/gateway/limits
import gleam/bit_array
import gleam/crypto
import gleam/erlang/atom.{type Atom}

const identify_table = "gateway_identify_rate"

const session_user_counts_table = "session_user_counts"

const identify_max_per_ip = 300

fn identify_window_secs() -> Int {
  durations.gateway().abuse_protection_identify_window_secs
}

const max_sessions_per_user = 100

pub type AbuseError {
  IdentifyRateLimited
  TooManySessions
}

pub fn create_tables() -> Nil {
  counter_table.create(identify_table)
  counter_table.create(session_user_counts_table)
}

pub fn check_identify_rate(peer_ip: String) -> Result(Nil, AbuseError) {
  case limits.rate_limits_disabled() {
    True -> Ok(Nil)
    False -> {
      let key = #(peer_ip, current_bucket())
      case counter_table.bump(identify_table, key) {
        Ok(count) if count > identify_max_per_ip -> Error(IdentifyRateLimited)
        _ -> Ok(Nil)
      }
    }
  }
}

pub fn check_token_identify_rate(
  token: String,
  limit_per_minute: Int,
) -> Result(Nil, AbuseError) {
  case limit_per_minute <= 0 || limits.rate_limits_disabled() {
    True -> Ok(Nil)
    False -> {
      let key = #(#(token_tag(), token_hash(token)), current_bucket())
      case counter_table.bump(identify_table, key) {
        Ok(count) if count > limit_per_minute -> Error(IdentifyRateLimited)
        _ -> Ok(Nil)
      }
    }
  }
}

pub fn reserve_user_session(user: UserId) -> Result(Nil, AbuseError) {
  let key = id.user_id_to_int(user)
  case counter_table.bump(session_user_counts_table, key) {
    Ok(count) if count > max_sessions_per_user ->
      case limits.rate_limits_disabled() {
        True -> Ok(Nil)
        False -> {
          counter_table.drop(session_user_counts_table, key)
          Error(TooManySessions)
        }
      }
    _ -> Ok(Nil)
  }
}

pub fn increment_user_sessions(user: UserId) -> Nil {
  let _ = counter_table.bump(session_user_counts_table, id.user_id_to_int(user))
  Nil
}

pub fn decrement_user_sessions(user: UserId) -> Nil {
  counter_table.drop(session_user_counts_table, id.user_id_to_int(user))
}

pub fn prune() -> Nil {
  counter_table.prune_buckets_before(identify_table, current_bucket() - 1)
}

fn current_bucket() -> Int {
  clock.system_ms() / 1000 / identify_window_secs()
}

fn token_tag() -> Atom {
  atom.create("identify_token")
}

fn token_hash(token: String) -> BitArray {
  let digest = crypto.hash(crypto.Sha256, bit_array.from_string(token))
  case bit_array.slice(digest, 0, 16) {
    Ok(prefix) -> prefix
    Error(Nil) -> digest
  }
}
