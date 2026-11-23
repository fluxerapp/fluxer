// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics
import gleam/list

pub const requests = "fluxer_gateway_api_requests_total"

pub const request_duration = "fluxer_gateway_api_request_duration_ms"

pub const retries = "fluxer_gateway_api_retries_total"

pub const in_flight = "fluxer_gateway_api_in_flight"

pub const breaker_state = "fluxer_gateway_api_breaker_state"

pub const breaker_transitions = "fluxer_gateway_api_breaker_transitions_total"

pub const request_types = [
  "call_ended", "forum_unreads", "get_dm_channel", "get_gateway_rollout_config",
  "get_read_state", "get_user_blocked_ids", "get_user_guild_settings",
  "get_users_adult_content_allowed", "guild_collection",
  "guild_thread_flip_data", "kick_temporary_member", "list_thread_members",
  "session", "validate_custom_status", "voice_force_disconnect_participant",
  "voice_get_token", "voice_state_remove", "voice_state_upsert",
  "voice_update_participant",
]

pub const breaker_states = ["closed", "open", "half_open"]

pub fn request_type(kind: String) -> String {
  case list.contains(request_types, kind) {
    True -> kind
    False -> "other"
  }
}

pub fn request(
  kind: String,
  outcome: String,
  elapsed_ms: Int,
  sent: Bool,
) -> Nil {
  let kind = request_type(kind)
  metrics.counter(requests, [#("request_type", kind), #("outcome", outcome)], 1)
  case sent {
    True ->
      metrics.observe_ms(
        request_duration,
        [#("request_type", kind)],
        elapsed_ms,
      )
    False -> Nil
  }
}

pub fn retry(kind: String) -> Nil {
  metrics.counter(retries, [#("request_type", request_type(kind))], 1)
}

pub fn breaker_closed() -> Nil {
  metrics.gauge(breaker_state, [], 0)
}

pub fn breaker(state: String, code: Int) -> Nil {
  metrics.gauge(breaker_state, [], code)
  metrics.counter(breaker_transitions, [#("state", state)], 1)
}

pub fn watch_in_flight(read: fn() -> Int) -> Nil {
  metrics.register_collector(in_flight, fn() {
    [metrics.Sample(in_flight, [], read())]
  })
}
