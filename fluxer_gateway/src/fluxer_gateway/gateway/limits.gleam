// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_gateway/config/durations

pub fn max_payload_size() -> Int {
  16_384
}

pub fn heartbeat_interval_ms() -> Int {
  durations.gateway().limits_heartbeat_interval_ms
}

pub fn heartbeat_timeout_ms() -> Int {
  durations.gateway().limits_heartbeat_timeout_ms
}

pub fn resume_timeout_ms() -> Int {
  durations.gateway().limits_resume_timeout_ms
}

pub fn rate_limits_disabled() -> Bool {
  case envoy.get("FLUXER_DISABLE_RATE_LIMITS") {
    Ok("1") | Ok("true") | Ok("TRUE") -> True
    _ -> False
  }
}
