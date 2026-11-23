// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/limits

pub opaque type Heartbeat {
  Heartbeat(last_ack_ms: Int, waiting_for_ack: Bool)
}

pub type CheckOutcome {
  TimedOut
  RequestHeartbeat(Heartbeat)
  StillAlive
}

pub fn new(now_ms: Int) -> Heartbeat {
  Heartbeat(last_ack_ms: now_ms, waiting_for_ack: False)
}

pub fn acked(now_ms: Int) -> Heartbeat {
  new(now_ms)
}

pub fn check_interval_ms() -> Int {
  limits.heartbeat_interval_ms() / 3
}

pub fn check(heartbeat: Heartbeat, now_ms: Int) -> CheckOutcome {
  let since_ack = now_ms - heartbeat.last_ack_ms
  case heartbeat.waiting_for_ack && since_ack > limits.heartbeat_timeout_ms() {
    True -> TimedOut
    False ->
      case since_ack * 10 >= limits.heartbeat_interval_ms() * 9 {
        True -> RequestHeartbeat(Heartbeat(..heartbeat, waiting_for_ack: True))
        False -> StillAlive
      }
  }
}
