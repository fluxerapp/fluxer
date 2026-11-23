// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/heartbeat

pub fn interval_test() {
  assert heartbeat.check_interval_ms() == 13_750
}

pub fn fresh_ack_is_alive_test() {
  let beat = heartbeat.new(0)
  assert heartbeat.check(beat, 13_750) == heartbeat.StillAlive
  assert heartbeat.check(beat, 37_124) == heartbeat.StillAlive
}

pub fn requests_at_ninety_percent_test() {
  let beat = heartbeat.new(0)
  let assert heartbeat.RequestHeartbeat(waiting) = heartbeat.check(beat, 37_125)
  assert heartbeat.check(waiting, 45_000) != heartbeat.TimedOut
  assert heartbeat.check(waiting, 45_001) == heartbeat.TimedOut
}

pub fn not_waiting_never_times_out_test() {
  let beat = heartbeat.new(0)
  let assert heartbeat.RequestHeartbeat(_) = heartbeat.check(beat, 90_000)
}

pub fn ack_resets_test() {
  let assert heartbeat.RequestHeartbeat(_) =
    heartbeat.check(heartbeat.new(0), 40_000)
  assert heartbeat.check(heartbeat.acked(40_000), 50_000)
    == heartbeat.StillAlive
}
