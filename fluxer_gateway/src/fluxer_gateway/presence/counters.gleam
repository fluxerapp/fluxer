// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics
import gleam/int

pub type Counter {
  Coalesced
  Dropped
  KilledOffline
  InterestResync
  RemoteSendFailed
  BusyDistPort
  DrainDropped
  PushBufferOverflow
  ValidationTimeout
  RemoteReadTimeout
  Handoff
  ForwardDropped
}

fn counter_name(counter: Counter) -> String {
  case counter {
    Coalesced -> "presence_coalesced_total"
    Dropped -> "presence_dropped_total"
    KilledOffline -> "presence_killed_offline_total"
    InterestResync -> "presence_interest_resyncs_total"
    RemoteSendFailed -> "presence_remote_send_failed_total"
    BusyDistPort -> "presence_busy_dist_port_total"
    DrainDropped -> "presence_drain_dropped_total"
    PushBufferOverflow -> "presence_push_buffer_overflow_total"
    ValidationTimeout -> "presence_custom_status_validation_timeouts_total"
    RemoteReadTimeout -> "presence_remote_read_timeouts_total"
    Handoff -> "presence_handoffs_total"
    ForwardDropped -> "presence_handoff_forward_dropped_total"
  }
}

pub fn add(counter: Counter, by: Int) -> Nil {
  case by > 0 {
    True -> metrics.counter(counter_name(counter), [], by)
    False -> Nil
  }
}

pub fn incr(counter: Counter) -> Nil {
  add(counter, 1)
}

pub fn interest_rows(rows: Int) -> Nil {
  metrics.gauge("presence_interest_rows", [], rows)
}

pub fn subscription_rows(rows: Int) -> Nil {
  metrics.gauge("presence_subscription_rows", [], rows)
}

pub fn session_visibility(hidden: Bool) -> Nil {
  let visibility = case hidden {
    True -> "hidden"
    False -> "active"
  }
  metrics.counter(
    "presence_session_connects_by_visibility_total",
    [#("visibility", visibility)],
    1,
  )
}

pub fn local_presences(shard: Int, count: Int) -> Nil {
  metrics.gauge(
    "presence_local_processes",
    [#("shard", int.to_string(shard))],
    count,
  )
}
