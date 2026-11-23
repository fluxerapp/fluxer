// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/fixed_counters.{Series}
import fluxer_gateway/telemetry/metrics
import gleam/int
import gleam/list

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

pub type InputKind {
  ConnectInput
  RepointInput
  DisconnectInput
  UpdateInput
  SubscriptionInput
  ForeignInput
  ResendInput
  UserDispatchInput
  GuildInput
  TerminateInput
  TimerInput
  HandoffInput
  OtherInput
}

pub type Delivery {
  LocalDelivery
  NodeDelivery
  RelayedDelivery
}

pub type StartOutcome {
  StartFound
  StartCalled
  StartTimeout
  StartUnavailable
  StartBusy
}

pub type LookupOutcome {
  LookupFound
  LookupNotFound
  LookupOwnerUnavailable
  LookupFailed
}

const throughput = "fluxer_gateway_presence_throughput"

pub const inputs_metric = "fluxer_gateway_presence_inputs_total"

pub const published_metric = "fluxer_gateway_presence_events_published_total"

pub const deliveries_metric = "fluxer_gateway_presence_deliveries_total"

pub const start_metric = "fluxer_gateway_presence_start_ms"

pub const lookup_metric = "fluxer_gateway_presence_lookup_ms"

pub const input_kinds = [
  "connect", "repoint", "disconnect", "update", "subscription", "foreign",
  "resend", "user_dispatch", "guild", "terminate", "timer", "handoff", "other",
]

pub const delivery_targets = ["local", "node", "relayed"]

const published_index = 14

const deliveries_index = 15

fn throughput_series() -> List(fixed_counters.Series) {
  list.flatten([
    list.map(input_kinds, fn(kind) { Series(inputs_metric, [#("kind", kind)]) }),
    [Series(published_metric, [])],
    list.map(delivery_targets, fn(target) {
      Series(deliveries_metric, [#("target", target)])
    }),
  ])
}

fn input_index(kind: InputKind) -> Int {
  case kind {
    ConnectInput -> 1
    RepointInput -> 2
    DisconnectInput -> 3
    UpdateInput -> 4
    SubscriptionInput -> 5
    ForeignInput -> 6
    ResendInput -> 7
    UserDispatchInput -> 8
    GuildInput -> 9
    TerminateInput -> 10
    TimerInput -> 11
    HandoffInput -> 12
    OtherInput -> 13
  }
}

fn delivery_offset(target: Delivery) -> Int {
  case target {
    LocalDelivery -> 0
    NodeDelivery -> 1
    RelayedDelivery -> 2
  }
}

pub fn input(kind: InputKind) -> Nil {
  fixed_counters.incr(throughput, throughput_series, input_index(kind))
}

pub fn published() -> Nil {
  fixed_counters.incr(throughput, throughput_series, published_index)
}

pub fn delivered(target: Delivery, count: Int) -> Nil {
  fixed_counters.add(
    throughput,
    throughput_series,
    deliveries_index + delivery_offset(target),
    count,
  )
}

pub fn started(outcome: StartOutcome, elapsed_ms: Int) -> Nil {
  let label = case outcome {
    StartFound -> "found"
    StartCalled -> "called"
    StartTimeout -> "timeout"
    StartUnavailable -> "unavailable"
    StartBusy -> "busy"
  }
  metrics.observe_ms(start_metric, [#("outcome", label)], elapsed_ms)
}

pub fn looked_up(outcome: LookupOutcome, elapsed_ms: Int) -> Nil {
  let label = case outcome {
    LookupFound -> "found"
    LookupNotFound -> "not_found"
    LookupOwnerUnavailable -> "owner_unavailable"
    LookupFailed -> "failed"
  }
  metrics.observe_ms(lookup_metric, [#("outcome", label)], elapsed_ms)
}
