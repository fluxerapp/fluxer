// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/fixed_counters.{type Series, Series}
import fluxer_gateway/telemetry/metrics
import gleam/list

pub const connections = "fluxer_gateway_nats_connections"

pub const connection_events = "fluxer_gateway_nats_connection_events_total"

pub const subscriptions = "fluxer_gateway_nats_subscriptions"

pub const publishes = "fluxer_gateway_nats_publishes_total"

pub const messages = "fluxer_gateway_nats_messages_total"

pub const bytes = "fluxer_gateway_nats_bytes_total"

pub const connection_states = ["ready", "down"]

pub const events = ["connected", "connect_failed", "lost"]

pub const publish_kinds = ["reply", "request"]

pub const outcomes = [
  "ok", "not_connected", "timeout", "no_responders", "closed", "protocol_error",
]

pub fn watch_connections(read: fn() -> #(Int, Int)) -> Nil {
  metrics.register_collector(connections, fn() {
    let #(ready, down) = read()
    [
      metrics.Sample(connections, [#("state", "ready")], ready),
      metrics.Sample(connections, [#("state", "down")], down),
    ]
  })
}

pub fn connection_event(event: String) -> Nil {
  metrics.counter(connection_events, [#("event", event)], 1)
}

pub fn subscriptions_held(count: Int) -> Nil {
  metrics.gauge(subscriptions, [], count)
}

pub fn published(kind: String, outcome: String) -> Nil {
  case index_of(publish_kinds, kind, 0), index_of(outcomes, outcome, 0) {
    Ok(kind), Ok(outcome) ->
      count(traffic_slots + kind * list.length(outcomes) + outcome, 1)
    _, _ -> Nil
  }
}

pub fn sent(size: Int) -> Nil {
  count(1, 1)
  count(3, size)
}

pub fn received(messages: Int, size: Int) -> Nil {
  count(0, messages)
  count(2, size)
}

const traffic = "fluxer_gateway_nats_traffic"

const traffic_slots = 4

fn count(slot: Int, by: Int) -> Nil {
  fixed_counters.add(traffic, traffic_series, slot + 1, by)
}

fn index_of(values: List(String), wanted: String, at: Int) -> Result(Int, Nil) {
  case values {
    [] -> Error(Nil)
    [found, ..] if found == wanted -> Ok(at)
    [_, ..rest] -> index_of(rest, wanted, at + 1)
  }
}

fn traffic_series() -> List(Series) {
  list.flatten([
    [
      Series(messages, [#("direction", "in")]),
      Series(messages, [#("direction", "out")]),
      Series(bytes, [#("direction", "in")]),
      Series(bytes, [#("direction", "out")]),
    ],
    list.flat_map(publish_kinds, fn(kind) {
      list.map(outcomes, fn(outcome) {
        Series(publishes, [#("kind", kind), #("outcome", outcome)])
      })
    }),
  ])
}
