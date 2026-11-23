// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics as telemetry
import gleam/list
import gleam/option.{type Option, None, Some}

type Counters

type CountersOption {
  WriteConcurrency
}

type Key {
  FluxerGatewayClusterOwnerResolutions
}

type Stored

const owner_self = 1

const owner_peer = 2

const owner_resolutions = "fluxer_gateway_cluster_owner_resolutions_total"

@external(erlang, "counters", "new")
fn counters_new(size: Int, options: List(CountersOption)) -> Counters

@external(erlang, "counters", "add")
fn counters_add(counters: Counters, index: Int, by: Int) -> Stored

@external(erlang, "counters", "get")
fn counters_get(counters: Counters, index: Int) -> Int

@external(erlang, "persistent_term", "get")
fn get_counters(key: Key, default: Option(Counters)) -> Option(Counters)

@external(erlang, "persistent_term", "put")
fn put_counters(key: Key, value: Option(Counters)) -> Stored

fn counters() -> Counters {
  case get_counters(FluxerGatewayClusterOwnerResolutions, None) {
    Some(found) -> found
    None -> {
      let created = counters_new(2, [WriteConcurrency])
      put_counters(FluxerGatewayClusterOwnerResolutions, Some(created))
      telemetry.register_collector(owner_resolutions, collect)
      created
    }
  }
}

fn collect() -> List(telemetry.Sample) {
  let found = counters()
  [
    telemetry.Sample(
      owner_resolutions,
      [#("result", "self")],
      counters_get(found, owner_self),
    ),
    telemetry.Sample(
      owner_resolutions,
      [#("result", "peer")],
      counters_get(found, owner_peer),
    ),
  ]
}

pub fn record_discovery_resolve_failure() -> Nil {
  telemetry.counter(
    "fluxer_gateway_cluster_discovery_resolve_failures_total",
    [],
    1,
  )
}

pub fn record_membership_transition(up: Bool) -> Nil {
  let direction = case up {
    True -> "up"
    False -> "down"
  }
  telemetry.counter(
    "fluxer_gateway_cluster_membership_transitions_total",
    [#("direction", direction)],
    1,
  )
}

pub fn record_owner_resolution(local: Bool) -> Nil {
  let index = case local {
    True -> owner_self
    False -> owner_peer
  }
  counters_add(counters(), index, 1)
  Nil
}

pub fn set_member_count(count: Int) -> Nil {
  telemetry.gauge("fluxer_gateway_cluster_member_count", [], count)
}

pub fn record_peer_state(peer: String, state: String, value: Int) -> Nil {
  telemetry.gauge(
    "fluxer_gateway_cluster_peer_state",
    [#("peer", peer), #("state", state)],
    value,
  )
}

pub fn forget_peer(peer: String) -> Nil {
  list.each(["up", "suspect", "gone", "joining", "draining"], fn(state) {
    telemetry.remove(telemetry.Gauge, "fluxer_gateway_cluster_peer_state", [
      #("peer", peer),
      #("state", state),
    ])
  })
  list.each(
    ["connection_closed", "net_tick_timeout", "disconnect", "other"],
    fn(reason) {
      telemetry.remove(
        telemetry.Counter,
        "fluxer_gateway_cluster_nodedown_total",
        [#("peer", peer), #("reason", reason)],
      )
    },
  )
}

pub fn record_nodedown(peer: String, reason: String) -> Nil {
  telemetry.counter(
    "fluxer_gateway_cluster_nodedown_total",
    [#("peer", peer), #("reason", reason)],
    1,
  )
}

pub fn record_down_duration(ms: Int) -> Nil {
  telemetry.observe_ms("fluxer_gateway_cluster_down_duration_ms", [], ms)
}

pub fn record_gone_vetoed() -> Nil {
  telemetry.counter("fluxer_gateway_cluster_gone_vetoed_total", [], 1)
}

pub fn record_topology_publish() -> Nil {
  telemetry.counter("fluxer_gateway_cluster_topology_publishes_total", [], 1)
}

pub fn record_mesh_size(size: Int) -> Nil {
  telemetry.gauge("fluxer_gateway_cluster_mesh_size", [], size)
}

pub fn record_moves(reason: String, count: Int) -> Nil {
  telemetry.counter(
    "fluxer_gateway_guild_moves_total",
    [#("reason", reason)],
    count,
  )
}

pub const beat_rtt = "fluxer_gateway_bouncer_beat_rtt_ms"

pub const beat_acks = "fluxer_gateway_bouncer_beat_acks_total"

pub const probe_duration = "fluxer_gateway_bouncer_probe_ms"

pub const close_duration = "fluxer_gateway_bouncer_close_ms"

pub const close_outcomes = ["closed", "refused", "unreachable"]

pub fn record_beat_rtt(ms: Int) -> Nil {
  telemetry.observe_ms(beat_rtt, [], ms)
}

pub fn record_beat_ack(matched: Bool) -> Nil {
  let outcome = case matched {
    True -> "matched"
    False -> "mismatched"
  }
  telemetry.counter(beat_acks, [#("outcome", outcome)], 1)
}

pub fn record_probe(isolated: Bool, ms: Int) -> Nil {
  let outcome = case isolated {
    True -> "isolated"
    False -> "not_isolated"
  }
  telemetry.observe_ms(probe_duration, [#("outcome", outcome)], ms)
}

pub fn record_close(outcome: String, ms: Int) -> Nil {
  telemetry.observe_ms(close_duration, [#("outcome", outcome)], ms)
}
