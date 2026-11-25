// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/node_name
import fluxer_gateway/core/node.{type Node}
import gleam/int
import gleam/json.{type Json}
import gleam/list

pub opaque type Timing {
  Timing(
    name: String,
    started_at_us: Int,
    mark_us: Int,
    origin: Node,
    pod_name: String,
    trace: List(Span),
  )
}

type Span {
  Span(name: String, duration_us: Int)
}

type TimeUnit {
  Microsecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

pub fn now_us() -> Int {
  monotonic_time(Microsecond)
}

pub fn start(name: String) -> Timing {
  let now = now_us()
  Timing(
    name:,
    started_at_us: now,
    mark_us: now,
    origin: node.self(),
    pod_name: node_name.pod_name(),
    trace: [],
  )
}

pub fn step(timing: Timing, step: String) -> Timing {
  let now = now_us()
  Timing(..timing, mark_us: now, trace: [
    Span(step, int.max(now - timing.mark_us, 0)),
    ..timing.trace
  ])
}

pub fn record(timing: Timing, step: String, started_at_us: Int) -> Timing {
  let now = now_us()
  Timing(..timing, mark_us: now, trace: [
    Span(step, int.max(now - started_at_us, 0)),
    ..timing.trace
  ])
}

pub fn finish(timing: Timing) -> Json {
  let trace = list.reverse(timing.trace)
  let total_us = case trace {
    [] ->
      case timing.origin == node.self() {
        True -> int.max(now_us() - timing.started_at_us, 0)
        False -> 0
      }
    _ -> list.fold(trace, 0, fn(total, span) { total + span.duration_us })
  }
  let pod_name = case timing.pod_name {
    "" -> "unknown"
    pod_name -> pod_name
  }
  json.object([
    #("pod_name", json.string(pod_name)),
    #("total_us", json.int(total_us)),
    #("trace", json.array(trace, span_to_json)),
    #("unit", json.string("microseconds")),
  ])
}

fn span_to_json(span: Span) -> Json {
  json.object([
    #("duration_us", json.int(span.duration_us)),
    #("name", json.string(span.name)),
  ])
}
