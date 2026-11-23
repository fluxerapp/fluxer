// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_gateway/config/durations
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

fn remote_pod_lookup_timeout_ms() -> Int {
  durations.telemetry().timings_remote_pod_lookup_timeout_ms
}

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
  Span(name: String, duration_us: Int, remote: Option(Remote))
}

pub opaque type Remote {
  Remote(operation: String, pod_name: Option(String))
}

type TimeUnit {
  Microsecond
}

type PodNameKey {
  FluxerGatewayTimingsPodName(node: Atom)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "rpc", "call")
fn rpc_call(
  node: Atom,
  module: Atom,
  function: Atom,
  args: List(Nil),
  timeout_ms: Int,
) -> RemoteReply

type RemoteReply

@external(erlang, "erlang", "is_binary")
fn is_text(reply: RemoteReply) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn reply_text(reply: RemoteReply) -> String

@external(erlang, "persistent_term", "get")
fn cached_pod_name(key: PodNameKey, default: String) -> String

type Stored

@external(erlang, "persistent_term", "put")
fn cache_pod_name(key: PodNameKey, value: String) -> Stored

@external(erlang, "inet", "gethostname")
fn gethostname() -> Result(Charlist, Atom)

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
    pod_name: local_pod_name(),
    trace: [],
  )
}

pub fn step(timing: Timing, step: String) -> Timing {
  let now = now_us()
  Timing(..timing, mark_us: now, trace: [
    Span(step, int.max(now - timing.mark_us, 0), None),
    ..timing.trace
  ])
}

pub fn record(
  timing: Timing,
  step: String,
  started_at_us: Int,
  remote: Option(Remote),
) -> Timing {
  let now = now_us()
  Timing(..timing, mark_us: now, trace: [
    Span(step, int.max(now - started_at_us, 0), remote),
    ..timing.trace
  ])
}

pub fn remote_node(operation: String, target: Node) -> Option(Remote) {
  case target == node.self() {
    True -> None
    False -> Some(Remote(operation, remote_pod_name(target)))
  }
}

pub fn api_remote(pod_name: Option(String)) -> Option(Remote) {
  Some(Remote("api", non_empty(pod_name)))
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
  let remote = case span.remote {
    Some(remote) -> [#("remote", remote_to_json(remote))]
    None -> []
  }
  json.object([
    #("duration_us", json.int(span.duration_us)),
    #("name", json.string(span.name)),
    ..remote
  ])
}

fn remote_to_json(remote: Remote) -> Json {
  let pod_name = case remote.pod_name {
    Some(pod_name) -> [#("pod_name", json.string(pod_name))]
    None -> []
  }
  json.object([#("operation", json.string(remote.operation)), ..pod_name])
}

pub fn local_pod_name() -> String {
  use <- non_blank_or(envoy.get("POD_NAME"))
  use <- non_blank_or(envoy.get("HOSTNAME"))
  case gethostname() {
    Ok(hostname) -> string.trim(charlist.to_string(hostname))
    Error(_) -> ""
  }
}

fn non_blank_or(
  found: Result(String, Nil),
  otherwise: fn() -> String,
) -> String {
  case result.map(found, string.trim) {
    Ok(value) if value != "" -> value
    _ -> otherwise()
  }
}

fn remote_pod_name(target: Node) -> Option(String) {
  let key = FluxerGatewayTimingsPodName(node.to_atom(target))
  case cached_pod_name(key, "") {
    "" -> lookup_remote_pod_name(key, target)
    pod_name -> Some(pod_name)
  }
}

fn lookup_remote_pod_name(key: PodNameKey, target: Node) -> Option(String) {
  let reply =
    rpc_call(
      node.to_atom(target),
      atom.create("fluxer_gateway@telemetry@timings"),
      atom.create("local_pod_name"),
      [],
      remote_pod_lookup_timeout_ms(),
    )
  case is_text(reply) {
    False -> None
    True -> remembered(key, reply_text(reply))
  }
}

fn remembered(key: PodNameKey, pod_name: String) -> Option(String) {
  case pod_name {
    "" -> None
    _ -> {
      let _ = cache_pod_name(key, pod_name)
      Some(pod_name)
    }
  }
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some("") -> None
    other -> other
  }
}
