// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/lease as proto_lease
import fluxer_gateway/app/drain
import fluxer_gateway/app/drain_report.{type Report}
import fluxer_gateway/app/health
import fluxer_gateway/cluster/bouncer_client
import fluxer_gateway/cluster/handoff
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/router
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/build_info
import fluxer_gateway/config/draining
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/gateway/ws/server
import fluxer_gateway/handoff/move
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/prometheus
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bytes_tree
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option
import gleam/otp/supervision
import gleam/result
import gleam/set
import gleam/string
import glisten.{type IpAddress}
import logging

const listen_interface = "0.0.0.0"

const port_query_timeout_ms = 5000

const metrics_content_type = "text/plain; version=0.0.4; charset=utf-8"

pub type Status {
  Status(
    role: String,
    template: String,
    version: String,
    revision: String,
    wire_epoch: Int,
    wire_epoch_floor: Int,
    epoch_refused_peers: Int,
    bridged_peers: Int,
    ready: Bool,
    ready_reason: String,
    in_service: Bool,
    draining: Bool,
    tier_grace: Bool,
    lease_grace: Bool,
    drain: Report,
    last_drain: Report,
    fenced: Bool,
    fence_age_seconds: Int,
    joined: Bool,
    bouncers_seen: Int,
    bouncers_configured: Int,
    bouncer_quorum: Bool,
    join_expiries: Int,
    session_move_failures: Int,
    sessions: Int,
    departing_sessions: Int,
    guilds: Int,
    presences: Int,
    buckets: Int,
    moves_in_flight: Int,
    lingering_sockets: Int,
    calls: Int,
    push_jobs: Int,
  )
}

pub fn snapshot() -> Status {
  let configured = settings.get()
  let held = drain.holdings()
  let #(ready_code, ready_reason) = health.peek_readiness()
  let seen = bouncer_client.bouncers_seen()
  let bouncers = case configured.bouncer {
    option.Some(bouncer) -> bouncer.count
    option.None -> 0
  }
  let in_service = router.in_service()
  let joined = links.joined()
  Status(
    role: node.role_name(configured.role),
    template: case configured.bouncer {
      option.Some(bouncer) -> bouncer.template_hash
      option.None -> ""
    },
    version: build_info.version(),
    revision: build_info.revision(),
    wire_epoch: build_info.wire_epoch,
    wire_epoch_floor: wire.floor(),
    epoch_refused_peers: list.length(wire.refused_now()),
    bridged_peers: list.length(wire.bridged_now()),
    ready: ready_code == 200,
    ready_reason:,
    in_service:,
    draining: draining.is_draining(),
    tier_grace: in_service && !joined && router.bouncer_mode(),
    lease_grace: lease.isolated(),
    drain: drain_report.current(drain.left()),
    last_drain: drain_report.last(),
    fenced: bouncer_client.fenced(),
    fence_age_seconds: fence_age_seconds(
      bouncer_client.last_fence_ms(),
      clock.monotonic_ms(),
    ),
    joined:,
    bouncers_seen: seen,
    bouncers_configured: bouncers,
    bouncer_quorum: bouncers > 0 && proto_lease.has_quorum(seen, bouncers),
    join_expiries: metrics.total(rollout_metrics.joins_expired, fn(_) { True }),
    session_move_failures: drain_report.failed_of("session"),
    sessions: held.owned_sessions,
    departing_sessions: int.max(0, held.sessions - held.owned_sessions),
    guilds: held.guilds,
    presences: held.presences,
    buckets: held.buckets,
    moves_in_flight: metrics.total(move.moves_in_flight, fn(_) { True }),
    lingering_sockets: held.sockets,
    calls: held.calls,
    push_jobs: held.push_jobs,
  )
}

pub fn fence_age_seconds(last_fence_ms: Result(Int, Nil), now_ms: Int) -> Int {
  case last_fence_ms {
    Ok(at) -> int.max(0, now_ms - at) / 1000
    Error(Nil) -> -1
  }
}

fn flag(value: Bool) -> Int {
  case value {
    True -> 1
    False -> 0
  }
}

fn report_fields(report: Report) -> List(#(String, Int)) {
  [
    #("state_code", drain_report.phase_code(report.phase)),
    #("failed_flag", flag(report.phase == drain_report.Failed)),
    #("started_at_ms", report.started_at_ms),
    #("finished_at_ms", report.finished_at_ms),
    #("moved", report.moved),
    #("failed", report.failed),
    #("left", report.left),
    #("buckets_left", report.buckets_left),
    #("fenced", flag(report.fenced)),
  ]
}

pub fn numbers(status: Status) -> List(#(String, Int)) {
  list.flatten([
    [
      #("wire_epoch", status.wire_epoch),
      #("wire_epoch_floor", status.wire_epoch_floor),
      #("epoch_mismatch", flag(status.epoch_refused_peers > 0)),
      #("epoch_refused_peers", status.epoch_refused_peers),
      #("bridged_peers", status.bridged_peers),
      #("ready", flag(status.ready)),
      #("in_service", flag(status.in_service)),
      #("draining", flag(status.draining)),
      #("tier_grace", flag(status.tier_grace)),
      #("lease_grace", flag(status.lease_grace)),
      #("fenced", flag(status.fenced)),
      #("fence_age_seconds", status.fence_age_seconds),
      #("joined", flag(status.joined)),
      #("bouncers_seen", status.bouncers_seen),
      #("bouncers_configured", status.bouncers_configured),
      #("bouncer_quorum", flag(status.bouncer_quorum)),
      #("join_expiries", status.join_expiries),
      #("session_move_failures", status.session_move_failures),
      #("sessions", status.sessions),
      #("guilds", status.guilds),
      #("presences", status.presences),
      #("buckets", status.buckets),
      #("moves_in_flight", status.moves_in_flight),
    ],
    prefixed("drain_", report_fields(status.drain)),
    prefixed("last_drain_", report_fields(status.last_drain)),
  ])
}

fn prefixed(
  prefix: String,
  fields: List(#(String, Int)),
) -> List(#(String, Int)) {
  list.map(fields, fn(field) { #(prefix <> field.0, field.1) })
}

fn report_json(report: Report) -> Json {
  json.object([
    #("state", json.string(drain_report.phase_name(report.phase))),
    #("reason", json.string(report.reason)),
    ..list.map(report_fields(report), fn(field) {
      #(field.0, json.int(field.1))
    })
  ])
}

pub fn to_json(status: Status) -> String {
  json.object([
    #("role", json.string(status.role)),
    #("template", json.string(status.template)),
    #("version", json.string(status.version)),
    #("revision", json.string(status.revision)),
    #("wire_epoch", json.int(status.wire_epoch)),
    #("wire_epoch_floor", json.int(status.wire_epoch_floor)),
    #("epoch_mismatch", json.int(flag(status.epoch_refused_peers > 0))),
    #("epoch_refused_peers", json.int(status.epoch_refused_peers)),
    #("bridged_peers", json.int(status.bridged_peers)),
    #("ready", json.int(flag(status.ready))),
    #("ready_reason", json.string(status.ready_reason)),
    #("in_service", json.int(flag(status.in_service))),
    #("draining", json.int(flag(status.draining))),
    #("tier_grace", json.int(flag(status.tier_grace))),
    #("lease_grace", json.int(flag(status.lease_grace))),
    #("drain", report_json(status.drain)),
    #("last_drain", report_json(status.last_drain)),
    #("fenced", json.int(flag(status.fenced))),
    #("fence_age_seconds", json.int(status.fence_age_seconds)),
    #("joined", json.int(flag(status.joined))),
    #(
      "bouncers",
      json.object([
        #("seen", json.int(status.bouncers_seen)),
        #("configured", json.int(status.bouncers_configured)),
        #("quorum", json.int(flag(status.bouncer_quorum))),
      ]),
    ),
    #("join_expiries", json.int(status.join_expiries)),
    #("session_move_failures", json.int(status.session_move_failures)),
    #("sessions", json.int(status.sessions)),
    #("guilds", json.int(status.guilds)),
    #("presences", json.int(status.presences)),
    #("buckets", json.int(status.buckets)),
    #("moves_in_flight", json.int(status.moves_in_flight)),
    #(
      "analysis",
      json.object(
        list.map(numbers(status), fn(field) { #(field.0, json.int(field.1)) }),
      ),
    ),
  ])
  |> json.to_string
}

pub fn ready_reason_label(reason: String) -> String {
  case reason {
    "OK" -> "ok"
    "DRAINING" -> "draining"
    "JOINING" -> "joining"
    "NATS_UNAVAILABLE" -> "nats_unavailable"
    _ ->
      case string.starts_with(reason, "EPOCH_MISMATCH") {
        True -> "epoch_mismatch"
        False -> "other"
      }
  }
}

fn status_family(key: String) -> rollout_metrics.Family {
  rollout_metrics.Family(
    name: "fluxer_gateway_status_" <> key,
    kind: rollout_metrics.Gauge,
    source: rollout_metrics.Computed,
    help: "Status field " <> key <> ", the same number as /_status analysis",
    labels: [],
    scope: rollout_metrics.Everywhere,
    seed: rollout_metrics.NoSeed,
  )
}

pub fn catalogue(status: Status) -> List(rollout_metrics.Family) {
  list.append(
    list.map(numbers(status), fn(field) { status_family(field.0) }),
    rollout_metrics.families(),
  )
}

fn sample(
  name: String,
  labels: List(#(String, String)),
  value: Int,
) -> rollout_metrics.Sample {
  rollout_metrics.Sample(name:, labels:, value:)
}

pub fn computed(
  status: Status,
  role: node.Role,
) -> List(rollout_metrics.Sample) {
  let held =
    lease.held()
    |> list.map(fn(claim) { rollout_metrics.kind_of_ns(claim.bucket.ns) })
  let held_of = fn(kind) { list.count(held, fn(found) { found == kind }) }
  let reason = ready_reason_label(status.ready_reason)
  let movable = rollout_metrics.role_kinds(role)
  let entity_kinds =
    [
      #(node.Sessions, "session", status.sessions + status.departing_sessions),
      #(node.Guilds, "guild", status.guilds),
      #(node.Presence, "presence", status.presences),
    ]
    |> list.filter(fn(entry) { node.runs(role, entry.0) })
  list.flatten([
    list.map(numbers(status), fn(field) {
      sample("fluxer_gateway_status_" <> field.0, [], field.1)
    }),
    list.map(rollout_metrics.ready_reasons, fn(found) {
      sample(
        rollout_metrics.ready_reason,
        [#("reason", found)],
        flag(found == reason),
      )
    }),
    list.map(movable, fn(kind) {
      sample(rollout_metrics.buckets_held, [#("kind", kind)], held_of(kind))
    }),
    list.map(entity_kinds, fn(entry) {
      sample(rollout_metrics.drain_left, [#("kind", entry.1)], entry.2)
    }),
    movable
      |> list.filter_map(fn(kind) {
        case kind {
          "call" -> Ok(#(kind, status.calls))
          "push" -> Ok(#(kind, status.push_jobs))
          _ -> Error(Nil)
        }
      })
      |> list.map(fn(entry) {
        sample(rollout_metrics.drain_left, [#("kind", entry.0)], entry.1)
      }),
    case handoff.holds_sockets() {
      True -> [
        sample(
          rollout_metrics.drain_left,
          [#("kind", "socket")],
          status.lingering_sockets,
        ),
      ]
      False -> []
    },
  ])
}

pub fn constant_labels() -> List(#(String, String)) {
  let configured = settings.get()
  let pool = case configured.bouncer {
    option.Some(bouncer) -> bouncer.pool
    option.None -> "default"
  }
  [#("role", node.role_name(configured.role)), #("pool", pool)]
}

pub const unscraped = [
  "fluxer_gateway_bouncer_beat_mismatch_total",
  "fluxer_gateway_cluster_nodedown_total",
  "fluxer_gateway_cluster_peer_state",
  "fluxer_gateway_handoff_freeze_ms",
  "fluxer_gateway_handoff_moves_total",
  "fluxer_gateway_handoff_precopy_ms",
  "fluxer_gateway_handoff_precopy_total",
  "fluxer_gateway_handoff_retries_total",
  "fluxer_gateway_wire_epoch_refused_peers_total",
]

pub fn metrics_text(status: Status) -> String {
  render_metrics(status, settings.get().role)
}

pub fn render_metrics(status: Status, role: node.Role) -> String {
  let families = catalogue(status)
  let taken =
    families
    |> list.map(fn(found) { found.name })
    |> list.append(unscraped)
    |> set.from_list
  rollout_metrics.render(
    families,
    role,
    constant_labels(),
    computed(status, role),
  )
  <> prometheus.render_without(taken)
}

pub type Route {
  StatusRoute
  MetricsRoute
  NotFound
}

pub fn route(path: String) -> Route {
  case path {
    "/_status" -> StatusRoute
    "/_metrics" -> MetricsRoute
    _ -> NotFound
  }
}

const scrape_header = "x-prometheus-scrape-timeout-seconds"

pub fn scrape(by_scraper: Bool, render: fn() -> String) -> String {
  let at = clock.system_ms()
  let text = render()
  case by_scraper {
    True -> rollout_metrics.mark_scraped(at)
    False -> Nil
  }
  text
}

fn scraper(incoming: Request(IpAddress)) -> Bool {
  result.is_ok(request.get_header(incoming, scrape_header))
}

pub fn handle(incoming: Request(IpAddress)) -> Response(bytes_tree.BytesTree) {
  let #(code, content_type, body) = case incoming.method, route(incoming.path) {
    http.Get, StatusRoute | http.Head, StatusRoute -> #(
      200,
      "application/json",
      to_json(snapshot()),
    )
    http.Get, MetricsRoute -> #(
      200,
      metrics_content_type,
      scrape(scraper(incoming), fn() { metrics_text(snapshot()) }),
    )
    http.Head, MetricsRoute -> #(200, metrics_content_type, "")
    _, NotFound -> #(404, "text/plain", "NOT_FOUND")
    _, _ -> #(405, "text/plain", "METHOD_NOT_ALLOWED")
  }
  let #(header, version) = build_info.version_header()
  response.new(code)
  |> response.set_header("content-type", content_type)
  |> response.set_header("cache-control", "no-store")
  |> response.set_header(header, version)
  |> response.set_body(bytes_tree.from_string(body))
}

pub fn supervised(settings: Settings) -> supervision.ChildSpecification(Nil) {
  listening(settings.status_port, listener_name())
}

pub fn listening(port: Int, name) -> supervision.ChildSpecification(Nil) {
  server.supervised(
    listen_interface,
    port,
    name,
    fn(incoming) { server.Respond(handle(incoming)) },
    log_listening,
  )
}

pub fn bound_port() -> Result(Int, Nil) {
  port_of(listener_name())
}

pub fn port_of(name) -> Result(Int, Nil) {
  use _ <- result.map(process.named(name))
  glisten.get_server_info(name, port_query_timeout_ms).port
}

fn listener_name() {
  env.process_name("fluxer_gateway_status_listener")
}

fn log_listening(port: Int) -> Nil {
  logging.log(
    logging.Info,
    "Gateway status listening on "
      <> listen_interface
      <> ":"
      <> int.to_string(port),
  )
}
