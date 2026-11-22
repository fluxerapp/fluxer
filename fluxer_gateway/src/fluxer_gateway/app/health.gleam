// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/app/drain_report
import fluxer_gateway/cluster/router as cluster_router
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/build_info
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/handoff/service as handoff_service
import fluxer_gateway/rpc/server as rpc_server
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bytes_tree.{type BytesTree}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import glisten.{type IpAddress}

pub type Mode {
  Liveness
  Readiness
  Drain
  Drained
  Undrain
}

pub fn handle(request: Request(IpAddress)) -> Response(BytesTree) {
  let #(status, body) = case mode(request.path) {
    Liveness -> #(200, "OK")
    Readiness -> loopback_only(request, readiness)
    Drain -> loopback_only(request, activate_drain)
    Drained -> loopback_only(request, drained)
    Undrain -> loopback_only(request, deactivate_drain)
  }
  let #(header, version) = build_info.version_header()
  response.new(status)
  |> response.set_header("content-type", "text/plain")
  |> response.set_header(header, version)
  |> response.set_body(bytes_tree.from_string(body))
}

pub fn mode(path: String) -> Mode {
  case path {
    "/_health/ready" -> Readiness
    "/_health/drain" -> Drain
    "/_health/drained" -> Drained
    "/_health/undrain" -> Undrain
    _ -> Liveness
  }
}

pub fn readiness_status(
  ready: Bool,
  joined: Bool,
  wire: wire.Assessment,
  nats_ready: Bool,
) -> #(Int, String) {
  case ready, wire, joined, nats_ready {
    False, _, _, _ -> #(503, "DRAINING")
    True, wire.Mismatch(local:, peer:), _, _ -> #(
      503,
      "EPOCH_MISMATCH local="
        <> int.to_string(local)
        <> " peer="
        <> int.to_string(peer),
    )
    True, wire.Pending, _, _ | True, wire.Accepted, False, _ -> #(
      503,
      "JOINING",
    )
    True, wire.Accepted, True, False -> #(503, "NATS_UNAVAILABLE")
    True, wire.Accepted, True, True -> #(200, "OK")
  }
}

pub fn is_loopback(address: IpAddress) -> Bool {
  case address {
    glisten.IpV4(127, 0, 0, 1) -> True
    glisten.IpV6(0, 0, 0, 0, 0, 0, 0, 1) -> True
    _ -> False
  }
}

fn loopback_only(
  request: Request(IpAddress),
  respond: fn() -> #(Int, String),
) -> #(Int, String) {
  case is_loopback(request.body) {
    True -> respond()
    False -> #(403, "FORBIDDEN")
  }
}

pub fn readiness() -> #(Int, String) {
  let found = current_readiness()
  rollout_metrics.ready_seen(found.0 == 200)
  found
}

fn current_readiness() -> #(Int, String) {
  readiness_status(
    draining.is_ready(),
    cluster_router.in_service(),
    case cluster_router.bouncer_mode() {
      True -> handoff_service.wire_readiness()
      False -> wire.Accepted
    },
    rpc_server.nats_ready(),
  )
}

pub fn peek_readiness() -> #(Int, String) {
  readiness_status(
    draining.is_ready(),
    cluster_router.in_service(),
    case cluster_router.bouncer_mode() {
      True -> handoff_service.peek_wire_readiness()
      False -> wire.Accepted
    },
    rpc_server.nats_ready(),
  )
}

fn activate_drain() -> #(Int, String) {
  drain.start()
  #(200, "DRAINING")
}

fn drained() -> #(Int, String) {
  case drain.outcome() {
    Ok(report) ->
      drained_reply(
        report,
        settings.get().status_port > 0,
        rollout_metrics.last_scrape_ms(),
        clock.system_ms(),
      )
    Error(Nil) -> #(404, "NOT_FOUND")
  }
}

pub const scrape_hold_ms = 20_000

pub fn drained_reply(
  report: drain_report.Report,
  scraped: Bool,
  last_scrape_ms: Int,
  now_ms: Int,
) -> #(Int, String) {
  let seen =
    !scraped
    || last_scrape_ms > report.finished_at_ms
    || now_ms - report.finished_at_ms >= scrape_hold_ms
  case report.phase, seen {
    drain_report.Drained, False | drain_report.Failed, False -> #(
      503,
      "DRAINING",
    )
    phase, _ -> drained_status(phase)
  }
}

pub fn drained_status(phase: drain_report.Phase) -> #(Int, String) {
  case phase {
    drain_report.Drained -> #(200, "DRAINED")
    drain_report.Failed -> #(503, "DRAIN_FAILED")
    drain_report.Draining -> #(503, "DRAINING")
    drain_report.Idle -> #(503, "NOT_DRAINING")
  }
}

fn deactivate_drain() -> #(Int, String) {
  case drain.undrain() {
    Ok(Nil) -> #(200, "READY")
    Error(drain.HandoffInFlight) -> #(409, "HANDOFF_IN_FLIGHT")
    Error(drain.UndrainUnavailable) -> #(409, "UNAVAILABLE")
  }
}
