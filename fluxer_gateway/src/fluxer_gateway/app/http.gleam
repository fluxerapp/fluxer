// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/health
import fluxer_gateway/config/build_info
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/gateway/socket
import fluxer_gateway/gateway/ws/server
import fluxer_gateway/telemetry/prometheus
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/list
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import gleam/uri
import glisten.{type IpAddress}
import logging

const port_query_timeout_ms = 5000

pub type Route {
  HealthRoute
  MetricsRoute
  SocketRoute
  NotFound
}

const metrics_content_type = "text/plain; version=0.0.4; charset=utf-8"

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  listening(settings, listener_name())
}

pub fn listening(settings: Settings, listener_name) -> ChildSpecification(Nil) {
  server.supervised(
    settings.bind_host,
    settings.port,
    listener_name,
    fn(incoming) { handle(incoming, settings) },
    log_listening(settings.bind_host, _),
  )
}

pub fn bound_port() -> Result(Int, Nil) {
  use _ <- result.map(process.named(listener_name()))
  glisten.get_server_info(listener_name(), port_query_timeout_ms).port
}

fn listener_name() {
  env.process_name("fluxer_gateway_http_listener")
}

fn canonical_path(path: String) -> String {
  case string.split(path, "/") {
    ["", ..segments] -> "/" <> string.join(resolve(segments, []), "/")
    _ -> path
  }
}

fn resolve(segments: List(String), kept: List(String)) -> List(String) {
  case segments {
    [] | [""] -> list.reverse(kept)
    [segment, ..rest] ->
      case uri.percent_decode(segment) |> result.unwrap(segment), kept {
        ".", _ -> resolve(rest, kept)
        "..", [] -> resolve(rest, kept)
        "..", [_, ..parent] -> resolve(rest, parent)
        decoded, _ -> resolve(rest, [decoded, ..kept])
      }
  }
}

pub fn route(path: String) -> Route {
  case path {
    "/_health"
    | "/_health/ready"
    | "/_health/drain"
    | "/_health/drained"
    | "/_health/undrain" -> HealthRoute
    "/_metrics" -> MetricsRoute
    "/" -> SocketRoute
    _ -> NotFound
  }
}

fn handle(incoming: Request(IpAddress), settings: Settings) -> server.Route {
  let incoming = request.set_path(incoming, canonical_path(incoming.path))
  case route(incoming.path) {
    HealthRoute -> server.Respond(health.handle(incoming))
    MetricsRoute -> server.Respond(metrics(incoming))
    SocketRoute -> server.Websocket(socket.upgrade(incoming, settings))
    NotFound ->
      server.Respond(
        response.new(404)
        |> versioned
        |> response.set_body(bytes_tree.new()),
      )
  }
}

fn versioned(found: Response(body)) -> Response(body) {
  let #(header, version) = build_info.version_header()
  response.set_header(found, header, version)
}

fn metrics(incoming: Request(IpAddress)) -> Response(BytesTree) {
  let #(status, content_type, body) = case health.is_loopback(incoming.body) {
    True -> #(200, metrics_content_type, prometheus.render())
    False -> #(403, "text/plain", "FORBIDDEN")
  }
  response.new(status)
  |> response.set_header("content-type", content_type)
  |> versioned
  |> response.set_body(bytes_tree.from_string(body))
}

fn log_listening(host: String, port: Int) -> Nil {
  logging.log(
    logging.Info,
    "Gateway listening on " <> host <> ":" <> int.to_string(port),
  )
}
