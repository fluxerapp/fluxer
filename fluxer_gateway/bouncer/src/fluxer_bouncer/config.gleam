// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/lease
import bouncer_proto/transport
import envoy
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Timings {
  Timings(
    beat_interval_ms: Int,
    lease_ttl_ms: Int,
    skew_margin_ms: Int,
    regrant_silence_ms: Int,
    isolation_cap_ms: Int,
    claim_window_ms: Int,
    request_timeout_ms: Int,
    freeze_bound_ms: Int,
    tick_ms: Int,
    blob_ttl_ms: Int,
    leave_bound_ms: Int,
  )
}

pub type Budgets {
  Budgets(blob_bytes: Int, entity_buffer_bytes: Int, buffer_bytes: Int)
}

pub type Config {
  Config(
    ordinal: Int,
    count: Int,
    basename: String,
    dns_name: String,
    quorum: Int,
    status_port: Int,
    timings: Timings,
    budgets: Budgets,
  )
}

pub type ConfigError {
  Missing(name: String)
  Invalid(name: String, value: String)
  OrdinalOutOfRange(ordinal: Int, count: Int)
}

pub const default_status_port = 8082

pub fn default_timings() -> Timings {
  Timings(
    beat_interval_ms: 1000,
    lease_ttl_ms: 6000,
    skew_margin_ms: 1000,
    regrant_silence_ms: 8000,
    isolation_cap_ms: 120_000,
    claim_window_ms: 3000,
    request_timeout_ms: 500,
    freeze_bound_ms: 2000,
    tick_ms: 100,
    blob_ttl_ms: 60_000,
    leave_bound_ms: 5000,
  )
}

pub fn default_budgets() -> Budgets {
  Budgets(
    blob_bytes: 1_073_741_824,
    entity_buffer_bytes: 8_388_608,
    buffer_bytes: 268_435_456,
  )
}

pub fn ordinal_of(hostname: String) -> Result(Int, ConfigError) {
  hostname
  |> string.split("-")
  |> list.last
  |> result.try(int.parse)
  |> result.replace_error(Invalid("HOSTNAME", hostname))
  |> result.try(fn(ordinal) {
    case ordinal >= 0 {
      True -> Ok(ordinal)
      False -> Error(Invalid("HOSTNAME", hostname))
    }
  })
}

fn required(name: String) -> Result(String, ConfigError) {
  envoy.get(name) |> result.replace_error(Missing(name))
}

fn count_of(text: String) -> Result(Int, ConfigError) {
  case int.parse(text) {
    Ok(count) if count >= 1 -> Ok(count)
    _ -> Error(Invalid("FLUXER_BOUNCER_COUNT", text))
  }
}

pub fn build(
  hostname hostname: String,
  count count: String,
  basename basename: String,
  dns_name dns_name: String,
) -> Result(Config, ConfigError) {
  use ordinal <- result.try(ordinal_of(hostname))
  use count <- result.try(count_of(count))
  case ordinal < count {
    False -> Error(OrdinalOutOfRange(ordinal: ordinal, count: count))
    True ->
      Ok(Config(
        ordinal: ordinal,
        count: count,
        basename: basename,
        dns_name: dns_name,
        quorum: lease.quorum(count),
        status_port: default_status_port,
        timings: default_timings(),
        budgets: default_budgets(),
      ))
  }
}

pub fn from_env() -> Result(Config, ConfigError) {
  use hostname <- result.try(required("HOSTNAME"))
  use count <- result.try(required("FLUXER_BOUNCER_COUNT"))
  use basename <- result.try(required("FLUXER_BOUNCER_BASENAME"))
  use dns_name <- result.try(required("FLUXER_BOUNCER_DNS_NAME"))
  use config <- result.try(build(
    hostname: hostname,
    count: count,
    basename: basename,
    dns_name: dns_name,
  ))
  use config <- result.try(case envoy.get("FLUXER_BOUNCER_QUORUM") {
    Ok(text) -> with_quorum(config, text)
    Error(Nil) -> Ok(config)
  })
  use config <- result.try(case envoy.get("FLUXER_BOUNCER_STATUS_PORT") {
    Ok(text) -> with_status_port(config, text)
    Error(Nil) -> Ok(config)
  })
  case envoy.get("FLUXER_BOUNCER_LEAVE_BOUND_MS") {
    Ok(text) -> with_leave_bound(config, text)
    Error(Nil) -> Ok(config)
  }
}

pub fn with_leave_bound(
  config: Config,
  text: String,
) -> Result(Config, ConfigError) {
  case int.parse(string.trim(text)) {
    Ok(bound) if bound >= 0 ->
      Ok(
        Config(
          ..config,
          timings: Timings(..config.timings, leave_bound_ms: bound),
        ),
      )
    _ -> Error(Invalid("FLUXER_BOUNCER_LEAVE_BOUND_MS", text))
  }
}

pub fn with_status_port(
  config: Config,
  text: String,
) -> Result(Config, ConfigError) {
  case int.parse(string.trim(text)) {
    Ok(port) if port >= 0 && port <= 65_535 ->
      Ok(Config(..config, status_port: port))
    _ -> Error(Invalid("FLUXER_BOUNCER_STATUS_PORT", text))
  }
}

pub fn with_quorum(
  config: Config,
  text: String,
) -> Result(Config, ConfigError) {
  case int.parse(string.trim(text)) {
    Ok(quorum) if quorum >= 1 && quorum <= config.count ->
      Ok(Config(..config, quorum: quorum))
    _ -> Error(Invalid("FLUXER_BOUNCER_QUORUM", text))
  }
}

pub fn peer_node(config: Config, ordinal: Int) -> Atom {
  transport.bouncer_node(
    basename: config.basename,
    dns_name: config.dns_name,
    ordinal: ordinal,
  )
}
