//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import envoy
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

pub type Config {
  Config(
    secret_key_base: String,
    api_endpoint: String,
    api_host: String,
    app_endpoint: String,
    cdn_endpoint: String,
    marketing_endpoint: String,
    port: Int,
    base_path: String,
    build_timestamp: String,
    release_channel: String,
    gateway_rpc_secret: String,
    metrics_endpoint: option.Option(String),
  )
}

fn normalize_base_path(base_path: String) -> String {
  let segments =
    base_path
    |> string.trim
    |> string.split("/")
    |> list.filter(fn(segment) { segment != "" })

  case segments {
    [] -> ""
    _ -> "/" <> string.join(segments, "/")
  }
}

fn normalize_endpoint(endpoint: String) -> String {
  let len = string.length(endpoint)
  case len > 0 && string.ends_with(endpoint, "/") {
    True -> normalize_endpoint(string.slice(endpoint, 0, len - 1))
    False -> endpoint
  }
}

fn required_env(key: String) -> Result(String, String) {
  case envoy.get(key) {
    Ok(value) ->
      case string.trim(value) {
        "" -> Error("Missing required env: " <> key)
        trimmed -> Ok(trimmed)
      }
    Error(_) -> Error("Missing required env: " <> key)
  }
}

fn optional_env(key: String) -> option.Option(String) {
  case envoy.get(key) {
    Ok(value) ->
      case string.trim(value) {
        "" -> option.None
        trimmed -> option.Some(trimmed)
      }
    Error(_) -> option.None
  }
}

fn required_int_env(key: String) -> Result(Int, String) {
  use raw <- result.try(required_env(key))
  case int.parse(raw) {
    Ok(n) -> Ok(n)
    Error(_) -> Error("Invalid integer for env " <> key <> ": " <> raw)
  }
}

pub fn load_config() -> Result(Config, String) {
  use secret_key_base <- result.try(required_env("SECRET_KEY_BASE"))
  use api_endpoint_raw <- result.try(required_env("FLUXER_API_PUBLIC_ENDPOINT"))
  use api_host <- result.try(required_env("FLUXER_API_HOST"))
  use app_endpoint_raw <- result.try(required_env("FLUXER_APP_ENDPOINT"))
  use cdn_endpoint_raw <- result.try(required_env("FLUXER_CDN_ENDPOINT"))
  use marketing_endpoint_raw <- result.try(required_env(
    "FLUXER_MARKETING_ENDPOINT",
  ))
  use base_path_raw <- result.try(required_env("FLUXER_PATH_MARKETING"))
  use port <- result.try(required_int_env("FLUXER_MARKETING_PORT"))
  use release_channel <- result.try(required_env("RELEASE_CHANNEL"))
  use gateway_rpc_secret <- result.try(required_env("GATEWAY_RPC_SECRET"))

  let api_endpoint = normalize_endpoint(api_endpoint_raw)
  let app_endpoint = normalize_endpoint(app_endpoint_raw)
  let cdn_endpoint = normalize_endpoint(cdn_endpoint_raw)
  let marketing_endpoint = normalize_endpoint(marketing_endpoint_raw)
  let base_path = normalize_base_path(base_path_raw)
  let metrics_endpoint = case optional_env("FLUXER_METRICS_HOST") {
    option.Some(host) -> option.Some("http://" <> host)
    option.None -> option.None
  }

  Ok(Config(
    secret_key_base: secret_key_base,
    api_endpoint: api_endpoint,
    api_host: api_host,
    app_endpoint: app_endpoint,
    cdn_endpoint: cdn_endpoint,
    marketing_endpoint: marketing_endpoint,
    port: port,
    base_path: base_path,
    build_timestamp: optional_env("BUILD_TIMESTAMP") |> option.unwrap(""),
    release_channel: release_channel,
    gateway_rpc_secret: gateway_rpc_secret,
    metrics_endpoint: metrics_endpoint,
  ))
}
