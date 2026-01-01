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
    media_endpoint: String,
    cdn_endpoint: String,
    admin_endpoint: String,
    web_app_endpoint: String,
    metrics_endpoint: option.Option(String),
    oauth_client_id: String,
    oauth_client_secret: String,
    oauth_redirect_uri: String,
    port: Int,
    base_path: String,
    build_timestamp: String,
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

fn build_redirect_uri(
  endpoint: String,
  base_path: String,
  override: option.Option(String),
) -> String {
  case override {
    option.Some(uri) -> uri
    option.None ->
      endpoint <> normalize_base_path(base_path) <> "/oauth2_callback"
  }
}

pub fn load_config() -> Result(Config, String) {
  use api_endpoint_raw <- result.try(required_env("FLUXER_API_PUBLIC_ENDPOINT"))
  use media_endpoint_raw <- result.try(required_env("FLUXER_MEDIA_ENDPOINT"))
  use cdn_endpoint_raw <- result.try(required_env("FLUXER_CDN_ENDPOINT"))
  use admin_endpoint_raw <- result.try(required_env("FLUXER_ADMIN_ENDPOINT"))
  use web_app_endpoint_raw <- result.try(required_env("FLUXER_APP_ENDPOINT"))
  use secret_key_base <- result.try(required_env("SECRET_KEY_BASE"))
  use client_id <- result.try(required_env("ADMIN_OAUTH2_CLIENT_ID"))
  use client_secret <- result.try(required_env("ADMIN_OAUTH2_CLIENT_SECRET"))
  use base_path_raw <- result.try(required_env("FLUXER_PATH_ADMIN"))
  use port <- result.try(required_int_env("FLUXER_ADMIN_PORT"))

  let api_endpoint = normalize_endpoint(api_endpoint_raw)
  let media_endpoint = normalize_endpoint(media_endpoint_raw)
  let cdn_endpoint = normalize_endpoint(cdn_endpoint_raw)
  let admin_endpoint = normalize_endpoint(admin_endpoint_raw)
  let web_app_endpoint = normalize_endpoint(web_app_endpoint_raw)
  let base_path = normalize_base_path(base_path_raw)
  let redirect_uri =
    build_redirect_uri(
      admin_endpoint,
      base_path,
      optional_env("ADMIN_OAUTH2_REDIRECT_URI"),
    )

  let metrics_endpoint = case optional_env("FLUXER_METRICS_HOST") {
    option.Some(host) -> option.Some("http://" <> host)
    option.None -> option.None
  }

  Ok(Config(
    secret_key_base: secret_key_base,
    api_endpoint: api_endpoint,
    media_endpoint: media_endpoint,
    cdn_endpoint: cdn_endpoint,
    admin_endpoint: admin_endpoint,
    web_app_endpoint: web_app_endpoint,
    metrics_endpoint: metrics_endpoint,
    oauth_client_id: client_id,
    oauth_client_secret: client_secret,
    oauth_redirect_uri: redirect_uri,
    port: port,
    base_path: base_path,
    build_timestamp: envoy.get("BUILD_TIMESTAMP") |> result.unwrap(""),
  ))
}
