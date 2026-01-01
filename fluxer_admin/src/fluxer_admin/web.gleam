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

import gleam/list
import gleam/option.{type Option}
import gleam/string
import lustre/attribute
import wisp

pub type Context {
  Context(
    api_endpoint: String,
    oauth_client_id: String,
    oauth_client_secret: String,
    oauth_redirect_uri: String,
    secret_key_base: String,
    static_directory: String,
    media_endpoint: String,
    cdn_endpoint: String,
    asset_version: String,
    base_path: String,
    app_endpoint: String,
    web_app_endpoint: String,
    metrics_endpoint: Option(String),
  )
}

pub type Session {
  Session(user_id: String, access_token: String, created_at: Int)
}

pub fn cache_busted_asset(ctx: Context, path: String) -> String {
  prepend_base_path(ctx, cache_busted_with_version(path, ctx.asset_version))
}

pub fn cache_busted_with_version(path: String, version: String) -> String {
  let separator = case string.contains(path, "?") {
    True -> "&"
    False -> "?"
  }

  path <> separator <> "t=" <> version
}

fn normalize_path(path: String) -> String {
  let segments =
    path
    |> string.trim
    |> string.split("/")
    |> list.filter(fn(segment) { segment != "" })

  case segments {
    [] -> "/"
    _ -> "/" <> string.join(segments, "/")
  }
}

pub fn normalize_base_path(base_path: String) -> String {
  case normalize_path(base_path) {
    "/" -> ""
    path -> path
  }
}

pub fn normalize_endpoint(endpoint: String) -> String {
  let len = string.length(endpoint)
  case len > 0 && string.ends_with(endpoint, "/") {
    True -> normalize_endpoint(string.slice(endpoint, 0, len - 1))
    False -> endpoint
  }
}

pub fn prepend_base_path(ctx: Context, path: String) -> String {
  let base_path = normalize_base_path(ctx.base_path)
  let normalized_path = normalize_path(path)

  let full_path = case base_path {
    "" -> normalized_path
    base -> base <> normalized_path
  }

  normalize_endpoint(ctx.app_endpoint) <> full_path
}

pub fn href(ctx: Context, path: String) -> attribute.Attribute(a) {
  attribute.href(prepend_base_path(ctx, path))
}

pub fn action(ctx: Context, path: String) -> attribute.Attribute(a) {
  attribute.action(prepend_base_path(ctx, path))
}

pub fn redirect(ctx: Context, path: String) -> wisp.Response {
  wisp.redirect(prepend_base_path(ctx, path))
}
