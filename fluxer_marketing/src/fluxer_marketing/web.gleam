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

import fluxer_marketing/locale.{type Locale}
import fluxer_marketing/visionary_slots.{type VisionarySlots}
import gleam/option.{type Option}
import gleam/string
import kielet/database.{type Database}
import lustre/attribute

pub type Platform {
  Windows
  MacOS
  Linux
  IOS
  Android
  Unknown
}

pub type Architecture {
  X64
  ARM64
  ArchUnknown
}

pub type Context {
  Context(
    locale: Locale,
    i18n_db: Database,
    static_directory: String,
    base_url: String,
    country_code: String,
    api_endpoint: String,
    app_endpoint: String,
    cdn_endpoint: String,
    asset_version: String,
    base_path: String,
    platform: Platform,
    architecture: Architecture,
    geoip_host: String,
    release_channel: String,
    visionary_slots: VisionarySlots,
    metrics_endpoint: Option(String),
  )
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

pub fn prepend_base_path(ctx: Context, path: String) -> String {
  case ctx.base_path {
    "" -> path
    base_path -> base_path <> path
  }
}

pub fn href(ctx: Context, path: String) -> attribute.Attribute(a) {
  attribute.href(prepend_base_path(ctx, path))
}

pub fn api_url(ctx: Context, path: String) -> String {
  ctx.api_endpoint <> path
}

pub fn detect_platform(user_agent: String) -> Platform {
  let ua = string.lowercase(user_agent)

  case string.contains(ua, "iphone") || string.contains(ua, "ipad") {
    True -> IOS
    False ->
      case string.contains(ua, "android") {
        True -> Android
        False ->
          case string.contains(ua, "windows") {
            True -> Windows
            False ->
              case
                string.contains(ua, "macintosh")
                || string.contains(ua, "mac os x")
              {
                True -> MacOS
                False ->
                  case
                    string.contains(ua, "linux")
                    && !string.contains(ua, "android")
                  {
                    True -> Linux
                    False -> Unknown
                  }
              }
          }
      }
  }
}

pub fn detect_architecture(
  user_agent: String,
  platform: Platform,
) -> Architecture {
  let ua = string.lowercase(user_agent)

  case platform {
    Windows -> {
      case string.contains(ua, "arm64") || string.contains(ua, "aarch64") {
        True -> ARM64
        False -> X64
      }
    }
    Linux -> {
      case
        string.contains(ua, "arm64")
        || string.contains(ua, "aarch64")
        || string.contains(ua, "armv8")
      {
        True -> ARM64
        False -> X64
      }
    }
    MacOS -> {
      case
        string.contains(ua, "arm64")
        || string.contains(ua, "aarch64")
        || string.contains(ua, "apple silicon")
        || string.contains(ua, "apple m1")
        || string.contains(ua, "apple m2")
        || string.contains(ua, "apple m3")
      {
        True -> ARM64
        False -> {
          case string.contains(ua, "x86_64") {
            True -> X64
            False -> ARM64
          }
        }
      }
    }
    Android -> ARM64
    _ -> ArchUnknown
  }
}

pub fn is_canary(ctx: Context) -> Bool {
  ctx.release_channel == "canary"
}
