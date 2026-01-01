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

import fluxer_marketing/config
import fluxer_marketing/geoip
import fluxer_marketing/i18n
import fluxer_marketing/locale.{type Locale, get_locale_from_code}
import fluxer_marketing/metrics
import fluxer_marketing/middleware/cache_middleware
import fluxer_marketing/router
import fluxer_marketing/visionary_slots
import fluxer_marketing/web
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/http/request
import gleam/list
import gleam/result
import gleam/string
import mist
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  let assert Ok(cfg) = config.load_config()

  let i18n_db = i18n.setup_database()
  let slots_cache =
    visionary_slots.start(visionary_slots.Settings(
      api_host: cfg.api_host,
      rpc_secret: cfg.gateway_rpc_secret,
    ))

  let assert Ok(_) =
    wisp_mist.handler(
      handle_request(_, i18n_db, cfg, slots_cache),
      cfg.secret_key_base,
    )
    |> mist.new
    |> mist.bind("0.0.0.0")
    |> mist.port(cfg.port)
    |> mist.start

  process.sleep_forever()
}

fn handle_request(
  req: wisp.Request,
  i18n_db,
  cfg: config.Config,
  slots_cache: visionary_slots.Cache,
) -> wisp.Response {
  let locale = get_request_locale(req)

  let base_url = cfg.marketing_endpoint <> cfg.base_path

  let country_code = geoip.country_code(req, cfg.geoip_host)

  let user_agent = case request.get_header(req, "user-agent") {
    Ok(ua) -> ua
    Error(_) -> ""
  }

  let platform = web.detect_platform(user_agent)
  let architecture = web.detect_architecture(user_agent, platform)

  let ctx =
    web.Context(
      locale: locale,
      i18n_db: i18n_db,
      static_directory: "priv/static",
      base_url: base_url,
      country_code: country_code,
      api_endpoint: cfg.api_endpoint,
      app_endpoint: cfg.app_endpoint,
      cdn_endpoint: cfg.cdn_endpoint,
      asset_version: cfg.build_timestamp,
      base_path: cfg.base_path,
      platform: platform,
      architecture: architecture,
      geoip_host: cfg.geoip_host,
      release_channel: cfg.release_channel,
      visionary_slots: visionary_slots.current(slots_cache),
      metrics_endpoint: cfg.metrics_endpoint,
    )

  use <- wisp.log_request(req)

  let start = monotonic_milliseconds()

  let response = case wisp.path_segments(req) {
    ["static", ..] -> {
      use <- wisp.serve_static(
        req,
        under: "/static",
        from: ctx.static_directory,
      )
      router.handle_request(req, ctx)
    }
    _ -> router.handle_request(req, ctx)
  }

  let duration = monotonic_milliseconds() - start

  metrics.track_request(ctx, req, response.status, duration)

  response |> cache_middleware.add_cache_headers
}

fn monotonic_milliseconds() -> Int {
  do_monotonic_time(atom.create("millisecond"))
}

@external(erlang, "erlang", "monotonic_time")
fn do_monotonic_time(unit: Atom) -> Int

fn get_request_locale(req: wisp.Request) -> Locale {
  case wisp.get_cookie(req, "locale", wisp.PlainText) {
    Ok(locale_code) ->
      get_locale_from_code(locale_code) |> result.unwrap(locale.EnUS)
    Error(_) -> detect_browser_locale(req)
  }
}

fn detect_browser_locale(req: wisp.Request) -> Locale {
  case request.get_header(req, "accept-language") {
    Ok(header) -> parse_accept_language(header)
    Error(_) -> locale.EnUS
  }
}

fn parse_accept_language(header: String) -> Locale {
  header
  |> string.split(",")
  |> list.map(string.trim)
  |> list.map(fn(lang) {
    case string.split(lang, ";") {
      [code, ..] -> string.trim(code) |> string.lowercase
      _ -> ""
    }
  })
  |> list.filter(fn(code) { code != "" })
  |> list.find_map(fn(code) {
    let clean_code = case string.split(code, "-") {
      [lang, region] -> lang <> "-" <> string.uppercase(region)
      [lang] -> lang
      _ -> code
    }
    get_locale_from_code(clean_code)
  })
  |> result.unwrap(locale.EnUS)
}
