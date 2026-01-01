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

import fluxer_admin/config
import fluxer_admin/middleware/cache_middleware
import fluxer_admin/router
import fluxer_admin/web.{type Context, Context, normalize_base_path}
import gleam/erlang/process
import mist
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  let assert Ok(cfg) = config.load_config()

  let base_path = normalize_base_path(cfg.base_path)

  let ctx =
    Context(
      api_endpoint: cfg.api_endpoint,
      oauth_client_id: cfg.oauth_client_id,
      oauth_client_secret: cfg.oauth_client_secret,
      oauth_redirect_uri: cfg.oauth_redirect_uri,
      secret_key_base: cfg.secret_key_base,
      static_directory: "priv/static",
      media_endpoint: cfg.media_endpoint,
      cdn_endpoint: cfg.cdn_endpoint,
      asset_version: cfg.build_timestamp,
      base_path: base_path,
      app_endpoint: cfg.admin_endpoint,
      web_app_endpoint: cfg.web_app_endpoint,
      metrics_endpoint: cfg.metrics_endpoint,
    )

  let assert Ok(_) =
    wisp_mist.handler(handle_request(_, ctx), cfg.secret_key_base)
    |> mist.new
    |> mist.bind("0.0.0.0")
    |> mist.port(cfg.port)
    |> mist.start

  process.sleep_forever()
}

fn handle_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  let static_dir = ctx.static_directory

  case wisp.path_segments(req) {
    ["static", ..] -> {
      use <- wisp.serve_static(req, under: "/static", from: static_dir)
      router.handle_request(req, ctx)
    }
    _ -> router.handle_request(req, ctx)
  }
  |> cache_middleware.add_cache_headers
}
