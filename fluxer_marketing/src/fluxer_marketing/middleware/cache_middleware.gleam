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

import gleam/http/response.{type Response}
import gleam/list
import gleam/result
import gleam/string
import wisp

pub fn add_cache_headers(res: Response(wisp.Body)) -> Response(wisp.Body) {
  let content_type =
    list.key_find(res.headers, "content-type")
    |> result.unwrap("")

  let cache_header = case should_cache(content_type) {
    True -> "public, max-age=31536000, immutable"
    False -> "no-cache"
  }

  response.set_header(res, "cache-control", cache_header)
}

fn should_cache(content_type: String) -> Bool {
  let cacheable_types = [
    "text/css", "application/javascript", "application/json", "font/", "image/",
    "video/", "audio/", "application/font-woff2",
  ]

  list.any(cacheable_types, fn(type_prefix) {
    string.starts_with(content_type, type_prefix)
  })
}
