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

import fluxer_marketing/web.{type Context}
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option
import gleam/string
import wisp.{type Request}

pub fn track_request(ctx: Context, req: Request, status: Int, duration_ms: Int) {
  case ctx.metrics_endpoint {
    option.None -> Nil
    option.Some(endpoint) -> {
      let path = req.path
      let dims =
        json.object([
          #(
            "method",
            json.string(string.lowercase(http.method_to_string(req.method))),
          ),
          #("path", json.string(path)),
          #("status", json.int(status)),
          #("release_channel", json.string(ctx.release_channel)),
        ])

      let histogram_payload =
        json.object([
          #("name", json.string("marketing.request.latency")),
          #("dimensions", dims),
          #("value_ms", json.int(duration_ms)),
        ])

      fire_and_forget(endpoint, "/metrics/histogram", histogram_payload)

      let counter_payload =
        json.object([
          #("name", json.string("marketing.request.count")),
          #("dimensions", dims),
          #("value", json.int(1)),
        ])

      fire_and_forget(endpoint, "/metrics/counter", counter_payload)

      let category = case status >= 400 {
        True -> "failure"
        False -> "success"
      }

      let response_payload =
        json.object([
          #("name", json.string("marketing.request.outcome")),
          #(
            "dimensions",
            json.object([
              #(
                "method",
                json.string(string.lowercase(http.method_to_string(req.method))),
              ),
              #("path", json.string(path)),
              #("status", json.int(status)),
              #("outcome", json.string(category)),
            ]),
          ),
          #("value", json.int(1)),
        ])

      fire_and_forget(endpoint, "/metrics/counter", response_payload)
      Nil
    }
  }
}

fn fire_and_forget(endpoint: String, path: String, payload) {
  let body = json.to_string(payload)
  let url = endpoint <> path

  case request.to(url) {
    Ok(req) -> {
      let req =
        req
        |> request.set_method(http.Post)
        |> request.prepend_header("content-type", "application/json")
        |> request.set_body(body)
      let _ =
        process.spawn(fn() {
          case httpc.send(req) {
            Ok(resp) -> {
              let _ = resp.body
              Nil
            }
            Error(_) -> Nil
          }
        })
      Nil
    }
    Error(_) -> Nil
  }
}
