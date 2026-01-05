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

import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import wisp

pub type Settings {
  Settings(api_host: String, rpc_secret: String)
}

const default_cc = "US"

const log_prefix = "[geoip]"

pub fn country_code(req: wisp.Request, settings: Settings) -> String {
  case extract_client_ip(req) {
    "" -> default_cc
    ip ->
      case fetch_country_code(settings, ip) {
        Ok(code) -> code
        Error(_) -> default_cc
      }
  }
}

fn fetch_country_code(settings: Settings, ip: String) -> Result(String, Nil) {
  case rpc_url(settings.api_host) {
    "" -> {
      log_missing_api_host(settings.api_host)
      Error(Nil)
    }
    url -> {
      let body =
        json.object([
          #("type", json.string("geoip_lookup")),
          #("ip", json.string(ip)),
        ])
        |> json.to_string

      let assert Ok(req) = request.to(url)
      let req =
        req
        |> request.set_method(http.Post)
        |> request.prepend_header("content-type", "application/json")
        |> request.prepend_header(
          "Authorization",
          "Bearer " <> settings.rpc_secret,
        )
        |> request.set_body(body)

      case httpc.send(req) {
        Ok(resp) if resp.status >= 200 && resp.status < 300 ->
          decode_country_code(resp.body)
        Ok(resp) -> {
          log_rpc_status(settings.api_host, resp.status, resp.body)
          Error(Nil)
        }
        Error(error) -> {
          log_rpc_error(settings.api_host, string.inspect(error))
          Error(Nil)
        }
      }
    }
  }
}

fn decode_country_code(body: String) -> Result(String, Nil) {
  let response_decoder = {
    use data <- decode.field("data", {
      use code <- decode.field("country_code", decode.string)
      decode.success(code)
    })
    decode.success(data)
  }

  case json.parse(from: body, using: response_decoder) {
    Ok(code) -> Ok(string.uppercase(string.trim(code)))
    Error(_) -> Error(Nil)
  }
}

fn extract_client_ip(req: wisp.Request) -> String {
  case request.get_header(req, "x-forwarded-for") {
    Ok(xff) ->
      xff
      |> string.split(",")
      |> list.first
      |> result.unwrap("")
      |> string.trim
      |> strip_brackets
    Error(_) -> ""
  }
}

pub fn strip_brackets(ip: String) -> String {
  let len = string.length(ip)
  case
    len >= 2
    && string.first(ip) == Ok("[")
    && string.slice(ip, len - 1, 1) == "]"
  {
    True -> string.slice(ip, 1, len - 2)
    False -> ip
  }
}

fn log_missing_api_host(host: String) -> Nil {
  wisp.log_warning(
    string.concat([log_prefix, " missing api_host (", host, ")"]),
  )
}

fn log_rpc_status(api_host: String, status: Int, body: String) -> Nil {
  wisp.log_warning(
    string.concat([
      log_prefix,
      " rpc returned status ",
      int.to_string(status),
      " from ",
      host_display(api_host),
      ": ",
      response_snippet(body),
    ]),
  )
}

fn log_rpc_error(api_host: String, message: String) -> Nil {
  wisp.log_warning(
    string.concat([
      log_prefix,
      " rpc request to ",
      host_display(api_host),
      " failed: ",
      message,
    ]),
  )
}

fn host_display(api_host: String) -> String {
  case string.contains(api_host, "://") {
    True -> api_host
    False -> "http://" <> api_host
  }
}

fn rpc_url(api_host: String) -> String {
  let host = string.trim(api_host)
  case host {
    "" -> ""
    _ -> {
      let base = case string.contains(host, "://") {
        True -> host
        False -> "http://" <> host
      }

      let normalized = case string.ends_with(base, "/") {
        True -> string.slice(base, 0, string.length(base) - 1)
        False -> base
      }

      normalized <> "/_rpc"
    }
  }
}

fn response_snippet(body: String) -> String {
  let len = string.length(body)
  case len <= 256 {
    True -> body
    False -> string.slice(body, 0, 256) <> "..."
  }
}
