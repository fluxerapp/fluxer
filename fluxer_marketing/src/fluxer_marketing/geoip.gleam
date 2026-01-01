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

import gleam/bit_array
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import wisp

const default_cc = "US"

pub fn country_code(req: wisp.Request, geoip_host: String) -> String {
  let get_header = fn(name) { request.get_header(req, name) }
  country_code_core(get_header, geoip_host, fetch_country_code_http)
}

pub fn country_code_core(
  get_header: fn(String) -> Result(String, Nil),
  geoip_host: String,
  fetch_country: fn(String) -> Result(String, Nil),
) -> String {
  case geoip_host {
    "" -> default_cc
    _ -> {
      case extract_client_ip_from(get_header) {
        "" -> default_cc
        ip -> {
          let url =
            "http://" <> geoip_host <> "/lookup?ip=" <> percent_encode_ip(ip)
          case fetch_country(url) {
            Ok(body) -> {
              let cc = string.uppercase(string.trim(body))
              case is_valid_country_code(cc) {
                True -> cc
                False -> default_cc
              }
            }
            Error(_) -> default_cc
          }
        }
      }
    }
  }
}

fn fetch_country_code_http(url: String) -> Result(String, Nil) {
  let assert Ok(req) = request.to(url)
  let req = request.prepend_header(req, "accept", "text/plain")
  case httpc.send(req) {
    Ok(resp) if resp.status >= 200 && resp.status < 300 -> Ok(resp.body)
    _ -> Error(Nil)
  }
}

fn extract_client_ip(req: wisp.Request) -> String {
  extract_client_ip_from(fn(name) { request.get_header(req, name) })
}

pub fn extract_client_ip_from(
  get_header: fn(String) -> Result(String, Nil),
) -> String {
  case get_header("x-forwarded-for") {
    Ok(xff) -> {
      xff
      |> string.split(",")
      |> list.first
      |> result.unwrap("")
      |> string.trim
      |> strip_brackets
      |> validate_ip
    }
    Error(_) -> ""
  }
}

fn validate_ip(s: String) -> String {
  case string.contains(s, ".") || string.contains(s, ":") {
    True -> s
    False -> ""
  }
}

pub fn strip_brackets(ip: String) -> String {
  let len = string.length(ip)
  let has_brackets =
    len >= 2
    && string.first(ip) == Ok("[")
    && string.slice(ip, len - 1, 1) == "]"

  case has_brackets {
    True -> string.slice(ip, 1, len - 2)
    False -> ip
  }
}

pub fn percent_encode_ip(s: String) -> String {
  s
  |> string.replace("%", "%25")
  |> string.replace(":", "%3A")
  |> string.replace(" ", "%20")
}

pub fn is_ascii_upper_alpha2(s: String) -> Bool {
  case string.byte_size(s) == 2 {
    False -> False
    True ->
      case string.to_graphemes(s) {
        [a, b] -> is_uppercase_letter(a) && is_uppercase_letter(b)
        _ -> False
      }
  }
}

fn is_valid_country_code(s: String) -> Bool {
  is_ascii_upper_alpha2(s)
}

fn is_uppercase_letter(g: String) -> Bool {
  case bit_array.from_string(g) {
    <<c:8>> -> c >= 65 && c <= 90
    _ -> False
  }
}

pub fn debug_info(req: wisp.Request, geoip_host: String) -> String {
  let xff =
    request.get_header(req, "x-forwarded-for") |> result.unwrap("(not set)")
  let ip = extract_client_ip(req)
  let host_display = case geoip_host {
    "" -> "(not set)"
    h -> h
  }

  let url = case ip {
    "" -> "(empty IP - no URL)"
    _ -> "http://" <> host_display <> "/lookup?ip=" <> percent_encode_ip(ip)
  }

  let response = case ip {
    "" -> "(empty IP - no request)"
    _ -> fetch_geoip_debug(url)
  }

  json.object([
    #("x_forwarded_for_header", json.string(xff)),
    #("extracted_ip", json.string(ip)),
    #(
      "stripped_brackets",
      json.string(case ip {
        "" -> "(empty)"
        i -> strip_brackets(i)
      }),
    ),
    #(
      "percent_encoded",
      json.string(case ip {
        "" -> "(empty)"
        i -> percent_encode_ip(i)
      }),
    ),
    #("geoip_host", json.string(host_display)),
    #("geoip_url", json.string(url)),
    #("geoip_response", json.string(response)),
    #("final_country_code", json.string(country_code(req, geoip_host))),
  ])
  |> json.to_string
}

fn fetch_geoip_debug(url: String) -> String {
  let assert Ok(req) = request.to(url)
  let req = request.prepend_header(req, "accept", "text/plain")
  case httpc.send(req) {
    Ok(resp) ->
      "Status: " <> string.inspect(resp.status) <> ", Body: " <> resp.body
    Error(_) -> "(request failed)"
  }
}
