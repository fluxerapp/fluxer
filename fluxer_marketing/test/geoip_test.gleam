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

import fluxer_marketing/geoip
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn gh(headers: List(#(String, String))) -> fn(String) -> Result(String, Nil) {
  fn(name) {
    let lname = string.lowercase(name)

    let found =
      list.find(headers, fn(pair) {
        let #(k, _) = pair
        string.lowercase(k) == lname
      })

    case found {
      Ok(#(_, v)) -> Ok(v)
      Error(_) -> Error(Nil)
    }
  }
}

fn gh_empty() -> fn(String) -> Result(String, Nil) {
  fn(_) { Error(Nil) }
}

fn http_ok(body: String) -> fn(String) -> Result(String, Nil) {
  fn(_url) { Ok(body) }
}

fn http_err() -> fn(String) -> Result(String, Nil) {
  fn(_url) { Error(Nil) }
}

pub fn no_host_defaults_to_us_test() {
  let cc = geoip.country_code_core(gh_empty(), "", http_ok("SE"))
  cc |> should.equal("US")
}

pub fn missing_ip_defaults_to_us_test() {
  let cc = geoip.country_code_core(gh_empty(), "geoip:8080", http_ok("SE"))
  cc |> should.equal("US")
}

pub fn invalid_ip_defaults_to_us_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "not_an_ip")]),
      "geoip:8080",
      http_ok("SE"),
    )
  cc |> should.equal("US")
}

pub fn ipv4_success_uppercases_and_validates_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "8.8.8.8")]),
      "geoip:8080",
      http_ok("se"),
    )
  cc |> should.equal("SE")
}

pub fn ipv6_bracketed_success_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "[2001:db8::1]")]),
      "geoip:8080",
      http_ok("de"),
    )
  cc |> should.equal("DE")
}

pub fn multiple_xff_uses_first_token_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "1.1.1.1, 8.8.8.8")]),
      "geoip:8080",
      http_ok("gb"),
    )
  cc |> should.equal("GB")
}

pub fn http_error_falls_back_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "8.8.4.4")]),
      "geoip:8080",
      http_err(),
    )
  cc |> should.equal("US")
}

pub fn invalid_body_falls_back_test() {
  let cc =
    geoip.country_code_core(
      gh([#("x-forwarded-for", "8.8.4.4")]),
      "geoip:8080",
      http_ok("USA"),
    )
  cc |> should.equal("US")
}

pub fn strip_brackets_helper_test() {
  geoip.strip_brackets("[::1]") |> should.equal("::1")
  geoip.strip_brackets("127.0.0.1") |> should.equal("127.0.0.1")
}

pub fn percent_encode_ip_helper_test() {
  geoip.percent_encode_ip("2001:db8::1") |> should.equal("2001%3Adb8%3A%3A1")
  geoip.percent_encode_ip("100% legit") |> should.equal("100%25%20legit")
}

pub fn is_ascii_upper_alpha2_helper_test() {
  geoip.is_ascii_upper_alpha2("US") |> should.equal(True)
  geoip.is_ascii_upper_alpha2("uS") |> should.equal(False)
  geoip.is_ascii_upper_alpha2("USA") |> should.equal(False)
  geoip.is_ascii_upper_alpha2("") |> should.equal(False)
}
