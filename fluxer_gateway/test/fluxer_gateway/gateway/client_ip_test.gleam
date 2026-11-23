// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/client_ip
import gleam/option.{None, Some}

pub fn first_forwarded_address_test() {
  assert client_ip.parse_forwarded_for(" 203.0.113.9 , 10.0.0.1")
    == Ok("203.0.113.9")
}

pub fn ipv4_port_is_stripped_test() {
  assert client_ip.parse_forwarded_for("203.0.113.9:4430") == Ok("203.0.113.9")
}

pub fn bracketed_ipv6_test() {
  assert client_ip.parse_forwarded_for("[2001:db8::1]:443") == Ok("2001:db8::1")
  assert client_ip.parse_forwarded_for("[]") == Error(Nil)
}

pub fn ipv6_is_normalised_test() {
  assert client_ip.parse_forwarded_for("2001:DB8:0:0:0:0:0:1")
    == Ok("2001:db8::1")
}

pub fn garbage_test() {
  assert client_ip.parse_forwarded_for("") == Error(Nil)
  assert client_ip.parse_forwarded_for("not-an-ip") == Error(Nil)
}

pub fn resolve_test() {
  assert client_ip.resolve(Some("203.0.113.9"), True, "10.0.0.1")
    == "203.0.113.9"
  assert client_ip.resolve(Some("203.0.113.9"), False, "10.0.0.1") == "10.0.0.1"
  assert client_ip.resolve(None, True, "10.0.0.1") == "10.0.0.1"
  assert client_ip.resolve(Some("junk"), True, "10.0.0.1") == "10.0.0.1"
}

pub fn peer_formatting_test() {
  assert client_ip.from_ipv4(127, 0, 0, 1) == "127.0.0.1"
  assert client_ip.from_ipv6(0, 0, 0, 0, 0, 0, 0, 1) == "::1"
}

pub fn header_lookup_is_lowercase_test() {
  assert client_ip.header_value([#("x-real-ip", "1.2.3.4")], "X-Real-IP")
    == Some("1.2.3.4")
}
