// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/charlist.{type Charlist}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import gleam/string

type IpAddress

@external(erlang, "inet", "parse_address")
fn inet_parse_address(text: Charlist) -> Result(IpAddress, IpParseError)

@external(erlang, "inet", "ntoa")
fn inet_ntoa(address: IpAddress) -> Charlist

@external(erlang, "inet", "ntoa")
fn ntoa_v4(address: #(Int, Int, Int, Int)) -> Charlist

@external(erlang, "inet", "ntoa")
fn ntoa_v6(address: #(Int, Int, Int, Int, Int, Int, Int, Int)) -> Charlist

type IpParseError

pub fn from_ipv4(a: Int, b: Int, c: Int, d: Int) -> String {
  charlist.to_string(ntoa_v4(#(a, b, c, d)))
}

pub fn from_ipv6(
  a: Int,
  b: Int,
  c: Int,
  d: Int,
  e: Int,
  f: Int,
  g: Int,
  h: Int,
) -> String {
  charlist.to_string(ntoa_v6(#(a, b, c, d, e, f, g, h)))
}

fn to_string(address: IpAddress) -> String {
  charlist.to_string(inet_ntoa(address))
}

pub fn resolve(
  header: Option(String),
  trust_header: Bool,
  peer: String,
) -> String {
  case trust_header, header {
    True, Some(value) -> parse_forwarded_for(value) |> result.unwrap(peer)
    _, _ -> peer
  }
}

pub fn parse_forwarded_for(value: String) -> Result(String, Nil) {
  let first = case string.split_once(value, ",") {
    Ok(#(first, _)) -> first
    Error(Nil) -> value
  }
  case string.trim(first) {
    "" -> Error(Nil)
    "[" <> rest -> strip_brackets(rest)
    trimmed -> validate(strip_ipv4_port(trimmed))
  }
}

fn strip_brackets(rest: String) -> Result(String, Nil) {
  case string.split_once(rest, "]") {
    Ok(#(inner, _)) if inner != "" -> validate(inner)
    _ -> Error(Nil)
  }
}

fn strip_ipv4_port(ip: String) -> String {
  case string.contains(ip, ".") {
    False -> ip
    True ->
      case string.split(ip, ":") {
        [address, _port] -> address
        _ -> ip
      }
  }
}

fn validate(ip: String) -> Result(String, Nil) {
  inet_parse_address(charlist.from_string(ip))
  |> result.map(to_string)
  |> result.replace_error(Nil)
}

pub fn header_value(
  headers: List(#(String, String)),
  name: String,
) -> Option(String) {
  list.key_find(headers, string.lowercase(name))
  |> option.from_result
}
