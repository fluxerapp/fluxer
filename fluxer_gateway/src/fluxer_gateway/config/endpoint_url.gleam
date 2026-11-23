// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import gleam/string

pub fn normalize(
  url: String,
  base_domain base_domain: Option(String),
  public_port public_port: Option(Int),
) -> String {
  case base_domain, public_port {
    Some(base_domain), Some(port) if port > 0 ->
      case base_domain |> string.trim |> trim_root_dot {
        "" -> url
        domain -> insert_port(url, domain, port)
      }
    _, _ -> url
  }
}

pub fn leading_int(text: String) -> Result(Int, Nil) {
  case text {
    "-" <> rest -> digits(rest, "") |> result.map(int.negate)
    "+" <> rest -> digits(rest, "")
    _ -> digits(text, "")
  }
}

fn digits(text: String, acc: String) -> Result(Int, Nil) {
  case string.pop_grapheme(text) {
    Ok(#(char, rest)) ->
      case is_digit(char) {
        True -> digits(rest, acc <> char)
        False -> int.parse(acc)
      }
    Error(Nil) -> int.parse(acc)
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn insert_port(url: String, domain: String, port: Int) -> String {
  case string.split_once(url, "://") {
    Ok(#(_, "/" <> _)) -> url
    Ok(#(scheme, rest)) -> {
      let #(authority, tail) = split_authority(rest)
      case insertable(scheme, authority, domain, port) {
        True ->
          scheme <> "://" <> authority <> ":" <> int.to_string(port) <> tail
        False -> url
      }
    }
    Error(Nil) -> url
  }
}

fn insertable(
  scheme: String,
  authority: String,
  domain: String,
  port: Int,
) -> Bool {
  let host = after_last(authority, "@")
  case default_port(scheme) {
    Ok(default) -> port != default && !has_port(host) && same_host(host, domain)
    Error(Nil) -> False
  }
}

fn default_port(scheme: String) -> Result(Int, Nil) {
  case string.lowercase(scheme) {
    "http" | "ws" -> Ok(80)
    "https" | "wss" -> Ok(443)
    _ -> Error(Nil)
  }
}

fn split_authority(rest: String) -> #(String, String) {
  ["/", "\\", "?", "#"]
  |> list.filter_map(fn(separator) {
    case string.split_once(rest, separator) {
      Ok(#(head, tail)) -> Ok(#(head, separator <> tail))
      Error(Nil) -> Error(Nil)
    }
  })
  |> list.fold(#(rest, ""), fn(best, candidate) {
    case string.byte_size(candidate.0) < string.byte_size(best.0) {
      True -> candidate
      False -> best
    }
  })
}

fn after_last(subject: String, separator: String) -> String {
  subject
  |> string.split(separator)
  |> list.last
  |> result.unwrap(subject)
}

fn has_port(host: String) -> Bool {
  host |> after_last("]") |> string.contains(":")
}

fn same_host(host: String, domain: String) -> Bool {
  let host = case host {
    "[" <> _ -> host
    _ -> trim_root_dot(host)
  }
  string.lowercase(host) == string.lowercase(domain)
}

fn trim_root_dot(host: String) -> String {
  case string.ends_with(host, ".") {
    True -> string.drop_end(host, 1)
    False -> host
  }
}
