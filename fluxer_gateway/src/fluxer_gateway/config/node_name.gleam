// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import gleam/erlang/charlist.{type Charlist}
import gleam/list
import gleam/result
import gleam/string

const max_name_length = 255

const max_part_length = 128

@external(erlang, "inet", "gethostname")
fn gethostname() -> Result(Charlist, Nil)

pub fn pod_name() -> String {
  ["POD_NAME", "HOSTNAME"]
  |> list.find_map(fn(name) { envoy.get(name) |> result.try(non_empty) })
  |> result.lazy_or(fn() {
    gethostname() |> result.map(charlist.to_string) |> result.try(non_empty)
  })
  |> result.unwrap("unknown")
}

pub fn parse(raw: String) -> Result(String, Nil) {
  let name = string.trim(raw)
  let length = string.length(name)
  case length > 0 && length <= max_name_length && valid_parts(name) {
    True -> Ok(name)
    False -> Error(Nil)
  }
}

pub fn from_parts(base base: String, host host: String) -> Result(String, Nil) {
  parse(base <> "@" <> host)
}

fn non_empty(value: String) -> Result(String, Nil) {
  case string.trim(value) {
    "" -> Error(Nil)
    trimmed -> Ok(trimmed)
  }
}

fn valid_parts(name: String) -> Bool {
  case string.split(name, "@") {
    [name_part, host_part] -> valid_part(name_part) && valid_part(host_part)
    _ -> False
  }
}

fn valid_part(part: String) -> Bool {
  let codepoints = string.to_utf_codepoints(part)
  let length = list.length(codepoints)
  length > 0
  && length <= max_part_length
  && list.all(codepoints, fn(codepoint) {
    valid_char(string.utf_codepoint_to_int(codepoint))
  })
}

fn valid_char(char: Int) -> Bool {
  { char >= 97 && char <= 122 }
  || { char >= 65 && char <= 90 }
  || { char >= 48 && char <= 57 }
  || char == 95
  || char == 45
  || char == 46
}
