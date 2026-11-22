// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type EnvError {
  AmbiguousEnv(name: String, file_name: String)
  UnreadableEnvFile(file_name: String, path: String, reason: String)
  InvalidEnvFile(file_name: String, path: String)
  InvalidIntegerEnv(name: String, value: String)
}

pub type Source {
  Source(
    getenv: fn(String) -> Result(String, Nil),
    read_file: fn(String) -> Result(BitArray, String),
  )
}

type FileReason

@external(erlang, "file", "read_file")
fn read_file_raw(path: String) -> Result(BitArray, FileReason)

@external(erlang, "erlang", "binary_to_atom")
fn name_atom(text: String) -> process.Name(message)

pub fn system() -> Source {
  Source(getenv: envoy.get, read_file: read_system_file)
}

fn read_system_file(path: String) -> Result(BitArray, String) {
  read_file_raw(path) |> result.map_error(string.inspect)
}

pub fn value(source: Source, name: String) -> Result(Option(String), EnvError) {
  let direct = non_blank_env(source, name)
  let file_name = name <> "_FILE"
  case non_blank_env(source, file_name), direct {
    None, _ -> Ok(direct)
    Some(path), None -> read_env_file(source, file_name, path)
    Some(_), Some(_) -> Error(AmbiguousEnv(name:, file_name:))
  }
}

pub fn string(
  source: Source,
  name: String,
  default default: String,
) -> Result(String, EnvError) {
  value(source, name) |> result.map(option.unwrap(_, default))
}

pub fn int(
  source: Source,
  name: String,
  default default: Int,
) -> Result(Int, EnvError) {
  use found <- result.try(value(source, name))
  case found {
    None -> Ok(default)
    Some(text) ->
      text
      |> string.trim
      |> int.parse
      |> result.replace_error(InvalidIntegerEnv(name:, value: text))
  }
}

pub fn bool(
  source: Source,
  name: String,
  default default: Bool,
) -> Result(Bool, EnvError) {
  use text <- result.map(string(source, name, default: ""))
  case text |> string.trim |> string.lowercase {
    "1" | "true" | "yes" -> True
    "0" | "false" | "no" -> False
    _ -> default
  }
}

pub fn lookup(name: String) -> Option(String) {
  value(system(), name) |> result.unwrap(None)
}

pub fn describe(error: EnvError) -> String {
  case error {
    AmbiguousEnv(name:, file_name:) ->
      "both " <> name <> " and " <> file_name <> " are set, set only one"
    UnreadableEnvFile(file_name:, path:, reason:) ->
      file_name <> " could not read " <> path <> ": " <> reason
    InvalidEnvFile(file_name:, path:) ->
      file_name <> " points at " <> path <> ", which is not UTF-8 text"
    InvalidIntegerEnv(name:, value:) -> name <> " is not an integer: " <> value
  }
}

pub fn process_name(text: String) -> process.Name(message) {
  name_atom(text)
}

fn non_blank_env(source: Source, name: String) -> Option(String) {
  source.getenv(name) |> option.from_result |> option.then(non_blank)
}

fn non_blank(found: String) -> Option(String) {
  case string.trim(found) {
    "" -> None
    _ -> Some(found)
  }
}

fn read_env_file(
  source: Source,
  file_name: String,
  path: String,
) -> Result(Option(String), EnvError) {
  case source.read_file(path) {
    Ok(contents) ->
      contents
      |> strip_newline
      |> bit_array.to_string
      |> result.map(non_blank)
      |> result.replace_error(InvalidEnvFile(file_name:, path:))
    Error(reason) -> Error(UnreadableEnvFile(file_name:, path:, reason:))
  }
}

fn strip_newline(contents: BitArray) -> BitArray {
  let size = bit_array.byte_size(contents)
  case
    bit_array.slice(contents, size - 2, 2),
    bit_array.slice(contents, size - 1, 1)
  {
    Ok(<<"\r\n":utf8>>), _ -> take(contents, size - 2)
    _, Ok(<<"\n":utf8>>) -> take(contents, size - 1)
    _, _ -> contents
  }
}

fn take(contents: BitArray, count: Int) -> BitArray {
  bit_array.slice(contents, 0, count) |> result.unwrap(contents)
}
