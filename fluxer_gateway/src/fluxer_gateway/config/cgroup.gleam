// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

const cgroup_root = "/sys/fs/cgroup"

const bytes_per_session_start = 6_291_456

const min_session_start_ceiling = 8

type FileReason

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, FileReason)

pub fn memory_limit() -> Option(Int) {
  read_text("/proc/self/cgroup")
  |> option.from_result
  |> option.then(fn(contents) {
    own_memory_limit(contents, fn(path) {
      read_text(path) |> option.from_result
    })
  })
}

pub fn session_start_ceiling(limit_bytes: Int) -> Int {
  int.max(min_session_start_ceiling, limit_bytes / bytes_per_session_start)
}

pub fn own_memory_limit(
  contents: String,
  read: fn(String) -> Option(String),
) -> Option(Int) {
  let paths =
    contents
    |> string.split("\n")
    |> list.filter_map(fn(line) {
      case line {
        "0::" <> path -> Ok(path)
        _ -> Error(Nil)
      }
    })
  case paths {
    [path, ..] ->
      path
      |> directories
      |> list.fold(None, fn(lowest, directory) {
        lower(read_memory_max(directory, read), lowest)
      })
    [] -> None
  }
}

pub fn directories(path: String) -> List(String) {
  let parts = path |> string.split("/") |> list.filter(fn(part) { part != "" })
  int.range(from: list.length(parts), to: -1, with: [], run: list.prepend)
  |> list.map(fn(count) {
    parts
    |> list.take(count)
    |> list.fold(cgroup_root, fn(directory, part) { directory <> "/" <> part })
  })
}

fn parse_memory_max(value: String) -> Option(Int) {
  case int.parse(value) {
    Ok(bytes) if bytes > 0 -> Some(bytes)
    _ -> None
  }
}

fn read_memory_max(
  directory: String,
  read: fn(String) -> Option(String),
) -> Option(Int) {
  read(directory <> "/memory.max")
  |> option.then(fn(contents) {
    case string.split_once(contents, "\n") {
      Ok(#(first, _)) -> parse_memory_max(first)
      Error(Nil) -> parse_memory_max(contents)
    }
  })
}

fn lower(limit: Option(Int), lowest: Option(Int)) -> Option(Int) {
  case limit, lowest {
    None, _ -> lowest
    Some(limit), None -> Some(limit)
    Some(limit), Some(current) -> Some(int.min(limit, current))
  }
}

fn read_text(path: String) -> Result(String, Nil) {
  read_file(path)
  |> result.replace_error(Nil)
  |> result.try(bit_array.to_string)
}
