// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/telemetry/process_info
import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Held {
  Held(name: String, count: Int, bytes: Int)
}

pub type Singleton {
  Singleton(name: String, queued: Int, longest: Int, bytes: Int)
}

type Info {
  Size
  Memory
}

type SystemItem {
  Wordsize
}

@external(erlang, "ets", "all")
fn ets_all() -> List(Dynamic)

@external(erlang, "ets", "info")
fn ets_info(table: Atom, item: Info) -> Dynamic

@external(erlang, "erlang", "system_info")
fn system_info(item: SystemItem) -> Int

@external(erlang, "erlang", "is_atom")
fn is_atom(value: Dynamic) -> Bool

pub fn tables() -> List(Held) {
  let word = system_info(Wordsize)
  ets_all()
  |> list.filter_map(fn(found) {
    use name <- result.try(case is_atom(found) {
      True -> decode.run(found, atom.decoder()) |> result.replace_error(Nil)
      False -> Error(Nil)
    })
    let text = atom.to_string(name)
    use <- bool.guard(!string.starts_with(text, "fluxer_"), Error(Nil))
    use rows <- result.try(info_int(name, Size))
    use words <- result.try(info_int(name, Memory))
    Ok(Held(family(text), rows, words * word))
  })
  |> merge(fn(found) { found.name }, fn(a, b) {
    Held(a.name, a.count + b.count, a.bytes + b.bytes)
  })
}

pub fn singletons() -> List(Singleton) {
  process_info.gateway_processes()
  |> list.filter_map(fn(entry) {
    let #(name, pid) = entry
    use queue <- result.try(process_info.message_queue_len(pid))
    use bytes <- result.try(process_info.memory(pid))
    Ok(Singleton(family(name), queue, queue, bytes))
  })
  |> merge(fn(found) { found.name }, fn(a, b) {
    Singleton(
      a.name,
      a.queued + b.queued,
      int.max(a.longest, b.longest),
      a.bytes + b.bytes,
    )
  })
}

pub fn family(name: String) -> String {
  case string.split_once(name, "$") {
    Ok(#(prefix, _)) -> family(prefix)
    Error(Nil) -> without_index(string.replace(name, "@", "/"))
  }
}

fn without_index(name: String) -> String {
  case list.reverse(string.split(name, "_")) {
    [last, ..rest] if rest != [] ->
      case id.parse_decimal(last) {
        Ok(_) -> string.join(list.reverse(rest), "_")
        Error(Nil) -> name
      }
    _ -> name
  }
}

fn merge(
  found: List(a),
  name: fn(a) -> String,
  combine: fn(a, a) -> a,
) -> List(a) {
  found
  |> list.fold(dict.new(), fn(acc, held) {
    case dict.get(acc, name(held)) {
      Ok(seen) -> dict.insert(acc, name(held), combine(seen, held))
      Error(Nil) -> dict.insert(acc, name(held), held)
    }
  })
  |> dict.values
  |> list.sort(fn(a, b) { string.compare(name(a), name(b)) })
}

fn info_int(name: Atom, item: Info) -> Result(Int, Nil) {
  decode.run(ets_info(name, item), decode.int) |> result.replace_error(Nil)
}
