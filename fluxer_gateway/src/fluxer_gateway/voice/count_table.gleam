// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/config/env
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/registry
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub type Message {
  OwnerSeen(Pid)
  OwnerDown(process.Down)
  RunSync
}

pub type Row {
  Row(key: VoiceKey, region: Option(String), server: Option(String), owner: Pid)
}

type Stored =
  #(ConnectionId, VoiceKey, Option(String), Option(String), Pid)

@external(erlang, "ets", "insert")
fn insert_row(table: Atom, row: Stored) -> Bool

@external(erlang, "ets", "insert_new")
fn insert_new_row(table: Atom, row: Stored) -> Bool

@external(erlang, "ets", "match_delete")
fn match_delete_key(
  table: Atom,
  pattern: #(ConnectionId, VoiceKey, Atom, Atom, Atom),
) -> Bool

@external(erlang, "ets", "match_delete")
fn match_delete_owner(
  table: Atom,
  pattern: #(Atom, Atom, Atom, Atom, Pid),
) -> Bool

@external(erlang, "ets", "match_delete")
fn match_delete_row(
  table: Atom,
  pattern: #(ConnectionId, VoiceKey, Atom, Atom, Pid),
) -> Bool

@external(erlang, "ets", "tab2list")
fn table_rows(table: Atom) -> List(Stored)

@external(erlang, "ets", "insert_new")
fn insert_new_owner(table: Atom, row: #(Pid, Bool)) -> Bool

@external(erlang, "ets", "delete")
fn delete_owner(table: Atom, key: Pid) -> Bool

@external(erlang, "ets", "delete_all_objects")
fn delete_all(table: Atom) -> Bool

pub fn janitor() -> Name(Message) {
  env.process_name("fluxer_gateway_voice_counts")
}

fn wildcard() -> Atom {
  atom.create("_")
}

pub fn upsert(value: Entry) -> Nil {
  put(value, process.self(), insert_row)
}

pub fn adopt_missing(value: Entry, owner: Pid) -> Nil {
  put(value, owner, insert_new_row)
}

fn put(value: Entry, owner: Pid, write: fn(Atom, Stored) -> Bool) -> Nil {
  let table = registry.connections_table()
  case registry.exists(table), entry.key(value) {
    False, _ | True, Error(Nil) -> Nil
    True, Ok(key) ->
      case
        write(table, #(
          voice_key.connection(key),
          key,
          entry.region_id(value),
          entry.server_id(value),
          owner,
        ))
      {
        True -> watch(owner)
        False -> Nil
      }
  }
}

fn watch(owner: Pid) -> Nil {
  let owners = registry.count_owners_table()
  case registry.exists(owners) && insert_new_owner(owners, #(owner, True)) {
    False -> Nil
    True ->
      case remote.send_local(janitor(), OwnerSeen(owner)) {
        Ok(Nil) -> Nil
        Error(Nil) -> {
          delete_owner(owners, owner)
          Nil
        }
      }
  }
}

pub fn remove(key: VoiceKey) -> Nil {
  let table = registry.connections_table()
  case registry.exists(table) {
    False -> Nil
    True -> {
      match_delete_key(table, #(
        voice_key.connection(key),
        key,
        wildcard(),
        wildcard(),
        wildcard(),
      ))
      Nil
    }
  }
}

pub fn remove_row(row: Row) -> Nil {
  let table = registry.connections_table()
  case registry.exists(table) {
    False -> Nil
    True -> {
      match_delete_row(table, #(
        voice_key.connection(row.key),
        row.key,
        wildcard(),
        wildcard(),
        row.owner,
      ))
      Nil
    }
  }
}

pub fn drop_owner(owner: Pid) -> Nil {
  let table = registry.connections_table()
  case registry.exists(table) {
    False -> Nil
    True -> {
      match_delete_owner(table, #(
        wildcard(),
        wildcard(),
        wildcard(),
        wildcard(),
        owner,
      ))
      Nil
    }
  }
  let owners = registry.count_owners_table()
  case registry.exists(owners) {
    True -> {
      delete_owner(owners, owner)
      Nil
    }
    False -> Nil
  }
}

pub fn claim_owners() -> List(Pid) {
  let owners = registry.count_owners_table()
  case registry.exists(owners) {
    False -> []
    True -> {
      delete_all(owners)
      rows()
      |> list.map(fn(row) { row.owner })
      |> set.from_list
      |> set.to_list
      |> list.filter(fn(owner) { insert_new_owner(owners, #(owner, True)) })
    }
  }
}

pub fn rows() -> List(Row) {
  case registry.exists(registry.connections_table()) {
    True ->
      table_rows(registry.connections_table())
      |> list.map(fn(stored) {
        let #(_, key, region, server, owner) = stored
        Row(key:, region:, server:, owner:)
      })
    False -> []
  }
}

pub fn connection_ids() -> List(ConnectionId) {
  rows() |> list.map(fn(row) { voice_key.connection(row.key) })
}

pub fn total() -> Int {
  list.length(rows())
}

pub fn regions() -> List(#(String, Int)) {
  rows() |> list.map(fn(row) { row.region }) |> tally
}

pub fn servers() -> List(#(String, Int)) {
  rows() |> list.map(fn(row) { row.server }) |> tally
}

fn tally(keys: List(Option(String))) -> List(#(String, Int)) {
  keys
  |> list.fold(dict.new(), fn(acc: Dict(String, Int), key) {
    case key {
      Some(key) ->
        dict.upsert(acc, key, fn(found) { option.unwrap(found, 0) + 1 })
      None -> acc
    }
  })
  |> dict.to_list
}
