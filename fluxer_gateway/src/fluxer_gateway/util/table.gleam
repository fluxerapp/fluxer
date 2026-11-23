// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/list

fn await_poll_ms() -> Int {
  durations.util().table_await_poll_ms
}

pub opaque type Table {
  Table(name: Atom)
}

type TableId

type TableOption {
  NamedTable
  Public
  Set
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

type Created

fn keeper_ready_ms() -> Int {
  durations.util().table_keeper_ready_ms
}

@external(erlang, "erlang", "binary_to_atom")
fn name_from_string(name: String) -> process.Name(message)

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Created

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> TableId

@external(erlang, "erlang", "is_reference")
fn is_reference(value: TableId) -> Bool

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: row) -> Bool

@external(erlang, "ets", "insert_new")
fn ets_insert_new(table: Atom, row: row) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: key) -> List(row)

@external(erlang, "ets", "delete")
fn ets_delete(table: Atom, key: key) -> Bool

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Atom, row: row) -> Bool

@external(erlang, "ets", "update_counter")
fn ets_update_counter(
  table: Atom,
  key: key,
  operation: #(Int, Int),
  default: row,
) -> Int

@external(erlang, "ets", "tab2list")
fn ets_tab2list(table: Atom) -> List(row)

@external(erlang, "ets", "select")
fn ets_select(table: Atom, spec: spec) -> List(result)

type Page(result)

@external(erlang, "ets", "select")
fn ets_select_limit(table: Atom, spec: spec, limit: Int) -> Page(result)

@external(erlang, "erlang", "is_tuple")
fn is_page(page: Page(result)) -> Bool

@external(erlang, "erlang", "element")
fn page_rows(index: Int, page: Page(result)) -> List(result)

@external(erlang, "ets", "select_delete")
fn ets_select_delete(table: Atom, spec: spec) -> Int

@external(erlang, "ets", "select_count")
fn ets_select_count(table: Atom, spec: spec) -> Int

pub fn named(name: String) -> Table {
  Table(atom.create(name))
}

fn exists(table: Table) -> Bool {
  is_reference(ets_whereis(table.name))
}

pub fn ensure_public(table: Table) -> Bool {
  case exists(table) {
    True -> True
    False -> start_keeper(table)
  }
}

fn start_keeper(table: Table) -> Bool {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    case process.register(process.self(), keeper_name(table)) {
      Ok(Nil) -> {
        create(table)
        process.send(ready, Nil)
        process.sleep_forever()
      }
      Error(Nil) -> {
        await_table(table, keeper_ready_ms())
        process.send(ready, Nil)
      }
    }
  })
  let _ = process.receive(ready, keeper_ready_ms())
  exists(table)
}

fn create(table: Table) -> Nil {
  case exists(table) {
    True -> Nil
    False -> {
      let _ =
        ets_new(table.name, [
          NamedTable,
          Public,
          Set,
          ReadConcurrency(True),
          WriteConcurrency(True),
        ])
      Nil
    }
  }
}

fn keeper_name(table: Table) -> process.Name(Nil) {
  name_from_string(atom.to_string(table.name) <> "_keeper")
}

fn await_table(table: Table, remaining_ms: Int) -> Nil {
  case exists(table) || remaining_ms <= 0 {
    True -> Nil
    False -> {
      process.sleep(await_poll_ms())
      await_table(table, remaining_ms - 1)
    }
  }
}

fn when_exists(table: Table, otherwise: a, run: fn(Atom) -> a) -> a {
  case exists(table) {
    True -> run(table.name)
    False -> otherwise
  }
}

pub fn insert(table: Table, row: row) -> Nil {
  use name <- when_exists(table, Nil)
  let _ = ets_insert(name, row)
  Nil
}

pub fn insert_new(table: Table, row: row) -> Bool {
  use name <- when_exists(table, False)
  ets_insert_new(name, row)
}

pub fn lookup(table: Table, key: key) -> List(row) {
  use name <- when_exists(table, [])
  ets_lookup(name, key)
}

pub fn delete(table: Table, key: key) -> Nil {
  use name <- when_exists(table, Nil)
  let _ = ets_delete(name, key)
  Nil
}

pub fn delete_object(table: Table, row: row) -> Nil {
  use name <- when_exists(table, Nil)
  let _ = ets_delete_object(name, row)
  Nil
}

pub fn update_counter(
  table: Table,
  key: key,
  by: Int,
  default: row,
) -> Result(Int, Nil) {
  use name <- when_exists(table, Error(Nil))
  Ok(ets_update_counter(name, key, #(2, by), default))
}

pub fn to_list(table: Table) -> List(row) {
  use name <- when_exists(table, [])
  ets_tab2list(name)
}

pub fn select(table: Table, spec: spec) -> List(result) {
  use name <- when_exists(table, [])
  ets_select(name, spec)
}

pub fn select_first(table: Table, spec: spec) -> Result(result, Nil) {
  use name <- when_exists(table, Error(Nil))
  let page = ets_select_limit(name, spec, 1)
  case is_page(page) {
    True -> list.first(page_rows(1, page))
    False -> Error(Nil)
  }
}

pub fn select_delete(table: Table, spec: spec) -> Int {
  use name <- when_exists(table, 0)
  ets_select_delete(name, spec)
}

pub fn select_count(table: Table, spec: spec) -> Int {
  use name <- when_exists(table, 0)
  ets_select_count(name, spec)
}
