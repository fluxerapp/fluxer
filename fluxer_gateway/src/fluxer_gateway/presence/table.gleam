// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}

type WriteMode {
  Auto
}

type TableOption {
  Set
  OrderedSet
  DuplicateBag
  Public
  NamedTable
  ReadConcurrency(Bool)
  WriteConcurrency(WriteMode)
}

type Where

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> Where

@external(erlang, "erlang", "is_reference")
fn is_table(found: Where) -> Bool

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: row) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: key) -> List(row)

@external(erlang, "ets", "delete")
fn ets_delete(table: Atom, key: key) -> Bool

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Atom, row: row) -> Bool

@external(erlang, "ets", "select")
fn ets_select(
  table: Atom,
  spec: List(#(pattern, List(Nil), List(Atom))),
) -> List(a)

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Atom,
  spec: List(#(pattern, List(Nil), List(Bool))),
) -> Int

@external(erlang, "ets", "match_delete")
fn ets_match_delete(table: Atom, pattern: pattern) -> Bool

@external(erlang, "ets", "insert_new")
fn ets_insert_new(table: Atom, row: row) -> Bool

@external(erlang, "ets", "member")
fn ets_member(table: Atom, key: key) -> Bool

@external(erlang, "ets", "lookup_element")
fn ets_lookup_element(
  table: Atom,
  key: key,
  position: Int,
  default: List(value),
) -> List(value)

@external(erlang, "ets", "info")
fn ets_info(table: Atom, item: Atom) -> Int

fn exists(table: Atom) -> Bool {
  is_table(ets_whereis(table))
}

pub fn create(table: Atom) -> Nil {
  create_as(table, Set)
}

pub fn create_duplicate_bag(table: Atom) -> Nil {
  create_as(table, DuplicateBag)
}

pub fn create_ordered(table: Atom) -> Nil {
  create_as(table, OrderedSet)
}

fn create_as(table: Atom, kind: TableOption) -> Nil {
  case exists(table) {
    True -> Nil
    False -> {
      let _ =
        ets_new(table, [
          kind,
          Public,
          NamedTable,
          ReadConcurrency(True),
          WriteConcurrency(Auto),
        ])
      Nil
    }
  }
}

pub fn member(table: Atom, key: key) -> Bool {
  exists(table) && ets_member(table, key)
}

pub fn values(table: Atom, key: key) -> List(value) {
  case exists(table) {
    True -> ets_lookup_element(table, key, 2, [])
    False -> []
  }
}

pub fn insert(table: Atom, row: row) -> Nil {
  case exists(table) {
    True -> {
      let _ = ets_insert(table, row)
      Nil
    }
    False -> Nil
  }
}

pub fn insert_new(table: Atom, row: row) -> Bool {
  exists(table) && ets_insert_new(table, row)
}

pub fn lookup(table: Atom, key: key) -> List(row) {
  case exists(table) {
    True -> ets_lookup(table, key)
    False -> []
  }
}

pub fn delete(table: Atom, key: key) -> Nil {
  case exists(table) {
    True -> {
      let _ = ets_delete(table, key)
      Nil
    }
    False -> Nil
  }
}

pub fn delete_row(table: Atom, row: row) -> Nil {
  case exists(table) {
    True -> {
      let _ = ets_delete_object(table, row)
      Nil
    }
    False -> Nil
  }
}

pub fn take_row(table: Atom, row: row) -> Bool {
  exists(table) && ets_select_delete(table, [#(row, [], [True])]) > 0
}

pub fn delete_matching(table: Atom, pattern: pattern) -> Nil {
  case exists(table) {
    True -> {
      let _ = ets_match_delete(table, pattern)
      Nil
    }
    False -> Nil
  }
}

pub fn any() -> Atom {
  atom.create("_")
}

pub fn keys(table: Atom) -> List(key) {
  case exists(table) {
    True ->
      ets_select(table, [
        #(#(atom.create("$1"), atom.create("_")), [], [atom.create("$1")]),
      ])
    False -> []
  }
}

pub fn rows(table: Atom) -> List(row) {
  case exists(table) {
    True -> ets_select(table, [#(atom.create("_"), [], [atom.create("$_")])])
    False -> []
  }
}

pub fn size(table: Atom) -> Int {
  case exists(table) {
    True -> ets_info(table, atom.create("size"))
    False -> 0
  }
}
