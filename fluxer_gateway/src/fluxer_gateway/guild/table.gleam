// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result

pub type Table(key, value)

pub type TableOption {
  Set
  OrderedSet
  Public
  NamedTable
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
  Heir(Pid, Nil)
}

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Table(key, value)

@external(erlang, "gleam_stdlib", "identity")
fn atom_to_table(name: Atom) -> Table(key, value)

@external(erlang, "ets", "insert")
fn ets_insert(table: Table(key, value), row: #(key, value)) -> Bool

@external(erlang, "ets", "insert")
fn ets_insert_many(table: Table(key, value), rows: List(#(key, value))) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Table(key, value), key: key) -> List(#(key, value))

@external(erlang, "ets", "delete")
fn ets_delete_key(table: Table(key, value), key: key) -> Bool

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Table(key, value), row: #(key, value)) -> Bool

@external(erlang, "ets", "delete")
fn ets_delete(table: Table(key, value)) -> Bool

@external(erlang, "ets", "delete_all_objects")
fn ets_delete_all(table: Table(key, value)) -> Bool

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Table(key, value),
  spec: List(#(#(key, value), List(Nil), List(Bool))),
) -> Int

@external(erlang, "ets", "select_replace")
fn ets_select_replace(
  table: Table(key, value),
  spec: List(#(#(key, value), List(Nil), List(#(Atom, #(key, value))))),
) -> Int

@external(erlang, "ets", "info")
fn ets_info(table: Table(key, value), item: Atom) -> Dynamic

@external(erlang, "ets", "foldl")
fn ets_foldl(
  step: fn(#(key, value), acc) -> acc,
  initial: acc,
  table: Table(key, value),
) -> acc

@external(erlang, "ets", "first")
fn ets_first(table: Table(key, value)) -> Dynamic

@external(erlang, "ets", "next")
fn ets_next(table: Table(key, value), key: key) -> Dynamic

@external(erlang, "ets", "safe_fixtable")
fn ets_safe_fixtable(table: Table(key, value), fix: Bool) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn stored_key(key: Dynamic) -> key

@external(erlang, "ets", "tab2list")
fn ets_to_list(table: Table(key, value)) -> List(#(key, value))

pub fn new(name: String, options: List(TableOption)) -> Table(key, value) {
  ets_new(atom.create(name), options)
}

pub fn named(name: String) -> Table(key, value) {
  atom_to_table(atom.create(name))
}

pub fn exists(table: Table(key, value)) -> Bool {
  ets_info(table, atom.create("id"))
  != atom.to_dynamic(atom.create("undefined"))
}

pub fn size(table: Table(key, value)) -> Int {
  ets_size(table) |> result.unwrap(0)
}

fn ets_size(table: Table(key, value)) -> Result(Int, Nil) {
  decode.run(ets_info(table, atom.create("size")), decode.int)
  |> result.replace_error(Nil)
}

fn when_exists(table: Table(key, value), run: fn() -> Bool) -> Nil {
  case exists(table) {
    True -> {
      let _ = run()
      Nil
    }
    False -> Nil
  }
}

pub fn ensure_named(name: String, options: List(TableOption)) -> Nil {
  case exists(atom_to_table(atom.create(name))) {
    True -> Nil
    False -> {
      let _ = ets_new(atom.create(name), [NamedTable, ..options])
      Nil
    }
  }
}

pub fn insert(table: Table(key, value), key: key, value: value) -> Nil {
  let _ = ets_insert(table, #(key, value))
  Nil
}

pub fn insert_many(table: Table(key, value), rows: List(#(key, value))) -> Nil {
  let _ = ets_insert_many(table, rows)
  Nil
}

pub fn lookup(table: Table(key, value), key: key) -> Result(value, Nil) {
  case exists(table) {
    False -> Error(Nil)
    True ->
      case ets_lookup(table, key) {
        [#(_, value), ..] -> Ok(value)
        [] -> Error(Nil)
      }
  }
}

pub fn delete(table: Table(key, value), key: key) -> Nil {
  use <- when_exists(table)
  ets_delete_key(table, key)
}

pub fn delete_row(table: Table(key, value), key: key, value: value) -> Nil {
  use <- when_exists(table)
  ets_delete_object(table, #(key, value))
}

pub fn take_row(table: Table(key, value), key: key, value: value) -> Bool {
  exists(table) && ets_select_delete(table, [#(#(key, value), [], [True])]) == 1
}

pub fn swap_row(
  table: Table(key, value),
  key: key,
  old: value,
  new: value,
) -> Bool {
  exists(table)
  && ets_select_replace(table, [
    #(#(key, old), [], [#(atom.create("const"), #(key, new))]),
  ])
  == 1
}

pub fn clear(table: Table(key, value)) -> Nil {
  use <- when_exists(table)
  ets_delete_all(table)
}

pub fn drop(table: Table(key, value)) -> Nil {
  use <- when_exists(table)
  ets_delete(table)
}

pub fn fold(
  table: Table(key, value),
  from initial: acc,
  with step: fn(acc, key, value) -> acc,
) -> acc {
  case exists(table) {
    False -> initial
    True -> ets_foldl(fn(row, acc) { step(acc, row.0, row.1) }, initial, table)
  }
}

pub fn fold_until(
  table: Table(key, value),
  from initial: acc,
  with step: fn(acc, key, value) -> list.ContinueOrStop(acc),
) -> acc {
  case exists(table) {
    False -> initial
    True -> {
      let _ = ets_safe_fixtable(table, True)
      let end = atom.to_dynamic(atom.create("$end_of_table"))
      let folded = fold_from(table, ets_first(table), end, initial, step)
      let _ = ets_safe_fixtable(table, False)
      folded
    }
  }
}

fn fold_from(
  table: Table(key, value),
  cursor: Dynamic,
  end: Dynamic,
  acc: acc,
  step: fn(acc, key, value) -> list.ContinueOrStop(acc),
) -> acc {
  case cursor == end {
    True -> acc
    False -> {
      let key = stored_key(cursor)
      case ets_lookup(table, key) {
        [#(_, value)] ->
          case step(acc, key, value) {
            list.Continue(next) ->
              fold_from(table, ets_next(table, key), end, next, step)
            list.Stop(done) -> done
          }
        _ -> fold_from(table, ets_next(table, key), end, acc, step)
      }
    }
  }
}

pub fn to_list(table: Table(key, value)) -> List(#(key, value)) {
  case exists(table) {
    False -> []
    True -> ets_to_list(table)
  }
}
