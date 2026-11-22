// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/table as kept
import gleam/erlang/atom.{type Atom}
import gleam/list

type TableId

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> TableId

@external(erlang, "erlang", "is_reference")
fn is_reference(value: TableId) -> Bool

@external(erlang, "ets", "update_counter")
fn ets_update_counter(
  table: Atom,
  key: key,
  op: #(Int, Int),
  default: #(key, Int),
) -> Int

@external(erlang, "ets", "update_counter")
fn ets_update_counter_floor(
  table: Atom,
  key: key,
  op: #(Int, Int, Int, Int),
) -> Int

@external(erlang, "ets", "delete_object")
fn ets_delete_object(table: Atom, row: #(key, Int)) -> Bool

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: #(key)) -> Bool

@external(erlang, "ets", "take")
fn ets_take(table: Atom, key: key) -> List(row)

@external(erlang, "ets", "member")
fn ets_member(table: Atom, key: key) -> Bool

@external(erlang, "ets", "tab2list")
fn ets_tab2list(table: Atom) -> List(#(key))

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Atom,
  spec: List(#(#(#(Atom, Atom), Atom), List(#(Atom, Atom, Int)), List(Bool))),
) -> Int

pub fn create(name: String) -> Nil {
  let _ = kept.ensure_public(kept.named(name))
  Nil
}

fn lookup(name: String) -> Result(Atom, Nil) {
  let table = atom.create(name)
  case is_reference(ets_whereis(table)) {
    True -> Ok(table)
    False -> Error(Nil)
  }
}

pub fn bump(name: String, key: key) -> Result(Int, Nil) {
  case lookup(name) {
    Ok(table) -> Ok(ets_update_counter(table, key, #(2, 1), #(key, 0)))
    Error(Nil) -> Error(Nil)
  }
}

pub fn drop(name: String, key: key) -> Nil {
  case lookup(name) {
    Ok(table) ->
      case ets_member(table, key) {
        True ->
          case ets_update_counter_floor(table, key, #(2, -1, 0, 0)) {
            0 -> {
              ets_delete_object(table, #(key, 0))
              Nil
            }
            _ -> Nil
          }
        False -> Nil
      }
    Error(Nil) -> Nil
  }
}

pub fn insert_marker(name: String, key: key) -> Nil {
  case lookup(name) {
    Ok(table) -> {
      ets_insert(table, #(key))
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn take_marker(name: String, key: key) -> Bool {
  case lookup(name) {
    Ok(table) -> {
      let taken: List(#(key)) = ets_take(table, key)
      !list.is_empty(taken)
    }
    Error(Nil) -> False
  }
}

pub fn markers(name: String) -> List(key) {
  case lookup(name) {
    Ok(table) ->
      ets_tab2list(table)
      |> list.map(fn(row: #(key)) { row.0 })
    Error(Nil) -> []
  }
}

pub fn prune_buckets_before(name: String, cutoff: Int) -> Nil {
  case lookup(name) {
    Ok(table) -> {
      let key = atom.create("$1")
      let bucket = atom.create("$2")
      ets_select_delete(table, [
        #(
          #(#(key, bucket), atom.create("_")),
          [#(atom.create("<"), bucket, cutoff)],
          [
            True,
          ],
        ),
      ])
      Nil
    }
    Error(Nil) -> Nil
  }
}
