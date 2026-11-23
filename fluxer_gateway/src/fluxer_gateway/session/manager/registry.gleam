// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionRef}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

type TableOption {
  Set
  Public
  NamedTable
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "insert")
fn insert_session(table: Atom, row: #(String, SessionRef)) -> Bool

@external(erlang, "ets", "lookup")
fn lookup_session(table: Atom, key: String) -> List(#(String, SessionRef))

@external(erlang, "ets", "delete")
fn delete_key(table: Atom, key: String) -> Bool

@external(erlang, "ets", "info")
fn info(table: Atom, item: Atom) -> Info

type Info

type TableRef

@external(erlang, "erlang", "is_integer")
fn is_size(info: Info) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn info_size(info: Info) -> Int

@external(erlang, "erlang", "is_reference")
fn is_table(table: TableRef) -> Bool

@external(erlang, "ets", "whereis")
fn whereis(table: Atom) -> TableRef

fn exists() -> Bool {
  table_exists(sessions_table())
}

fn table_exists(table: Atom) -> Bool {
  is_table(whereis(table))
}

@external(erlang, "ets", "tab2list")
fn all_sessions(table: Atom) -> List(#(String, SessionRef))

fn sessions_table() -> Atom {
  atom.create("fluxer_gateway_sessions")
}

fn departing_table() -> Atom {
  atom.create("fluxer_gateway_sessions_departing")
}

pub fn create_tables() -> Nil {
  create_table(sessions_table())
  create_table(departing_table())
}

fn create_table(table: Atom) -> Nil {
  case table_exists(table) {
    True -> Nil
    False -> {
      ets_new(table, [
        Set,
        Public,
        NamedTable,
        ReadConcurrency(True),
        WriteConcurrency(True),
      ])
      Nil
    }
  }
}

fn row(ref: SessionRef) -> #(String, SessionRef) {
  #(session.session_id_to_string(ref.id), ref)
}

pub fn insert(ref: SessionRef) -> Nil {
  insert_session(sessions_table(), row(ref))
  Nil
}

pub fn remove(ref: SessionRef) -> Nil {
  remove_from(sessions_table(), ref)
  case table_exists(departing_table()) {
    True -> remove_from(departing_table(), ref)
    False -> Nil
  }
}

fn remove_from(table: Atom, ref: SessionRef) -> Nil {
  let #(key, _) = row(ref)
  case lookup_session(table, key) {
    [#(_, current)] if current.inbox == ref.inbox -> {
      delete_key(table, key)
      Nil
    }
    _ -> Nil
  }
}

pub fn depart(ref: SessionRef) -> Nil {
  case table_exists(departing_table()) {
    True -> {
      insert_session(departing_table(), row(ref))
      Nil
    }
    False -> Nil
  }
}

pub fn lookup(key: String) -> Result(SessionRef, Nil) {
  case exists() {
    False -> Error(Nil)
    True ->
      case lookup_session(sessions_table(), key) {
        [#(_, ref)] ->
          case live_owner(ref) {
            Ok(_) -> Ok(ref)
            Error(Nil) -> Error(Nil)
          }
        _ -> Error(Nil)
      }
  }
}

pub fn count() -> Int {
  let found = info(sessions_table(), atom.create("size"))
  case is_size(found) {
    True -> info_size(found)
    False -> 0
  }
}

pub fn owned() -> Int {
  case table_exists(departing_table()) {
    False -> count()
    True ->
      count()
      - list.count(all_sessions(departing_table()), fn(departing) {
        let #(key, departed) = departing
        case lookup_session(sessions_table(), key) {
          [#(_, current)] -> current.inbox == departed.inbox
          _ -> False
        }
      })
  }
}

pub fn all() -> List(SessionRef) {
  case exists() {
    False -> []
    True ->
      sessions_table()
      |> all_sessions
      |> list.filter_map(fn(row) {
        case live_owner(row.1) {
          Ok(_) -> Ok(row.1)
          Error(Nil) -> Error(Nil)
        }
      })
  }
}

pub fn sweep_dead() -> List(#(SessionRef, Pid)) {
  case exists() {
    False -> []
    True ->
      sessions_table()
      |> all_sessions
      |> list.filter_map(fn(row) {
        let ref = row.1
        case process.subject_owner(ref.inbox) {
          Ok(pid) ->
            case process.is_alive(pid) {
              True -> Error(Nil)
              False -> {
                remove(ref)
                Ok(#(ref, pid))
              }
            }
          Error(Nil) -> Error(Nil)
        }
      })
  }
}

fn live_owner(ref: SessionRef) -> Result(Pid, Nil) {
  case process.subject_owner(ref.inbox) {
    Ok(pid) ->
      case process.is_alive(pid) {
        True -> Ok(pid)
        False -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}
