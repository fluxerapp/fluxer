// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/member_list/native.{type MlEvent}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/set.{type Set}

pub opaque type SessionTable {
  SessionTable(
    refs: Table(Int, SessionRef),
    subscribers: Table(SessionId, Int),
    resynced: Table(Nil, Int),
    synced: Table(Nil, #(Int, Set(BitArray))),
    parked: Table(BitArray, MlEvent),
  )
}

type Table(key, value)

pub fn new() -> SessionTable {
  SessionTable(
    refs: table("fluxer_gateway_member_list_sessions"),
    subscribers: table("fluxer_gateway_member_list_subscribers"),
    resynced: table("fluxer_gateway_member_list_resynced"),
    synced: table("fluxer_gateway_member_list_synced"),
    parked: table("fluxer_gateway_member_list_parked"),
  )
}

fn table(name: String) -> Table(key, value) {
  ets_new(atom.create(name), [atom.create("set"), atom.create("protected")])
}

pub fn subscriber(table: SessionTable, id: SessionId) -> Int {
  case ets_lookup(table.subscribers, id) {
    [#(_, found), ..] -> found
    [] -> unique_integer([atom.create("positive")])
  }
}

pub fn put(table: SessionTable, subscriber: Int, ref: SessionRef) -> Nil {
  let _ = ets_insert(table.refs, #(subscriber, ref))
  let _ = ets_insert(table.subscribers, #(ref.id, subscriber))
  Nil
}

pub fn rebind(table: SessionTable, ref: SessionRef) -> Nil {
  case ets_lookup(table.subscribers, ref.id) {
    [#(_, subscriber), ..] -> {
      let _ = ets_insert(table.refs, #(subscriber, ref))
      Nil
    }
    [] -> Nil
  }
}

fn lookup(table: SessionTable, subscriber: Int) -> Result(SessionRef, Nil) {
  case ets_lookup(table.refs, subscriber) {
    [#(_, ref), ..] -> Ok(ref)
    [] -> Error(Nil)
  }
}

pub fn refs(table: SessionTable, subscribers: List(Int)) -> List(SessionRef) {
  list.filter_map(subscribers, lookup(table, _))
}

pub fn remove(table: SessionTable, id: SessionId) -> Nil {
  case ets_take(table.subscribers, id) {
    [#(_, subscriber), ..] -> {
      let _ = ets_delete_key(table.refs, subscriber)
      Nil
    }
    [] -> Nil
  }
}

pub fn claim_resync(table: SessionTable, epoch: Int) -> Bool {
  let fresh = case ets_lookup(table.resynced, Nil) {
    [#(_, done), ..] -> epoch > done
    [] -> True
  }
  case fresh {
    True -> {
      let _ = ets_insert(table.resynced, #(Nil, epoch))
      Nil
    }
    False -> Nil
  }
  fresh
}

pub fn synced_at(table: SessionTable, epoch: Int) -> Set(BitArray) {
  case ets_lookup(table.synced, Nil) {
    [#(_, #(at, keys)), ..] if at == epoch -> keys
    _ -> set.new()
  }
}

pub fn record_synced(
  table: SessionTable,
  epoch: Int,
  keys: Set(BitArray),
) -> Nil {
  let _ = ets_insert(table.synced, #(Nil, #(epoch, keys)))
  Nil
}

pub fn forget_synced(table: SessionTable) -> Nil {
  let _ = ets_delete_key(table.synced, Nil)
  Nil
}

pub fn park(table: SessionTable, session: BitArray, event: MlEvent) -> Nil {
  let _ = ets_insert(table.parked, #(session, event))
  Nil
}

pub fn unpark(table: SessionTable, session: BitArray) -> Nil {
  let _ = ets_delete_key(table.parked, session)
  Nil
}

pub fn take_parked(table: SessionTable) -> List(MlEvent) {
  let found = ets_tab2list(table.parked)
  let _ = ets_delete_all_objects(table.parked)
  list.map(found, fn(row) { row.1 })
}

pub fn forget_resyncs(table: SessionTable) -> Nil {
  let _ = ets_delete_key(table.resynced, Nil)
  Nil
}

pub fn delete(table: SessionTable) -> Nil {
  let _ = ets_delete(table.refs)
  let _ = ets_delete(table.subscribers)
  let _ = ets_delete(table.resynced)
  let _ = ets_delete(table.synced)
  let _ = ets_delete(table.parked)
  Nil
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(Atom)) -> Int

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(Atom)) -> Table(key, value)

@external(erlang, "ets", "insert")
fn ets_insert(table: Table(key, value), row: #(key, value)) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Table(key, value), key: key) -> List(#(key, value))

@external(erlang, "ets", "take")
fn ets_take(table: Table(key, value), key: key) -> List(#(key, value))

@external(erlang, "ets", "delete")
fn ets_delete_key(table: Table(key, value), key: key) -> Bool

@external(erlang, "ets", "tab2list")
fn ets_tab2list(table: Table(key, value)) -> List(#(key, value))

@external(erlang, "ets", "delete_all_objects")
fn ets_delete_all_objects(table: Table(key, value)) -> Bool

@external(erlang, "ets", "delete")
fn ets_delete(table: Table(key, value)) -> Bool
