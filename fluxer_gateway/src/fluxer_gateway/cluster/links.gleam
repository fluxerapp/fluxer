// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/transport
import bouncer_proto/worker.{type Entry, type Status, Draining, Gone, Ready}
import fluxer_gateway/core/node.{type Node, type Role}
import fluxer_gateway/util/table
import gleam/bool
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/result
import gleam/string

type Meta {
  Version
  Joined
  ServingUntil
}

fn entries_table() -> table.Table {
  table.named("fluxer_gateway_links")
}

fn meta_table() -> table.Table {
  table.named("fluxer_gateway_links_meta")
}

pub fn start() -> Bool {
  table.ensure_public(entries_table()) && table.ensure_public(meta_table())
}

pub fn replace(entries: List(Entry)) -> Bool {
  use <- bool.guard(!start(), False)
  let current = all()
  let incoming =
    list.map(entries, fn(entry) { #(entry.worker.node, entry) })
    |> list.sort(fn(a, b) {
      string.compare(atom.to_string(a.0), atom.to_string(b.0))
    })
  let changed = shape(current) != shape(list.map(incoming, fn(row) { row.1 }))
  list.each(current, fn(entry) {
    case list.key_find(incoming, entry.worker.node) {
      Ok(_) -> Nil
      Error(Nil) -> table.delete(entries_table(), entry.worker.node)
    }
  })
  list.each(incoming, fn(row) { table.insert(entries_table(), row) })
  case changed {
    True -> bump()
    False -> Nil
  }
  changed
}

fn shape(entries: List(Entry)) -> List(#(Atom, Int, Status, String)) {
  entries
  |> list.map(fn(entry) {
    #(
      entry.worker.node,
      entry.worker.incarnation,
      entry.status,
      entry.profile.role,
    )
  })
  |> list.sort(fn(a, b) {
    string.compare(atom.to_string(a.0), atom.to_string(b.0))
  })
}

pub fn all() -> List(Entry) {
  let rows: List(#(Atom, Entry)) = table.to_list(entries_table())
  list.map(rows, fn(row) { row.1 })
}

pub fn entry(peer: Node) -> Result(Entry, Nil) {
  let rows: List(#(Atom, Entry)) =
    table.lookup(entries_table(), node.to_atom(peer))
  rows |> list.first |> result.map(fn(row) { row.1 })
}

pub fn is_live(peer: Node) -> Bool {
  use <- bool.guard(peer == node.self(), True)
  case entry(peer) {
    Ok(found) -> found.status != Gone
    Error(Nil) -> False
  }
}

fn runs(found: Entry, role: Role) -> Bool {
  case node.role_from_name(found.profile.role) {
    Ok(peer_role) -> node.runs(peer_role, role)
    Error(Nil) -> False
  }
}

fn with(role: Role, keep: fn(Status) -> Bool) -> List(Node) {
  all()
  |> list.filter(fn(found) { keep(found.status) && runs(found, role) })
  |> list.map(fn(found) { node.from_atom(found.worker.node) })
  |> list.sort(fn(a, b) { string.compare(node.to_string(a), node.to_string(b)) })
}

pub fn live_with(role: Role) -> List(Node) {
  with(role, fn(status) { status == Ready || status == Draining })
}

pub fn serving_with(role: Role) -> List(Node) {
  with(role, fn(status) { status == Ready })
}

pub fn known() -> List(Node) {
  all()
  |> list.filter(fn(found) { found.status != Gone })
  |> list.map(fn(found) { node.from_atom(found.worker.node) })
}

@external(erlang, "erlang", "nodes")
fn connected() -> List(Atom)

pub fn ensure(peer: Node) -> Bool {
  let name = node.to_atom(peer)
  use <- bool.guard(peer == node.self(), True)
  use <- bool.guard(list.contains(connected(), name), True)
  case is_live(peer) {
    True -> transport.connect(name)
    False -> False
  }
}

fn meta(key: Meta) -> Int {
  let rows: List(#(Meta, Int)) = table.lookup(meta_table(), key)
  case rows {
    [#(_, value), ..] -> value
    [] -> 0
  }
}

fn bump() -> Nil {
  let _ = table.update_counter(meta_table(), Version, 1, #(Version, 0))
  Nil
}

pub fn version() -> Int {
  meta(Version)
}

pub fn set_joined(joined: Bool) -> Nil {
  use <- bool.guard(!start(), Nil)
  table.insert(
    meta_table(),
    #(Joined, case joined {
      True -> 1
      False -> 0
    }),
  )
}

pub fn joined() -> Bool {
  meta(Joined) == 1
}

pub fn hold_serving(until: Int) -> Nil {
  use <- bool.guard(!start(), Nil)
  table.insert(meta_table(), #(ServingUntil, until))
}

pub fn release_serving() -> Nil {
  use <- bool.guard(!start(), Nil)
  table.delete(meta_table(), ServingUntil)
}

pub fn serving(now: Int) -> Bool {
  let rows: List(#(Meta, Int)) = table.lookup(meta_table(), ServingUntil)
  joined()
  || case rows {
    [#(_, until), ..] -> now < until
    [] -> False
  }
}
