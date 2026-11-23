// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/presence/protocol.{type Subscriber}
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import fluxer_gateway/presence/version.{type Version}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub type Gate {
  Pending
  AdmittedAt(version: Version)
}

fn gate_table(target: Int) -> Atom {
  route.table(route.GateTable, route.shard(target))
}

pub fn bag_tables() -> List(Atom) {
  list.map(route.shards(), route.table(route.GateTable, _))
}

pub fn hold(target: Int, subscriber: Subscriber) -> Nil {
  table.insert(gate_table(target), #(target, subscriber, Pending))
}

pub fn open(target: Int, subscriber: Subscriber) -> Nil {
  table.delete_row(gate_table(target), #(target, subscriber, Pending))
}

pub fn open_all(target: Int) -> Nil {
  table.delete_matching(gate_table(target), #(target, table.any(), Pending))
}

pub fn drop(target: Int, subscriber: Subscriber) -> Nil {
  table.delete_matching(gate_table(target), #(target, subscriber, table.any()))
}

pub fn admit(
  target: Int,
  subscriber: Subscriber,
  last: Option(Version),
) -> Bool {
  let gates = gate_table(target)
  case last {
    Some(found) -> table.insert(gates, #(target, subscriber, AdmittedAt(found)))
    None -> Nil
  }
  let taken = table.take_row(gates, #(target, subscriber, Pending))
  case taken, last {
    False, Some(found) ->
      table.delete_row(gates, #(target, subscriber, AdmittedAt(found)))
    _, _ -> Nil
  }
  taken
}

pub fn settle(target: Int, subscriber: Subscriber, last: Version) -> Nil {
  table.delete_row(gate_table(target), #(target, subscriber, AdmittedAt(last)))
}

pub fn screen(
  target: Int,
  at: Version,
  subscribers: List(Subscriber),
) -> List(Subscriber) {
  case table.lookup(gate_table(target), target) {
    [] -> subscribers
    rows -> {
      let held =
        list.fold(rows, set.new(), fn(held, row: #(Int, Subscriber, Gate)) {
          case blocks(row.2, at) {
            True -> set.insert(held, row.1)
            False -> held
          }
        })
      case set.is_empty(held) {
        True -> subscribers
        False ->
          list.filter(subscribers, fn(found) { !set.contains(held, found) })
      }
    }
  }
}

fn blocks(gate: Gate, at: Version) -> Bool {
  case gate {
    Pending -> True
    AdmittedAt(last) -> version.compare(at, last) == version.Stale
  }
}
