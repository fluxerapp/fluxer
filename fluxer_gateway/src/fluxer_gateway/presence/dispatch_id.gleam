// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/set.{type Set}

pub opaque type DispatchId {
  DispatchId(node: Atom, serial: Int)
}

pub opaque type Recent {
  Recent(current: Set(DispatchId), previous: Set(DispatchId))
}

const generation_size = 256

type UniqueOption {
  Positive
  Monotonic
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "erlang", "node")
fn self_node() -> Atom

pub fn mint() -> DispatchId {
  DispatchId(node: self_node(), serial: unique_integer([Positive, Monotonic]))
}

pub fn empty() -> Recent {
  Recent(current: set.new(), previous: set.new())
}

pub fn contains(recent: Recent, id: DispatchId) -> Bool {
  set.contains(recent.current, id) || set.contains(recent.previous, id)
}

pub fn remember(recent: Recent, id: DispatchId) -> Recent {
  case contains(recent, id) {
    True -> recent
    False ->
      case set.size(recent.current) >= generation_size {
        True -> Recent(current: set.from_list([id]), previous: recent.current)
        False -> Recent(..recent, current: set.insert(recent.current, id))
      }
  }
}

pub fn merge(recent: Recent, other: Recent) -> Recent {
  list.append(set.to_list(other.previous), set.to_list(other.current))
  |> list.fold(recent, remember)
}

pub fn parts(id: DispatchId) -> #(Atom, Int) {
  #(id.node, id.serial)
}

pub fn restore(node: Atom, serial: Int) -> DispatchId {
  DispatchId(node:, serial:)
}

pub fn oldest_first(recent: Recent) -> List(DispatchId) {
  list.append(set.to_list(recent.previous), set.to_list(recent.current))
}

pub fn from_oldest_first(ids: List(DispatchId)) -> Recent {
  list.fold(ids, empty(), remember)
}
