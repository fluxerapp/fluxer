// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/links
import fluxer_gateway/core/node.{type Node}
import gleam
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/result

type Crash

type Reply {
  Ok(Int)
  Error(Crash)
  Throw(Crash)
  Exit(Crash)
}

@external(erlang, "erpc", "multicall")
fn multicall(
  nodes: List(Atom),
  module: Atom,
  function: Atom,
  args: List(Int),
  timeout_ms: Int,
) -> List(Reply)

pub fn of(targets: List(Node), timeout_ms: Int) -> List(#(Node, Int)) {
  let reachable = list.filter(targets, links.ensure)
  multicall(
    list.map(reachable, node.to_atom),
    atom.create("fluxer_gateway@session@manager@registry"),
    atom.create("count"),
    [],
    timeout_ms,
  )
  |> list.zip(reachable, _)
  |> list.filter_map(fn(pair) {
    case pair.1 {
      Ok(count) -> gleam.Ok(#(pair.0, count))
      Error(_) | Throw(_) | Exit(_) -> gleam.Error(Nil)
    }
  })
}

pub fn balance(keys: List(a), loads: List(#(b, Int))) -> List(#(a, b)) {
  let #(_, placed) =
    list.fold(keys, #(loads, []), fn(state, key) {
      let #(current, placed) = state
      case lightest(current) {
        gleam.Ok(target) -> #(add_one(current, target), [
          #(key, target),
          ..placed
        ])
        gleam.Error(Nil) -> state
      }
    })
  list.reverse(placed)
}

fn lightest(loads: List(#(b, Int))) -> gleam.Result(b, Nil) {
  loads
  |> list.reduce(fn(best, next) {
    case next.1 < best.1 {
      True -> next
      False -> best
    }
  })
  |> result.map(fn(pair) { pair.0 })
}

fn add_one(loads: List(#(b, Int)), target: b) -> List(#(b, Int)) {
  list.map(loads, fn(pair) {
    case pair.0 == target {
      True -> #(pair.0, pair.1 + 1)
      False -> pair
    }
  })
}
