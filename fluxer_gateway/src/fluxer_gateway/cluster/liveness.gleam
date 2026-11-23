// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/util/table
import gleam/bool
import gleam/list

fn up_nodes() -> table.Table {
  table.named("fluxer_gateway_cluster_liveness")
}

pub fn init(connected: List(Node)) -> Nil {
  case table.ensure_public(up_nodes()) {
    False -> Nil
    True -> {
      let rows: List(#(Node)) = table.to_list(up_nodes())
      list.each(rows, fn(row) {
        case list.contains(connected, row.0) {
          True -> Nil
          False -> table.delete(up_nodes(), row.0)
        }
      })
    }
  }
}

pub fn set(peer: Node, up: Bool) -> Nil {
  case up {
    True -> table.insert(up_nodes(), #(peer))
    False -> table.delete(up_nodes(), peer)
  }
}

pub fn is_up(peer: Node) -> Bool {
  use <- bool.guard(peer == node.self(), True)
  case table.lookup(up_nodes(), peer) {
    [] -> False
    _ -> True
  }
}
