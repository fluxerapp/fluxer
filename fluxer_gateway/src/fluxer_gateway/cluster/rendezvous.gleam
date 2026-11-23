// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/rendezvous
import fluxer_gateway/core/node.{type Node}
import gleam/list
import gleam/result

pub fn select(key: Int, nodes: List(Node)) -> Result(Node, Nil) {
  nodes
  |> list.map(node.to_atom)
  |> rendezvous.select(key, _)
  |> result.map(node.from_atom)
}
