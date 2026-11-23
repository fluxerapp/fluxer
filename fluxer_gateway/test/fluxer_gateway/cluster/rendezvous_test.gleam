// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/rendezvous
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom
import gleam/list

fn peer(name: String) -> Node {
  node.from_atom(atom.create(name))
}

pub fn select_empty_and_single_test() {
  assert rendezvous.select(1, []) == Error(Nil)
  assert rendezvous.select(1, [peer("a@h")]) == Ok(peer("a@h"))
}

pub fn select_is_order_independent_test() {
  let nodes = [peer("a@h"), peer("b@h"), peer("c@h"), peer("d@h")]
  let keys = [1, 2, 3, 99, 123_456_789]
  assert list.map(keys, rendezvous.select(_, nodes))
    == list.map(keys, rendezvous.select(_, list.reverse(nodes)))
}

pub fn removing_a_node_moves_only_its_keys_test() {
  let nodes = [peer("a@h"), peer("b@h"), peer("c@h")]
  let fewer = [peer("a@h"), peer("b@h")]
  let removed = peer("c@h")
  let moved =
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    |> list.filter(fn(key) {
      case rendezvous.select(key, nodes) {
        Ok(owner) if owner != removed ->
          rendezvous.select(key, fewer) != Ok(owner)
        _ -> False
      }
    })
  assert moved == []
}
