// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/liveness
import fluxer_gateway/cluster/router
import fluxer_gateway/cluster/topology.{type Topology}
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom
import gleam/int
import gleam/list

type Key {
  FluxerGatewayClusterTopology
}

@external(erlang, "persistent_term", "erase")
fn erase_topology(key: Key) -> Bool

fn peer(name: String) -> Node {
  node.from_atom(atom.create(name))
}

fn draining_cluster(b: Node, c: Node) -> Topology {
  topology.new(node.self(), node.AllRoles, [])
  |> topology.set_role(b, node.Sessions, [])
  |> topology.place(b)
  |> topology.set_role(c, node.Sessions, [])
  |> topology.place(c)
  |> topology.leave(node.self())
  |> topology.bump
}

fn session_keys() -> List(String) {
  int.range(from: 1, to: 201, with: [], run: fn(acc, n) {
    ["session-" <> int.to_string(n), ..acc]
  })
}

fn owner_of(key: String) -> Result(Node, Nil) {
  router.owner_node(router.SessionKey(key), node.Sessions)
}

fn with_cluster(run: fn(Node, Node) -> Nil) -> Nil {
  let b = peer("suspect_b@127.0.0.1")
  let c = peer("live_c@127.0.0.1")
  router.publish(draining_cluster(b, c))
  liveness.init([])
  liveness.set(b, True)
  liveness.set(c, True)
  run(b, c)
  liveness.set(b, False)
  liveness.set(c, False)
  erase_topology(FluxerGatewayClusterTopology)
  Nil
}

pub fn sessions_of_a_suspect_owner_move_to_a_live_peer_test() {
  use b, c <- with_cluster
  let of_b = list.filter(session_keys(), fn(key) { owner_of(key) == Ok(b) })
  let of_c = list.filter(session_keys(), fn(key) { owner_of(key) == Ok(c) })
  assert of_b != []
  assert of_c != []
  assert list.length(of_b) + list.length(of_c) == 200
  liveness.set(b, False)
  assert list.all(of_b, fn(key) { owner_of(key) == Ok(c) })
  assert list.all(of_c, fn(key) { owner_of(key) == Ok(c) })
  assert list.all(of_b, fn(key) {
    router.owner(router.SessionKey(key), node.Sessions) == router.Remote(c)
  })
  liveness.set(b, True)
  assert list.all(of_b, fn(key) { owner_of(key) == Ok(b) })
  assert list.all(of_c, fn(key) { owner_of(key) == Ok(c) })
}

pub fn a_draining_node_never_picks_itself_while_every_peer_is_suspect_test() {
  use b, c <- with_cluster
  liveness.set(b, False)
  liveness.set(c, False)
  assert list.all(session_keys(), fn(key) { owner_of(key) == Error(Nil) })
  assert list.all(session_keys(), fn(key) {
    case router.owner(router.SessionKey(key), node.Sessions) {
      router.OwnerUnavailable(_) -> True
      _ -> False
    }
  })
  liveness.set(c, True)
  assert list.all(session_keys(), fn(key) { owner_of(key) == Ok(c) })
}
