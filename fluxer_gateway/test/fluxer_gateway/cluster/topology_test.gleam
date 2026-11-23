// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/topology
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/set

fn peer(name: String) -> Node {
  node.from_atom(atom.create(name))
}

fn cluster() -> topology.Topology {
  topology.new(peer("a@h"), node.Guilds, [])
  |> topology.set_role(peer("b@h"), node.Guilds, [])
  |> topology.place(peer("b@h"))
  |> topology.set_role(peer("c@h"), node.Guilds, [])
  |> topology.place(peer("c@h"))
  |> topology.set_role(peer("w@h"), node.Websocket, [])
  |> topology.place(peer("w@h"))
  |> topology.mark_joined
}

pub fn websocket_nodes_never_own_guilds_test() {
  let t = cluster()
  assert list.contains(topology.serving(t), peer("w@h")) == False
  assert list.all([1, 2, 3, 4, 5, 6, 7, 8, 9], fn(key) {
    topology.guild_owner(t, key) != Ok(peer("w@h"))
  })
}

pub fn role_nodes_test() {
  let t = cluster()
  assert topology.role_nodes(t, node.Websocket) == [peer("w@h")]
  assert topology.nodes_running(t, node.Guilds)
    == [peer("a@h"), peer("b@h"), peer("c@h")]
}

pub fn pins_override_placement_test() {
  let t =
    cluster()
    |> topology.set_role(peer("p@h"), node.Guilds, [42])
    |> topology.place(peer("p@h"))
  assert topology.guild_owner(t, 42) == Ok(peer("p@h"))
  assert list.contains(topology.serving(t), peer("p@h")) == False
}

pub fn draining_node_loses_ownership_test() {
  let t = cluster() |> topology.leave(peer("b@h"))
  assert list.all([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], fn(key) {
    topology.guild_owner(t, key) != Ok(peer("b@h"))
  })
}

pub fn majority_counts_expected_guild_nodes_test() {
  let t =
    cluster()
    |> topology.set_expected(
      set.from_list([peer("b@h"), peer("c@h"), peer("d@h"), peer("e@h")]),
    )
  let only_b = fn(n) { n == peer("b@h") }
  assert topology.has_majority(t, only_b) == False
  let b_and_c = fn(n) { n == peer("b@h") || n == peer("c@h") }
  assert topology.has_majority(t, b_and_c)
}

pub fn start_allowed_test() {
  let t = cluster()
  let everyone = fn(_) { True }
  let nobody = fn(_) { False }
  let foreign =
    list.find([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], fn(key) {
      topology.previous_owner(t, key) == Ok(peer("b@h"))
    })
  let assert Ok(key) = foreign
  assert topology.start_allowed(t, everyone, key) == False
  let gone =
    topology.remove(t, peer("b@h"))
    |> topology.set_expected(set.from_list([peer("b@h"), peer("c@h")]))
  assert topology.start_allowed(gone, fn(n) { n == peer("c@h") }, key)
  assert topology.start_allowed(gone, nobody, key) == False
}

pub fn draining_node_does_not_block_new_owner_start_test() {
  let t = cluster()
  let everyone = fn(_) { True }
  let keys = int.range(from: 200, to: 0, with: [], run: list.prepend)
  let assert Ok(key) =
    list.find(keys, fn(key) {
      topology.previous_owner(t, key) == Ok(peer("b@h"))
      && topology.guild_owner(topology.leave(t, peer("b@h")), key)
      == Ok(peer("a@h"))
    })
  assert topology.start_allowed(t, everyone, key) == False
  let drained = topology.leave(t, peer("b@h"))
  assert topology.previous_owner(drained, key) == Ok(peer("a@h"))
  assert topology.start_allowed(drained, everyone, key)
  let assert Ok(foreign) =
    list.find(keys, fn(key) {
      topology.guild_owner(drained, key) == Ok(peer("c@h"))
    })
  assert topology.start_allowed(drained, everyone, foreign) == False
}

pub fn booting_node_starts_nothing_until_it_has_joined_test() {
  let booting =
    topology.new(peer("a@h"), node.Guilds, [])
    |> topology.set_expected(set.from_list([peer("b@h"), peer("c@h")]))
  let b_up = fn(n) { n == peer("b@h") }
  assert topology.previous_owner(booting, 7) == Ok(peer("a@h"))
  assert topology.start_allowed(booting, fn(_) { True }, 7) == False
  let joined = topology.mark_joined(booting)
  assert topology.start_allowed(joined, fn(_) { False }, 7) == False
  assert topology.start_allowed(joined, b_up, 7)
  let alone = topology.new(peer("a@h"), node.Guilds, [])
  assert topology.start_allowed(alone, fn(_) { False }, 7) == False
  assert topology.start_allowed(topology.mark_joined(alone), fn(_) { False }, 7)
}
