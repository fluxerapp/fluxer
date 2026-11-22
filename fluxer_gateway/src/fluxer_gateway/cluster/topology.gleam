// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/rendezvous
import fluxer_gateway/core/node.{type Node, type Role}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}
import gleam/string

pub opaque type Topology {
  Topology(
    self: Node,
    version: Int,
    roles: Dict(Node, Role),
    members: Set(Node),
    placement: List(Node),
    leaving: Set(Node),
    removed: Set(Node),
    departed: Set(Node),
    expected: Set(Node),
    pins: Dict(Int, Node),
    pinned_only: Set(Node),
    joined: Bool,
  )
}

pub fn new(self: Node, role: Role, pins: List(Int)) -> Topology {
  let empty =
    Topology(
      self:,
      version: 0,
      roles: dict.new(),
      members: set.from_list([self]),
      placement: [],
      leaving: set.new(),
      removed: set.new(),
      departed: set.new(),
      expected: set.from_list([self]),
      pins: dict.new(),
      pinned_only: set.new(),
      joined: False,
    )
  empty
  |> set_role(self, role, pins)
  |> place(self)
}

pub fn local(topology: Topology) -> Node {
  topology.self
}

pub fn role_of(topology: Topology, peer: Node) -> Result(Role, Nil) {
  dict.get(topology.roles, peer)
}

pub fn joined(topology: Topology) -> Bool {
  topology.joined
}

pub fn mark_joined(topology: Topology) -> Topology {
  Topology(..topology, joined: True)
}

pub fn leaving(topology: Topology) -> Set(Node) {
  topology.leaving
}

pub fn removed(topology: Topology) -> Set(Node) {
  topology.removed
}

pub fn departed(topology: Topology) -> Set(Node) {
  topology.departed
}

pub fn version(topology: Topology) -> Int {
  topology.version
}

pub fn bump(topology: Topology) -> Topology {
  Topology(..topology, version: topology.version + 1)
}

pub fn members(topology: Topology) -> Set(Node) {
  topology.members
}

pub fn expected(topology: Topology) -> Set(Node) {
  topology.expected
}

pub fn set_role(
  topology: Topology,
  peer: Node,
  role: Role,
  pinned: List(Int),
) -> Topology {
  let pins =
    topology.pins
    |> dict.filter(fn(_, owner) { owner != peer })
    |> dict.merge(dict.from_list(list.map(pinned, fn(key) { #(key, peer) })))
  let pinned_only = case pinned {
    [] -> set.delete(topology.pinned_only, peer)
    _ -> set.insert(topology.pinned_only, peer)
  }
  let topology =
    Topology(
      ..topology,
      roles: dict.insert(topology.roles, peer, role),
      pins:,
      pinned_only:,
    )
  case placeable(topology, peer) {
    True -> topology
    False ->
      Topology(
        ..topology,
        placement: list.filter(topology.placement, fn(n) { n != peer }),
      )
  }
}

fn placeable(topology: Topology, peer: Node) -> Bool {
  case dict.get(topology.roles, peer) {
    Ok(role) ->
      node.runs(role, node.Guilds) && !set.contains(topology.pinned_only, peer)
    Error(Nil) -> False
  }
}

fn add_member(topology: Topology, peer: Node) -> Topology {
  Topology(..topology, members: set.insert(topology.members, peer))
}

pub fn place(topology: Topology, peer: Node) -> Topology {
  let topology = add_member(topology, peer)
  case placeable(topology, peer) && !list.contains(topology.placement, peer) {
    True ->
      Topology(
        ..topology,
        placement: sort_nodes([peer, ..topology.placement]),
        removed: set.delete(topology.removed, peer),
        departed: set.delete(topology.departed, peer),
        leaving: set.delete(topology.leaving, peer),
      )
    False ->
      Topology(
        ..topology,
        removed: set.delete(topology.removed, peer),
        departed: set.delete(topology.departed, peer),
        leaving: set.delete(topology.leaving, peer),
      )
  }
}

pub fn leave(topology: Topology, peer: Node) -> Topology {
  Topology(..topology, leaving: set.insert(topology.leaving, peer))
}

pub fn remove(topology: Topology, peer: Node) -> Topology {
  let was_placed = list.contains(topology.placement, peer)
  let was_leaving = set.contains(topology.leaving, peer)
  Topology(
    ..topology,
    members: set.delete(topology.members, peer),
    placement: list.filter(topology.placement, fn(n) { n != peer }),
    leaving: set.delete(topology.leaving, peer),
    removed: case was_placed && !was_leaving {
      True -> set.insert(topology.removed, peer)
      False -> topology.removed
    },
    departed: case was_leaving {
      True -> set.insert(topology.departed, peer)
      False -> topology.departed
    },
  )
}

pub fn forget(topology: Topology, peer: Node) -> Topology {
  Topology(
    ..topology,
    roles: dict.delete(topology.roles, peer),
    members: set.delete(topology.members, peer),
    placement: list.filter(topology.placement, fn(n) { n != peer }),
    leaving: set.delete(topology.leaving, peer),
    removed: set.delete(topology.removed, peer),
    departed: set.delete(topology.departed, peer),
    pins: dict.filter(topology.pins, fn(_, owner) { owner != peer }),
    pinned_only: set.delete(topology.pinned_only, peer),
  )
}

pub fn set_expected(topology: Topology, expected: Set(Node)) -> Topology {
  Topology(..topology, expected: set.insert(expected, topology.self))
}

pub fn guild_owner(topology: Topology, key: Int) -> Result(Node, Nil) {
  case dict.get(topology.pins, key) {
    Ok(pinned) ->
      case set.contains(topology.members, pinned) {
        True -> Ok(pinned)
        False -> rendezvous.select(key, serving(topology))
      }
    Error(Nil) -> rendezvous.select(key, serving(topology))
  }
}

pub fn serving(topology: Topology) -> List(Node) {
  list.filter(topology.placement, fn(n) { !set.contains(topology.leaving, n) })
}

pub fn role_owner(
  topology: Topology,
  role: Role,
  key: Int,
) -> Result(Node, Nil) {
  rendezvous.select(key, role_nodes(topology, role))
}

pub fn live_role_owner(
  topology: Topology,
  role: Role,
  key: Int,
  is_up: fn(Node) -> Bool,
) -> Result(Node, Nil) {
  role_nodes(topology, role)
  |> list.filter(fn(n) { n == topology.self || is_up(n) })
  |> rendezvous.select(key, _)
}

pub fn role_nodes(topology: Topology, role: Role) -> List(Node) {
  case role {
    node.Guilds -> serving(topology)
    _ ->
      topology.members
      |> set.to_list
      |> list.filter(fn(n) {
        !set.contains(topology.leaving, n)
        && case dict.get(topology.roles, n) {
          Ok(peer_role) -> node.runs(peer_role, role)
          Error(Nil) -> False
        }
      })
      |> sort_nodes
  }
}

pub fn nodes_running(topology: Topology, role: Role) -> List(Node) {
  topology.members
  |> set.to_list
  |> list.filter(fn(n) {
    case dict.get(topology.roles, n) {
      Ok(peer_role) -> node.runs(peer_role, role)
      Error(Nil) -> False
    }
  })
  |> sort_nodes
}

pub fn known_running(topology: Topology, role: Role) -> List(Node) {
  topology.roles
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(peer, peer_role) = entry
    case node.runs(peer_role, role) {
      True -> Ok(peer)
      False -> Error(Nil)
    }
  })
  |> list.append(nodes_running(topology, role))
  |> sort_nodes
}

pub fn has_majority(topology: Topology, is_up: fn(Node) -> Bool) -> Bool {
  let counted =
    set.filter(topology.expected, fn(n) {
      !set.contains(topology.departed, n)
      && case dict.get(topology.roles, n) {
        Ok(role) -> node.runs(role, node.Guilds)
        Error(Nil) -> True
      }
    })
  let up =
    set.fold(counted, 0, fn(acc, n) {
      case n == topology.self || is_up(n) {
        True -> acc + 1
        False -> acc
      }
    })
  up * 2 > set.size(counted)
}

pub fn previous_owner(topology: Topology, key: Int) -> Result(Node, Nil) {
  case dict.get(topology.pins, key) {
    Ok(pinned) -> Ok(pinned)
    Error(Nil) ->
      rendezvous.select(
        key,
        sort_nodes(list.append(serving(topology), set.to_list(topology.removed))),
      )
  }
}

pub fn start_allowed(
  topology: Topology,
  is_up: fn(Node) -> Bool,
  key: Int,
) -> Bool {
  use <- bool.guard(!topology.joined, False)
  case previous_owner(topology, key) {
    Ok(owner) if owner == topology.self -> has_majority(topology, is_up)
    Ok(owner) -> !is_up(owner) && has_majority(topology, is_up)
    Error(Nil) -> has_majority(topology, is_up)
  }
}

fn sort_nodes(nodes: List(Node)) -> List(Node) {
  nodes
  |> list.unique
  |> list.sort(fn(a, b) { string.compare(node.to_string(a), node.to_string(b)) })
}
