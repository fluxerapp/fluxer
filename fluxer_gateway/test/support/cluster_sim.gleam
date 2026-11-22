// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/membership/state.{type Membership}
import fluxer_gateway/cluster/topology.{type Topology}
import fluxer_gateway/core/node.{type Node, type Role}
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/list
import gleam/option.{type Option, None}
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub const tick_ms = 500

pub type Move {
  Out(at: Node, targets: List(Node), reason: state.MoveReason)
  In(at: Node, from: Node, reason: state.MoveReason)
}

pub type Running {
  Running(membership: Membership, role: Role, pins: List(Int))
}

pub type Sim {
  Sim(
    now: Int,
    running: Dict(Node, Running),
    draining: Set(Node),
    links: Set(#(Node, Node)),
    blocked: Set(#(Node, Node)),
    queue: List(#(Node, state.Event)),
    published: Dict(Node, Topology),
    liveness: Dict(Node, Set(Node)),
    moves: List(Move),
    seed: Int,
  )
}

pub fn node(name: String) -> Node {
  node.from_atom(atom.create(name <> "@sim"))
}

pub fn new() -> Sim {
  Sim(
    now: 0,
    running: dict.new(),
    draining: set.new(),
    links: set.new(),
    blocked: set.new(),
    queue: [],
    published: dict.new(),
    liveness: dict.new(),
    moves: [],
    seed: 1,
  )
}

fn pair(a: Node, b: Node) -> #(Node, Node) {
  case string.compare(node.to_string(a), node.to_string(b)) {
    order.Lt -> #(a, b)
    _ -> #(b, a)
  }
}

pub fn linked(sim: Sim, a: Node, b: Node) -> Bool {
  set.contains(sim.links, pair(a, b))
}

pub fn is_running(sim: Sim, at: Node) -> Bool {
  dict.has_key(sim.running, at)
}

fn config(sim: Sim) -> state.Config {
  state.Config(..state.default_config(), seed: sim.seed)
}

pub fn boot(sim: Sim, at: Node, role: Role, pins: List(Int)) -> Sim {
  let sim =
    Sim(
      ..sim,
      seed: sim.seed + 7919,
      draining: set.delete(sim.draining, at),
      published: dict.delete(sim.published, at),
      liveness: dict.insert(sim.liveness, at, set.new()),
    )
  let #(membership, decisions) =
    state.start(at, role, pins, config(sim), None, [], False, False, sim.now)
  let sim =
    Sim(
      ..sim,
      running: dict.insert(sim.running, at, Running(membership, role, pins)),
    )
  apply(sim, at, decisions)
}

pub fn restart_membership(sim: Sim, at: Node) -> Sim {
  case dict.get(sim.running, at) {
    Error(Nil) -> sim
    Ok(Running(role:, pins:, ..)) -> {
      let connected = connected_to(sim, at)
      let sim = Sim(..sim, seed: sim.seed + 104_729)
      let #(membership, decisions) =
        state.start(
          at,
          role,
          pins,
          config(sim),
          option.from_result(dict.get(sim.published, at)),
          connected,
          set.contains(sim.draining, at),
          False,
          sim.now,
        )
      let sim =
        Sim(
          ..sim,
          running: dict.insert(sim.running, at, Running(membership, role, pins)),
        )
      let sim = apply(sim, at, decisions)
      list.fold(connected, sim, fn(sim, peer) {
        enqueue(sim, at, state.NodeUp(peer))
      })
      |> settle
    }
  }
}

pub fn connected_to(sim: Sim, at: Node) -> List(Node) {
  set.fold(sim.links, [], fn(acc, link) {
    case link {
      #(a, b) if a == at -> [b, ..acc]
      #(a, b) if b == at -> [a, ..acc]
      _ -> acc
    }
  })
  |> sort
}

fn sort(nodes: List(Node)) -> List(Node) {
  list.sort(nodes, fn(a, b) {
    string.compare(node.to_string(a), node.to_string(b))
  })
}

pub fn discover(sim: Sim, at: Node, peers: List(Node)) -> Sim {
  sim
  |> deliver(at, state.DnsAnswer(peers))
  |> settle
}

pub fn discover_all(sim: Sim) -> Sim {
  let everyone = dict.keys(sim.running)
  list.fold(sort(everyone), sim, fn(sim, at) { discover(sim, at, everyone) })
}

pub fn drain(sim: Sim, at: Node) -> Sim {
  let sim = Sim(..sim, draining: set.insert(sim.draining, at))
  let sim =
    list.fold(connected_to(sim, at), sim, fn(sim, peer) {
      enqueue(sim, peer, state.DrainAnnounced(at))
    })
  sim
  |> deliver(at, state.DrainAnnounced(at))
  |> settle
}

pub fn undrain(sim: Sim, at: Node) -> Sim {
  let sim = Sim(..sim, draining: set.delete(sim.draining, at))
  let sim =
    list.fold(connected_to(sim, at), sim, fn(sim, peer) {
      enqueue(sim, peer, state.UndrainAnnounced(at))
    })
  sim
  |> deliver(at, state.UndrainAnnounced(at))
  |> settle
}

pub fn partition(sim: Sim, a: Node, b: Node) -> Sim {
  let sim = Sim(..sim, blocked: set.insert(sim.blocked, pair(a, b)))
  disconnect(sim, a, b, state.NetTickTimeout) |> settle
}

pub fn isolate(sim: Sim, at: Node) -> Sim {
  dict.keys(sim.running)
  |> list.filter(fn(peer) { peer != at })
  |> sort
  |> list.fold(sim, fn(sim, peer) { partition(sim, at, peer) })
}

pub fn heal(sim: Sim, a: Node, b: Node) -> Sim {
  Sim(..sim, blocked: set.delete(sim.blocked, pair(a, b)))
}

pub fn heal_all(sim: Sim) -> Sim {
  Sim(..sim, blocked: set.new())
}

pub fn crash(sim: Sim, at: Node) -> Sim {
  let sim =
    list.fold(connected_to(sim, at), sim, fn(sim, peer) {
      disconnect(sim, at, peer, state.ConnectionClosed)
    })
  Sim(
    ..sim,
    running: dict.delete(sim.running, at),
    draining: set.delete(sim.draining, at),
    published: dict.delete(sim.published, at),
    queue: list.filter(sim.queue, fn(entry) { entry.0 != at }),
  )
  |> settle
}

fn disconnect(sim: Sim, a: Node, b: Node, reason: state.DownReason) -> Sim {
  case linked(sim, a, b) {
    False -> sim
    True ->
      Sim(..sim, links: set.delete(sim.links, pair(a, b)))
      |> enqueue(a, state.NodeDown(b, reason))
      |> enqueue(b, state.NodeDown(a, reason))
  }
}

fn connect(sim: Sim, a: Node, b: Node) -> #(Sim, Bool) {
  case a == b, linked(sim, a, b) {
    True, _ -> #(sim, True)
    False, True -> #(sim, True)
    False, False ->
      case
        is_running(sim, a)
        && is_running(sim, b)
        && !set.contains(sim.blocked, pair(a, b))
      {
        False -> #(sim, False)
        True -> #(
          Sim(..sim, links: set.insert(sim.links, pair(a, b)))
            |> enqueue(a, state.NodeUp(b))
            |> enqueue(b, state.NodeUp(a)),
          True,
        )
      }
  }
}

fn enqueue(sim: Sim, at: Node, event: state.Event) -> Sim {
  Sim(..sim, queue: list.append(sim.queue, [#(at, event)]))
}

fn deliver(sim: Sim, at: Node, event: state.Event) -> Sim {
  case dict.get(sim.running, at) {
    Error(Nil) -> sim
    Ok(found) -> {
      let #(membership, decisions) =
        state.step(found.membership, event, sim.now)
      let sim =
        Sim(
          ..sim,
          running: dict.insert(
            sim.running,
            at,
            Running(..found, membership: membership),
          ),
        )
      apply(sim, at, decisions)
    }
  }
}

fn apply(sim: Sim, at: Node, decisions: List(state.Decision)) -> Sim {
  list.fold(decisions, sim, fn(sim, decision) { execute(sim, at, decision) })
}

fn execute(sim: Sim, at: Node, decision: state.Decision) -> Sim {
  case decision {
    state.Publish(found) ->
      Sim(..sim, published: dict.insert(sim.published, at, found))
    state.SetLiveness(peer, up) -> set_liveness(sim, at, peer, up)
    state.Forget(peer) -> set_liveness(sim, at, peer, False)
    state.Reconnect(peer) -> connect(sim, at, peer).0
    state.Probe(target, via) ->
      list.fold(via, sim, fn(sim, helper) { probe(sim, at, helper, target) })
    state.SendInfo(to, draining, wants_reply) ->
      case linked(sim, at, to), dict.get(sim.running, at) {
        True, Ok(Running(role:, pins:, ..)) ->
          enqueue(
            sim,
            to,
            state.PeerInfo(at, role, pins, draining, wants_reply),
          )
        _, _ -> sim
      }
    state.RefreshDiscovery -> sim
    state.MoveOut(targets, reason) ->
      Sim(..sim, moves: [Out(at, targets, reason), ..sim.moves])
    state.MoveIn(from, reason) ->
      Sim(..sim, moves: [In(at, from, reason), ..sim.moves])
    state.Count(_) -> sim
  }
}

fn probe(sim: Sim, asker: Node, helper: Node, target: Node) -> Sim {
  case linked(sim, asker, helper) && is_running(sim, helper) {
    False -> sim
    True -> {
      let #(sim, reachable) = connect(sim, helper, target)
      case linked(sim, helper, asker) {
        True -> enqueue(sim, asker, state.ProbeReply(target, helper, reachable))
        False -> sim
      }
    }
  }
}

fn set_liveness(sim: Sim, at: Node, peer: Node, up: Bool) -> Sim {
  let current = result.unwrap(dict.get(sim.liveness, at), set.new())
  let next = case up {
    True -> set.insert(current, peer)
    False -> set.delete(current, peer)
  }
  Sim(..sim, liveness: dict.insert(sim.liveness, at, next))
}

pub fn settle(sim: Sim) -> Sim {
  settle_loop(sim, 10_000)
}

fn settle_loop(sim: Sim, budget: Int) -> Sim {
  case sim.queue, budget {
    [], _ -> sim
    _, 0 -> sim
    [#(at, event), ..rest], _ ->
      settle_loop(deliver(Sim(..sim, queue: rest), at, event), budget - 1)
  }
}

pub fn run_for(sim: Sim, ms: Int) -> Sim {
  let until = sim.now + ms
  run_until(sim, until)
}

fn run_until(sim: Sim, until: Int) -> Sim {
  case sim.now + tick_ms > until {
    True -> sim
    False -> {
      let sim = Sim(..sim, now: sim.now + tick_ms)
      let sim =
        list.fold(sort(dict.keys(sim.running)), sim, fn(sim, at) {
          deliver(sim, at, state.Tick(sim.now)) |> settle
        })
      run_until(sim, until)
    }
  }
}

pub fn peer_state(sim: Sim, at: Node, peer: Node) -> Option(state.PeerState) {
  case dict.get(sim.running, at) {
    Ok(found) -> state.peer_state(found.membership, peer)
    Error(Nil) -> None
  }
}

pub fn published(sim: Sim, at: Node) -> Result(Topology, Nil) {
  dict.get(sim.published, at)
}

pub fn serving(sim: Sim, at: Node) -> List(Node) {
  case dict.get(sim.published, at) {
    Ok(found) -> topology.serving(found)
    Error(Nil) -> []
  }
}

pub fn current(sim: Sim, at: Node) -> Result(Topology, Nil) {
  dict.get(sim.running, at)
  |> result.map(fn(found) { state.current_topology(found.membership) })
}

pub fn is_up(sim: Sim, at: Node) -> fn(Node) -> Bool {
  let up = result.unwrap(dict.get(sim.liveness, at), set.new())
  fn(peer) { peer == at || set.contains(up, peer) }
}

pub fn owners(sim: Sim, at: Node, keys: List(Int)) -> List(Result(Node, Nil)) {
  case dict.get(sim.published, at) {
    Ok(found) -> list.map(keys, topology.guild_owner(found, _))
    Error(Nil) -> list.map(keys, fn(_) { Error(Nil) })
  }
}

pub fn agree(sim: Sim, keys: List(Int)) -> Bool {
  let views =
    sort(dict.keys(sim.running))
    |> list.map(fn(at) { #(serving(sim, at), owners(sim, at, keys)) })
  case views {
    [] -> True
    [first, ..rest] -> list.all(rest, fn(view) { view == first })
  }
}

pub fn moves_since(sim: Sim, count: Int) -> List(Move) {
  list.take(sim.moves, list.length(sim.moves) - count) |> list.reverse
}

pub fn drain_targets(sim: Sim, at: Node) -> Result(List(Node), Nil) {
  case dict.get(sim.running, at) {
    Ok(found) -> state.drain_targets(found.membership)
    Error(Nil) -> Error(Nil)
  }
}
