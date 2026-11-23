// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/rng.{type Rng}
import fluxer_gateway/cluster/topology.{type Topology}
import fluxer_gateway/config/durations
import fluxer_gateway/core/node.{type Node, type Role}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

fn default_gone_after_ms() -> Int {
  durations.cluster().membership_state_gone_after_ms
}

fn default_reconnect_min_ms() -> Int {
  durations.cluster().membership_state_reconnect_min_ms
}

fn default_reconnect_max_ms() -> Int {
  durations.cluster().membership_state_reconnect_max_ms
}

fn default_reconnect_cap_ms() -> Int {
  durations.cluster().membership_state_reconnect_cap_ms
}

fn default_probe_timeout_ms() -> Int {
  durations.cluster().membership_state_probe_timeout_ms
}

fn default_flap_window_ms() -> Int {
  durations.cluster().membership_state_flap_window_ms
}

fn default_flap_stable_ms() -> Int {
  durations.cluster().membership_state_flap_stable_ms
}

fn default_joining_stable_ms() -> Int {
  durations.cluster().membership_state_joining_stable_ms
}

fn default_hello_resend_ms() -> Int {
  durations.cluster().membership_state_hello_resend_ms
}

fn default_publish_interval_ms() -> Int {
  durations.cluster().membership_state_publish_interval_ms
}

fn default_gone_reconnect_ms() -> Int {
  durations.cluster().membership_state_gone_reconnect_ms
}

fn default_discovery_refresh_ms() -> Int {
  durations.cluster().membership_state_discovery_refresh_ms
}

fn default_forget_after_ms() -> Int {
  durations.cluster().membership_state_forget_after_ms
}

fn tick_margin_ms() -> Int {
  durations.cluster().state_tick_margin_ms
}

pub type DownReason {
  ConnectionClosed
  NetTickTimeout
  Disconnect
  OtherReason
}

pub type PeerState {
  Up
  Suspect(since: Int)
  Gone(since: Int)
  Joining(since: Int)
  Draining
}

pub type Event {
  NodeUp(Node)
  NodeDown(Node, reason: DownReason)
  DrainAnnounced(Node)
  UndrainAnnounced(Node)
  PeerInfo(Node, role: Role, pins: List(Int), draining: Bool, wants_reply: Bool)
  ProbeReply(target: Node, from: Node, reachable: Bool)
  Tick(now: Int)
  DnsAnswer(List(Node))
}

pub type MoveReason {
  MovedGone
  MovedDraining
  MovedJoin
}

pub type Metric {
  NodeDownSeen(Node, DownReason)
  DownDuration(Node, ms: Int)
  PeerStateChanged(Node, PeerState)
  GoneVetoed(Node)
  TopologyPublished
  MembersChanged(count: Int)
  Moves(reason: MoveReason, targets: Int)
}

pub type Decision {
  Publish(Topology)
  SetLiveness(Node, Bool)
  Reconnect(Node)
  Probe(target: Node, via: List(Node))
  SendInfo(to: Node, draining: Bool, wants_reply: Bool)
  RefreshDiscovery
  MoveOut(targets: List(Node), reason: MoveReason)
  MoveIn(from: Node, reason: MoveReason)
  Forget(Node)
  Count(Metric)
}

pub type Config {
  Config(
    gone_after_ms: Int,
    timer_jitter_percent: Int,
    reconnect_min_ms: Int,
    reconnect_max_ms: Int,
    reconnect_cap_ms: Int,
    probe_helpers: Int,
    probe_timeout_ms: Int,
    flap_transitions: Int,
    flap_window_ms: Int,
    flap_stable_ms: Int,
    joining_stable_ms: Int,
    hello_resend_ms: Int,
    publish_interval_ms: Int,
    dns_misses_to_drop: Int,
    gone_reconnect_ms: Int,
    discovery_refresh_ms: Int,
    forget_after_ms: Int,
    seed: Int,
  )
}

pub fn default_config() -> Config {
  Config(
    gone_after_ms: default_gone_after_ms(),
    timer_jitter_percent: 20,
    reconnect_min_ms: default_reconnect_min_ms(),
    reconnect_max_ms: default_reconnect_max_ms(),
    reconnect_cap_ms: default_reconnect_cap_ms(),
    probe_helpers: 3,
    probe_timeout_ms: default_probe_timeout_ms(),
    flap_transitions: 5,
    flap_window_ms: default_flap_window_ms(),
    flap_stable_ms: default_flap_stable_ms(),
    joining_stable_ms: default_joining_stable_ms(),
    hello_resend_ms: default_hello_resend_ms(),
    publish_interval_ms: default_publish_interval_ms(),
    dns_misses_to_drop: 2,
    gone_reconnect_ms: default_gone_reconnect_ms(),
    discovery_refresh_ms: default_discovery_refresh_ms(),
    forget_after_ms: default_forget_after_ms(),
    seed: 1,
  )
}

type Peer {
  Peer(
    state: PeerState,
    role: Option(Role),
    transitions: List(Int),
    next_reconnect: Int,
    gone_at: Int,
    ready_at: Option(Int),
    hello_at: Int,
    confirmed: Bool,
  )
}

type Round {
  Round(asked: List(Node), started: Int, unreachable: Set(Node))
}

pub opaque type Membership {
  Membership(
    self: Node,
    config: Config,
    peers: Dict(Node, Peer),
    dns_misses: Dict(Node, Int),
    topology: Topology,
    dirty: Bool,
    last_publish: Int,
    last_refresh: Int,
    rounds: Dict(Node, Round),
    rng: Rng,
    draining: Bool,
    born: Int,
    answered: Bool,
  )
}

pub fn new(
  self: Node,
  role: Role,
  pins: List(Int),
  config: Config,
) -> Membership {
  Membership(
    self:,
    config:,
    peers: dict.new(),
    dns_misses: dict.new(),
    topology: topology.new(self, role, pins),
    dirty: True,
    last_publish: -1_000_000_000,
    last_refresh: -1_000_000_000,
    rounds: dict.new(),
    rng: rng.seed(config.seed),
    draining: False,
    born: 0,
    answered: False,
  )
}

pub fn start(
  self: Node,
  role: Role,
  pins: List(Int),
  config: Config,
  previous: Option(Topology),
  connected: List(Node),
  draining: Bool,
  discovering: Bool,
  now: Int,
) -> #(Membership, List(Decision)) {
  let fresh = Membership(..new(self, role, pins, config), born: now)
  let fresh = case discovering {
    True -> fresh
    False -> Membership(..fresh, topology: topology.mark_joined(fresh.topology))
  }
  let #(m, restored) = case previous {
    Some(published) ->
      case topology.local(published) == self {
        True -> resume(fresh, published, role, pins, connected, now)
        False -> #(fresh, [])
      }
    None -> #(fresh, [])
  }
  let #(m, opening) = case draining {
    True -> step(m, DrainAnnounced(self), now)
    False -> publish_now(m, now)
  }
  #(m, list.append(restored, opening))
}

fn resume(
  m: Membership,
  previous: Topology,
  role: Role,
  pins: List(Int),
  connected: List(Node),
  now: Int,
) -> #(Membership, List(Decision)) {
  let connected = set.from_list(connected)
  let leaving = topology.leaving(previous)
  let m =
    Membership(
      ..m,
      topology: previous
        |> topology.set_role(m.self, role, pins)
        |> topology.place(m.self),
    )
  let members =
    topology.members(previous)
    |> set.delete(m.self)
    |> set.to_list
    |> sort_nodes
  let #(m, decisions) =
    list.fold(members, #(m, []), fn(acc, peer) {
      let #(m, out) = acc
      let state = case set.contains(leaving, peer) {
        True -> Draining
        False -> Up
      }
      let p = restored_peer(previous, peer, state, now)
      let m = put_peer(m, peer, p)
      let #(m, down) = case set.contains(connected, peer), state {
        True, _ -> #(m, [SetLiveness(peer, True)])
        False, Draining -> #(gone(m, peer, now), [
          SetLiveness(peer, False),
          Count(PeerStateChanged(peer, Gone(now))),
        ])
        False, _ -> suspect(m, peer, p, now)
      }
      #(m, list.append(out, down))
    })
  let absent =
    topology.removed(previous)
    |> set.union(topology.departed(previous))
    |> set.filter(fn(peer) { peer != m.self && !set.contains(connected, peer) })
    |> set.to_list
    |> sort_nodes
  let m =
    list.fold(absent, m, fn(m, peer) {
      let p = restored_peer(previous, peer, Gone(now), now)
      put_peer(
        m,
        peer,
        Peer(..p, next_reconnect: now + m.config.gone_reconnect_ms),
      )
    })
  #(m, decisions)
}

fn restored_peer(
  previous: Topology,
  peer: Node,
  state: PeerState,
  now: Int,
) -> Peer {
  Peer(
    state:,
    role: option.from_result(topology.role_of(previous, peer)),
    transitions: [],
    next_reconnect: 0,
    gone_at: 0,
    ready_at: None,
    hello_at: now,
    confirmed: False,
  )
}

pub fn drain_targets(membership: Membership) -> Result(List(Node), Nil) {
  case membership.draining {
    True -> Ok(others(membership))
    False -> Error(Nil)
  }
}

fn others(m: Membership) -> List(Node) {
  topology.members(m.topology)
  |> set.delete(m.self)
  |> set.to_list
  |> sort_nodes
}

pub fn current_topology(membership: Membership) -> Topology {
  membership.topology
}

pub fn peer_state(membership: Membership, peer: Node) -> Option(PeerState) {
  case dict.get(membership.peers, peer) {
    Ok(found) -> Some(found.state)
    Error(Nil) -> None
  }
}

pub fn publish_now(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  maybe_publish(Membership(..m, dirty: True), now, True)
}

pub fn step(
  m: Membership,
  event: Event,
  now: Int,
) -> #(Membership, List(Decision)) {
  let #(m, decisions) = case event {
    NodeDown(peer, reason) -> node_down(m, peer, reason, now)
    NodeUp(peer) -> node_up(m, peer, now)
    DrainAnnounced(peer) -> drain(m, peer, now)
    UndrainAnnounced(peer) -> undrain(m, peer, now)
    PeerInfo(peer, role, pins, draining, wants_reply) ->
      peer_info(m, peer, role, pins, draining, wants_reply, now)
    ProbeReply(target, from, reachable) ->
      probe_reply(m, target, from, reachable, now)
    Tick(at) -> tick(m, at)
    DnsAnswer(nodes) -> dns(Membership(..m, answered: True), nodes)
  }
  let #(m, joined) = settle_join(m, now)
  let #(m, published) = maybe_publish(m, now, joined)
  #(m, list.append(decisions, published))
}

fn node_down(
  m: Membership,
  peer: Node,
  reason: DownReason,
  now: Int,
) -> #(Membership, List(Decision)) {
  let seen = Count(NodeDownSeen(peer, reason))
  case dict.get(m.peers, peer) {
    Error(Nil) -> #(m, [seen])
    Ok(p) ->
      case p.state {
        Up -> {
          let #(m, suspected) = suspect(m, peer, p, now)
          #(m, [seen, ..suspected])
        }
        Draining -> {
          let m = gone(m, peer, now)
          let #(m, published) = maybe_publish(m, now, True)
          #(
            m,
            list.flatten([
              [
                seen,
                SetLiveness(peer, False),
                Count(PeerStateChanged(peer, Gone(now))),
              ],
              published,
            ]),
          )
        }
        Joining(_) -> {
          let m =
            put_peer(
              m,
              peer,
              Peer(
                ..p,
                state: Gone(now),
                ready_at: None,
                next_reconnect: now + m.config.gone_reconnect_ms,
              ),
            )
          #(m, [
            seen,
            SetLiveness(peer, False),
            Count(PeerStateChanged(peer, Gone(now))),
          ])
        }
        Suspect(_) | Gone(_) -> #(m, [seen])
      }
  }
}

fn suspect(
  m: Membership,
  peer: Node,
  p: Peer,
  now: Int,
) -> #(Membership, List(Decision)) {
  let transitions = recent(m, [now, ..p.transitions], now)
  let factor = case flapping(m, transitions) {
    True -> 2
    False -> 1
  }
  let #(gone_in, generator) =
    rng.jitter(
      m.rng,
      m.config.gone_after_ms * factor,
      m.config.timer_jitter_percent,
    )
  let #(first, generator) =
    rng.uniform(generator, m.config.reconnect_min_ms, m.config.reconnect_max_ms)
  let p =
    Peer(
      ..p,
      state: Suspect(now),
      transitions:,
      next_reconnect: now + first,
      gone_at: now + gone_in,
    )
  let m = Membership(..put_peer(m, peer, p), rng: generator)
  #(m, [SetLiveness(peer, False), Count(PeerStateChanged(peer, Suspect(now)))])
}

fn node_up(
  m: Membership,
  peer: Node,
  now: Int,
) -> #(Membership, List(Decision)) {
  case peer == m.self {
    True -> #(m, [])
    False -> {
      let #(m, refresh) = refresh_if_unexpected(m, peer, now)
      let #(m, decisions) = case dict.get(m.peers, peer) {
        Ok(Peer(state: Suspect(since), ..) as p) -> {
          let transitions = recent(m, [now, ..p.transitions], now)
          let p = Peer(..p, state: Up, transitions:)
          let m =
            Membership(
              ..put_peer(m, peer, p),
              rounds: dict.delete(m.rounds, peer),
            )
          #(m, [
            SetLiveness(peer, True),
            Count(DownDuration(peer, now - since)),
            Count(PeerStateChanged(peer, Up)),
          ])
        }
        Ok(Peer(state: Gone(_), ..) as p) -> {
          let p = Peer(..p, state: Joining(now), ready_at: None)
          #(put_peer(m, peer, p), [
            SetLiveness(peer, True),
            Count(PeerStateChanged(peer, Joining(now))),
          ])
        }
        Ok(Peer(state: Draining, ..)) -> #(m, [SetLiveness(peer, True)])
        Ok(Peer(state: Up, ..)) | Ok(Peer(state: Joining(_), ..)) -> #(m, [])
        Error(Nil) -> {
          let p =
            Peer(
              state: Joining(now),
              role: None,
              transitions: [],
              next_reconnect: 0,
              gone_at: 0,
              ready_at: None,
              hello_at: now,
              confirmed: False,
            )
          #(put_peer(m, peer, p), [
            SetLiveness(peer, True),
            Count(PeerStateChanged(peer, Joining(now))),
          ])
        }
      }
      let #(m, hello) = send_info(m, peer, now)
      #(m, list.flatten([decisions, hello, refresh]))
    }
  }
}

fn send_info(
  m: Membership,
  peer: Node,
  now: Int,
) -> #(Membership, List(Decision)) {
  case dict.get(m.peers, peer) {
    Error(Nil) -> #(m, [])
    Ok(p) -> {
      let wants_reply = needs_info(p)
      let m = put_peer(m, peer, Peer(..p, hello_at: now))
      #(m, [SendInfo(peer, m.draining, wants_reply)])
    }
  }
}

fn needs_info(p: Peer) -> Bool {
  case p.state, p.ready_at {
    Joining(_), None -> True
    _, _ -> !p.confirmed
  }
}

fn refresh_if_unexpected(
  m: Membership,
  peer: Node,
  now: Int,
) -> #(Membership, List(Decision)) {
  let known = set.contains(topology.expected(m.topology), peer)
  case known || now - m.last_refresh < m.config.discovery_refresh_ms {
    True -> #(m, [])
    False -> #(Membership(..m, last_refresh: now), [RefreshDiscovery])
  }
}

fn drain(m: Membership, peer: Node, now: Int) -> #(Membership, List(Decision)) {
  case peer == m.self {
    True -> {
      let m =
        Membership(
          ..m,
          topology: topology.leave(m.topology, m.self),
          dirty: True,
          draining: True,
        )
      let targets = others(m)
      let #(m, published) = maybe_publish(m, now, True)
      #(
        m,
        list.flatten([
          published,
          [
            MoveOut(targets, MovedDraining),
            Count(Moves(MovedDraining, list.length(targets))),
          ],
        ]),
      )
    }
    False ->
      case dict.get(m.peers, peer) {
        Ok(Peer(state: Up, ..) as p) | Ok(Peer(state: Suspect(_), ..) as p) ->
          drain_peer(m, peer, p, [MoveIn(peer, MovedDraining)], now)
        Ok(Peer(state: Joining(_), ..) as p) -> drain_peer(m, peer, p, [], now)
        _ -> #(m, [])
      }
  }
}

fn drain_peer(
  m: Membership,
  peer: Node,
  p: Peer,
  moves: List(Decision),
  now: Int,
) -> #(Membership, List(Decision)) {
  let m =
    Membership(
      ..put_peer(m, peer, Peer(..p, state: Draining)),
      topology: m.topology
        |> topology.leave(peer)
        |> topology.set_expected(set.delete(topology.expected(m.topology), peer)),
      dirty: True,
      rounds: dict.delete(m.rounds, peer),
    )
  let #(m, published) = maybe_publish(m, now, True)
  #(
    m,
    list.flatten([[Count(PeerStateChanged(peer, Draining))], moves, published]),
  )
}

fn undrain(
  m: Membership,
  peer: Node,
  now: Int,
) -> #(Membership, List(Decision)) {
  case peer == m.self {
    True -> {
      let m =
        Membership(
          ..m,
          topology: topology.place(m.topology, m.self),
          dirty: True,
          draining: False,
        )
      maybe_publish(m, now, True)
    }
    False ->
      case dict.get(m.peers, peer) {
        Ok(Peer(state: Draining, ..) as p) -> {
          let t =
            m.topology
            |> topology.set_expected(set.insert(
              topology.expected(m.topology),
              peer,
            ))
            |> topology.place(peer)
          let m =
            Membership(
              ..put_peer(m, peer, Peer(..p, state: Up)),
              topology: t,
              dirty: True,
            )
          let #(m, published) = maybe_publish(m, now, True)
          let moves = case list.contains(topology.serving(t), peer) {
            True -> [MoveOut([peer], MovedJoin), Count(Moves(MovedJoin, 1))]
            False -> []
          }
          #(
            m,
            list.flatten([
              published,
              [Count(PeerStateChanged(peer, Up))],
              moves,
            ]),
          )
        }
        _ -> #(m, [])
      }
  }
}

fn peer_info(
  m: Membership,
  peer: Node,
  role: Role,
  pins: List(Int),
  draining: Bool,
  wants_reply: Bool,
  now: Int,
) -> #(Membership, List(Decision)) {
  case peer == m.self {
    True -> #(m, [])
    False -> {
      let #(m, first) = case dict.get(m.peers, peer) {
        Ok(_) -> #(m, [])
        Error(Nil) -> node_up(m, peer, now)
      }
      case dict.get(m.peers, peer) {
        Error(Nil) -> #(m, first)
        Ok(p) -> {
          let ready_at = case p.state, p.ready_at {
            Joining(_), None -> Some(now)
            _, ready -> ready
          }
          let p = Peer(..p, role: Some(role), ready_at:, confirmed: True)
          let t = topology.set_role(m.topology, peer, role, pins)
          let t = case p.state, draining {
            Up, False | Suspect(_), False -> topology.place(t, peer)
            _, _ -> t
          }
          let m = Membership(..put_peer(m, peer, p), topology: t, dirty: True)
          let #(m, drained) = case draining, p.state {
            True, Draining | False, Joining(_) | False, Gone(_) -> #(m, [])
            True, _ -> drain(m, peer, now)
            False, Draining -> undrain(m, peer, now)
            False, Up | False, Suspect(_) -> #(m, [])
          }
          let reply = case wants_reply && !list.any(first, is_send_info) {
            True -> [SendInfo(peer, m.draining, False)]
            False -> []
          }
          #(m, list.flatten([first, drained, reply]))
        }
      }
    }
  }
}

fn is_send_info(decision: Decision) -> Bool {
  case decision {
    SendInfo(..) -> True
    _ -> False
  }
}

fn dns(m: Membership, nodes: List(Node)) -> #(Membership, List(Decision)) {
  let answer = set.from_list(nodes) |> set.delete(m.self)
  let expected = topology.expected(m.topology)
  let misses =
    set.fold(expected, m.dns_misses, fn(acc, n) {
      case set.contains(answer, n) || n == m.self {
        True -> dict.delete(acc, n)
        False -> dict.insert(acc, n, miss_count(acc, n) + 1)
      }
    })
  let kept =
    set.filter(expected, fn(n) {
      miss_count(misses, n) < m.config.dns_misses_to_drop
    })
  let next = set.union(kept, answer) |> set.insert(m.self)
  let misses = dict.filter(misses, fn(n, _) { set.contains(next, n) })
  let unseen =
    answer
    |> set.to_list
    |> list.filter(fn(n) { !dict.has_key(m.peers, n) })
    |> sort_nodes
    |> list.map(Reconnect)
  case next == expected {
    True -> #(Membership(..m, dns_misses: misses), unseen)
    False -> #(
      Membership(
        ..m,
        dns_misses: misses,
        topology: topology.set_expected(m.topology, next),
        dirty: True,
      ),
      unseen,
    )
  }
}

fn settle_join(m: Membership, now: Int) -> #(Membership, Bool) {
  case topology.joined(m.topology) || !joined_cluster(m, now) {
    True -> #(m, False)
    False -> #(
      Membership(..m, topology: topology.mark_joined(m.topology), dirty: True),
      True,
    )
  }
}

fn joined_cluster(m: Membership, now: Int) -> Bool {
  let settled =
    m.answered
    && !list.any(dict.values(m.peers), fn(p) {
      case p.state {
        Joining(_) -> True
        _ -> False
      }
    })
    && set.fold(topology.expected(m.topology), True, fn(all, peer) {
      all && { peer == m.self || dict.has_key(m.peers, peer) }
    })
  settled || now - m.born >= m.config.joining_stable_ms * join_deadline_factor
}

const join_deadline_factor = 3

fn miss_count(misses: Dict(Node, Int), n: Node) -> Int {
  result.unwrap(dict.get(misses, n), 0)
}

fn probe_reply(
  m: Membership,
  target: Node,
  from: Node,
  reachable: Bool,
  now: Int,
) -> #(Membership, List(Decision)) {
  case dict.get(m.rounds, target), dict.get(m.peers, target) {
    Ok(round), Ok(Peer(state: Suspect(_), ..) as p) ->
      case list.contains(round.asked, from), reachable {
        False, _ -> #(m, [])
        True, True -> {
          let #(gone_in, generator) =
            rng.jitter(m.rng, gone_after(m, p), m.config.timer_jitter_percent)
          let m =
            Membership(
              ..put_peer(m, target, Peer(..p, gone_at: now + gone_in)),
              rounds: dict.delete(m.rounds, target),
              rng: generator,
            )
          #(m, [Count(GoneVetoed(target))])
        }
        True, False -> {
          let unreachable = set.insert(round.unreachable, from)
          case set.size(unreachable) * 2 > list.length(round.asked) {
            True -> decide_gone(m, target, now)
            False -> #(
              Membership(
                ..m,
                rounds: dict.insert(
                  m.rounds,
                  target,
                  Round(..round, unreachable:),
                ),
              ),
              [],
            )
          }
        }
      }
    _, _ -> #(m, [])
  }
}

fn tick(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  let #(m, forgotten) = forget_due(m, now)
  let #(m, reconnects) = reconnect_due(m, now)
  let #(m, probes) = gone_due(m, now)
  let #(m, joins) = joining_due(m, now)
  let #(m, hellos) = hello_due(m, now)
  let #(m, published) = case list.any(joins, is_move_out) {
    True -> maybe_publish(m, now, True)
    False -> #(m, [])
  }
  #(m, list.flatten([forgotten, reconnects, probes, published, joins, hellos]))
}

fn hello_due(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  let due =
    dict.fold(m.peers, [], fn(acc, peer, p) {
      case p.state, p.ready_at {
        Joining(_), None if now - p.hello_at >= m.config.hello_resend_ms -> [
          peer,
          ..acc
        ]
        _, _ -> acc
      }
    })
  list.fold(sort_nodes(due), #(m, []), fn(acc, peer) {
    let #(m, out) = acc
    let #(m, hello) = send_info(m, peer, now)
    #(m, list.append(out, hello))
  })
}

fn is_move_out(decision: Decision) -> Bool {
  case decision {
    MoveOut(..) -> True
    _ -> False
  }
}

fn forget_due(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  let expected = topology.expected(m.topology)
  let stale =
    dict.fold(m.peers, [], fn(acc, peer, p) {
      case p.state {
        Gone(since) if now - since >= m.config.forget_after_ms -> [peer, ..acc]
        _ -> acc
      }
    })
    |> list.filter(fn(peer) { !set.contains(expected, peer) })
    |> sort_nodes
  case stale {
    [] -> #(m, [])
    _ -> {
      let m =
        list.fold(stale, m, fn(m, peer) {
          Membership(
            ..m,
            peers: dict.delete(m.peers, peer),
            dns_misses: dict.delete(m.dns_misses, peer),
            rounds: dict.delete(m.rounds, peer),
            topology: topology.forget(m.topology, peer),
            dirty: True,
          )
        })
      #(m, list.map(stale, Forget))
    }
  }
}

fn round_deadline_ms(m: Membership) -> Int {
  m.config.probe_timeout_ms * 2 + tick_margin_ms()
}

fn reconnect_due(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  dict.fold(m.peers, #(m, []), fn(acc, peer, p) {
    let #(m, out) = acc
    case p.state {
      Suspect(since) if now >= p.next_reconnect -> {
        let #(low, high) = case now - since < m.config.gone_after_ms {
          True -> #(m.config.reconnect_min_ms, m.config.reconnect_max_ms)
          False -> #(m.config.reconnect_max_ms, m.config.reconnect_cap_ms)
        }
        let #(wait, generator) = rng.uniform(m.rng, low, high)
        let m =
          Membership(
            ..put_peer(m, peer, Peer(..p, next_reconnect: now + wait)),
            rng: generator,
          )
        #(m, [Reconnect(peer), ..out])
      }
      Gone(_) if now >= p.next_reconnect ->
        case set.contains(topology.expected(m.topology), peer) {
          False -> acc
          True -> {
            let #(wait, generator) =
              rng.jitter(
                m.rng,
                m.config.gone_reconnect_ms,
                m.config.timer_jitter_percent,
              )
            let m =
              Membership(
                ..put_peer(m, peer, Peer(..p, next_reconnect: now + wait)),
                rng: generator,
              )
            #(m, [Reconnect(peer), ..out])
          }
        }
      _ -> acc
    }
  })
}

fn gone_due(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  let due =
    dict.fold(m.peers, [], fn(acc, peer, p) {
      case p.state {
        Suspect(_) if now >= p.gone_at -> [peer, ..acc]
        _ -> acc
      }
    })
  let deadline = round_deadline_ms(m)
  list.fold(sort_nodes(due), #(m, []), fn(acc, peer) {
    let #(m, out) = acc
    case dict.get(m.rounds, peer) {
      Ok(round) if now - round.started < deadline -> acc
      _ -> {
        let helpers =
          dict.fold(m.peers, [], fn(found, n, q) {
            case q.state, serves_guilds(q.role), n == peer {
              Up, True, False -> [n, ..found]
              _, _, _ -> found
            }
          })
          |> sort_nodes
        let #(via, generator) = rng.pick(m.rng, helpers, m.config.probe_helpers)
        let m = Membership(..m, rng: generator)
        case via {
          [] -> {
            let #(m, decided) = decide_gone(m, peer, now)
            #(m, list.append(out, decided))
          }
          _ -> {
            let round = Round(via, now, set.new())
            #(
              Membership(..m, rounds: dict.insert(m.rounds, peer, round)),
              list.append(out, [Probe(peer, via)]),
            )
          }
        }
      }
    }
  })
}

fn serves_guilds(role: Option(Role)) -> Bool {
  case role {
    Some(found) -> node.runs(found, node.Guilds)
    None -> False
  }
}

fn decide_gone(
  m: Membership,
  peer: Node,
  now: Int,
) -> #(Membership, List(Decision)) {
  let m = gone(m, peer, now)
  #(m, [Count(PeerStateChanged(peer, Gone(now))), MoveIn(peer, MovedGone)])
}

fn gone(m: Membership, peer: Node, now: Int) -> Membership {
  let m =
    update_peer(m, peer, fn(p) {
      Peer(
        ..p,
        state: Gone(now),
        ready_at: None,
        next_reconnect: now + m.config.gone_reconnect_ms,
      )
    })
  Membership(
    ..m,
    topology: topology.remove(m.topology, peer),
    rounds: dict.delete(m.rounds, peer),
    dirty: True,
  )
}

fn joining_due(m: Membership, now: Int) -> #(Membership, List(Decision)) {
  dict.fold(m.peers, #(m, []), fn(acc, peer, p) {
    let #(m, out) = acc
    let stable = case flapping(m, recent(m, p.transitions, now)) {
      True -> m.config.flap_stable_ms
      False -> m.config.joining_stable_ms
    }
    case p.state, p.ready_at {
      Joining(_), Some(ready) if now - ready >= stable -> {
        let t = topology.place(m.topology, peer)
        let m =
          Membership(
            ..put_peer(m, peer, Peer(..p, state: Up)),
            topology: t,
            dirty: True,
          )
        let moves = case list.contains(topology.serving(t), peer) {
          True -> [MoveOut([peer], MovedJoin), Count(Moves(MovedJoin, 1))]
          False -> []
        }
        #(m, list.flatten([out, [Count(PeerStateChanged(peer, Up))], moves]))
      }
      _, _ -> acc
    }
  })
}

fn maybe_publish(
  m: Membership,
  now: Int,
  force: Bool,
) -> #(Membership, List(Decision)) {
  case
    m.dirty && { force || now - m.last_publish >= m.config.publish_interval_ms }
  {
    False -> #(m, [])
    True -> {
      let t = topology.bump(m.topology)
      let m = Membership(..m, topology: t, dirty: False, last_publish: now)
      #(m, [
        Publish(t),
        Count(TopologyPublished),
        Count(MembersChanged(set.size(topology.members(t)))),
      ])
    }
  }
}

fn gone_after(m: Membership, p: Peer) -> Int {
  case flapping(m, p.transitions) {
    True -> m.config.gone_after_ms * 2
    False -> m.config.gone_after_ms
  }
}

fn flapping(m: Membership, transitions: List(Int)) -> Bool {
  list.length(transitions) > m.config.flap_transitions
}

fn recent(m: Membership, transitions: List(Int), now: Int) -> List(Int) {
  list.filter(transitions, fn(at) { now - at <= m.config.flap_window_ms })
}

fn put_peer(m: Membership, peer: Node, p: Peer) -> Membership {
  Membership(..m, peers: dict.insert(m.peers, peer, p))
}

fn update_peer(m: Membership, peer: Node, f: fn(Peer) -> Peer) -> Membership {
  case dict.get(m.peers, peer) {
    Ok(p) -> put_peer(m, peer, f(p))
    Error(Nil) -> m
  }
}

fn sort_nodes(nodes: List(Node)) -> List(Node) {
  list.sort(nodes, fn(a, b) {
    string.compare(node.to_string(a), node.to_string(b))
  })
}
