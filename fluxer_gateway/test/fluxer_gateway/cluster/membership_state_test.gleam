// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/membership/state
import fluxer_gateway/cluster/topology
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom
import gleam/list
import gleam/option.{None, Some}
import gleam/set

fn peer(name: String) -> Node {
  node.from_atom(atom.create(name))
}

fn fresh() -> state.Membership {
  state.new(peer("a@h"), node.Guilds, [], state.default_config())
}

fn step_all(
  start: state.Membership,
  events: List(#(state.Event, Int)),
) -> #(state.Membership, List(state.Decision)) {
  list.fold(events, #(start, []), fn(acc, entry) {
    let #(membership, decisions) = acc
    let #(event, at) = entry
    let #(membership, more) = state.step(membership, event, at)
    #(membership, list.append(decisions, more))
  })
}

fn joined(name: String, at: Int) -> state.Membership {
  let #(membership, _) =
    step_all(fresh(), [
      #(state.NodeUp(peer(name)), at),
      #(state.PeerInfo(peer(name), node.Guilds, [], False, False), at),
      #(state.Tick(at + 10_000), at + 10_000),
    ])
  membership
}

pub fn nodeup_joins_and_introduces_test() {
  let #(membership, decisions) =
    state.step(fresh(), state.NodeUp(peer("b@h")), 0)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Joining(0))
  assert list.contains(decisions, state.SetLiveness(peer("b@h"), True))
  assert list.contains(decisions, state.SendInfo(peer("b@h"), False, True))
}

pub fn ready_peer_becomes_up_and_takes_keys_test() {
  let #(membership, decisions) =
    step_all(fresh(), [
      #(state.NodeUp(peer("b@h")), 0),
      #(state.PeerInfo(peer("b@h"), node.Guilds, [], False, False), 0),
      #(state.Tick(5000), 5000),
      #(state.Tick(10_000), 10_000),
    ])
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
  assert list.contains(decisions, state.MoveOut([peer("b@h")], state.MovedJoin))
  assert list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
}

pub fn nodedown_only_suspects_test() {
  let membership = joined("b@h", 0)
  let #(membership, decisions) =
    state.step(
      membership,
      state.NodeDown(peer("b@h"), state.NetTickTimeout),
      20_000,
    )
  assert state.peer_state(membership, peer("b@h"))
    == Some(state.Suspect(20_000))
  assert list.contains(decisions, state.SetLiveness(peer("b@h"), False))
  assert list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
  let #(membership, _) =
    state.step(membership, state.NodeUp(peer("b@h")), 21_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
}

pub fn suspect_reconnects_test() {
  let membership = joined("b@h", 0)
  let #(membership, _) =
    state.step(
      membership,
      state.NodeDown(peer("b@h"), state.OtherReason),
      20_000,
    )
  let #(_, decisions) = state.step(membership, state.Tick(23_000), 23_000)
  assert list.contains(decisions, state.Reconnect(peer("b@h")))
}

pub fn gone_without_helpers_test() {
  let membership = joined("b@h", 0)
  let #(membership, _) =
    state.step(
      membership,
      state.NodeDown(peer("b@h"), state.OtherReason),
      20_000,
    )
  let #(membership, decisions) =
    state.step(membership, state.Tick(80_000), 80_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Gone(80_000))
  assert list.contains(decisions, state.MoveIn(peer("b@h"), state.MovedGone))
  assert !list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
}

pub fn indirect_probe_veto_test() {
  let #(membership, _) =
    step_all(fresh(), [
      #(state.NodeUp(peer("b@h")), 0),
      #(state.PeerInfo(peer("b@h"), node.Guilds, [], False, False), 0),
      #(state.NodeUp(peer("c@h")), 0),
      #(state.PeerInfo(peer("c@h"), node.Guilds, [], False, False), 0),
      #(state.Tick(10_000), 10_000),
      #(state.NodeDown(peer("b@h"), state.OtherReason), 20_000),
    ])
  let #(membership, decisions) =
    state.step(membership, state.Tick(80_000), 80_000)
  assert list.contains(decisions, state.Probe(peer("b@h"), [peer("c@h")]))
  let #(vetoed, _) =
    state.step(
      membership,
      state.ProbeReply(peer("b@h"), peer("c@h"), True),
      80_500,
    )
  assert state.peer_state(vetoed, peer("b@h")) == Some(state.Suspect(20_000))
  let #(confirmed, _) =
    state.step(
      membership,
      state.ProbeReply(peer("b@h"), peer("c@h"), False),
      80_500,
    )
  assert state.peer_state(confirmed, peer("b@h")) == Some(state.Gone(80_500))
}

pub fn drain_self_moves_out_test() {
  let membership = joined("b@h", 0)
  let #(_, decisions) =
    state.step(membership, state.DrainAnnounced(peer("a@h")), 30_000)
  assert list.contains(
    decisions,
    state.MoveOut([peer("b@h")], state.MovedDraining),
  )
  assert list.any(decisions, fn(decision) {
    case decision {
      state.Publish(_) -> True
      _ -> False
    }
  })
}

pub fn draining_peer_leaves_placement_test() {
  let membership = joined("b@h", 0)
  let #(membership, _) =
    state.step(membership, state.DrainAnnounced(peer("b@h")), 30_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Draining)
  assert !list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
}

pub fn dns_answer_reconnects_unseen_test() {
  let #(_, decisions) =
    state.step(fresh(), state.DnsAnswer([peer("b@h"), peer("a@h")]), 0)
  assert list.contains(decisions, state.Reconnect(peer("b@h")))
  assert !list.contains(decisions, state.Reconnect(peer("a@h")))
}

fn expects(membership: state.Membership, name: String) -> Bool {
  set.contains(
    topology.expected(state.current_topology(membership)),
    peer(name),
  )
}

pub fn repeated_dns_answer_drops_and_forgets_removed_node_test() {
  let #(membership, _) =
    step_all(joined("b@h", 0), [
      #(state.DnsAnswer([peer("b@h")]), 1000),
      #(state.NodeDown(peer("b@h"), state.OtherReason), 20_000),
      #(state.Tick(80_000), 80_000),
    ])
  assert state.peer_state(membership, peer("b@h")) == Some(state.Gone(80_000))
  assert expects(membership, "b@h")
  let #(membership, _) = state.step(membership, state.DnsAnswer([]), 81_000)
  assert expects(membership, "b@h")
  let #(membership, _) = state.step(membership, state.DnsAnswer([]), 86_000)
  assert !expects(membership, "b@h")
  let #(membership, decisions) =
    state.step(membership, state.Tick(680_000), 680_000)
  assert list.contains(decisions, state.Forget(peer("b@h")))
  assert !list.contains(decisions, state.Reconnect(peer("b@h")))
  assert state.peer_state(membership, peer("b@h")) == None
}

fn restart(
  previous: state.Membership,
  connected: List(String),
  draining: Bool,
  at: Int,
) -> #(state.Membership, List(state.Decision)) {
  state.start(
    peer("a@h"),
    node.Guilds,
    [],
    state.default_config(),
    Some(state.current_topology(previous)),
    list.map(connected, peer),
    draining,
    False,
    at,
  )
}

fn published(decisions: List(state.Decision)) -> List(topology.Topology) {
  list.filter_map(decisions, fn(decision) {
    case decision {
      state.Publish(found) -> Ok(found)
      _ -> Error(Nil)
    }
  })
}

pub fn restart_keeps_connected_peers_placed_test() {
  let previous = joined("b@h", 0)
  let #(membership, decisions) = restart(previous, ["b@h"], False, 20_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
  let assert [first] = published(decisions)
  assert topology.serving(first) == [peer("a@h"), peer("b@h")]
  assert topology.version(first)
    > topology.version(state.current_topology(previous))
  assert !topology.start_allowed(first, fn(_) { True }, guild_of(first, "b@h"))
}

fn guild_of(published: topology.Topology, name: String) -> Int {
  find_guild(published, name, 1)
}

fn find_guild(published: topology.Topology, name: String, key: Int) -> Int {
  case topology.guild_owner(published, key) == Ok(peer(name)) {
    True -> key
    False -> find_guild(published, name, key + 1)
  }
}

pub fn restart_suspects_disconnected_peer_test() {
  let #(membership, decisions) = restart(joined("b@h", 0), [], False, 20_000)
  assert state.peer_state(membership, peer("b@h"))
    == Some(state.Suspect(20_000))
  assert list.contains(decisions, state.SetLiveness(peer("b@h"), False))
  assert list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
}

pub fn restart_while_draining_stays_draining_test() {
  let #(previous, _) =
    state.step(joined("b@h", 0), state.DrainAnnounced(peer("a@h")), 30_000)
  let #(membership, decisions) = restart(previous, ["b@h"], True, 40_000)
  assert list.contains(
    decisions,
    state.MoveOut([peer("b@h")], state.MovedDraining),
  )
  assert state.drain_targets(membership) == Ok([peer("b@h")])
  assert list.all(published(decisions), fn(found) {
    topology.serving(found) == [peer("b@h")]
  })
  let #(_, hello) = state.step(membership, state.NodeUp(peer("b@h")), 40_001)
  assert list.contains(hello, state.SendInfo(peer("b@h"), True, True))
}

pub fn restart_restores_liveness_of_connected_peer_test() {
  let #(suspected, _) =
    state.step(
      joined("b@h", 0),
      state.NodeDown(peer("b@h"), state.ConnectionClosed),
      20_000,
    )
  let #(membership, decisions) = restart(suspected, ["b@h"], False, 21_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
  assert list.contains(decisions, state.SetLiveness(peer("b@h"), True))
  assert !list.contains(decisions, state.SetLiveness(peer("b@h"), False))
}

pub fn restart_learns_drain_missed_while_down_test() {
  let #(membership, _) = restart(joined("b@h", 0), ["b@h"], False, 20_000)
  let #(membership, hello) =
    state.step(membership, state.NodeUp(peer("b@h")), 20_001)
  assert list.contains(hello, state.SendInfo(peer("b@h"), False, True))
  let #(membership, _) =
    state.step(
      membership,
      state.PeerInfo(peer("b@h"), node.Guilds, [], True, False),
      20_002,
    )
  assert state.peer_state(membership, peer("b@h")) == Some(state.Draining)
  assert !list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
}

pub fn restart_learns_undrain_missed_while_down_test() {
  let #(previous, _) =
    state.step(joined("b@h", 0), state.DrainAnnounced(peer("b@h")), 30_000)
  let #(membership, _) = restart(previous, ["b@h"], False, 40_000)
  assert state.peer_state(membership, peer("b@h")) == Some(state.Draining)
  let #(membership, hello) =
    state.step(membership, state.NodeUp(peer("b@h")), 40_001)
  assert list.contains(hello, state.SendInfo(peer("b@h"), False, True))
  let #(membership, _) =
    state.step(
      membership,
      state.PeerInfo(peer("b@h"), node.Guilds, [], False, False),
      40_002,
    )
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
  assert list.contains(
    topology.serving(state.current_topology(membership)),
    peer("b@h"),
  )
  let #(_, again) = state.step(membership, state.NodeUp(peer("b@h")), 40_003)
  assert list.contains(again, state.SendInfo(peer("b@h"), False, False))
}

pub fn fresh_start_while_draining_announces_drain_test() {
  let #(membership, decisions) =
    state.start(
      peer("a@h"),
      node.Guilds,
      [],
      state.default_config(),
      None,
      [],
      True,
      False,
      0,
    )
  assert state.drain_targets(membership) == Ok([])
  assert list.all(published(decisions), fn(found) {
    topology.serving(found) == []
  })
  let #(_, hello) = state.step(membership, state.NodeUp(peer("b@h")), 1)
  assert list.contains(hello, state.SendInfo(peer("b@h"), True, True))
}

fn booted(discovering: Bool) -> state.Membership {
  let #(membership, _) =
    state.start(
      peer("a@h"),
      node.Guilds,
      [],
      state.default_config(),
      None,
      [],
      False,
      discovering,
      0,
    )
  membership
}

fn has_joined(membership: state.Membership) -> Bool {
  topology.joined(state.current_topology(membership))
}

pub fn node_without_discovery_has_joined_at_start_test() {
  assert has_joined(booted(False))
  assert !has_joined(booted(True))
}

pub fn booting_node_joins_once_its_discovered_peers_are_up_test() {
  let #(membership, _) =
    step_all(booted(True), [#(state.DnsAnswer([peer("b@h")]), 100)])
  assert !has_joined(membership)
  let #(membership, _) =
    step_all(membership, [
      #(state.NodeUp(peer("b@h")), 200),
      #(state.PeerInfo(peer("b@h"), node.Guilds, [], False, False), 200),
      #(state.Tick(5000), 5000),
    ])
  assert state.peer_state(membership, peer("b@h")) == Some(state.Joining(200))
  assert !has_joined(membership)
  let #(membership, decisions) =
    step_all(membership, [#(state.Tick(10_200), 10_200)])
  assert state.peer_state(membership, peer("b@h")) == Some(state.Up)
  assert has_joined(membership)
  assert list.any(published(decisions), topology.joined)
}

pub fn booting_node_joins_after_the_deadline_when_a_peer_never_answers_test() {
  let #(membership, _) =
    step_all(booted(True), [
      #(state.DnsAnswer([peer("b@h")]), 100),
      #(state.Tick(29_000), 29_000),
    ])
  assert !has_joined(membership)
  let #(membership, _) = step_all(membership, [#(state.Tick(30_000), 30_000)])
  assert has_joined(membership)
}
