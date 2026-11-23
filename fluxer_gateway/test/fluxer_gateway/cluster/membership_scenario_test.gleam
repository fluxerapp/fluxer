// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/membership/state
import fluxer_gateway/cluster/topology
import fluxer_gateway/core/node
import gleam/int
import gleam/list
import gleam/option.{Some}
import support/cluster_sim.{type Sim} as sim

fn keys() -> List(Int) {
  int.range(from: 300, to: 0, with: [], run: list.prepend)
}

fn a() {
  sim.node("a")
}

fn b() {
  sim.node("b")
}

fn c() {
  sim.node("c")
}

fn d() {
  sim.node("d")
}

fn cluster(names: List(node.Node)) -> Sim {
  names
  |> list.fold(sim.new(), fn(s, at) { sim.boot(s, at, node.Guilds, []) })
  |> sim.discover_all
  |> sim.run_for(15_000)
}

fn all_up(s: Sim, names: List(node.Node)) -> Bool {
  list.all(names, fn(at) {
    list.all(names, fn(peer) {
      peer == at || sim.peer_state(s, at, peer) == Some(state.Up)
    })
  })
}

pub fn three_nodes_handshake_and_agree_test() {
  let s = cluster([a(), b(), c()])
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
  assert list.all([a(), b(), c()], fn(at) {
    list.all([a(), b(), c()], fn(peer) { sim.is_up(s, at)(peer) })
  })
}

pub fn joiner_is_placed_everywhere_and_takes_keys_test() {
  let s = cluster([a(), b(), c()])
  let s =
    sim.boot(s, d(), node.Guilds, [])
    |> sim.discover(d(), [a(), b(), c(), d()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  let s = sim.run_for(s, 15_000)
  assert all_up(s, [a(), b(), c(), d()])
  assert sim.serving(s, b()) == [a(), b(), c(), d()]
  assert sim.agree(s, keys())
  assert list.contains(
    s.moves,
    sim.Out(at: a(), targets: [d()], reason: state.MovedJoin),
  )
}

pub fn hello_lost_during_membership_restart_is_resent_test() {
  let s =
    [a(), b()]
    |> list.fold(sim.new(), fn(s, at) { sim.boot(s, at, node.Guilds, []) })
    |> sim.discover(a(), [a(), b()])
  let s = sim.crash(s, b()) |> sim.boot(b(), node.Guilds, [])
  let s = sim.discover(s, b(), [a(), b()]) |> sim.run_for(20_000)
  assert all_up(s, [a(), b()])
  assert sim.agree(s, keys())
}

pub fn drain_removes_node_everywhere_and_moves_out_test() {
  let s = cluster([a(), b(), c()])
  let before = list.length(s.moves)
  let s = sim.drain(s, c())
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.serving(s, b()) == [a(), b()]
  assert sim.serving(s, c()) == [a(), b()]
  assert sim.agree(s, keys())
  assert sim.drain_targets(s, c()) == Ok([a(), b()])
  let moves = sim.moves_since(s, before)
  assert list.contains(moves, sim.Out(c(), [a(), b()], state.MovedDraining))
  assert list.contains(moves, sim.In(a(), c(), state.MovedDraining))
  let s = sim.run_for(s, 30_000)
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.agree(s, keys())
}

pub fn undrain_places_node_again_everywhere_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c())
  let before = list.length(s.moves)
  let s = sim.undrain(s, c())
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
  assert sim.drain_targets(s, c()) == Error(Nil)
  assert list.contains(
    sim.moves_since(s, before),
    sim.Out(a(), [c()], state.MovedJoin),
  )
  let s = sim.run_for(s, 30_000)
  assert sim.serving(s, b()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn late_joiner_never_places_a_draining_node_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c())
  let s =
    sim.boot(s, d(), node.Guilds, [])
    |> sim.discover(d(), [a(), b(), c(), d()])
  assert !list.contains(sim.serving(s, d()), c())
  let s = sim.run_for(s, 20_000)
  assert !list.contains(sim.serving(s, d()), c())
  assert sim.serving(s, d()) == [a(), b(), d()]
  assert sim.agree(s, keys())
}

pub fn drain_while_partitioned_is_learned_on_heal_test() {
  let s = cluster([a(), b(), c()]) |> sim.partition(a(), c())
  let s = sim.drain(s, c())
  assert sim.serving(s, b()) == [a(), b()]
  let s = sim.heal(s, a(), c()) |> sim.run_for(20_000)
  assert !list.contains(sim.serving(s, a()), c())
  assert sim.agree(s, keys())
}

pub fn undrain_while_partitioned_is_learned_on_heal_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c())
  let s = sim.partition(s, a(), c()) |> sim.undrain(c())
  assert list.contains(sim.serving(s, b()), c())
  let s = sim.heal(s, a(), c()) |> sim.run_for(20_000)
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn node_down_suspects_then_gone_and_keys_move_test() {
  let s = cluster([a(), b(), c()])
  let s = sim.isolate(s, c())
  assert sim.peer_state(s, a(), c()) == Some(state.Suspect(s.now))
  assert !sim.is_up(s, a())(c())
  assert sim.serving(s, a()) == [a(), b(), c()]
  let before = list.length(s.moves)
  let s = sim.run_for(s, 45_000)
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.serving(s, b()) == [a(), b()]
  assert list.contains(
    sim.moves_since(s, before),
    sim.In(a(), c(), state.MovedGone),
  )
  let assert Ok(seen_by_c) = sim.published(s, c())
  assert !topology.has_majority(seen_by_c, sim.is_up(s, c()))
  let assert Ok(seen_by_a) = sim.published(s, a())
  assert topology.has_majority(seen_by_a, sim.is_up(s, a()))
}

pub fn partitioned_node_rejoins_after_heal_test() {
  let s = cluster([a(), b(), c()]) |> sim.isolate(c()) |> sim.run_for(45_000)
  let s = sim.heal_all(s) |> sim.run_for(30_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn short_blip_does_not_move_keys_test() {
  let s = cluster([a(), b(), c()])
  let before = list.length(s.moves)
  let s = sim.isolate(s, c()) |> sim.run_for(3000) |> sim.heal_all
  let s = sim.run_for(s, 10_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.moves_since(s, before) == []
  assert sim.agree(s, keys())
}

pub fn crashed_node_reboots_and_rejoins_test() {
  let s = cluster([a(), b(), c()]) |> sim.crash(c())
  assert sim.peer_state(s, a(), c()) == Some(state.Suspect(s.now))
  let s =
    sim.boot(s, c(), node.Guilds, [])
    |> sim.discover(c(), [a(), b(), c()])
    |> sim.run_for(20_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn crashed_draining_node_is_gone_at_once_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c()) |> sim.crash(c())
  let assert Some(state.Gone(_)) = sim.peer_state(s, a(), c())
  assert sim.serving(s, a()) == [a(), b()]
  let s =
    sim.boot(s, c(), node.Guilds, [])
    |> sim.discover(c(), [a(), b(), c()])
    |> sim.run_for(20_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn peer_membership_restart_keeps_drain_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c())
  let s = sim.restart_membership(s, a())
  assert sim.serving(s, a()) == [a(), b()]
  let s = sim.run_for(s, 20_000)
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.agree(s, keys())
}

pub fn draining_node_membership_restart_stays_draining_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c())
  let s = sim.restart_membership(s, c())
  assert sim.drain_targets(s, c()) == Ok([a(), b()])
  let s = sim.run_for(s, 20_000)
  assert sim.serving(s, c()) == [a(), b()]
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.agree(s, keys())
  let s = sim.undrain(s, c()) |> sim.run_for(5000)
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn membership_restart_keeps_placement_and_liveness_test() {
  let s = cluster([a(), b(), c()])
  let s = sim.restart_membership(s, b())
  assert sim.serving(s, b()) == [a(), b(), c()]
  assert sim.is_up(s, b())(a())
  let s = sim.run_for(s, 20_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn membership_restart_during_partition_test() {
  let s = cluster([a(), b(), c()]) |> sim.isolate(c())
  let s = sim.restart_membership(s, a()) |> sim.run_for(45_000)
  assert sim.serving(s, a()) == [a(), b()]
  let s = sim.heal_all(s) |> sim.run_for(30_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn non_guild_node_is_never_placed_test() {
  let s =
    sim.new()
    |> sim.boot(a(), node.Guilds, [])
    |> sim.boot(b(), node.Guilds, [])
    |> sim.boot(c(), node.Sessions, [])
    |> sim.discover_all
    |> sim.run_for(15_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.serving(s, c()) == [a(), b()]
  assert sim.agree(s, keys())
  let assert Ok(seen) = sim.published(s, a())
  assert topology.role_nodes(seen, node.Sessions) == [c()]
}

pub fn pinned_guild_is_owned_by_its_pin_everywhere_test() {
  let s =
    sim.new()
    |> sim.boot(a(), node.Guilds, [])
    |> sim.boot(b(), node.Guilds, [])
    |> sim.boot(c(), node.Guilds, [42])
    |> sim.discover_all
    |> sim.run_for(15_000)
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.owners(s, a(), [42]) == [Ok(c())]
  assert sim.owners(s, b(), [42]) == [Ok(c())]
  assert sim.owners(s, c(), [42]) == [Ok(c())]
}

pub fn one_sided_partition_is_vetoed_by_a_helper_test() {
  let s = cluster([a(), b(), c()]) |> sim.partition(a(), c())
  let before = list.length(s.moves)
  let s = sim.run_for(s, 120_000)
  let assert Some(state.Suspect(_)) = sim.peer_state(s, a(), c())
  assert sim.peer_state(s, b(), c()) == Some(state.Up)
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
  assert !list.any(sim.moves_since(s, before), fn(move) {
    case move {
      sim.In(_, _, state.MovedGone) -> True
      _ -> False
    }
  })
  let s = sim.heal_all(s) |> sim.run_for(5000)
  assert all_up(s, [a(), b(), c()])
}

pub fn two_nodes_drain_together_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(b()) |> sim.drain(c())
  assert sim.serving(s, a()) == [a()]
  assert sim.serving(s, b()) == [a()]
  assert sim.serving(s, c()) == [a()]
  let s = sim.run_for(s, 20_000) |> sim.undrain(b())
  assert sim.serving(s, c()) == [a(), b()]
  assert sim.agree(s, keys())
}

pub fn drain_during_join_keeps_drained_node_out_test() {
  let s = cluster([a(), b(), c()])
  let s =
    sim.boot(s, d(), node.Guilds, [])
    |> sim.discover(d(), [a(), b(), c(), d()])
    |> sim.run_for(2000)
    |> sim.drain(c())
  let s = sim.run_for(s, 20_000)
  assert sim.serving(s, d()) == [a(), b(), d()]
  assert sim.agree(s, keys())
}

fn key_owned_by(s: Sim, at: node.Node, owner: node.Node) -> Int {
  let assert Ok(seen) = sim.published(s, at)
  let assert Ok(found) =
    list.find(keys(), fn(key) { topology.guild_owner(seen, key) == Ok(owner) })
  found
}

fn start_allowed(s: Sim, at: node.Node, key: Int) -> Bool {
  let assert Ok(seen) = sim.published(s, at)
  topology.start_allowed(seen, sim.is_up(s, at), key)
}

pub fn drained_node_that_leaves_does_not_block_its_old_keys_test() {
  let s = cluster([b(), c()])
  let key = key_owned_by(s, b(), c())
  let s = sim.drain(s, c())
  assert start_allowed(s, b(), key)
  let s = sim.crash(s, c()) |> sim.discover(b(), [b(), c()])
  let s = sim.run_for(s, 30_000) |> sim.discover(b(), [b(), c()])
  assert sim.serving(s, b()) == [b()]
  assert start_allowed(s, b(), key)
  let s = sim.restart_membership(s, b())
  assert start_allowed(s, b(), key)
  let s =
    sim.boot(s, c(), node.Guilds, [])
    |> sim.discover(c(), [b(), c()])
    |> sim.run_for(20_000)
  assert all_up(s, [b(), c()])
  assert sim.serving(s, b()) == [b(), c()]
  assert sim.agree(s, keys())
}

pub fn crashed_node_still_blocks_its_keys_without_majority_test() {
  let s = cluster([b(), c()])
  let key = key_owned_by(s, b(), c())
  let s = sim.crash(s, c()) |> sim.run_for(45_000)
  assert sim.serving(s, b()) == [b()]
  assert !start_allowed(s, b(), key)
}

pub fn drained_and_departed_node_does_not_count_for_majority_test() {
  let s = cluster([a(), b(), c()]) |> sim.drain(c()) |> sim.crash(c())
  let s = sim.partition(s, a(), b()) |> sim.run_for(45_000)
  let key = key_owned_by(cluster([a(), b(), c()]), a(), b())
  let assert Ok(seen) = sim.published(s, a())
  assert !topology.has_majority(seen, sim.is_up(s, a()))
  assert !start_allowed(s, a(), key)
}
