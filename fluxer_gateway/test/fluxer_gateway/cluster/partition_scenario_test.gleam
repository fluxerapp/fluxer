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

fn rediscover(s: Sim) -> Sim {
  sim.discover_all(s)
}

fn converge(s: Sim) -> Sim {
  s |> rediscover |> sim.run_for(40_000) |> rediscover |> sim.run_for(20_000)
}

pub fn isolated_node_and_majority_both_converge_after_heal_test() {
  let s = cluster([a(), b(), c()]) |> sim.isolate(a()) |> sim.run_for(60_000)
  let assert Some(state.Gone(_)) = sim.peer_state(s, b(), a())
  let assert Some(state.Gone(_)) = sim.peer_state(s, c(), a())
  assert sim.serving(s, b()) == [b(), c()]
  assert sim.serving(s, c()) == [b(), c()]
  let assert Ok(seen) = sim.published(s, a())
  assert !topology.has_majority(seen, sim.is_up(s, a()))
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn every_link_cut_then_healed_converges_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.partition(a(), b())
    |> sim.partition(b(), c())
    |> sim.partition(a(), c())
    |> sim.run_for(60_000)
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn flapping_link_settles_once_stable_test() {
  let s = cluster([a(), b(), c()])
  let s =
    int.range(from: 0, to: 8, with: s, run: fn(s, _) {
      s
      |> sim.partition(a(), c())
      |> sim.run_for(3000)
      |> sim.heal(a(), c())
      |> sim.run_for(3000)
    })
  let s = converge(s) |> sim.run_for(150_000)
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn drain_announced_during_partition_then_crash_and_reboot_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.partition(a(), c())
    |> sim.drain(c())
  assert sim.serving(s, b()) == [a(), b()]
  let s = sim.crash(s, c()) |> sim.run_for(45_000)
  assert sim.serving(s, a()) == [a(), b()]
  let s = sim.heal_all(s) |> sim.boot(c(), node.Guilds, []) |> converge
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn drained_node_partitioned_away_is_not_placed_again_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.drain(c())
    |> sim.isolate(c())
    |> sim.run_for(60_000)
  assert sim.serving(s, a()) == [a(), b()]
  let s = sim.heal_all(s) |> converge
  assert sim.serving(s, a()) == [a(), b()]
  assert sim.serving(s, b()) == [a(), b()]
  assert sim.drain_targets(s, c()) != Error(Nil)
}

pub fn undrain_after_partition_heals_is_seen_everywhere_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.drain(c())
    |> sim.isolate(c())
    |> sim.run_for(60_000)
    |> sim.undrain(c())
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn membership_restart_on_every_node_after_partition_converges_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.isolate(b())
    |> sim.run_for(45_000)
    |> sim.restart_membership(a())
    |> sim.restart_membership(b())
    |> sim.restart_membership(c())
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn quick_reboot_inside_the_gone_window_rejoins_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.crash(c())
    |> sim.run_for(2000)
    |> sim.boot(c(), node.Guilds, [])
  let s = converge(s)
  assert all_up(s, [a(), b(), c()])
  assert sim.agree(s, keys())
}

pub fn reboot_while_draining_comes_back_serving_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.drain(c())
    |> sim.crash(c())
    |> sim.run_for(1000)
    |> sim.boot(c(), node.Guilds, [])
  let s = converge(s)
  assert all_up(s, [a(), b(), c()])
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}

pub fn two_node_split_keeps_foreign_keys_blocked_then_heals_test() {
  let s = cluster([b(), c()])
  let assert Ok(seen) = sim.published(s, b())
  let assert Ok(key) =
    list.find(keys(), fn(key) { topology.guild_owner(seen, key) == Ok(c()) })
  let s = sim.partition(s, b(), c()) |> sim.run_for(60_000)
  assert sim.serving(s, b()) == [b()]
  assert sim.serving(s, c()) == [c()]
  let assert Ok(split) = sim.published(s, b())
  assert !topology.start_allowed(split, sim.is_up(s, b()), key)
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [b(), c()])
  assert sim.serving(s, b()) == [b(), c()]
  assert sim.agree(s, keys())
}

pub fn node_joining_during_a_partition_is_placed_after_heal_test() {
  let d = sim.node("d")
  let s =
    cluster([a(), b(), c()])
    |> sim.partition(a(), c())
    |> sim.boot(d, node.Guilds, [])
    |> sim.discover(d, [a(), b(), c(), d])
    |> sim.run_for(30_000)
  let s = sim.heal_all(s) |> converge
  assert all_up(s, [a(), b(), c(), d])
  assert sim.serving(s, a()) == [a(), b(), c(), d]
  assert sim.agree(s, keys())
}

pub fn drain_and_undrain_racing_a_partition_converge_test() {
  let s =
    cluster([a(), b(), c()])
    |> sim.partition(a(), b())
    |> sim.drain(b())
    |> sim.run_for(1000)
    |> sim.undrain(b())
    |> sim.run_for(1000)
    |> sim.drain(b())
  let s = sim.heal_all(s) |> converge
  assert sim.serving(s, a()) == [a(), c()]
  assert sim.serving(s, c()) == [a(), c()]
  assert sim.agree(s, keys())
  let s = sim.undrain(s, b()) |> converge
  assert sim.serving(s, a()) == [a(), b(), c()]
  assert sim.agree(s, keys())
}
