// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/discovery
import fluxer_gateway/cluster/membership
import fluxer_gateway/cluster/router
import fluxer_gateway/cluster/topology
import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/option.{None, Some}
import gleam/set
import support/cluster_app
import support/cluster_fake_nats as fake
import support/isolation

type Key {
  FluxerGatewayClusterTopology
  FluxerGatewayClusterPeers
}

@external(erlang, "persistent_term", "erase")
fn erase(key: Key) -> Bool

fn start_discovery(peers: List(String)) -> Pid {
  let configured =
    settings.Settings(
      ..settings.get(),
      cluster_enabled: True,
      cluster_static_peers: peers,
      cluster_discovery_poll_interval_ms: 100,
    )
  let assert Ok(started) = discovery.supervised(configured).start()
  process.unlink(started.pid)
  started.pid
}

fn start_membership() -> Pid {
  let configured = settings.Settings(..settings.get(), cluster_enabled: True)
  let assert Ok(started) = membership.supervised(configured).start()
  process.unlink(started.pid)
  started.pid
}

fn expects(name: String) -> Bool {
  case router.published() {
    Some(published) ->
      set.contains(
        topology.expected(published),
        node.from_atom(atom.create(name)),
      )
    None -> False
  }
}

fn fast_publish() -> Nil {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      cluster: settings.ClusterDurations(
        ..d.cluster,
        membership_state_publish_interval_ms: 0,
      ),
    )
  })
}

fn cleanup() -> Nil {
  case cluster_app.registered("fluxer_gateway_cluster_membership") {
    Ok(pid) -> cluster_app.stop_linked(pid)
    Error(Nil) -> Nil
  }
  case cluster_app.registered("fluxer_gateway_cluster_discovery") {
    Ok(pid) -> cluster_app.stop_linked(pid)
    Error(Nil) -> Nil
  }
  erase(FluxerGatewayClusterTopology)
  erase(FluxerGatewayClusterPeers)
  Nil
}

pub fn membership_follows_a_restarted_discovery_test() {
  fast_publish()
  let first = start_discovery(["scn_first@127.0.0.1"])
  start_membership()
  assert fake.wait_until(3000, fn() { expects("scn_first@127.0.0.1") })
  cluster_app.stop_linked(first)
  start_discovery(["scn_second@127.0.0.1"])
  assert fake.wait_until(3000, fn() {
    expects("scn_second@127.0.0.1") && !expects("scn_first@127.0.0.1")
  })
  cleanup()
}

pub fn membership_started_before_discovery_subscribes_once_it_runs_test() {
  fast_publish()
  start_membership()
  process.sleep(200)
  start_discovery(["scn_late@127.0.0.1"])
  assert fake.wait_until(3000, fn() { expects("scn_late@127.0.0.1") })
  cleanup()
}

fn count_changes(
  inbox: process.Subject(discovery.PeersChanged),
  n: Int,
) -> Int {
  case process.receive(inbox, 0) {
    Ok(_) -> count_changes(inbox, n + 1)
    Error(Nil) -> n
  }
}

pub fn restarted_discovery_polls_on_a_single_timer_test() {
  let first = start_discovery(["scn_poll@127.0.0.1"])
  cluster_app.stop_linked(first)
  let second = start_discovery(["scn_poll@127.0.0.1"])
  cluster_app.stop_linked(second)
  let third = start_discovery(["scn_poll@127.0.0.1"])
  cluster_app.stop_linked(third)
  start_discovery(["scn_poll@127.0.0.1"])
  let inbox = process.new_subject()
  let assert Ok(_) = discovery.subscribe(inbox)
  process.sleep(1000)
  let polls = count_changes(inbox, 0)
  cleanup()
  assert polls <= 13
}
