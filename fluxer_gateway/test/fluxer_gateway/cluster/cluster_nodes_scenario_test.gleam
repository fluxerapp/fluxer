// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/cluster_relay_probe as probe

fn env(names: List(String)) -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_PORT", "0"),
    #("FLUXER_GATEWAY_CLUSTER_ENABLED", "true"),
    #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", string.join(names, ",")),
    #("FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", "500"),
    #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
    #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
    #("FLUXER_GATEWAY_LOGGER_LEVEL", "critical"),
  ]
}

fn boot(name: String, names: List(String)) -> Peer {
  let peer = peers.start(name, env(names))
  let started =
    peers.call(peer, "application", "ensure_all_started", [
      atom.to_dynamic(atom.create("fluxer_gateway")),
    ])
  let assert Ok("ok") =
    decode.run(
      started,
      decode.at([0], atom.decoder()) |> decode.map(atom.to_string),
    )
    as { "gateway failed to start on peer: " <> string.inspect(started) }
  peer
}

fn node_value(name: String) -> Dynamic {
  peers.to_dynamic(#(atom.create("node"), atom.create(name)))
}

fn nodes(found: Dynamic) -> List(String) {
  let assert Ok(names) =
    decode.run(
      found,
      decode.list(decode.at([1], atom.decoder() |> decode.map(atom.to_string))),
    )
  list.sort(names, string.compare)
}

fn serving(peer: Peer) -> List(String) {
  nodes(peers.call(peer, "fluxer_gateway@cluster@router", "serving", []))
}

fn members(peer: Peer) -> List(String) {
  nodes(peers.call(peer, "fluxer_gateway@cluster@router", "members", []))
}

fn is_up(peer: Peer, name: String) -> Bool {
  let found =
    peers.call(peer, "fluxer_gateway@cluster@liveness", "is_up", [
      node_value(name),
    ])
  decode.run(found, decode.bool) == Ok(True)
}

fn undrain(peer: Peer) -> Bool {
  let found = peers.call(peer, "fluxer_gateway@app@drain", "undrain", [])
  decode.run(
    found,
    decode.at([0], atom.decoder()) |> decode.map(atom.to_string),
  )
  == Ok("ok")
}

pub fn two_nodes_join_drain_undrain_and_detect_down_test() {
  let b = peers.unique_name("scn_b")
  let c = peers.unique_name("scn_c")
  let sorted = list.sort([b, c], string.compare)
  let peer_b = boot(b, [b, c])
  let peer_c = boot(c, [b, c])
  let port_c = peers.gateway_port(peer_c)
  assert peers.wait_until(25_000, fn() {
    serving(peer_b) == sorted && serving(peer_c) == sorted
  })
  assert is_up(peer_b, c)
  assert is_up(peer_c, b)
  assert members(peer_b) == sorted

  assert peers.http_get(peer_c, port_c, "/_health/ready")
    == Ok(#(503, "NATS_UNAVAILABLE"))
  assert peers.http_get(peer_c, port_c, "/_health/drain")
    == Ok(#(200, "DRAINING"))
  assert peers.wait_until(3000, fn() {
    serving(peer_b) == [b] && serving(peer_c) == [b]
  })
  assert members(peer_b) == sorted
  assert peers.http_get(peer_c, port_c, "/_health/ready")
    == Ok(#(503, "DRAINING"))

  assert peers.wait_until(10_000, fn() {
    peers.http_get(peer_c, port_c, "/_health/undrain") == Ok(#(200, "READY"))
  })
  assert peers.wait_until(3000, fn() {
    serving(peer_b) == sorted && serving(peer_c) == sorted
  })
  assert peers.http_get(peer_c, port_c, "/_health/ready")
    == Ok(#(503, "NATS_UNAVAILABLE"))

  peers.stop(peer_c)
  assert peers.wait_until(5000, fn() { !is_up(peer_b, c) })
  assert serving(peer_b) == sorted
  peers.stop(peer_b)
}

fn collected(peer: Peer, collector: Dynamic) -> List(String) {
  let found =
    peers.call(peer, "support@cluster_relay_probe", "collected", [collector])
  let assert Ok(events) = decode.run(found, decode.list(decode.string))
  events
}

fn kill_registered(peer: Peer, name: String) -> Nil {
  let pid =
    peers.call(peer, "erlang", "whereis", [atom.to_dynamic(atom.create(name))])
  peers.call(peer, "erlang", "exit", [
    pid,
    atom.to_dynamic(atom.create("kill")),
  ])
  Nil
}

fn registered(peer: Peer, name: String) -> Dynamic {
  peers.call(peer, "erlang", "whereis", [atom.to_dynamic(atom.create(name))])
}

fn recoveries(peer: Peer) -> Int {
  let found =
    peers.call(peer, "support@cluster_liveness_probe", "recoveries", [])
  let assert Ok(count) = decode.run(found, decode.int)
  count
}

pub fn relay_ordering_blip_and_membership_restart_across_nodes_test() {
  let b = peers.unique_name("scn_rb")
  let c = peers.unique_name("scn_rc")
  let sorted = list.sort([b, c], string.compare)
  let peer_b = boot(b, [b, c])
  let peer_c = boot(c, [b, c])
  assert peers.wait_until(25_000, fn() {
    serving(peer_b) == sorted && serving(peer_c) == sorted
  })

  let near =
    peers.call(peer_b, "support@cluster_relay_probe", "start_collector", [])
  let far =
    peers.call(peer_c, "support@cluster_relay_probe", "start_collector", [])
  peers.call(peer_b, "support@cluster_relay_probe", "send_events", [
    peers.to_dynamic([near, far]),
    peers.to_dynamic(400),
  ])
  let expected = probe.events(400)
  assert peers.wait_until(5000, fn() { collected(peer_c, far) == expected })
  assert collected(peer_b, near) == expected

  let healed_b = recoveries(peer_b)
  let healed_c = recoveries(peer_c)
  peers.call(peer_b, "erlang", "disconnect_node", [
    atom.to_dynamic(atom.create(c)),
  ])
  assert peers.wait_until(8000, fn() {
    recoveries(peer_b) > healed_b && recoveries(peer_c) > healed_c
  })
  assert is_up(peer_b, c)
  assert is_up(peer_c, b)
  assert serving(peer_b) == sorted
  assert serving(peer_c) == sorted

  let before = registered(peer_b, "fluxer_gateway_cluster_membership")
  kill_registered(peer_b, "fluxer_gateway_cluster_membership")
  assert peers.wait_until(3000, fn() {
    let now = registered(peer_b, "fluxer_gateway_cluster_membership")
    now != before && now != atom.to_dynamic(atom.create("undefined"))
  })
  assert serving(peer_b) == sorted
  assert is_up(peer_b, c)

  peers.call(peer_c, "fluxer_gateway@app@drain", "start", [])
  assert peers.wait_until(3000, fn() {
    serving(peer_b) == [b] && serving(peer_c) == [b]
  })
  assert peers.wait_until(10_000, fn() { undrain(peer_c) })
  assert peers.wait_until(3000, fn() {
    serving(peer_b) == sorted && serving(peer_c) == sorted
  })

  peers.stop(peer_c)
  peers.stop(peer_b)
}
