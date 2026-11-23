// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

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

pub fn boot(name: String, names: List(String)) -> Peer {
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

pub fn name_value(name: String) -> Dynamic {
  atom.to_dynamic(atom.create(name))
}

fn node_value(name: String) -> Dynamic {
  peers.to_dynamic(#(atom.create("node"), atom.create(name)))
}

pub fn serving(peer: Peer) -> List(String) {
  let found = peers.call(peer, "fluxer_gateway@cluster@router", "serving", [])
  let assert Ok(names) =
    decode.run(
      found,
      decode.list(decode.at([1], atom.decoder() |> decode.map(atom.to_string))),
    )
  list.sort(names, string.compare)
}

pub fn is_up(peer: Peer, name: String) -> Bool {
  let found =
    peers.call(peer, "fluxer_gateway@cluster@liveness", "is_up", [
      node_value(name),
    ])
  decode.run(found, decode.bool) == Ok(True)
}

fn set_cookie(peer: Peer, other: String, cookie: String) -> Nil {
  peers.call(peer, "erlang", "set_cookie", [
    name_value(other),
    name_value(cookie),
  ])
  Nil
}

pub fn cut(x: Peer, x_name: String, y: Peer, y_name: String) -> Nil {
  set_cookie(x, y_name, "scn_cut_near")
  set_cookie(y, x_name, "scn_cut_far")
  peers.call(x, "erlang", "disconnect_node", [name_value(y_name)])
  Nil
}

pub fn mend(x: Peer, x_name: String, y: Peer, y_name: String) -> Nil {
  set_cookie(x, y_name, "fluxer_scn_cluster")
  set_cookie(y, x_name, "fluxer_scn_cluster")
}

pub fn crash(peer: Peer) -> Nil {
  peers.cast(peer, "erlang", "halt", [peers.to_dynamic(137)])
}

pub fn probe_int(peer: Peer, function: String, args: List(Dynamic)) -> Int {
  let found = peers.call(peer, "support@cluster_route_probe", function, args)
  let assert Ok(value) = decode.run(found, decode.int)
    as { "probe " <> function <> " failed: " <> string.inspect(found) }
  value
}

pub fn probe_names(peer: Peer, function: String) -> List(String) {
  let found = peers.call(peer, "support@cluster_route_probe", function, [])
  let assert Ok(names) = decode.run(found, decode.list(decode.string))
  list.sort(names, string.compare)
}

pub fn quicken(peer: Peer, gone_after_ms: Int) -> Nil {
  let current: settings.Settings =
    coerce(peers.call(peer, "fluxer_gateway@config@settings", "get", []))
  let d = current.durations
  let faster =
    settings.Settings(
      ..current,
      durations: settings.Durations(
        ..d,
        cluster: settings.ClusterDurations(
          ..d.cluster,
          membership_state_gone_after_ms: gone_after_ms,
          membership_state_joining_stable_ms: 1500,
          membership_state_gone_reconnect_ms: 1000,
          membership_state_publish_interval_ms: 1000,
        ),
      ),
    )
  peers.call(peer, "fluxer_gateway@config@settings", "store", [
    peers.to_dynamic(faster),
  ])
  let before =
    peers.call(peer, "erlang", "whereis", [
      name_value("fluxer_gateway_cluster_membership"),
    ])
  peers.call(peer, "erlang", "exit", [before, name_value("kill")])
  let assert True =
    peers.wait_until(3000, fn() {
      let now =
        peers.call(peer, "erlang", "whereis", [
          name_value("fluxer_gateway_cluster_membership"),
        ])
      now != before && now != name_value("undefined")
    })
  Nil
}
