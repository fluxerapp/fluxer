// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers

const nodes_module = "support@presence_nodes"

fn env(names: List(String)) -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_PORT", "0"),
    #("FLUXER_GATEWAY_CLUSTER_ENABLED", "true"),
    #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", string.join(names, ",")),
    #("FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", "200"),
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
  let _ = peers.call(peer, nodes_module, "tune", [])
  peer
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, nodes_module, function, arguments)
}

fn arg(value: a) -> Dynamic {
  peers.to_dynamic(value)
}

fn decoded(found: Dynamic, decoder: decode.Decoder(a)) -> a {
  let assert Ok(value) = decode.run(found, decoder) as string.inspect(found)
  value
}

fn owners(peer: Peer, users: List(Int)) -> List(String) {
  decoded(on(peer, "owners", [arg(users)]), decode.list(decode.string))
}

fn hosted(peer: Peer, users: List(Int)) -> List(Int) {
  decoded(on(peer, "hosted", [arg(users)]), decode.list(decode.int))
}

fn statuses(peer: Peer, guild: Int, users: List(Int)) -> List(String) {
  decoded(
    on(peer, "observer_statuses", [arg(guild), arg(users)]),
    decode.list(decode.string),
  )
}

fn check(found: Dynamic) -> Bool {
  decoded(found, decode.bool)
}

fn users_from(base: Int, count: Int) -> List(Int) {
  int.range(1, count + 1, [], list.prepend)
  |> list.reverse
  |> list.map(fn(n) { base + n })
}

fn two_nodes(prefix: String) -> #(Peer, Peer, String, String) {
  let a = peers.unique_name(prefix <> "a")
  let b = peers.unique_name(prefix <> "b")
  let peer_a = boot(a, [a, b])
  let peer_b = boot(b, [a, b])
  #(peer_a, peer_b, a, b)
}

fn owned_by(
  peer_a: Peer,
  peer_b: Peer,
  users: List(Int),
  a: String,
  b: String,
) -> List(Int) {
  assert peers.wait_until(20_000, fn() {
    let found = owners(peer_a, users)
    list.contains(found, a) && list.contains(found, b)
  })
  assert peers.wait_until(5000, fn() {
    owners(peer_b, users) == owners(peer_a, users)
  })
  list.filter(users, fn(user) { owners(peer_a, [user]) == [b] })
}

pub fn remote_catch_up_covers_new_and_stopped_presences_test_() {
  #(atom.create("timeout"), 120, fn() {
    let #(peer_a, peer_b, a, b) = two_nodes("scn_rc")
    let users = users_from(7_900_000_000_003_000, 40)
    let guild = 7_900_000_000_003_900
    let assert [arriving, leaving, crashing, ..] =
      owned_by(peer_a, peer_b, users, a, b)
    let watched = [arriving, leaving, crashing]
    let _ = on(peer_b, "start_sessions", [arg([leaving, crashing]), arg(1)])
    assert peers.wait_until(5000, fn() {
      hosted(peer_b, [leaving, crashing]) == [leaving, crashing]
    })
    assert check(on(peer_b, "freeze_registries", [arg(watched)]))
    assert check(on(peer_a, "start_observer", [arg(guild), arg(watched)]))
    let started = on(peer_b, "start_sessions", [arg([arriving]), arg(1)])
    assert decoded(started, decode.int) == 1
    let _ =
      on(peer_b, "exit_session", [
        arg(leaving),
        arg(1),
        on(peer_b, "closed_reason", []),
      ])
    let _ = on(peer_b, "kill_presence", [arg(crashing), arg(1)])
    let settled =
      peers.wait_until(5000, fn() { hosted(peer_b, watched) == [arriving] })
    assert settled as string.inspect(#(watched, hosted(peer_b, watched)))
    assert check(on(peer_b, "thaw_registries", [arg(watched)]))
    let wanted = ["online", "offline", "offline"]
    let caught_up =
      peers.wait_until(5000, fn() { statuses(peer_a, guild, watched) == wanted })
    assert caught_up
      as string.inspect(#(wanted, statuses(peer_a, guild, watched)))
    peers.stop(peer_b)
    peers.stop(peer_a)
  })
}
