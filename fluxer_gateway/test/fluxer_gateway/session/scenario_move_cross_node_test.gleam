// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/guild_fixture as fixture
import support/session_harness as h
import support/session_move as move

const boot_ms = 45_000

type Node {
  Node(peer: Peer, name: String)
}

fn env(names: List(String), api_port: Int) -> List(#(String, String)) {
  [
    #("FLUXER_GATEWAY_PORT", "0"),
    #("FLUXER_GATEWAY_CLUSTER_ENABLED", "true"),
    #("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS", string.join(names, ",")),
    #("FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", "500"),
    #("FLUXER_NATS_URL", "nats://127.0.0.1:1"),
    #("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:1"),
    #(
      "FLUXER_GATEWAY_API_RPC_ENDPOINT",
      "http://127.0.0.1:" <> int.to_string(api_port) <> "/internal/rpc",
    ),
    #("FLUXER_GATEWAY_LOGGER_LEVEL", "critical"),
  ]
}

fn boot(name: String, names: List(String), api_port: Int) -> Node {
  let peer = peers.start_within(name, env(names, api_port), boot_ms)
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
  Node(peer, name)
}

fn serving(found: Node) -> List(String) {
  let names =
    peers.call(found.peer, "fluxer_gateway@cluster@router", "serving", [])
  let assert Ok(names) =
    decode.run(
      names,
      decode.list(decode.at([1], atom.decoder() |> decode.map(atom.to_string))),
    )
  list.sort(names, string.compare)
}

fn on(found: Node, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(found.peer, "support@session_move", function, arguments)
}

fn on_peer(found: Node, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(found.peer, "support@session_peer", function, arguments)
}

fn is_true(value: Dynamic) -> Bool {
  decode.run(value, decode.bool) == Ok(True)
}

fn number(value: Dynamic) -> Int {
  let assert Ok(found) = decode.run(value, decode.int)
  found
}

fn texts(value: Dynamic) -> List(String) {
  let assert Ok(found) = decode.run(value, decode.list(decode.string))
  found
}

fn count(found: Node, sock: Dynamic, event: String) -> Int {
  number(on(found, "count_of", [sock, dynamic.string(event)]))
}

pub fn a_session_moved_to_another_node_keeps_its_guild_and_loses_no_event_test_() {
  use <- fixture.slow(180)
  let user = h.unique_user()
  let found = move.guild(user)
  use <- h.with_api(move.responder(user, [found]))
  let api_port = h.stub_port()
  let b = peers.unique_name("mvx_b")
  let c = peers.unique_name("mvx_c")
  let sorted = list.sort([b, c], string.compare)
  let assert [node_b, node_c] =
    peers.in_parallel(
      [b, c],
      fn(name) { boot(name, [b, c], api_port) },
      boot_ms,
    )
  assert peers.wait_until(25_000, fn() {
    serving(node_b) == sorted && serving(node_c) == sorted
  })
  assert peers.wait_until(25_000, fn() {
    is_true(on_peer(node_b, "rebalance_settled", []))
    && is_true(on_peer(node_c, "rebalance_settled", []))
  })

  let sock = on(node_b, "socket", [])
  let key = on(node_b, "start", [sock, dynamic.string("tok-mvx")])
  let assert Ok(key) = decode.run(key, decode.string)
  assert key != ""
  assert peers.wait_until(8000, fn() {
    number(on(node_b, "ready_of", [sock])) > 0
  })
  let #(host, other) = case
    is_true(on_peer(node_b, "hosts", [dynamic.string(key)]))
  {
    True -> #(node_b, node_c)
    False -> #(node_c, node_b)
  }
  let guild_id = dynamic.int(found.id)
  let guild_node = case is_true(on(node_b, "hosts_guild", [guild_id])) {
    True -> node_b
    False -> node_c
  }
  assert is_true(on(guild_node, "hosts_guild", [guild_id]))
  assert is_true(
    on(guild_node, "send_messages", [
      guild_id,
      dynamic.int(1),
      dynamic.int(5),
      dynamic.int(0),
    ]),
  )
  assert peers.wait_until(8000, fn() {
    count(node_b, sock, "MESSAGE_CREATE") == 5
  })

  let traffic = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(
      traffic,
      on(guild_node, "send_messages", [
        guild_id,
        dynamic.int(6),
        dynamic.int(400),
        dynamic.int(2),
      ]),
    )
  })
  process.sleep(200)
  assert is_true(
    on_peer(host, "hand_off", [dynamic.string(key), dynamic.string(other.name)]),
  )
  let assert Ok(sent) = process.receive(traffic, 30_000)
  assert is_true(sent)
  assert peers.wait_until(15_000, fn() {
    count(node_b, sock, "MESSAGE_CREATE") >= 405
  })
  process.sleep(300)

  let described = on(node_b, "described", [sock])
  let assert Ok(described) = decode.run(described, decode.string)
  assert count(node_b, sock, "GUILD_CREATE") == 0 as described
  assert count(node_b, sock, "GUILD_DELETE") == 0 as described
  assert texts(on(node_b, "ids_of", [sock, dynamic.string("MESSAGE_CREATE")]))
    == move.numbers(1, 405)
    as described
  assert is_true(on(node_b, "follows", [sock])) as described
  assert !is_true(on(node_b, "reconnected", [sock])) as described
  assert is_true(on_peer(other, "hosts", [dynamic.string(key)]))
  assert peers.wait_until(5000, fn() {
    !is_true(on_peer(host, "hosts", [dynamic.string(key)]))
  })

  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}
