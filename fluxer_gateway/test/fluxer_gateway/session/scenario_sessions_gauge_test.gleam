// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/guild_fixture as fixture
import support/session_harness as h

const boot_ms = 45_000

const others = 3

type Node {
  Node(peer: Peer, name: String)
}

type Sampler {
  Stop(reply: Subject(List(Int)))
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

fn on_peer(found: Node, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(found.peer, "support@session_peer", function, arguments)
}

fn is_true(value: Dynamic) -> Bool {
  decode.run(value, decode.bool) == Ok(True)
}

fn gauge(found: Node) -> Int {
  let value =
    peers.call(found.peer, "support@session_gauge", "sessions_gauge", [])
  let assert Ok(count) = decode.run(value, decode.int)
  count
}

fn hosts(found: Node, key: String) -> Bool {
  is_true(on_peer(found, "hosts", [dynamic.string(key)]))
}

fn start(found: Node, sock: Dynamic, token: String) -> String {
  let assert Ok(#(key, ready)) =
    decode.run(on_peer(found, "start", [sock, dynamic.string(token)]), {
      use key <- decode.field(0, decode.string)
      use ready <- decode.field(1, decode.int)
      decode.success(#(key, ready))
    })
  assert key != ""
  assert ready > 0
  key
}

fn snapshot(first: Node, second: Node) -> Int {
  let a = gauge(first)
  let b = gauge(second)
  case gauge(first) == a && gauge(second) == b {
    True -> a + b
    False -> snapshot(first, second)
  }
}

fn sample(
  first: Node,
  second: Node,
  control: Subject(Sampler),
  sums: List(Int),
) -> Nil {
  case process.receive(control, 0) {
    Ok(Stop(reply)) -> process.send(reply, list.reverse(sums))
    Error(Nil) ->
      sample(first, second, control, [snapshot(first, second), ..sums])
  }
}

fn start_sampler(first: Node, second: Node) -> Subject(Sampler) {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let control = process.new_subject()
    process.send(ready, control)
    sample(first, second, control, [])
  })
  let assert Ok(control) = process.receive(ready, 1000)
  control
}

fn stop_sampler(control: Subject(Sampler)) -> List(Int) {
  let reply = process.new_subject()
  process.send(control, Stop(reply))
  let assert Ok(sums) = process.receive(reply, 20_000)
  sums
}

pub fn a_moving_session_counts_on_exactly_one_node_test_() {
  use <- fixture.slow(180)
  let user = h.unique_user()
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let api_port = h.stub_port()
  let b = peers.unique_name("sgz_b")
  let c = peers.unique_name("sgz_c")
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

  list.repeat(Nil, others)
  |> list.index_map(fn(_, index) { index })
  |> list.each(fn(index) {
    let sock = on_peer(node_b, "socket", [])
    start(node_b, sock, "tok-sgz-" <> int.to_string(index))
  })
  let silent =
    peers.call(node_b.peer, "support@session_gauge", "silent_socket", [])
  let key = start(node_b, silent, "tok-sgz-moving")
  let total = others + 1
  let #(host, other) = case hosts(node_b, key) {
    True -> #(node_b, node_c)
    False -> #(node_c, node_b)
  }
  assert peers.wait_until(5000, fn() { snapshot(host, other) == total })

  let before = start_sampler(host, other)
  process.sleep(300)
  let handed = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(
      handed,
      is_true(
        on_peer(host, "hand_off", [
          dynamic.string(key),
          dynamic.string(other.name),
        ]),
      ),
    )
  })
  let assert Ok(True) = process.receive(handed, 15_000)
  let committed_at = monotonic_ms()
  assert hosts(other, key)
  let during = stop_sampler(before)
  let departure = start_sampler(host, other)
  assert peers.wait_until(15_000, fn() { !hosts(host, key) })
  let departed_ms = monotonic_ms() - committed_at
  process.sleep(500)
  let departing = stop_sampler(departure)
  let sums = list.append(during, departing)

  let unexpected = list.filter(sums, fn(sum) { sum != total })
  assert list.is_empty(unexpected)
    as {
      "the gauge sum left "
      <> int.to_string(total)
      <> " in "
      <> int.to_string(list.length(unexpected))
      <> " of "
      <> int.to_string(list.length(sums))
      <> " snapshots: "
      <> string.inspect(list.unique(unexpected))
    }
  assert departed_ms >= 1000 as "the departure window was not held open"
  assert list.length(during) >= 5
  assert list.length(departing) >= 20

  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: atom.Atom) -> Int

fn monotonic_ms() -> Int {
  monotonic_time(atom.create("millisecond"))
}
