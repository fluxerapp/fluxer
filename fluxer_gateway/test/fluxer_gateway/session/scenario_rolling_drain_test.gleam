// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/session_harness as h

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

type Node {
  Node(peer: Peer, name: String)
}

fn boot(name: String, names: List(String), api_port: Int) -> Node {
  let peer = peers.start(name, env(names, api_port))
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
  on(peer, "slow_settings", [dynamic.int(120_000)])
  Node(peer, name)
}

fn pair(prefix: String, api_port: Int) -> #(Node, Node) {
  let b = peers.unique_name(prefix <> "b")
  let c = peers.unique_name(prefix <> "c")
  let sorted = list.sort([b, c], string.compare)
  let node_b = boot(b, [b, c], api_port)
  let node_c = boot(c, [b, c], api_port)
  assert peers.wait_until(30_000, fn() {
    serving(node_b) == sorted && serving(node_c) == sorted
  })
  #(node_b, node_c)
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, "support@session_peer", function, arguments)
}

fn text(value: String) -> Dynamic {
  dynamic.string(value)
}

fn strings(found: Dynamic) -> List(String) {
  let assert Ok(values) = decode.run(found, decode.list(decode.string))
  values
}

fn serving(found: Node) -> List(String) {
  strings(on(found.peer, "serving", []))
}

fn leaving(found: Node) -> List(String) {
  strings(on(found.peer, "leaving", []))
}

fn hosts(found: Node, key: String) -> Bool {
  decode.run(on(found.peer, "hosts", [text(key)]), decode.bool) == Ok(True)
}

fn events(found: Node, sock: Dynamic) -> List(String) {
  strings(on(found.peer, "events", [sock]))
}

fn reconnects(found: Node, sock: Dynamic) -> Int {
  list.count(events(found, sock), fn(event) { event == "RECONNECT" })
}

fn rebound(found: Node, sock: Dynamic, seq: Int) -> Bool {
  let seen = events(found, sock)
  list.count(seen, fn(event) { event == "MOVED:" <> int.to_string(seq) }) == 1
  && list.contains(seen, "REATTACHED")
}

fn resumed(found: Node, sock: Dynamic) -> Bool {
  list.any(events(found, sock), string.starts_with(_, "RESUMED:"))
}

fn start_owned(found: Node, sock: Dynamic, token: String) -> String {
  let assert Ok(key) =
    decode.run(
      on(found.peer, "start_owned_async", [sock, text(token)]),
      decode.string,
    )
  key
}

fn ready_seq(found: Node, sock: Dynamic) -> Int {
  let assert Ok(seq) =
    decode.run(on(found.peer, "ready_seq", [sock]), decode.int)
  seq
}

fn resume(
  found: Node,
  sock: Dynamic,
  key: String,
  token: String,
  seq: Int,
) -> String {
  let assert Ok(outcome) =
    decode.run(
      on(found.peer, "resume", [sock, text(key), text(token), dynamic.int(seq)]),
      decode.string,
    )
  outcome
}

fn drain_start(found: Node) -> Nil {
  let _ = on(found.peer, "drain_start", [])
  Nil
}

fn await_drained(found: Node) -> Bool {
  decode.run(
    on(found.peer, "await_drained", [dynamic.int(10_000)]),
    decode.bool,
  )
  == Ok(True)
}

fn gated_session_api(data: Json, called: Subject(Nil), delay_ms: Int) {
  fn(kind: String, _body: Dynamic) {
    case kind {
      "session" -> {
        process.send(called, Nil)
        process.sleep(delay_ms)
        h.ok_data(data)
      }
      _ -> h.ok_data(json.object([]))
    }
  }
}

pub fn session_placed_after_the_drain_handoff_moves_at_shutdown_test() {
  let user = h.unique_user()
  let called = process.new_subject()
  use <- h.with_api(gated_session_api(
    h.session_data(user, False, []),
    called,
    1500,
  ))
  let #(node_b, node_c) = pair("scn_rd1", h.stub_port())
  let sock = on(node_b.peer, "socket", [])
  let key = start_owned(node_b, sock, "tok-rd1")
  let assert Ok(Nil) = process.receive(called, 10_000)
  drain_start(node_b)
  assert await_drained(node_b)
  let seq = ready_seq(node_b, sock)
  assert seq > 0
  assert hosts(node_b, key)
  assert !hosts(node_c, key)
  let _ = on(node_b.peer, "prep_stop", [])
  assert hosts(node_c, key)
  assert peers.wait_until(3000, fn() { !hosts(node_b, key) })
  assert peers.wait_until(3000, fn() { rebound(node_b, sock, seq) })
  assert reconnects(node_b, sock) == 0
  let fresh = on(node_c.peer, "socket", [])
  assert resume(node_c, fresh, key, "tok-rd1", seq) == "ok"
  assert peers.wait_until(3000, fn() { resumed(node_c, fresh) })
  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}

pub fn drained_session_reconnects_once_and_resumes_on_its_new_host_test() {
  let user = h.unique_user()
  let called = process.new_subject()
  use <- h.with_api(gated_session_api(
    h.session_data(user, False, []),
    called,
    0,
  ))
  let #(node_b, node_c) = pair("scn_rd2", h.stub_port())
  let sock = on(node_b.peer, "socket", [])
  let key = start_owned(node_b, sock, "tok-rd2")
  let seq = ready_seq(node_b, sock)
  assert seq > 0
  assert hosts(node_b, key)
  drain_start(node_b)
  assert peers.wait_until(10_000, fn() { rebound(node_b, sock, seq) })
  assert reconnects(node_b, sock) == 0
  assert hosts(node_c, key)
  assert peers.wait_until(3000, fn() { !hosts(node_b, key) })
  let fresh = on(node_c.peer, "socket", [])
  assert resume(node_c, fresh, key, "tok-rd2", seq) == "ok"
  assert peers.wait_until(3000, fn() { resumed(node_c, fresh) })
  assert await_drained(node_b)
  process.sleep(500)
  assert reconnects(node_c, fresh) == 0
  assert reconnects(node_b, sock) == 1
  assert hosts(node_c, key)
  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}

pub fn node_that_joins_during_a_drain_resumes_sessions_still_on_the_drainer_test() {
  let user = h.unique_user()
  let called = process.new_subject()
  use <- h.with_api(gated_session_api(
    h.session_data(user, False, []),
    called,
    0,
  ))
  let api_port = h.stub_port()
  let b = peers.unique_name("scn_rd3b")
  let c = peers.unique_name("scn_rd3c")
  let node_b = boot(b, [b, c], api_port)
  let _ = on(node_b.peer, "slow_handoff_debounce", [dynamic.int(120_000)])
  assert peers.wait_until(15_000, fn() { serving(node_b) == [b] })
  let sock = on(node_b.peer, "socket", [])
  let key = start_owned(node_b, sock, "tok-rd3")
  let seq = ready_seq(node_b, sock)
  assert seq > 0
  drain_start(node_b)
  assert await_drained(node_b)
  assert hosts(node_b, key)
  let node_c = boot(c, [b, c], api_port)
  assert peers.wait_until(30_000, fn() {
    serving(node_c) == [c] && leaving(node_c) == [b]
  })
  assert hosts(node_b, key)
  let fresh = on(node_c.peer, "socket", [])
  assert resume(node_c, fresh, key, "tok-rd3", seq) == "ok"
  assert peers.wait_until(3000, fn() { resumed(node_c, fresh) })
  peers.stop(node_c.peer)
  peers.stop(node_b.peer)
}
