// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string
import support/cluster_peers.{type Peer} as peers
import support/cluster_trio as trio
import support/session_harness as h

const boot_ms = 45_000

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
  on(peer, "slow_settings", [dynamic.int(120_000)])
  Node(peer, name)
}

fn cluster(prefix: String, count: Int, api_port: Int) -> List(Node) {
  let names =
    int.range(from: 0, to: count, with: [], run: fn(acc, index) {
      [peers.unique_name(prefix <> int.to_string(index)), ..acc]
    })
    |> list.reverse
  let nodes = peers.in_parallel(names, boot(_, names, api_port), boot_ms)
  let sorted = list.sort(names, string.compare)
  assert peers.wait_until(30_000, fn() {
    list.all(nodes, fn(found) { trio.serving(found.peer) == sorted })
  })
  nodes
}

fn on(peer: Peer, function: String, arguments: List(Dynamic)) -> Dynamic {
  peers.call(peer, "support@session_peer", function, arguments)
}

fn text(value: String) -> Dynamic {
  dynamic.string(value)
}

fn hosts(found: Node, key: String) -> Bool {
  decode.run(on(found.peer, "hosts", [text(key)]), decode.bool) == Ok(True)
}

fn start(found: Node, sock: Dynamic, token: String) -> #(String, Int) {
  let assert Ok(started) =
    decode.run(on(found.peer, "start", [sock, text(token)]), {
      use key <- decode.field(0, decode.string)
      use seq <- decode.field(1, decode.int)
      decode.success(#(key, seq))
    })
  started
}

fn timed(run: fn() -> Dynamic) -> process.Subject(#(String, Int)) {
  let done = process.new_subject()
  process.spawn_unlinked(fn() {
    let began = h.now_ms()
    let outcome = case decode.run(run(), decode.string) {
      Ok(found) -> found
      Error(_) -> "undecodable"
    }
    process.send(done, #(outcome, h.now_ms() - began))
  })
  done
}

pub fn remote_resume_fails_fast_when_the_host_dies_mid_request_test() {
  let user = h.unique_user()
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let assert [first, second] = cluster("scn4_rr", 2, h.stub_port())
  let sock = on(first.peer, "socket", [])
  let #(key, ready_seq) = start(first, sock, "tok-rr")
  assert key != ""
  assert ready_seq > 0
  let #(host, other) = case hosts(first, key) {
    True -> #(first, second)
    False -> #(second, first)
  }
  assert on(host.peer, "suspend_session", [text(key)]) == dynamic.bool(True)

  let fresh = on(other.peer, "socket", [])
  let done =
    timed(fn() {
      on(other.peer, "resume_within", [
        fresh,
        text(key),
        text("tok-rr"),
        dynamic.int(ready_seq),
        dynamic.int(12_000),
      ])
    })
  process.sleep(800)
  peers.stop(host.peer)
  let assert Ok(#(outcome, elapsed)) = process.receive(done, 14_000)
  assert string.starts_with(outcome, "ResumeUnavailable")
  assert elapsed < 5000
  peers.stop(other.peer)
}

pub fn remote_start_fails_fast_when_the_owner_dies_mid_start_test() {
  let user = h.unique_user()
  let data = h.session_data(user, False, [])
  let slow = fn(kind, body) {
    case kind {
      "session" -> {
        process.sleep(6000)
        h.ok_data(data)
      }
      _ -> h.empty_ok(kind, body)
    }
  }
  use <- h.with_api(slow)
  let assert [first, second] = cluster("scn4_rs", 2, h.stub_port())
  let sock = on(first.peer, "socket", [])
  let done =
    timed(fn() {
      on(first.peer, "start_remote", [
        sock,
        text("tok-rs"),
        text(second.name),
        dynamic.int(12_000),
      ])
    })
  process.sleep(1000)
  peers.stop(second.peer)
  let assert Ok(#(outcome, elapsed)) = process.receive(done, 14_000)
  assert string.starts_with(outcome, "StartUnavailable")
  assert elapsed < 5000
  peers.stop(first.peer)
}

pub fn drain_hands_off_past_a_suspect_owner_and_resume_finds_it_test() {
  let user = h.unique_user()
  use <- h.with_api(h.session_api(h.session_data(user, False, [])))
  let assert [draining, suspect, live] = cluster("scn4_sd", 3, h.stub_port())
  let key = case
    decode.run(
      on(draining.peer, "shifting_key", [text(suspect.name)]),
      decode.string,
    )
  {
    Ok(found) -> found
    Error(_) -> ""
  }
  assert key != ""
  let sock = on(draining.peer, "socket", [])
  let assert Ok(ready_seq) =
    decode.run(
      on(draining.peer, "start_key", [sock, text("tok-sd"), text(key)]),
      decode.int,
    )
  assert ready_seq > 0
  assert hosts(draining, key)

  trio.cut(draining.peer, draining.name, suspect.peer, suspect.name)
  trio.cut(live.peer, live.name, suspect.peer, suspect.name)
  assert peers.wait_until(10_000, fn() {
    !trio.is_up(draining.peer, suspect.name)
    && !trio.is_up(live.peer, suspect.name)
  })
  assert list.contains(trio.serving(draining.peer), suspect.name)

  peers.call(draining.peer, "fluxer_gateway@app@drain", "start", [])
  assert peers.wait_until(10_000, fn() {
    hosts(live, key) && !hosts(draining, key)
  })

  let fresh = on(live.peer, "socket", [])
  assert on(live.peer, "resume_within", [
      fresh,
      text(key),
      text("tok-sd"),
      dynamic.int(ready_seq),
      dynamic.int(8000),
    ])
    == text("ok")
  let elsewhere = on(draining.peer, "socket", [])
  assert on(draining.peer, "resume_within", [
      elsewhere,
      text(key),
      text("tok-sd"),
      dynamic.int(ready_seq),
      dynamic.int(8000),
    ])
    == text("ok")
  list.each([draining, suspect, live], fn(found) { peers.stop(found.peer) })
}
