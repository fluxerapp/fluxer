// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_bouncer/config.{Timings}
import fluxer_bouncer/server
import fluxer_bouncer/sim
import fluxer_bouncer/status
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/string

type Socket

type ConnectOption {
  Binary
  Active(Bool)
}

@external(erlang, "gen_tcp", "connect")
fn connect(
  host: #(Int, Int, Int, Int),
  port: Int,
  options: List(ConnectOption),
  timeout: Int,
) -> Result(Socket, Dynamic)

@external(erlang, "gen_tcp", "send")
fn send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "recv")
fn recv(socket: Socket, length: Int, timeout: Int) -> Result(BitArray, Dynamic)

@external(erlang, "gen_tcp", "close")
fn close(socket: Socket) -> Dynamic

fn read_all(socket: Socket, found: BitArray) -> BitArray {
  case recv(socket, 0, 2000) {
    Ok(more) -> read_all(socket, bit_array.append(found, more))
    Error(_) -> found
  }
}

fn request(port: Int, method: String, path: String) -> String {
  let assert Ok(socket) =
    connect(#(127, 0, 0, 1), port, [Binary, Active(False)], 2000)
  let _ =
    send(
      socket,
      bit_array.from_string(
        method
        <> " "
        <> path
        <> " HTTP/1.1\r\nhost: probe\r\nuser-agent: kube-probe\r\n\r\n",
      ),
    )
  let answer = read_all(socket, <<>>)
  let _ = close(socket)
  let assert Ok(text) = bit_array.to_string(answer)
  text
}

fn short_timings() {
  Timings(
    ..sim.timings(),
    beat_interval_ms: 50,
    lease_ttl_ms: 300,
    regrant_silence_ms: 200,
    claim_window_ms: 50,
    request_timeout_ms: 100,
    tick_ms: 10,
  )
}

fn start(count: String) {
  let assert Ok(built) =
    config.build(
      hostname: "probe-0",
      count: count,
      basename: "probe",
      dns_name: "invalid",
    )
  let assert Ok(started) =
    server.start(config.Config(..built, timings: short_timings()))
  let name = process.new_name("fluxer_bouncer_listener_test")
  let assert Ok(listener) = status.start(0, name, status.current)
  #(started.pid, listener.pid, status.port_of(name))
}

fn stop(pid: Pid) {
  process.unlink(pid)
  process.kill(pid)
}

fn wait_ready(port: Int, left: Int) -> String {
  let answer = request(port, "GET", "/_health/ready")
  case string.starts_with(answer, "HTTP/1.1 200 ") || left == 0 {
    True -> answer
    False -> {
      process.sleep(20)
      wait_ready(port, left - 1)
    }
  }
}

pub fn a_bouncer_without_its_majority_is_never_ready_test() {
  let #(started, listener, port) = start("3")
  process.sleep(200)
  let ready = request(port, "GET", "/_health/ready")
  assert string.starts_with(ready, "HTTP/1.1 503 ")
  assert string.ends_with(ready, "\r\n\r\nNO_QUORUM")
  let found = request(port, "GET", "/_status?verbose=1")
  assert string.starts_with(found, "HTTP/1.1 200 ")
  assert string.contains(found, "content-type: application/json")
  assert string.contains(
    found,
    "\"ready_reason\":\"NO_QUORUM\",\"answered\":1,\"ready\":0,\"synced\":0,\"ordinal\":0,\"bouncers_configured\":3,\"bouncers_seen\":1,\"quorum\":2,\"bouncer_quorum\":0,\"recovering\":0,\"leaving\":0,\"peers_seen\":0,\"peers_synced\":0,\"workers\":0,\"buckets\":0,\"buckets_synced\":0,\"record_sync_lag\":0,\"moves_in_flight\":0,",
  )
  stop(listener)
  stop(started)
}

pub fn a_lone_bouncer_turns_ready_after_its_claim_window_test() {
  let #(started, listener, port) = start("1")
  let ready = wait_ready(port, 100)
  assert string.starts_with(ready, "HTTP/1.1 200 ")
  assert string.ends_with(ready, "\r\n\r\nOK")
  let found = request(port, "GET", "/_status")
  assert string.contains(found, "\"synced\":1")
  assert string.contains(found, "\"quorum\":1")
  assert string.starts_with(request(port, "GET", "/_health"), "HTTP/1.1 200 ")
  assert string.starts_with(request(port, "GET", "/nope"), "HTTP/1.1 404 ")
  assert string.starts_with(
    request(port, "POST", "/_health/ready"),
    "HTTP/1.1 405 ",
  )
  stop(started)
  process.sleep(50)
  let gone = request(port, "GET", "/_health/ready")
  assert string.starts_with(gone, "HTTP/1.1 503 ")
  assert string.ends_with(gone, "STARTING")
  stop(listener)
}
