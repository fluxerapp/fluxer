// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/lease.{Claim}
import bouncer_proto/message.{
  type ToWorker, Beat, BeatAck, Granted, Hello, Welcome, WorkerBeat, WorkerHello,
}
import bouncer_proto/transport
import bouncer_proto/worker.{WorkerId}
import fluxer_bouncer/config.{Timings}
import fluxer_bouncer/server
import fluxer_bouncer/sim
import fluxer_bouncer/status
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/list
import gleam/string

type Loaded

@external(erlang, "code", "all_loaded")
fn all_loaded() -> List(#(Atom, Loaded))

@external(erlang, "erlang", "node")
fn self_node() -> Atom

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

fn collect(inbox, found: List(ToWorker), until: fn(List(ToWorker)) -> Bool) {
  case until(found) {
    True -> found
    False ->
      case process.receive(inbox, 2000) {
        Ok(message) -> collect(inbox, [message, ..found], until)
        Error(Nil) -> found
      }
  }
}

pub fn a_started_bouncer_serves_a_worker_without_loading_the_gateway_test() {
  let assert Ok(built) =
    config.build(
      hostname: "boot-0",
      count: "1",
      basename: "boot",
      dns_name: "invalid",
    )
  let assert Ok(started) =
    server.start(config.Config(..built, timings: short_timings()))
  let assert Ok(Nil) = process.register(process.self(), transport.worker_name())
  let inbox = process.named_subject(transport.worker_name())
  let me = WorkerId(self_node(), 1)
  let assert Ok(Nil) =
    transport.deliver(
      to: self_node(),
      name: transport.bouncer_name(),
      message: WorkerHello(
        Hello(
          worker: me,
          profile: sim.profile(),
          protocol: bouncer_proto.own_range(),
          pinned: [],
        ),
      ),
    )
  let beat = fn(seq, digest) {
    process.send(
      started.data,
      WorkerBeat(
        Beat(
          worker: me,
          seq: seq,
          sent_mono: 0,
          held_digest: digest,
          roster_hash: 0,
          draining: False,
          load: 0,
          unreachable: [],
        ),
      ),
    )
  }
  let beater = process.spawn_unlinked(fn() { keep_beating(beat, 1) })
  let grants = fn(found: List(ToWorker)) {
    list.filter_map(found, fn(message) {
      case message {
        Granted(bucket, epoch, _) -> Ok(Claim(bucket, epoch))
        _ -> Error(Nil)
      }
    })
  }
  let found =
    collect(inbox, [], fn(found) { list.length(grants(found)) >= 1024 })
  assert list.any(found, fn(message) {
    case message {
      Welcome(bouncer: 0, ..) -> True
      _ -> False
    }
  })
  let held = grants(found)
  assert list.length(held) == 1024
  beat(checked_seq, lease.held_digest(held))
  let acks =
    collect(inbox, [], fn(found) {
      list.any(found, fn(message) {
        case message {
          BeatAck(seq: seq, ..) -> seq == checked_seq
          _ -> False
        }
      })
    })
  assert list.any(acks, fn(message) {
    case message {
      BeatAck(seq: seq, matched: matched, ..) -> seq == checked_seq && matched
      _ -> False
    }
  })
  let gateway_modules =
    all_loaded()
    |> list.filter(fn(entry) {
      string.starts_with(atom.to_string(entry.0), "fluxer_gateway")
    })
  assert gateway_modules == []
  assert list.any(all_loaded(), fn(entry) {
    atom.to_string(entry.0) == "fluxer_bouncer@core"
  })
  let assert Ok(summary) = status.current()
  assert summary.synced
  assert summary.peers_seen == 0 && summary.quorum == 1 && summary.count == 1
  assert summary.workers == 1
  assert summary.buckets >= 1024
  assert !summary.leaving
  let name = process.new_name("fluxer_bouncer_status_test")
  let assert Ok(listener) = status.start(0, name, status.current)
  let port = status.port_of(name)
  let served = http_get(port, "GET /_status HTTP/1.1\r\nHost: b\r\n\r\n")
  assert string.starts_with(served, "HTTP/1.1 200 OK\r\n")
  assert string.contains(served, "\"ready_reason\":\"OK\"")
  assert string.contains(served, "\"workers\":1")
  let scraped = http_get(port, "GET /_metrics HTTP/1.1\r\nHost: b\r\n\r\n")
  assert string.contains(
    scraped,
    "fluxer_bouncer_status_bouncer_quorum{role=\"bouncer\"} 1\n",
  )
  let refused = http_get(port, "POST /_status HTTP/1.1\r\nHost: b\r\n\r\n")
  assert string.starts_with(refused, "HTTP/1.1 405 ")
  process.unlink(listener.pid)
  process.kill(listener.pid)
  process.kill(beater)
  process.unlink(started.pid)
  process.kill(started.pid)
  let assert Ok(Nil) = process.unregister(transport.worker_name())
}

type Socket

type TcpReason

@external(erlang, "gen_tcp", "connect")
fn tcp_connect(
  host: Charlist,
  port: Int,
  options: List(Dynamic),
  timeout: Int,
) -> Result(Socket, TcpReason)

@external(erlang, "gen_tcp", "send")
fn tcp_send(socket: Socket, data: BitArray) -> Dynamic

@external(erlang, "gen_tcp", "recv")
fn tcp_recv(
  socket: Socket,
  length: Int,
  timeout: Int,
) -> Result(BitArray, TcpReason)

@external(erlang, "gen_tcp", "close")
fn tcp_close(socket: Socket) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

fn http_get(port: Int, request: String) -> String {
  let assert Ok(socket) =
    tcp_connect(
      charlist.from_string("127.0.0.1"),
      port,
      [
        to_dynamic(atom.create("binary")),
        to_dynamic(#(atom.create("active"), False)),
      ],
      2000,
    )
  let _ = tcp_send(socket, bit_array.from_string(request))
  let received = drain_socket(socket, <<>>)
  let _ = tcp_close(socket)
  let assert Ok(text) = bit_array.to_string(received)
  text
}

fn drain_socket(socket: Socket, acc: BitArray) -> BitArray {
  case tcp_recv(socket, 0, 2000) {
    Ok(data) -> drain_socket(socket, <<acc:bits, data:bits>>)
    Error(_) -> acc
  }
}

const checked_seq = 1_000_000

fn keep_beating(beat: fn(Int, Int) -> Nil, seq: Int) -> Nil {
  beat(seq, 0)
  process.sleep(25)
  keep_beating(beat, seq + 1)
}
