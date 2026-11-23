// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session as core_session
import fluxer_gateway/gateway/heartbeat
import fluxer_gateway/gateway/identify
import fluxer_gateway/gateway/socket_metrics as sm
import fluxer_gateway/gateway/zstd
import fluxer_gateway/telemetry/metrics
import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/set
import support/gateway_harness as h
import support/gateway_ws_client as ws

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn observed(name: String) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) { series.name == name && series.labels == [] })
  |> list.fold(0, fn(total, series) { total + series.count })
}

fn eventually(check: fn() -> Bool, remaining_ms: Int) -> Bool {
  case check() {
    True -> True
    False ->
      case remaining_ms <= 0 {
        True -> False
        False -> {
          process.sleep(20)
          eventually(check, remaining_ms - 20)
        }
      }
  }
}

fn delta(
  name: String,
  labels: List(#(String, String)),
  action: fn() -> Nil,
) -> Int {
  let before = value(name, labels)
  action()
  value(name, labels) - before
}

fn family_values(name: String) -> List(#(String, String)) {
  let assert Ok(#(family, _)) =
    list.find(sm.counter_families(), fn(entry) { { entry.0 }.name == name })
  list.map(family.values, fn(found) { #(family.label, found) })
}

fn only_bumps(
  name: String,
  expected: #(String, String),
  action: fn() -> Nil,
) -> Nil {
  let labels = family_values(name)
  let before = list.map(labels, fn(label) { value(name, [label]) })
  action()
  let after = list.map(labels, fn(label) { value(name, [label]) })
  list.zip(labels, list.zip(before, after))
  |> list.each(fn(entry) {
    let #(label, #(was, now)) = entry
    case label == expected {
      True -> {
        assert now - was == 1
      }
      False -> {
        assert now == was
      }
    }
  })
}

pub fn every_counter_slot_is_used_once_test() {
  let #(used, size) = sm.layout()
  assert list.length(used) == set.size(set.from_list(used))
  assert list.all(used, fn(index) { index >= 1 && index <= size })
  assert list.length(used) == size
}

pub fn each_recorder_bumps_only_its_own_label_test() {
  only_bumps(sm.rate_limited_total, #("kind", "identify_token"), fn() {
    sm.rate_limited(sm.IdentifyTokenLimit)
  })
  only_bumps(sm.buffer_overflows, #("buffer", "early"), fn() {
    sm.overflowed(sm.EarlyBuffer)
  })
  only_bumps(sm.heartbeat_checks, #("outcome", "timed_out"), fn() {
    sm.heartbeat_checked(heartbeat.TimedOut)
  })
  only_bumps(sm.heartbeat_checks, #("outcome", "alive"), fn() {
    sm.heartbeat_checked(heartbeat.StillAlive)
  })
  only_bumps(sm.disconnects, #("cause", "protocol_error"), fn() {
    sm.disconnected(sm.ProtocolError)
  })
  only_bumps(sm.identifies, #("outcome", "invalid_shard"), fn() {
    sm.identified(sm.IdentifyRejected(identify.InvalidShard))
  })
  only_bumps(sm.identify_holds, #("reason", "unavailable"), fn() {
    sm.held(sm.HoldUnavailable)
  })
  only_bumps(sm.session_starts, #("outcome", "too_many_sessions"), fn() {
    sm.start_replied(Error(core_session.StartTooManySessions))
  })
  only_bumps(sm.session_starts, #("outcome", "timeout"), sm.start_timed_out)
  only_bumps(sm.resume_attempts, #("outcome", "session_gone"), fn() {
    sm.resume_finished(sm.ResumeRejected(core_session.SessionGone))
  })
  only_bumps(sm.dispatches_by_family, #("family", "reaction"), fn() {
    sm.dispatch_sent("MESSAGE_REACTION_ADD")
  })
  only_bumps(sm.dispatches_by_family, #("family", "member"), fn() {
    sm.dispatch_sent("GUILD_MEMBER_LIST_UPDATE")
  })
  only_bumps(sm.dispatches_by_family, #("family", "other"), fn() {
    sm.dispatch_sent("SCENARIO_EVENT")
  })
  assert delta(sm.send_failures, [], sm.send_failed) == 1
}

pub fn identify_to_ready_counts_ops_bytes_states_and_latency_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000901"))
  let identify_op = value(sm.ops_received, [#("op", "identify")])
  let heartbeat_op = value(sm.ops_received, [#("op", "heartbeat")])
  let ready = value(sm.dispatches_by_family, [#("family", "ready")])
  let accepted = value(sm.connects, [#("outcome", "accepted")])
  let identified = value(sm.identifies, [#("outcome", "accepted")])
  let started = value(sm.session_starts, [#("outcome", "ok")])
  let transport = value(sm.disconnects, [#("cause", "transport")])
  let sent_wire = value(sm.sent_wire_bytes, [#("compression", "none")])
  let sent_payload = value(sm.sent_payload_bytes, [#("compression", "none")])
  let received_wire = value(sm.received_wire_bytes, [#("compression", "none")])
  let timed = observed(sm.identify_ready_ms)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-metrics"))
  let #(client, _) = ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  assert value(sm.ops_received, [#("op", "identify")]) - identify_op == 1
  assert value(sm.ops_received, [#("op", "heartbeat")]) - heartbeat_op == 1
  assert value(sm.dispatches_by_family, [#("family", "ready")]) - ready == 1
  assert value(sm.connects, [#("outcome", "accepted")]) - accepted == 1
  assert value(sm.identifies, [#("outcome", "accepted")]) - identified == 1
  assert value(sm.session_starts, [#("outcome", "ok")]) - started == 1
  assert observed(sm.identify_ready_ms) - timed == 1
  let wire = value(sm.sent_wire_bytes, [#("compression", "none")]) - sent_wire
  assert wire > 0
  assert value(sm.sent_payload_bytes, [#("compression", "none")]) - sent_payload
    == wire
  assert value(sm.received_wire_bytes, [#("compression", "none")])
    > received_wire
  assert value(sm.connections, [#("state", "attached")]) >= 1
  assert value(sm.process_memory, []) > 0
  ws.close_socket(client)
  assert eventually(
    fn() { value(sm.disconnects, [#("cause", "transport")]) > transport },
    2000,
  )
}

pub fn resume_counts_outcome_latency_and_replay_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000902"))
  let resumed = value(sm.resume_attempts, [#("outcome", "resumed")])
  let resumed_family = value(sm.dispatches_by_family, [#("family", "resumed")])
  let replays =
    value("fluxer_gateway_session_replays_total", [#("path", "resume")])
  let timed = observed(sm.resume_ms)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-resume-metrics"))
  let #(client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.close_socket(client)
  assert h.await_detached(h.session_id(ready))
  assert value("fluxer_gateway_sessions_detached", []) >= 1
  assert value("fluxer_gateway_session_process_memory_bytes", []) > 0
  let again = h.hello(scenario.gateway.port)
  ws.send_json(
    again,
    h.resume_payload("token-resume-metrics", h.session_id(ready), seq),
  )
  let #(_again, _) = ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert value(sm.resume_attempts, [#("outcome", "resumed")]) - resumed == 1
  assert value(sm.dispatches_by_family, [#("family", "resumed")])
    - resumed_family
    == 1
  assert value("fluxer_gateway_session_replays_total", [#("path", "resume")])
    - replays
    == 1
  assert observed(sm.resume_ms) - timed == 1
}

pub fn bad_api_version_is_a_refused_connect_and_a_server_close_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(h.sessions_for("1000000000000903"))
  let refused = value(sm.connects, [#("outcome", "bad_api_version")])
  let server = value(sm.disconnects, [#("cause", "server")])
  let client = ws.connect(scenario.gateway.port, "?v=9&encoding=json")
  let _ = ws.expect_close(client, 5000)
  assert value(sm.connects, [#("outcome", "bad_api_version")]) - refused == 1
  assert eventually(
    fn() { value(sm.disconnects, [#("cause", "server")]) > server },
    2000,
  )
}

pub fn unparseable_resume_session_id_is_counted_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(h.sessions_for("1000000000000904"))
  let bad = value(sm.resume_attempts, [#("outcome", "bad_session_id")])
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.resume_payload("token", "not-a-session", 0))
  let #(_client, invalid) = ws.expect_payload(client, 2000)
  assert invalid.op == 9
  assert value(sm.resume_attempts, [#("outcome", "bad_session_id")]) - bad == 1
}

pub fn a_normal_client_close_is_counted_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(h.sessions_for("1000000000000905"))
  let normal = value(sm.disconnects, [#("cause", "client_normal")])
  let client = h.hello(scenario.gateway.port)
  ws.send_close(client, 1000)
  assert eventually(
    fn() { value(sm.disconnects, [#("cause", "client_normal")]) > normal },
    2000,
  )
}

pub fn zstd_bytes_are_counted_on_both_sides_of_compression_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000906"))
  let zstd_label = [#("compression", "zstd_stream")]
  let sent_wire = value(sm.sent_wire_bytes, zstd_label)
  let sent_payload = value(sm.sent_payload_bytes, zstd_label)
  let received_wire = value(sm.received_wire_bytes, zstd_label)
  let received_payload = value(sm.received_payload_bytes, zstd_label)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client =
    ws.connect(
      scenario.gateway.port,
      "?v=1&encoding=json&compress=zstd-stream&stream=1",
    )
  let assert #(client, ws.BinaryFrame(data)) = ws.next(client, 5000)
  let assert Ok(hello) = zstd.decompress(decompressor, data, 1_000_000)
  let assert Ok(_) = bit_array.to_string(hello)
  let request = json.to_string(h.heartbeat_payload(json.null()))
  let assert Ok(packed) =
    zstd.compress(compressor, bytes_tree.from_string(request))
  ws.send_binary(client, packed)
  let assert #(_client, ws.BinaryFrame(_)) = ws.next(client, 5000)
  assert value(sm.sent_wire_bytes, zstd_label) > sent_wire
  assert value(sm.sent_payload_bytes, zstd_label) - sent_payload
    >= bit_array.byte_size(hello)
  assert value(sm.received_wire_bytes, zstd_label) - received_wire
    == bit_array.byte_size(packed)
  assert value(sm.received_payload_bytes, zstd_label) - received_payload
    == bit_array.byte_size(bit_array.from_string(request))
}
