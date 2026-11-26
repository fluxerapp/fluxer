// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/gateway/socket
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/isolation

fn session_id(ready: ws.Payload) -> String {
  let assert Ok(id) =
    decode.run(ready.d, decode.at(["session_id"], decode.string))
  id
}

fn ready(client: ws.Client, token: String) -> #(ws.Client, ws.Payload) {
  ws.send_json(client, h.identify_payload(token))
  ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
}

fn no_api(_kind: String, _body: String) -> #(Int, String) {
  #(200, "{\"data\":{}}")
}

fn slow_sessions(user_id: String, delay_ms: Int) -> stub.Responder {
  fn(kind, _body) {
    case kind {
      "session" -> {
        process.sleep(delay_ms)
        #(200, stub.session_reply(user_id))
      }
      _ -> #(200, "{\"data\":{}}")
    }
  }
}

fn expect_closed_with(client: ws.Client, code: Int, reason: String) -> Nil {
  let #(got_code, got_reason) = ws.expect_close(client, 5000)
  assert got_code == Some(code)
  assert got_reason == reason
  assert ws.expect_eof(client, 2000)
}

pub fn identify_ready_and_heartbeat_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000001"))
  let client = h.hello(scenario.gateway.port)
  let #(client, payload) = ready(client, "token-a")
  assert payload.op == 0
  assert session_id(payload) != ""
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  let assert Some(seq) = payload.s
  ws.send_json(client, h.heartbeat_payload(json.int(seq)))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  let assert [body] = stub.token_calls(scenario.api, "session", "token-a")
  assert string.contains(body, "\"token\":\"token-a\"")
}

pub fn hello_carries_the_heartbeat_interval_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = ws.connect(scenario.gateway.port, "?v=1&encoding=json")
  let #(_client, hello) = ws.expect_payload(client, 2000)
  assert hello.op == 10
  assert decode.run(hello.d, decode.at(["heartbeat_interval"], decode.int))
    == Ok(41_250)
}

pub fn invalid_api_version_closes_4012_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = ws.connect(scenario.gateway.port, "?v=2")
  expect_closed_with(client, 4012, "Invalid API version")
}

pub fn missing_api_version_closes_4012_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = ws.connect(scenario.gateway.port, "")
  expect_closed_with(client, 4012, "Invalid API version")
}

pub fn heartbeat_before_identify_is_acked_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.heartbeat_payload(json.int(42)))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn heartbeat_without_data_closes_4001_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "{\"op\":1}")
  expect_closed_with(client, 4001, "Unknown opcode")
}

pub fn invalid_heartbeat_sequence_after_ready_closes_4007_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000002"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-b")
  ws.send_json(client, h.heartbeat_payload(json.string("seven")))
  expect_closed_with(client, 4007, "Invalid sequence")
}

pub fn opcode_before_identify_closes_4003_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "{\"op\":3,\"d\":{\"status\":\"online\"}}")
  expect_closed_with(client, 4003, "Not authenticated")
}

pub fn undecodable_text_closes_4002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "not json")
  expect_closed_with(client, 4002, "Decode failed")
}

pub fn payload_without_op_closes_4002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "{\"d\":1}")
  expect_closed_with(client, 4002, "Invalid payload")
}

pub fn identify_without_token_closes_4002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "{\"op\":2,\"d\":{\"properties\":{}}}")
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(4002)
}

pub fn rejected_token_closes_4004_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> #(401, "{}")
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("bad-token"))
  expect_closed_with(client, 4004, "Invalid token")
}

fn sessions_answer(status: Int, body: String) -> stub.Responder {
  fn(kind, _body) {
    case kind {
      "session" -> #(status, body)
      _ -> #(200, "{\"data\":{}}")
    }
  }
}

fn short_hold_budget() -> Nil {
  isolation.set_durations(fn(durations) {
    settings.Durations(
      ..durations,
      gateway: settings.GatewayDurations(
        ..durations.gateway,
        socket_pending_identify_retry_base_ms: 20,
        socket_pending_identify_retry_jitter_ms: 1,
        socket_held_start_retry_cap_ms: 80,
        socket_held_start_budget_ms: 400,
      ),
    )
  })
}

pub fn forbidden_session_backs_off_and_closes_4008_without_logging_out_test_() {
  use <- h.within(30.0)
  short_hold_budget()
  use scenario <- h.run(sessions_answer(
    403,
    "{\"code\":\"GLOBAL_IP_BANNED\",\"message\":\"banned\"}",
  ))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-banned"))
  expect_closed_with(client, 4008, "Session RPC rate limited")
  let calls =
    list.length(stub.token_calls(scenario.api, "session", "token-banned"))
  assert calls >= 2
  assert calls <= 12
}

pub fn rate_limited_session_closes_4008_once_the_hold_budget_is_spent_test_() {
  use <- h.within(30.0)
  short_hold_budget()
  use scenario <- h.run(sessions_answer(429, "{\"retry_after\":1}"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-throttled"))
  expect_closed_with(client, 4008, "Session RPC rate limited")
  let calls =
    list.length(stub.token_calls(scenario.api, "session", "token-throttled"))
  assert calls >= 2
  assert calls <= 12
}

pub fn unavailable_session_closes_4000_once_the_hold_budget_is_spent_test_() {
  use <- h.within(30.0)
  short_hold_budget()
  use scenario <- h.run(sessions_answer(404, "{}"))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-unavailable"))
  expect_closed_with(client, 4000, "Session unavailable")
  let calls =
    list.length(stub.token_calls(scenario.api, "session", "token-unavailable"))
  assert calls >= 2
  assert calls <= 12
}

pub fn identify_after_ready_closes_4005_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000003"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-c")
  ws.send_json(client, h.identify_payload("token-c"))
  expect_closed_with(client, 4005, "Already authenticated")
}

pub fn identify_sent_while_starting_is_deferred_not_restarted_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(slow_sessions("1000000000000004", 600))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-d"))
  ws.send_json(client, h.identify_payload("token-d"))
  expect_closed_with(client, 4005, "Already authenticated")
  assert list.length(stub.token_calls(scenario.api, "session", "token-d")) == 1
}

pub fn deferred_payload_cap_closes_4008_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(slow_sessions("1000000000000005", 1500))
  let client = h.hello(scenario.gateway.port)
  let filler = ws.frame(1, <<"{\"op\":13,\"d\":1}":utf8>>, True)
  let identify =
    ws.frame(
      1,
      bit_array.from_string(json.to_string(h.identify_payload("token-e"))),
      True,
    )
  ws.send_frame(client, bit_array.concat([identify, ..list.repeat(filler, 65)]))
  expect_closed_with(client, 4008, "Rate limited")
}

pub fn deferred_payloads_pause_reading_until_the_session_starts_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(slow_sessions("1000000000000006", 1500))
  let client = h.hello(scenario.gateway.port)
  let filler = ws.frame(1, <<"{\"op\":13,\"d\":1}":utf8>>, True)
  let identify =
    ws.frame(
      1,
      bit_array.from_string(json.to_string(h.identify_payload("token-f"))),
      True,
    )
  ws.send_frame(client, bit_array.concat([identify, ..list.repeat(filler, 40)]))
  process.sleep(100)
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, quiet) = ws.silent(client, 800)
  assert quiet
  let #(client, first) = ws.expect_payload(client, 5000)
  let #(_client, second) = ws.expect_payload(client, 5000)
  let seen = [first, second]
  assert list.any(seen, fn(p) { p.t == Some("READY") })
  assert list.any(seen, fn(p) { p.op == 11 })
}

pub fn heartbeats_are_answered_while_the_session_starts_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(slow_sessions("1000000000000014", 1500))
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-n"))
  process.sleep(100)
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 800)
  assert ack.op == 11
  let #(_client, ready) =
    ws.wait_for(client, 5000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
}

pub fn connection_rate_limit_closes_4008_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  let beat = ws.frame(1, <<"{\"op\":1,\"d\":null}":utf8>>, True)
  ws.send_frame(client, bit_array.concat(list.repeat(beat, 601)))
  let acks = count_acks(client, 0)
  assert acks == 600
}

fn count_acks(client: ws.Client, acks: Int) -> Int {
  case ws.next(client, 5000) {
    #(client, ws.Text(text)) -> {
      let payload = ws.decode_payload(text)
      let assert 11 = payload.op
      count_acks(client, acks + 1)
    }
    #(client, ws.Close(code, reason)) -> {
      assert code == Some(4008)
      assert reason == "Rate limited"
      assert ws.expect_eof(client, 2000)
      acks
    }
    #(_, other) -> {
      let assert ws.Closed = other
      acks
    }
  }
}

pub fn oversize_frame_closes_1009_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(1, <<0:size(16_385)-unit(8)>>, True))
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(1009)
  assert ws.expect_eof(client, 2000)
}

pub fn oversize_fragmented_message_closes_1009_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  let part = <<"a":utf8>> |> list.repeat(10_000) |> bit_array.concat
  ws.send_frame(client, ws.frame(1, part, False))
  ws.send_frame(client, ws.frame(0, part, True))
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(1009)
}

pub fn fragmented_message_is_reassembled_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(1, <<"{\"op\":1,":utf8>>, False))
  ws.send_frame(client, ws.frame(9, <<"mid":utf8>>, True))
  ws.send_frame(client, ws.frame(0, <<"\"d\":null}":utf8>>, True))
  let assert #(client, ws.Pong(<<"mid":utf8>>)) = ws.next(client, 2000)
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn unmasked_frame_closes_1002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, <<0x81, 2, "{}":utf8>>)
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(1002)
}

pub fn invalid_utf8_text_closes_1007_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(1, <<0xC3, 0x28>>, True))
  let #(code, _) = ws.expect_close(client, 5000)
  assert code == Some(1007)
}

pub fn ping_is_answered_with_pong_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(9, <<"hi":utf8>>, True))
  let assert #(_client, ws.Pong(<<"hi":utf8>>)) = ws.next(client, 2000)
  Nil
}

pub fn client_close_code_is_echoed_once_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_close(client, 4000)
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(4000)
  assert ws.expect_eof(client, 2000)
}

pub fn empty_client_close_is_echoed_empty_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(8, <<>>, True))
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == None
  assert ws.expect_eof(client, 2000)
}

pub fn reserved_client_close_code_closes_1002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_close(client, 1005)
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1002)
}

pub fn resume_after_abnormal_close_replays_and_resumes_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000007"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-g")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.send_close(client, 4000)
  let _ = ws.expect_close(client, 2000)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-g", id, seq))
  let #(again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  ws.send_json(again, h.heartbeat_payload(json.int(seq)))
  let #(again, ack) = ws.wait_for(again, 2000, fn(p) { p.op == 11 })
  assert ack.op == 11
  ws.send_json(again, h.resume_payload("token-g", id, seq))
  expect_closed_with(again, 4005, "Already authenticated")
}

pub fn resume_after_tcp_drop_resumes_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000008"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-h")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.close_socket(client)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-h", id, seq))
  let #(_again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}

pub fn resume_takes_over_a_live_connection_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000009"))
  let #(first, payload) = ready(h.hello(scenario.gateway.port), "token-i")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  let second = h.hello(scenario.gateway.port)
  ws.send_json(second, h.resume_payload("token-i", id, seq))
  let #(second, resumed) =
    ws.wait_for(second, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  let #(first, reconnect) = ws.expect_payload(first, 2000)
  assert reconnect.op == 7
  let #(code, _) = ws.expect_close(first, 2000)
  assert code == Some(4000)
  ws.send_json(second, h.heartbeat_payload(json.int(seq)))
  let #(_second, ack) = ws.wait_for(second, 2000, fn(p) { p.op == 11 })
  assert ack.op == 11
}

pub fn resume_after_normal_close_is_invalid_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000010"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-j")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.send_close(client, 1000)
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1000)
  assert h.await_gone(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-j", id, seq))
  let #(_again, invalid) = ws.expect_payload(again, 5000)
  assert invalid.op == 9
  assert decode.run(invalid.d, decode.bool) == Ok(False)
}

pub fn resume_with_wrong_token_closes_4004_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000011"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-k")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.close_socket(client)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("other-token", id, seq))
  expect_closed_with(again, 4004, "Invalid token")
}

pub fn resume_with_future_sequence_closes_4007_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000012"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-l")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.close_socket(client)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-l", id, seq + 100))
  expect_closed_with(again, 4007, "Invalid sequence")
}

pub fn resume_of_unknown_session_is_invalid_session_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(
    client,
    h.resume_payload("token", "00000000000000000000000000000000", 1),
  )
  let #(client, invalid) = ws.expect_payload(client, 5000)
  assert invalid.op == 9
  assert decode.run(invalid.d, decode.bool) == Ok(False)
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn resume_with_malformed_session_id_is_invalid_session_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.resume_payload("token", "not a session", 1))
  let #(_client, invalid) = ws.expect_payload(client, 5000)
  assert invalid.op == 9
}

pub fn resume_without_fields_closes_4002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_text(client, "{\"op\":6,\"d\":{\"token\":\"x\"}}")
  expect_closed_with(client, 4002, "Invalid resume payload")
}

pub fn drain_sends_reconnect_then_closes_4000_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000013"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-m")
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let #(code, reason) = ws.expect_close(client, 2000)
  assert code == Some(4000)
  assert reason == "Session drain requested; reconnect to continue"
}

pub fn a_payload_in_flight_when_reconnect_goes_out_reaches_the_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000018"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-race")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  ws.send_frame(client, presence("dnd"))
  let #(code, _reason) = ws.expect_close(client, 2000)
  assert code == Some(4000)
  ws.send_close(client, 4000)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-race", id, seq))
  let #(resumed, seen) = resumed_statuses(again, False, [])
  assert resumed
  assert list.contains(seen, "dnd")
}

pub fn a_repeated_drain_while_lingering_counts_one_reconnect_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000019"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-twice")
  let before = rollout_metrics.total(rollout_metrics.op7_sent, [])
  let first = socket.reconnect_all()
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let #(code, _reason) = ws.expect_close(client, 2000)
  assert code == Some(4000)
  ws.send_close(client, 4000)
  assert ws.expect_eof(client, 2000)
  assert rollout_metrics.total(rollout_metrics.op7_sent, []) - before == first
}

pub fn a_silent_client_after_reconnect_is_closed_when_the_linger_ends_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000020"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-quiet")
  let id = session_id(payload)
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(client, ws.Close(Some(4000), _)) = ws.next(client, 2000)
  assert socket.lingering() >= 1
  assert ws.expect_eof(client, 3000)
  assert h.await_detached(id)
}

pub fn a_client_answering_the_drain_close_with_1000_can_still_resume_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000021"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-echo")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(client, ws.Close(Some(4000), _)) = ws.next(client, 2000)
  ws.send_close(client, 1000)
  assert ws.expect_eof(client, 2000)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-echo", id, seq))
  let #(_again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") || p.op == 9 })
  assert resumed.t == Some("RESUMED")
}

pub fn an_identify_on_a_lingering_socket_starts_no_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000022"))
  let client = h.hello(scenario.gateway.port)
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(client, ws.Close(Some(4000), _)) = ws.next(client, 2000)
  ws.send_json(client, h.identify_payload("token-late"))
  assert ws.expect_eof(client, 3000)
  assert stub.token_calls(scenario.api, "session", "token-late") == []
}

pub fn a_protocol_error_while_lingering_sends_no_second_close_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000023"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-bad")
  let _ = socket.reconnect_all()
  let #(client, reconnect) = ws.wait_for(client, 2000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(client, ws.Close(Some(4000), _)) = ws.next(client, 2000)
  ws.send_frame(client, ws.frame(1, <<0xff, 0xfe>>, True))
  let assert #(_client, ws.Closed) = ws.next(client, 3000)
  Nil
}

fn resumed_statuses(
  client: ws.Client,
  resumed: Bool,
  seen: List(String),
) -> #(Bool, List(String)) {
  case ws.next(client, 800) {
    #(client, ws.Text(text)) -> {
      let payload = ws.decode_payload(text)
      case payload.t, replaced_status(payload) {
        Some("RESUMED"), _ -> resumed_statuses(client, True, seen)
        _, Ok(status) -> resumed_statuses(client, resumed, [status, ..seen])
        _, Error(Nil) -> resumed_statuses(client, resumed, seen)
      }
    }
    #(_client, _) -> #(resumed, list.reverse(seen))
  }
}

fn presence(status: String) -> BitArray {
  ws.frame(
    1,
    bit_array.from_string("{\"op\":3,\"d\":{\"status\":\"" <> status <> "\"}}"),
    True,
  )
}

fn replaced_status(payload: ws.Payload) -> Result(String, Nil) {
  case payload.t {
    Some("SESSIONS_REPLACE") ->
      case
        decode.run(
          payload.d,
          decode.at([0], decode.at(["status"], decode.string)),
        )
      {
        Ok(status) -> Ok(status)
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn statuses(
  client: ws.Client,
  seen: List(String),
) -> #(ws.Client, List(String)) {
  case ws.next(client, 800) {
    #(client, ws.Text(text)) ->
      case replaced_status(ws.decode_payload(text)) {
        Ok(status) -> statuses(client, [status, ..seen])
        Error(Nil) -> statuses(client, seen)
      }
    #(client, _) -> #(client, list.reverse(seen))
  }
}

pub fn presence_updates_over_the_opcode_limit_are_dropped_silently_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000015"))
  let #(client, _) = ready(h.hello(scenario.gateway.port), "token-o")
  ws.send_frame(
    client,
    bit_array.concat([
      presence("idle"),
      presence("dnd"),
      presence("online"),
      presence("idle"),
      presence("dnd"),
      presence("online"),
      presence("invisible"),
    ]),
  )
  let #(client, seen) = statuses(client, [])
  assert seen != []
  assert list.length(seen) <= 5
  assert list.last(seen) == Ok("dnd")
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn payloads_sent_while_resuming_reach_the_resumed_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000016"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-p")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.close_socket(client)
  assert h.await_detached(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_frame(
    again,
    bit_array.concat([
      ws.frame(
        1,
        bit_array.from_string(
          json.to_string(h.resume_payload("token-p", id, seq)),
        ),
        True,
      ),
      presence("dnd"),
    ]),
  )
  let #(again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  let #(_again, seen) = statuses(again, [])
  assert list.last(seen) == Ok("dnd")
}

pub fn going_away_close_ends_the_session_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000017"))
  let #(client, payload) = ready(h.hello(scenario.gateway.port), "token-q")
  let id = session_id(payload)
  let assert Some(seq) = payload.s
  ws.send_close(client, 1001)
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1001)
  assert h.await_gone(id)
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.resume_payload("token-q", id, seq))
  let #(_again, invalid) = ws.expect_payload(again, 5000)
  assert invalid.op == 9
}

pub fn close_reason_with_invalid_utf8_closes_1007_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(8, <<4000:16, 0xC3, 0x28>>, True))
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1007)
}

pub fn one_byte_close_payload_closes_1002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(8, <<3>>, True))
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1002)
}

pub fn fragmented_ping_closes_1002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(9, <<"x":utf8>>, False))
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(1002)
}
