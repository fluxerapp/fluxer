// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Settings}
import fluxer_gateway/gateway/zstd
import gleam/bit_array
import gleam/bytes_tree
import gleam/json
import gleam/option.{Some}
import support/gateway_harness as h
import support/gateway_ws_client as ws

fn zstd_query() -> String {
  "?v=1&encoding=json&compress=zstd-stream&stream=1"
}

fn inflate(
  client: ws.Client,
  decompressor: zstd.Decompressor,
) -> #(ws.Client, ws.Payload) {
  let assert #(client, ws.BinaryFrame(data)) = ws.next(client, 5000)
  let assert Ok(bytes) = zstd.decompress(decompressor, data, 4_000_000)
  let assert Ok(text) = bit_array.to_string(bytes)
  #(client, ws.decode_payload(text))
}

fn inflate_until(
  client: ws.Client,
  decompressor: zstd.Decompressor,
  matches: fn(ws.Payload) -> Bool,
) -> #(ws.Client, ws.Payload) {
  let #(client, payload) = inflate(client, decompressor)
  case matches(payload) {
    True -> #(client, payload)
    False -> inflate_until(client, decompressor, matches)
  }
}

fn deflate(compressor: zstd.Compressor, value: json.Json) -> BitArray {
  let assert Ok(data) =
    zstd.compress(compressor, bytes_tree.from_string(json.to_string(value)))
  data
}

fn with_window_log(window_log: Int) -> Nil {
  settings.store(Settings(..settings.get(), zstd_window_log: window_log))
  Nil
}

fn round_trip(port: Int) -> Nil {
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(port, zstd_query())
  let #(client, hello) = inflate(client, decompressor)
  assert hello.op == 10
  ws.send_binary(client, deflate(compressor, h.heartbeat_payload(json.null())))
  let #(_client, ack) = inflate(client, decompressor)
  assert ack.op == 11
}

pub fn window_log_below_the_minimum_is_clamped_test_() {
  use <- h.within(20.0)
  with_window_log(5)
  use scenario <- h.run(h.sessions_for("1000000000000841"))
  round_trip(scenario.gateway.port)
}

pub fn window_log_above_the_maximum_is_clamped_test_() {
  use <- h.within(20.0)
  with_window_log(40)
  use scenario <- h.run(h.sessions_for("1000000000000842"))
  round_trip(scenario.gateway.port)
}

pub fn empty_zstd_message_closes_4002_test_() {
  use <- h.within(20.0)
  use scenario <- h.run(h.sessions_for("1000000000000843"))
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  ws.send_binary(client, <<>>)
  let #(code, reason) = ws.expect_close(client, 5000)
  assert code == Some(4002)
  assert reason == "Decode failed"
}

pub fn resume_on_a_fresh_zstd_stream_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000844"))
  let port = scenario.gateway.port
  let assert Ok(first_in) = zstd.decompressor_new(21)
  let assert Ok(first_out) = zstd.compressor_new(3, 17, 16_384)
  let first = ws.connect(port, zstd_query())
  let #(first, _) = inflate(first, first_in)
  ws.send_binary(first, deflate(first_out, h.identify_payload("t-c844")))
  let #(first, ready) =
    inflate_until(first, first_in, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.close_socket(first)
  assert h.await_detached(h.session_id(ready))
  let assert Ok(second_in) = zstd.decompressor_new(21)
  let assert Ok(second_out) = zstd.compressor_new(3, 17, 16_384)
  let second = ws.connect(port, zstd_query())
  let #(second, hello) = inflate(second, second_in)
  assert hello.op == 10
  ws.send_binary(
    second,
    deflate(second_out, h.resume_payload("t-c844", h.session_id(ready), seq)),
  )
  let #(second, resumed) =
    inflate_until(second, second_in, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  ws.send_binary(
    second,
    deflate(second_out, h.heartbeat_payload(json.int(seq))),
  )
  let #(_second, ack) = inflate_until(second, second_in, fn(p) { p.op == 11 })
  assert ack.op == 11
}

pub fn plain_resume_of_a_zstd_session_is_plain_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000845"))
  let port = scenario.gateway.port
  let assert Ok(first_in) = zstd.decompressor_new(21)
  let assert Ok(first_out) = zstd.compressor_new(3, 17, 16_384)
  let first = ws.connect(port, zstd_query())
  let #(first, _) = inflate(first, first_in)
  ws.send_binary(first, deflate(first_out, h.identify_payload("t-c845")))
  let #(first, ready) =
    inflate_until(first, first_in, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.close_socket(first)
  assert h.await_detached(h.session_id(ready))
  let second = h.hello(port)
  ws.send_json(second, h.resume_payload("t-c845", h.session_id(ready), seq))
  let #(_second, resumed) =
    ws.wait_for(second, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}
