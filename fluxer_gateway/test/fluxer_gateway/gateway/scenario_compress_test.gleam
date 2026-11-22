// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/zstd
import gleam/bit_array
import gleam/bytes_tree
import gleam/json
import gleam/option.{Some}
import gleam/string
import support/gateway_harness as h
import support/gateway_ws_client as ws

fn no_api(_kind: String, _body: String) -> #(Int, String) {
  #(200, "{\"data\":{}}")
}

fn zstd_query() -> String {
  "?v=1&encoding=json&compress=zstd-stream&stream=1"
}

fn inflate(
  client: ws.Client,
  decompressor: zstd.Decompressor,
) -> #(ws.Client, ws.Payload) {
  let assert #(client, ws.BinaryFrame(data)) = ws.next(client, 5000)
  let assert Ok(bytes) = zstd.decompress(decompressor, data, 1_000_000)
  let assert Ok(text) = bit_array.to_string(bytes)
  #(client, ws.decode_payload(text))
}

fn deflate(compressor: zstd.Compressor, value: json.Json) -> BitArray {
  let assert Ok(data) =
    zstd.compress(compressor, bytes_tree.from_string(json.to_string(value)))
  data
}

pub fn zstd_stream_round_trip_reaches_ready_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000201"))
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, hello) = inflate(client, decompressor)
  assert hello.op == 10
  ws.send_binary(client, deflate(compressor, h.heartbeat_payload(json.null())))
  let #(client, ack) = inflate(client, decompressor)
  assert ack.op == 11
  ws.send_binary(client, deflate(compressor, h.identify_payload("token-z")))
  let #(client, ready) = inflate(client, decompressor)
  assert ready.t == Some("READY")
  ws.send_binary(client, deflate(compressor, h.heartbeat_payload(json.null())))
  let #(_client, ack) = inflate(client, decompressor)
  assert ack.op == 11
}

pub fn zstd_without_stream_flag_falls_back_to_text_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = ws.connect(scenario.gateway.port, "?v=1&compress=zstd-stream")
  let assert #(_client, ws.Text(_)) = ws.next(client, 2000)
  Nil
}

pub fn zlib_stream_is_not_offered_and_falls_back_to_text_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client =
    ws.connect(scenario.gateway.port, "?v=1&compress=zlib-stream&stream=1")
  let #(client, hello) = ws.expect_payload(client, 2000)
  assert hello.op == 10
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn zstd_garbage_closes_4002_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  ws.send_binary(client, <<"definitely not zstd":utf8>>)
  let #(code, reason) = ws.expect_close(client, 2000)
  assert code == Some(4002)
  assert reason == "Decompression failed"
}

pub fn zstd_bomb_closes_4002_payload_too_large_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  let assert Ok(bomb) =
    zstd.compress_dirty(
      compressor,
      bytes_tree.from_bit_array(<<0:size(200_000)-unit(8)>>),
    )
  assert bit_array.byte_size(bomb) < 16_384
  ws.send_binary(client, bomb)
  let #(code, reason) = ws.expect_close(client, 2000)
  assert code == Some(4002)
  assert reason == "Payload too large"
}

pub fn zstd_text_frame_is_decompressed_too_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  ws.send_text(client, "{\"op\":1,\"d\":null}")
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(4002)
}

pub fn zstd_close_frame_is_plain_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  ws.send_close(client, 4321)
  let #(code, _) = ws.expect_close(client, 2000)
  assert code == Some(4321)
}

fn padded_heartbeat(size: Int) -> BitArray {
  let prefix = "{\"op\":1,\"d\":null,\"x\":\""
  let pad = size - string.byte_size(prefix) - 2
  bit_array.from_string(prefix <> string.repeat("a", pad) <> "\"}")
}

pub fn zstd_payload_of_exactly_the_cap_is_accepted_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  let payload = padded_heartbeat(16_384)
  assert bit_array.byte_size(payload) == 16_384
  let assert Ok(data) =
    zstd.compress(compressor, bytes_tree.from_bit_array(payload))
  ws.send_binary(client, data)
  let #(_client, ack) = inflate(client, decompressor)
  assert ack.op == 11
}

pub fn zstd_payload_one_over_the_cap_is_rejected_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  let assert Ok(data) =
    zstd.compress(
      compressor,
      bytes_tree.from_bit_array(padded_heartbeat(16_385)),
    )
  ws.send_binary(client, data)
  let #(code, reason) = ws.expect_close(client, 2000)
  assert code == Some(4002)
  assert reason == "Payload too large"
}

pub fn plain_payload_of_exactly_the_cap_is_accepted_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let client = h.hello(scenario.gateway.port)
  ws.send_frame(client, ws.frame(1, padded_heartbeat(16_384), True))
  let #(_client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
}

pub fn several_zstd_messages_share_one_stream_test_() {
  use <- h.within(10.0)
  use scenario <- h.run(no_api)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  let first = deflate(compressor, h.heartbeat_payload(json.null()))
  let second = deflate(compressor, h.heartbeat_payload(json.null()))
  ws.send_frame(
    client,
    bit_array.concat([ws.frame(2, first, True), ws.frame(2, second, True)]),
  )
  let #(client, ack) = inflate(client, decompressor)
  assert ack.op == 11
  let #(_client, ack) = inflate(client, decompressor)
  assert ack.op == 11
}

pub fn large_ready_is_streamed_compressed_test_() {
  use <- h.within(30.0)
  let big = string.repeat("{\"id\":\"1\",\"type\":1},", 9000)
  let reply =
    "{\"data\":{\"user\":{\"id\":\"1000000000000202\",\"username\":\"u\"},\"padding\":["
    <> big
    <> "{}]}}"
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> #(200, reply)
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let client = ws.connect(scenario.gateway.port, zstd_query())
  let #(client, _) = inflate(client, decompressor)
  ws.send_binary(client, deflate(compressor, h.identify_payload("token-big")))
  let #(client, ready) = inflate(client, decompressor)
  assert ready.t == Some("READY")
  ws.send_binary(client, deflate(compressor, h.heartbeat_payload(json.null())))
  let #(_client, ack) = inflate(client, decompressor)
  assert ack.op == 11
}

pub fn large_ready_is_sent_plain_test_() {
  use <- h.within(30.0)
  let big = string.repeat("{\"id\":\"1\",\"type\":1},", 9000)
  let reply =
    "{\"data\":{\"user\":{\"id\":\"1000000000000203\",\"username\":\"u\"},\"padding\":["
    <> big
    <> "{}]}}"
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> #(200, reply)
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.identify_payload("token-big-2"))
  let #(_client, ready) = ws.expect_payload(client, 5000)
  assert ready.t == Some("READY")
}
