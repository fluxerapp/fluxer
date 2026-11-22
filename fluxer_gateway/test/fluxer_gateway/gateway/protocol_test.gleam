// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/close_frame
import fluxer_gateway/gateway/codec
import fluxer_gateway/gateway/compress
import fluxer_gateway/gateway/limits
import fluxer_gateway/gateway/opcode
import fluxer_gateway/gateway/resume
import gleam/bit_array
import gleam/bytes_tree
import gleam/json
import gleam/option.{None, Some}
import gleam/string

fn resume_of(text: String) {
  let assert Ok(outcome) = json.parse(text, resume.decoder())
  outcome
}

pub fn limits_test() {
  assert limits.max_payload_size() == 16_384
  assert limits.heartbeat_interval_ms() == 41_250
  assert limits.heartbeat_timeout_ms() == 45_000
  assert limits.resume_timeout_ms() == 60_000
}

pub fn envelope_test() {
  let assert Ok(codec.Frame(
    op: opcode.Heartbeat,
    body: codec.HeartbeatBody(Some(Ok(None))),
  )) = codec.decode(<<"{\"op\":1,\"d\":null}">>)
  let assert Ok(codec.Frame(op: opcode.Identify, body: codec.IdentifyBody(None))) =
    codec.decode(<<"{\"op\":2}">>)
  let assert Ok(codec.Frame(op: opcode.Unknown, ..)) =
    codec.decode(<<"{\"op\":\"1\"}">>)
  let assert Ok(codec.Frame(op: opcode.Unknown, ..)) =
    codec.decode(<<"{\"op\":1.0}">>)
  assert codec.decode(<<"{\"d\":1}">>) == Error(codec.MissingOp)
  assert codec.decode(<<"[1]">>) == Error(codec.DecodeFailed)
  assert codec.decode(<<"{">>) == Error(codec.DecodeFailed)
}

pub fn resume_test() {
  assert resume_of(
      "{\"token\":\"t\",\"session_id\":\"0123456789abcdef0123456789abcdef\",\"seq\":5}",
    )
    == Ok(resume.ResumeRequest("t", "0123456789abcdef0123456789abcdef", 5))
  assert resume_of("{\"token\":\"t\",\"session_id\":\"s\"}")
    == Error(resume.InvalidResumePayload)
  assert resume_of("{\"token\":\"t\",\"session_id\":\"s\",\"seq\":\"5\"}")
    == Error(resume.InvalidResumePayload)
  assert resume_of("null") == Error(resume.InvalidResumePayload)
}

pub fn parse_compression_test() {
  assert compress.parse_compression(Some("zstd-stream"), Some("1"))
    == compress.ZstdStream
  assert compress.parse_compression(Some("zstd-stream"), Some("true"))
    == compress.ZstdStream
  assert compress.parse_compression(Some("zstd-stream"), None)
    == compress.NoCompression
  assert compress.parse_compression(Some("zstd"), Some("1"))
    == compress.NoCompression
  assert compress.parse_compression(Some("none"), Some("1"))
    == compress.NoCompression
  assert compress.parse_compression(None, None) == compress.NoCompression
}

pub fn window_log_is_clamped_test() {
  assert compress.clamp_window_log(5) == 10
  assert compress.clamp_window_log(17) == 17
  assert compress.clamp_window_log(30) == 21
}

pub fn plain_context_passes_through_test() {
  let context = compress.new_context(compress.NoCompression, 17)
  assert compress.compression(context) == compress.NoCompression
  let assert Ok(#(sent, _)) =
    compress.compress(context, bytes_tree.from_bit_array(<<"abc">>))
  assert bytes_tree.to_bit_array(sent) == <<"abc">>
  let assert Ok(#(<<"abc">>, _)) = compress.decompress(context, <<"abc">>, 2)
}

pub fn close_frame_test() {
  assert close_frame.encode(4004, "Invalid token")
    == <<0x88, 15, 4004:size(16), "Invalid token":utf8>>
  let long = close_frame.encode(4000, string.repeat("x", 200))
  let assert <<0x88, 125, 4000:size(16), reason:bytes>> = long
  assert reason == bit_array.from_string(string.repeat("x", 123))
  let wide = close_frame.encode(4000, string.repeat("é", 100))
  let assert <<0x88, 124, 4000:size(16), reason:bytes>> = wide
  assert bit_array.to_string(reason) == Ok(string.repeat("é", 61))
}
