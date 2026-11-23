// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/gateway/encode
import gleam/bytes_tree

pub fn dispatch_test() {
  let frame =
    encode.outbound(session.DispatchFrame(
      seq: 42,
      event: "MESSAGE_CREATE",
      payload: raw_json.wrap(<<"{\"id\":\"1\"}">>),
    ))
    |> bytes_tree.to_bit_array
  assert frame
    == <<"{\"op\":0,\"t\":\"MESSAGE_CREATE\",\"s\":42,\"d\":{\"id\":\"1\"}}">>
}

pub fn hello_test() {
  assert bytes_tree.to_bit_array(encode.hello(41_250))
    == <<"{\"d\":{\"heartbeat_interval\":41250},\"op\":10}">>
}

pub fn control_frames_test() {
  assert bytes_tree.to_bit_array(encode.heartbeat_request())
    == <<"{\"d\":null,\"op\":1}">>
  assert bytes_tree.to_bit_array(encode.outbound(session.HeartbeatAckFrame))
    == <<"{\"op\":11}">>
  assert bytes_tree.to_bit_array(encode.outbound(session.ReconnectFrame))
    == <<"{\"op\":7}">>
  assert bytes_tree.to_bit_array(encode.invalid_session(False))
    == <<"{\"d\":false,\"op\":9}">>
}

pub fn gateway_error_test() {
  let frame =
    encode.outbound(session.GatewayErrorFrame(
      error: error.VoiceChannelFull,
      payload: raw_json.null(),
    ))
    |> bytes_tree.to_bit_array
  assert frame
    == <<
      "{\"d\":{\"code\":\"VOICE_CHANNEL_FULL\",\"message\":\"Voice channel is full\"},\"op\":12}",
    >>
}
