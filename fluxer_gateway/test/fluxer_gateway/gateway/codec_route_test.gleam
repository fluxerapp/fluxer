// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/gateway/codec
import fluxer_gateway/gateway/opcode
import gleam/dynamic/decode
import gleam/json

fn data(text: String) {
  let assert Ok(value) = json.parse(text, decode.dynamic)
  value
}

pub fn presence_is_normalised_test() {
  let assert Ok(normalised) =
    codec.presence(data("{\"status\":\"offline\",\"afk\":true,\"mobile\":1}"))
  let decoder = {
    use status <- decode.field("status", decode.string)
    use afk <- decode.field("afk", decode.bool)
    use mobile <- decode.field("mobile", decode.bool)
    decode.success(#(status, afk, mobile))
  }
  assert decode.run(normalised, decoder) == Ok(#("invisible", True, False))
}

pub fn presence_keeps_custom_status_test() {
  let assert Ok(normalised) =
    codec.presence(data("{\"status\":\"\",\"custom_status\":{\"text\":\"hi\"}}"))
  assert decode.run(
      normalised,
      decode.at(["custom_status", "text"], decode.string),
    )
    == Ok("hi")
  assert decode.run(normalised, decode.at(["status"], decode.string))
    == Ok("online")
}

pub fn presence_rejects_test() {
  assert codec.presence(data("{}")) == Error(Nil)
  assert codec.presence(data("{\"status\":\"away\"}")) == Error(Nil)
  assert codec.presence(data("null")) == Error(Nil)
}

pub fn routes_test() {
  let assert codec.Reject(error.DecodeError, "Invalid presence payload") =
    codec.route(opcode.PresenceUpdate, data("null"))
  let assert codec.Reject(error.DecodeError, "Invalid voice payload") =
    codec.route(opcode.VoiceStateUpdate, data("[]"))
  let assert codec.QueueVoice(_) =
    codec.route(opcode.VoiceStateUpdate, data("{}"))
  assert codec.signal_route(<<"{\"op\":13,\"d\":1}">>) == codec.Ignore
  let assert codec.Forward(session.VoiceSignal(signal)) =
    codec.signal_route(<<"{\"op\":13,\"d\":{\"to\":\"c\"}}">>)
  assert raw_json.to_bits(signal) == <<"{\"to\":\"c\"}">>
  let assert codec.Forward(session.LazyRequest(_)) =
    codec.route(opcode.LazyRequest, data("null"))
  let assert codec.Reject(error.UnknownOpcode, _) =
    codec.route(opcode.VoiceServerPing, data("{}"))
  let assert codec.Reject(error.UnknownOpcode, _) =
    codec.route(opcode.RequestForumUnreads, data("1"))
  let assert codec.Forward(session.RequestForumUnreads(_)) =
    codec.route(opcode.RequestForumUnreads, data("{}"))
  let assert codec.Reject(error.UnknownOpcode, _) =
    codec.route(opcode.Dispatch, data("{}"))
}
