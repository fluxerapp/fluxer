// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nats/pool as nats
import fluxer_gateway/push/outbox/state.{type SendResult}
import gleam/dynamic/decode
import gleam/json

pub fn send(subject: String, body: BitArray, timeout_ms: Int) -> SendResult {
  case nats.request(subject, body, timeout_ms) {
    Ok(payload) -> decode_reply(payload)
    Error(nats.NatsTimeout) -> state.Failed("timeout")
    Error(nats.NotConnected) -> state.Failed("not_connected")
    Error(nats.NoResponders) -> state.Failed("no_responders")
    Error(nats.ConnectionClosed) -> state.Failed("connection_closed")
    Error(nats.ProtocolError(_)) -> state.Failed("protocol_error")
  }
}

type Reply {
  Accepted
  Rejected(error: String)
}

fn decode_reply(payload: BitArray) -> SendResult {
  let decoder = {
    use ok <- decode.field("ok", decode.bool)
    case ok {
      True -> decode.success(Accepted)
      False -> {
        use error <- decode.optional_field(
          "error",
          "null",
          decode.one_of(decode.string, [decode.success("null")]),
        )
        decode.success(Rejected(error))
      }
    }
  }
  case json.parse_bits(payload, decoder) {
    Ok(Accepted) -> state.Delivered
    Ok(Rejected("in_flight")) -> state.InFlightElsewhere
    Ok(Rejected(error)) -> state.Failed("rejected:" <> error)
    Error(_) -> state.Failed("invalid_reply")
  }
}
