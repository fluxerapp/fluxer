// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type Outbound}
import gleam/bytes_tree.{type BytesTree}
import gleam/int
import gleam/json

pub fn outbound(frame: Outbound) -> BytesTree {
  case frame {
    session.DispatchFrame(seq:, event:, payload:) ->
      dispatch(seq, event, payload)
    session.ReconnectFrame -> reconnect()
    session.HeartbeatAckFrame -> heartbeat_ack()
    session.GatewayErrorFrame(error:, payload:) -> gateway_error(error, payload)
  }
}

pub fn dispatch(seq: Int, event: String, payload: RawJson) -> BytesTree {
  <<
    "{\"op\":0,\"t\":\"":utf8,
    event:utf8,
    "\",\"s\":":utf8,
    int.to_string(seq):utf8,
    ",\"d\":":utf8,
  >>
  |> bytes_tree.from_bit_array
  |> bytes_tree.append_tree(raw_json.to_iodata(payload))
  |> bytes_tree.append(<<"}":utf8>>)
}

pub fn hello(heartbeat_interval_ms: Int) -> BytesTree {
  bytes_tree.from_bit_array(<<
    "{\"d\":{\"heartbeat_interval\":":utf8,
    int.to_string(heartbeat_interval_ms):utf8,
    "},\"op\":10}":utf8,
  >>)
}

pub fn heartbeat_request() -> BytesTree {
  bytes_tree.from_bit_array(<<"{\"d\":null,\"op\":1}":utf8>>)
}

pub fn heartbeat_ack() -> BytesTree {
  bytes_tree.from_bit_array(<<"{\"op\":11}":utf8>>)
}

fn reconnect() -> BytesTree {
  bytes_tree.from_bit_array(<<"{\"op\":7}":utf8>>)
}

pub fn invalid_session(resumable: Bool) -> BytesTree {
  case resumable {
    True -> <<"{\"d\":true,\"op\":9}":utf8>>
    False -> <<"{\"d\":false,\"op\":9}":utf8>>
  }
  |> bytes_tree.from_bit_array
}

fn gateway_error(error: GatewayError, payload: RawJson) -> BytesTree {
  let data = case error.voice_info(error) {
    Ok(info) ->
      json.object([
        #("code", json.string(info.code)),
        #("message", json.string(info.message)),
      ])
    Error(Nil) -> raw_json.to_json(payload)
  }
  json.object([#("d", data), #("op", json.int(12))])
  |> json.to_string_tree
  |> bytes_tree.from_string_tree
}
