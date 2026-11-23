// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/order.{type Order}
import gleam/string

pub opaque type ConnectionId {
  ConnectionId(String)
}

pub opaque type VoiceKey {
  VoiceKey(channel: ChannelId, user: UserId, connection: ConnectionId)
}

pub fn connection_id(text: String) -> Result(ConnectionId, Nil) {
  case text {
    "" -> Error(Nil)
    _ -> Ok(ConnectionId(text))
  }
}

pub fn decoder() -> Decoder(ConnectionId) {
  use text <- decode.then(decode.string)
  case text {
    "" -> decode.failure(ConnectionId(text), "ConnectionId")
    _ -> decode.success(ConnectionId(text))
  }
}

pub fn connection_text(connection: ConnectionId) -> String {
  let ConnectionId(text) = connection
  text
}

pub fn connection_json(connection: ConnectionId) -> Json {
  json.string(connection_text(connection))
}

pub fn compare_connection(a: ConnectionId, b: ConnectionId) -> Order {
  string.compare(connection_text(a), connection_text(b))
}

pub fn new(
  channel channel: ChannelId,
  user user: UserId,
  connection connection: ConnectionId,
) -> VoiceKey {
  VoiceKey(channel:, user:, connection:)
}

pub fn channel(key: VoiceKey) -> ChannelId {
  key.channel
}

pub fn user(key: VoiceKey) -> UserId {
  key.user
}

pub fn connection(key: VoiceKey) -> ConnectionId {
  key.connection
}

pub fn move(key: VoiceKey, to channel: ChannelId) -> VoiceKey {
  VoiceKey(..key, channel:)
}

pub fn compare(a: VoiceKey, b: VoiceKey) -> Order {
  case id.compare_user_id(a.user, b.user) {
    order.Eq ->
      case compare_connection(a.connection, b.connection) {
        order.Eq -> id.compare_channel_id(a.channel, b.channel)
        other -> other
      }
    other -> other
  }
}
