// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type GuildId}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/store.{type Store}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

const max_keys = 256

pub type Scope {
  GuildScope(GuildId)
  DmScope
}

pub type StreamKey {
  StreamKey(scope: Scope, channel: ChannelId, connection_id: ConnectionId)
}

pub fn parse(text: String) -> Result(StreamKey, Nil) {
  case string.split(text, ":") {
    [scope, channel, connection_id] if channel != "" -> {
      use connection_id <- result.try(voice_key.connection_id(connection_id))
      use channel <- result.try(
        id.parse_channel_id(channel) |> result.replace_error(Nil),
      )
      use scope <- result.try(case scope {
        "dm" -> Ok(DmScope)
        _ ->
          id.parse_guild_id(scope)
          |> result.map(GuildScope)
          |> result.replace_error(Nil)
      })
      Ok(StreamKey(scope:, channel:, connection_id:))
    }
    _ -> Error(Nil)
  }
}

pub fn bound(keys: List(String)) -> List(String) {
  keys
  |> list.unique
  |> list.take(max_keys)
}

pub fn validate(
  keys: List(String),
  guild: Option(GuildId),
  channel: ChannelId,
  states: Store(Entry),
) -> Result(List(String), GatewayError) {
  keys
  |> list.try_map(fn(key) { validate_one(key, guild, channel, states) })
  |> result.map(bound)
}

fn validate_one(
  key: String,
  guild: Option(GuildId),
  channel: ChannelId,
  states: Store(Entry),
) -> Result(String, GatewayError) {
  case parse(key) {
    Error(Nil) -> Error(error.VoiceInvalidState)
    Ok(parsed) -> {
      let scope_ok = case parsed.scope, guild {
        GuildScope(_), None -> True
        GuildScope(owner), Some(guild) -> owner == guild
        DmScope, _ -> False
      }
      case scope_ok && parsed.channel == channel {
        False -> Error(error.VoiceInvalidState)
        True -> stream_owner(key, parsed, channel, states)
      }
    }
  }
}

pub fn validate_dm(
  keys: List(String),
  channel: ChannelId,
  states: Store(Entry),
) -> Result(List(String), GatewayError) {
  keys
  |> list.try_map(fn(key) {
    case parse(key) {
      Ok(StreamKey(DmScope, parsed_channel, _) as parsed)
        if parsed_channel == channel
      -> stream_owner(key, parsed, channel, states)
      _ -> Error(error.VoiceInvalidState)
    }
  })
  |> result.map(bound)
}

fn stream_owner(
  key: String,
  parsed: StreamKey,
  channel: ChannelId,
  states: Store(Entry),
) -> Result(String, GatewayError) {
  use owner <- result.try(
    store.by_connection(states, parsed.connection_id)
    |> result.map(fn(pair) { pair.1 })
    |> result.replace_error(error.VoiceConnectionNotFound),
  )
  case entry.in_channel(owner, channel) {
    True -> Ok(key)
    False -> Error(error.VoiceInvalidState)
  }
}
