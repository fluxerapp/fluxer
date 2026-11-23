// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/guild.{type GuildFacts}
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/voice/entry.{type Entry}
import gleam/bit_array
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/list
import gleam/string

fn platform_wide() -> Bool {
  rollout.voice_e2ee_scope() == rollout.PlatformWide
}

pub fn enabled_for_guild(facts: GuildFacts) -> Bool {
  platform_wide() || guild.has_feature(facts, "VOICE_E2EE")
}

pub fn enabled_for_dm() -> Bool {
  platform_wide()
}

fn channel_active(states: List(Entry), channel: ChannelId) -> Bool {
  let in_channel = list.filter(states, entry.in_channel(_, channel))
  in_channel != [] && list.all(in_channel, entry.e2ee_capable)
}

pub fn check_join(
  states: List(Entry),
  channel: ChannelId,
  capable capable: Bool,
  bot bot: Bool,
) -> Result(Nil, GatewayError) {
  case bot || capable {
    True -> Ok(Nil)
    False ->
      case channel_active(states, channel) {
        True -> Error(error.VoiceE2eeRequired)
        False -> Ok(Nil)
      }
  }
}

pub fn room_key(
  keys: Dict(ChannelId, String),
  channel: ChannelId,
) -> #(String, Dict(ChannelId, String)) {
  case dict.get(keys, channel) {
    Ok(key) -> #(key, keys)
    Error(Nil) -> {
      let key = new_key()
      #(key, dict.insert(keys, channel, key))
    }
  }
}

pub fn forget_if_idle(
  keys: Dict(ChannelId, String),
  channel: ChannelId,
  states: List(Entry),
  pending_e2ee: Bool,
) -> Dict(ChannelId, String) {
  case list.any(states, entry.in_channel(_, channel)) || pending_e2ee {
    True -> keys
    False -> dict.delete(keys, channel)
  }
}

pub fn new_key() -> String {
  crypto.strong_random_bytes(32)
  |> bit_array.base64_url_encode(False)
  |> string.trim
}
