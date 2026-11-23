// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import gleam/int
import gleam/option.{type Option, None, Some}

pub opaque type VoiceState {
  VoiceState(
    user: UserId,
    guild: Option(GuildId),
    channel: Option(ChannelId),
    session_id: String,
    connection_id: ConnectionId,
    flags: Int,
    viewer_stream_keys: List(String),
    region_id: Option(String),
    server_id: Option(String),
    version: Int,
    member: Option(RawJson),
  )
}

pub type VoiceFlag {
  SelfMute
  SelfDeaf
  SelfVideo
  SelfStream
  IsMobile
  Suppress
  Mute
  Deaf
}

fn flag_bit(flag: VoiceFlag) -> Int {
  case flag {
    SelfMute -> 1
    SelfDeaf -> 2
    SelfVideo -> 4
    SelfStream -> 8
    IsMobile -> 16
    Suppress -> 32
    Mute -> 64
    Deaf -> 128
  }
}

pub fn new(
  user user: UserId,
  guild guild: Option(GuildId),
  channel channel: Option(ChannelId),
  session_id session_id: String,
  connection_id connection_id: ConnectionId,
) -> VoiceState {
  VoiceState(
    user:,
    guild:,
    channel:,
    session_id:,
    connection_id:,
    flags: 0,
    viewer_stream_keys: [],
    region_id: None,
    server_id: None,
    version: 0,
    member: None,
  )
}

pub fn user(state: VoiceState) -> UserId {
  state.user
}

pub fn guild(state: VoiceState) -> Option(GuildId) {
  state.guild
}

pub fn channel(state: VoiceState) -> Option(ChannelId) {
  state.channel
}

pub fn session_id(state: VoiceState) -> String {
  state.session_id
}

pub fn connection_id(state: VoiceState) -> ConnectionId {
  state.connection_id
}

pub fn key(state: VoiceState) -> Result(VoiceKey, Nil) {
  case state.channel {
    Some(channel) ->
      Ok(voice_key.new(
        channel:,
        user: state.user,
        connection: state.connection_id,
      ))
    None -> Error(Nil)
  }
}

pub fn has(state: VoiceState, flag: VoiceFlag) -> Bool {
  int.bitwise_and(state.flags, flag_bit(flag)) != 0
}

pub fn set(state: VoiceState, flag: VoiceFlag, on: Bool) -> VoiceState {
  let bit = flag_bit(flag)
  let flags = case on {
    True -> int.bitwise_or(state.flags, bit)
    False -> int.bitwise_and(state.flags, int.bitwise_not(bit))
  }
  VoiceState(..state, flags:)
}

pub fn viewer_stream_keys(state: VoiceState) -> List(String) {
  state.viewer_stream_keys
}

pub fn region_id(state: VoiceState) -> Option(String) {
  state.region_id
}

pub fn server_id(state: VoiceState) -> Option(String) {
  state.server_id
}

pub fn version(state: VoiceState) -> Int {
  state.version
}

pub fn member(state: VoiceState) -> Option(RawJson) {
  state.member
}

pub fn set_channel(
  state: VoiceState,
  channel: Option(ChannelId),
) -> VoiceState {
  VoiceState(..state, channel:)
}

pub fn set_viewer_stream_keys(
  state: VoiceState,
  keys: List(String),
) -> VoiceState {
  VoiceState(..state, viewer_stream_keys: keys)
}

pub fn set_server(
  state: VoiceState,
  region region_id: Option(String),
  server server_id: Option(String),
) -> VoiceState {
  VoiceState(..state, region_id:, server_id:)
}

pub fn set_member(state: VoiceState, member: Option(RawJson)) -> VoiceState {
  VoiceState(..state, member:)
}

pub fn bump_version(state: VoiceState) -> VoiceState {
  VoiceState(..state, version: state.version + 1)
}
