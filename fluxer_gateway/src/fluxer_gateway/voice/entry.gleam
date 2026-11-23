// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/core/voice_state.{type VoiceFlag, type VoiceState}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type Entry {
  Entry(
    state: VoiceState,
    session_id: Option(String),
    e2ee_capable: Bool,
    p2p: Bool,
    ids: Ids,
  )
}

pub opaque type BroadcastView {
  BroadcastView(
    guild: Option(GuildId),
    channel: Option(ChannelId),
    user: UserId,
    connection_id: ConnectionId,
    session_id: Option(String),
    member: Option(BitArray),
    flags: List(Bool),
    viewer_stream_keys: List(String),
    e2ee_capable: Bool,
    p2p: Bool,
    version: Int,
  )
}

type Ids {
  Ids(guild: Json, channel: Json, user: Json)
}

pub type Flags {
  Flags(
    self_mute: Bool,
    self_deaf: Bool,
    self_video: Bool,
    self_stream: Bool,
    is_mobile: Bool,
    suppress: Bool,
  )
}

pub type Shape {
  External
  Broadcast
}

pub fn no_flags() -> Flags {
  Flags(False, False, False, False, False, False)
}

pub fn new(
  user user: UserId,
  guild guild: Option(GuildId),
  channel channel: ChannelId,
  connection_id connection_id: ConnectionId,
  session_id session_id: Option(String),
  flags flags: Flags,
  mute mute: Bool,
  deaf deaf: Bool,
) -> Entry {
  let state =
    voice_state.new(
      user:,
      guild:,
      channel: Some(channel),
      session_id: option.unwrap(session_id, ""),
      connection_id:,
    )
    |> voice_state.set(voice_state.Mute, mute)
    |> voice_state.set(voice_state.Deaf, deaf)
  let state = apply_flags(state, flags)
  Entry(state:, session_id:, e2ee_capable: False, p2p: False, ids: ids(state))
}

fn ids(state: VoiceState) -> Ids {
  Ids(
    guild: nullable_id(voice_state.guild(state), id.guild_id_to_int),
    channel: channel_json(voice_state.channel(state)),
    user: id.json(id.user_id_to_int(voice_state.user(state))),
  )
}

fn channel_json(channel: Option(ChannelId)) -> Json {
  nullable_id(channel, id.channel_id_to_int)
}

fn apply_flags(state: VoiceState, flags: Flags) -> VoiceState {
  state
  |> voice_state.set(voice_state.SelfMute, flags.self_mute)
  |> voice_state.set(voice_state.SelfDeaf, flags.self_deaf)
  |> voice_state.set(voice_state.SelfVideo, flags.self_video)
  |> voice_state.set(voice_state.SelfStream, flags.self_stream)
  |> voice_state.set(voice_state.IsMobile, flags.is_mobile)
  |> voice_state.set(voice_state.Suppress, flags.suppress)
}

pub fn from_state(state: VoiceState) -> Entry {
  let session_id = case voice_state.session_id(state) {
    "" -> None
    text -> Some(text)
  }
  Entry(state:, session_id:, e2ee_capable: False, p2p: False, ids: ids(state))
}

pub fn state(entry: Entry) -> VoiceState {
  entry.state
}

pub fn user(entry: Entry) -> UserId {
  voice_state.user(entry.state)
}

pub fn channel(entry: Entry) -> Option(ChannelId) {
  voice_state.channel(entry.state)
}

pub fn in_channel(entry: Entry, channel: ChannelId) -> Bool {
  voice_state.channel(entry.state) == Some(channel)
}

pub fn connection_id(entry: Entry) -> ConnectionId {
  voice_state.connection_id(entry.state)
}

pub fn key(entry: Entry) -> Result(VoiceKey, Nil) {
  voice_state.key(entry.state)
}

pub fn session_id(entry: Entry) -> Option(String) {
  entry.session_id
}

pub fn has(entry: Entry, flag: VoiceFlag) -> Bool {
  voice_state.has(entry.state, flag)
}

pub fn flags(entry: Entry) -> Flags {
  Flags(
    self_mute: has(entry, voice_state.SelfMute),
    self_deaf: has(entry, voice_state.SelfDeaf),
    self_video: has(entry, voice_state.SelfVideo),
    self_stream: has(entry, voice_state.SelfStream),
    is_mobile: has(entry, voice_state.IsMobile),
    suppress: has(entry, voice_state.Suppress),
  )
}

pub fn set_flag(entry: Entry, flag: VoiceFlag, on: Bool) -> Entry {
  Entry(..entry, state: voice_state.set(entry.state, flag, on))
}

pub fn set_flags(entry: Entry, flags: Flags) -> Entry {
  Entry(..entry, state: apply_flags(entry.state, flags))
}

pub fn is_p2p(entry: Entry) -> Bool {
  entry.p2p
}

pub fn set_p2p(entry: Entry, p2p: Bool) -> Entry {
  Entry(..entry, p2p:)
}

pub fn e2ee_capable(entry: Entry) -> Bool {
  entry.e2ee_capable
}

pub fn set_e2ee_capable(entry: Entry, capable: Bool) -> Entry {
  Entry(..entry, e2ee_capable: capable)
}

pub fn member(entry: Entry) -> Option(RawJson) {
  voice_state.member(entry.state)
}

pub fn set_member(entry: Entry, member: Option(RawJson)) -> Entry {
  Entry(..entry, state: voice_state.set_member(entry.state, member))
}

pub fn viewer_stream_keys(entry: Entry) -> List(String) {
  voice_state.viewer_stream_keys(entry.state)
}

pub fn set_viewer_stream_keys(entry: Entry, keys: List(String)) -> Entry {
  Entry(..entry, state: voice_state.set_viewer_stream_keys(entry.state, keys))
}

pub fn region_id(entry: Entry) -> Option(String) {
  voice_state.region_id(entry.state)
}

pub fn server_id(entry: Entry) -> Option(String) {
  voice_state.server_id(entry.state)
}

pub fn set_routing(
  entry: Entry,
  region: Option(String),
  server: Option(String),
) -> Entry {
  let region = option.or(non_empty(region), region_id(entry))
  let server = option.or(non_empty(server), server_id(entry))
  Entry(..entry, state: voice_state.set_server(entry.state, region:, server:))
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some("") -> None
    other -> other
  }
}

pub fn version(entry: Entry) -> Int {
  voice_state.version(entry.state)
}

pub fn bump(entry: Entry) -> Entry {
  Entry(..entry, state: voice_state.bump_version(entry.state))
}

pub fn set_channel(entry: Entry, channel: Option(ChannelId)) -> Entry {
  let ids = case voice_state.channel(entry.state) == channel {
    True -> entry.ids
    False -> Ids(..entry.ids, channel: channel_json(channel))
  }
  Entry(..entry, state: voice_state.set_channel(entry.state, channel), ids:)
}

pub fn guild_json(entry: Entry) -> Json {
  entry.ids.guild
}

pub fn to_sfu(entry: Entry) -> Entry {
  Entry(..entry, p2p: False) |> bump
}

pub fn with_session(entry: Entry, session_id: Option(String)) -> Entry {
  case session_id == entry.session_id {
    True -> entry
    False -> {
      let old = entry.state
      let fresh =
        voice_state.new(
          user: voice_state.user(old),
          guild: voice_state.guild(old),
          channel: voice_state.channel(old),
          session_id: option.unwrap(session_id, ""),
          connection_id: voice_state.connection_id(old),
        )
        |> copy_flags(old)
        |> voice_state.set_viewer_stream_keys(voice_state.viewer_stream_keys(
          old,
        ))
        |> voice_state.set_server(
          region: voice_state.region_id(old),
          server: voice_state.server_id(old),
        )
        |> voice_state.set_member(voice_state.member(old))
        |> bump_to(voice_state.version(old))
      Entry(..entry, state: fresh, session_id:)
    }
  }
}

fn copy_flags(state: VoiceState, from: VoiceState) -> VoiceState {
  [
    voice_state.SelfMute,
    voice_state.SelfDeaf,
    voice_state.SelfVideo,
    voice_state.SelfStream,
    voice_state.IsMobile,
    voice_state.Suppress,
    voice_state.Mute,
    voice_state.Deaf,
  ]
  |> list.fold(state, fn(acc, flag) {
    voice_state.set(acc, flag, voice_state.has(from, flag))
  })
}

fn bump_to(state: VoiceState, target: Int) -> VoiceState {
  case voice_state.version(state) < target {
    True -> bump_to(voice_state.bump_version(state), target)
    False -> state
  }
}

pub fn to_json(entry: Entry, shape: Shape) -> Json {
  case shape {
    Broadcast -> json.object(fields(entry))
    External -> json.object(list.append(fields(entry), routing(entry)))
  }
}

pub fn to_raw(entry: Entry, shape: Shape) -> RawJson {
  raw_json.from_json(to_json(entry, shape))
}

pub fn external_from(entry: Entry, broadcast: RawJson) -> Json {
  routing(entry)
  |> list.try_fold(broadcast, fn(raw, pair) {
    raw_json.with_field(raw, pair.0, raw_json.from_json(pair.1))
  })
  |> result.map(raw_json.to_json)
  |> result.lazy_unwrap(fn() { to_json(entry, External) })
}

pub fn disconnected(entry: Entry) -> Entry {
  set_channel(entry, None)
}

fn fields(entry: Entry) -> List(#(String, Json)) {
  let state = entry.state
  [
    #("guild_id", entry.ids.guild),
    #("channel_id", entry.ids.channel),
    #("user_id", entry.ids.user),
    #(
      "connection_id",
      voice_key.connection_json(voice_state.connection_id(state)),
    ),
    #("session_id", json.nullable(entry.session_id, json.string)),
    #("member", json.nullable(voice_state.member(state), raw_json.to_json)),
    #("mute", json.bool(voice_state.has(state, voice_state.Mute))),
    #("deaf", json.bool(voice_state.has(state, voice_state.Deaf))),
    #("self_mute", json.bool(voice_state.has(state, voice_state.SelfMute))),
    #("self_deaf", json.bool(voice_state.has(state, voice_state.SelfDeaf))),
    #("self_video", json.bool(voice_state.has(state, voice_state.SelfVideo))),
    #("self_stream", json.bool(voice_state.has(state, voice_state.SelfStream))),
    #("is_mobile", json.bool(voice_state.has(state, voice_state.IsMobile))),
    #("suppress", json.bool(voice_state.has(state, voice_state.Suppress))),
    #(
      "viewer_stream_keys",
      json.array(voice_state.viewer_stream_keys(state), json.string),
    ),
    #("e2ee_capable", json.bool(entry.e2ee_capable)),
    #("p2p", json.bool(entry.p2p)),
    #("version", json.int(voice_state.version(state))),
  ]
}

pub fn broadcast_view(entry: Entry) -> BroadcastView {
  let state = entry.state
  BroadcastView(
    guild: voice_state.guild(state),
    channel: voice_state.channel(state),
    user: voice_state.user(state),
    connection_id: voice_state.connection_id(state),
    session_id: entry.session_id,
    member: option.map(voice_state.member(state), raw_json.to_bits),
    flags: list.map(
      [
        voice_state.Mute,
        voice_state.Deaf,
        voice_state.SelfMute,
        voice_state.SelfDeaf,
        voice_state.SelfVideo,
        voice_state.SelfStream,
        voice_state.IsMobile,
        voice_state.Suppress,
      ],
      voice_state.has(state, _),
    ),
    viewer_stream_keys: voice_state.viewer_stream_keys(state),
    e2ee_capable: entry.e2ee_capable,
    p2p: entry.p2p,
    version: voice_state.version(state),
  )
}

fn routing(entry: Entry) -> List(#(String, Json)) {
  [
    #(
      "region_id",
      json.nullable(voice_state.region_id(entry.state), json.string),
    ),
    #(
      "server_id",
      json.nullable(voice_state.server_id(entry.state), json.string),
    ),
  ]
}

fn nullable_id(value: Option(a), to_int: fn(a) -> Int) -> Json {
  case value {
    Some(found) -> id.json(to_int(found))
    None -> json.null()
  }
}

pub fn patch_member(member: RawJson, mute: Bool, deaf: Bool) -> RawJson {
  {
    use raw <- result.try(raw_json.with_field(member, "mute", flag_raw(mute)))
    raw_json.with_field(raw, "deaf", flag_raw(deaf))
  }
  |> result.map(raw_json.flatten)
  |> result.unwrap(member)
}

fn flag_raw(on: Bool) -> RawJson {
  case on {
    True -> raw_json.wrap(<<"true">>)
    False -> raw_json.wrap(<<"false">>)
  }
}

pub fn member_flag(member: RawJson, key: String) -> Bool {
  case raw_json.field(member, key) {
    Ok(value) -> raw_json.to_bits(value) == <<"true">>
    Error(Nil) -> False
  }
}
