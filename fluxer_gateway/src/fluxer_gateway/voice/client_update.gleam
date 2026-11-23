// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/util/decode as edge
import fluxer_gateway/voice/entry.{type Flags, Flags}
import fluxer_gateway/voice/geo.{type Geo}
import gleam/dict
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

pub type ViewerKeys {
  KeepKeys
  ClearKeys
  Keys(List(String))
  BadKeys
}

pub type Update {
  Update(
    channel: Result(Option(ChannelId), Nil),
    connection_id: Option(ConnectionId),
    flags: Flags,
    viewer_keys: ViewerKeys,
    geo: Geo,
    e2ee_capable: Bool,
    bot: Bool,
    p2p: Option(Bool),
    country_code: Option(String),
  )
}

pub type Signal {
  Signal(channel: ChannelId, to: String, data: RawJson)
}

pub fn empty() -> Update {
  Update(
    channel: Ok(None),
    connection_id: None,
    flags: Flags(
      self_mute: False,
      self_deaf: False,
      self_video: False,
      self_stream: False,
      is_mobile: False,
      suppress: False,
    ),
    viewer_keys: KeepKeys,
    geo: geo.unknown(),
    e2ee_capable: False,
    bot: False,
    p2p: None,
    country_code: None,
  )
}

pub fn decoder() -> Decoder(Update) {
  use channel <- decode.optional_field("channel_id", Ok(None), channel())
  use connection_id <- decode.optional_field(
    "connection_id",
    None,
    connection_id(),
  )
  use self_mute <- flag("self_mute")
  use self_deaf <- flag("self_deaf")
  use self_video <- flag("self_video")
  use self_stream <- flag("self_stream")
  use is_mobile <- flag("is_mobile")
  use viewer_keys <- decode.optional_field(
    "viewer_stream_keys",
    KeepKeys,
    viewer_keys(),
  )
  use geo <- decode.then(geo.decoder())
  use e2ee_capable <- flag("e2ee_capable")
  use bot <- flag("bot")
  use p2p <- decode.optional_field("p2p", None, lenient(decode.bool))
  use country_code <- decode.optional_field(
    "country_code",
    None,
    lenient(decode.string),
  )
  decode.success(Update(
    channel:,
    connection_id:,
    flags: Flags(
      self_mute:,
      self_deaf:,
      self_video:,
      self_stream:,
      is_mobile:,
      suppress: False,
    ),
    viewer_keys:,
    geo:,
    e2ee_capable:,
    bot:,
    p2p:,
    country_code:,
  ))
}

pub fn signal(raw: RawJson) -> Result(Signal, Nil) {
  use fields <- result.try(raw_json.fields(raw))
  let fields = dict.from_list(fields)
  use channel <- result.try(
    dict.get(fields, "channel_id")
    |> result.try(raw_json.snowflake)
    |> result.try(fn(found) {
      id.channel_id(found) |> result.replace_error(Nil)
    }),
  )
  use to <- result.try(dict.get(fields, "to") |> result.try(raw_json.string))
  use data <- result.try(dict.get(fields, "data"))
  use _ <- result.map(raw_json.fields(data))
  Signal(channel:, to:, data:)
}

fn flag(key: String, next: fn(Bool) -> Decoder(a)) -> Decoder(a) {
  decode.optional_field(
    key,
    False,
    decode.one_of(decode.bool, [
      decode.map(decode.string, fn(text) { text == "true" }),
      decode.success(False),
    ]),
    next,
  )
}

fn lenient(inner: Decoder(a)) -> Decoder(Option(a)) {
  decode.one_of(decode.map(inner, Some), [decode.success(None)])
}

fn channel() -> Decoder(Result(Option(ChannelId), Nil)) {
  decode.one_of(decode.map(decode.optional(edge.channel_id()), Ok), [
    decode.success(Error(Nil)),
  ])
}

fn connection_id() -> Decoder(Option(ConnectionId)) {
  decode.one_of(decode.map(decode.string, connection_of), [
    decode.map(decode.int, fn(number) { connection_of(int.to_string(number)) }),
    decode.success(None),
  ])
}

fn viewer_keys() -> Decoder(ViewerKeys) {
  decode.one_of(
    decode.map(decode.optional(decode.list(decode.string)), fn(keys) {
      case keys {
        None -> ClearKeys
        Some(keys) -> Keys(keys)
      }
    }),
    [decode.success(BadKeys)],
  )
}

fn connection_of(text: String) -> Option(ConnectionId) {
  voice_key.connection_id(text) |> option.from_result
}
