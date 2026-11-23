// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/data.{GuildData}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/sessions/passive_sync
import fluxer_gateway/guild/state/channels
import fluxer_gateway/voice/client_update
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/guild_state
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import support/voice_ids

fn decoded_channel(text: String) -> channel.Channel {
  let assert Ok(value) = json.parse(text, decode.dynamic)
  let assert Ok(found) = data_decode.decode_channel(value)
  found
}

fn sample() -> entry.Entry {
  let assert Ok(user) = id.user_id(3)
  let assert Ok(channel) = id.channel_id(4)
  let assert Ok(guild) = id.guild_id(5)
  entry.new(
    user:,
    guild: Some(guild),
    channel:,
    connection_id: voice_ids.conn("c1"),
    session_id: Some("s1"),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
  |> entry.set_routing(Some("eu"), Some("srv"))
}

pub fn channel_facts_are_typed_at_ingest_test() {
  let facts =
    decoded_channel(
      "{\"id\":\"9\",\"type\":2,\"user_limit\":7,\"voice_connection_limit\":500,\"rtc_p2p\":true}",
    )
    |> guild_state.channel_facts
  assert facts
    == guild_state.ChannelFacts(
      user_limit: 7,
      connection_limit: 100,
      rtc_p2p: True,
      is_voice: True,
    )
}

pub fn lenient_channel_facts_fall_back_to_defaults_test() {
  let facts =
    decoded_channel(
      "{\"id\":\"9\",\"type\":0,\"user_limit\":\"x\",\"voice_connection_limit\":0,\"rtc_p2p\":null}",
    )
    |> guild_state.channel_facts
  assert facts
    == guild_state.ChannelFacts(
      user_limit: 0,
      connection_limit: 5,
      rtc_p2p: False,
      is_voice: False,
    )
}

pub fn last_message_is_typed_at_ingest_test() {
  assert channel.last_message(decoded_channel(
      "{\"id\":\"9\",\"last_message_id\":\"1310000000000000001\"}",
    ))
    == Some(1_310_000_000_000_000_001)
  assert channel.last_message(decoded_channel(
      "{\"id\":\"9\",\"last_message_id\":null}",
    ))
    == None
}

pub fn patch_member_keeps_order_and_replaces_flags_test() {
  let member = raw_json.wrap(<<"{\"nick\":\"a\",\"mute\":false,\"roles\":[]}">>)
  assert raw_json.to_bits(entry.patch_member(member, True, True))
    == <<"{\"nick\":\"a\",\"mute\":true,\"roles\":[],\"deaf\":true}">>
}

pub fn member_flag_reads_one_field_test() {
  let member = raw_json.wrap(<<"{\"mute\":true,\"deaf\":null}">>)
  assert entry.member_flag(member, "mute")
  assert !entry.member_flag(member, "deaf")
  assert !entry.member_flag(member, "missing")
}

pub fn external_from_broadcast_matches_external_test() {
  let value = sample()
  let broadcast = entry.to_raw(value, entry.Broadcast)
  assert json.to_string(entry.external_from(value, broadcast))
    == json.to_string(entry.to_json(value, entry.External))
}

pub fn moved_entry_writes_the_new_channel_test() {
  let assert Ok(other) = id.channel_id(8)
  let text =
    sample()
    |> entry.set_channel(Some(other))
    |> entry.to_json(entry.Broadcast)
    |> json.to_string
  let assert Ok(channel_id) =
    json.parse(text, decode.at(["channel_id"], decode.string))
  assert channel_id == "8"
  let gone =
    sample()
    |> entry.disconnected
    |> entry.to_json(entry.Broadcast)
    |> json.to_string
  let assert Ok(None) =
    json.parse(gone, decode.at(["channel_id"], decode.optional(decode.string)))
}

pub fn signal_payload_is_the_client_slice_test() {
  let frame =
    raw_json.wrap(<<
      "{\"channel_id\":\"8\",\"to\":\"c2\",\"data\":{\"sdp\":\"v=0\",\"n\":2}}",
    >>)
  let assert Ok(signal) = client_update.signal(frame)
  assert raw_json.to_bits(signal.data) == <<"{\"sdp\":\"v=0\",\"n\":2}">>
  assert signal.to == "c2"
  let not_object =
    raw_json.wrap(<<"{\"channel_id\":\"8\",\"to\":\"c2\",\"data\":[1]}">>)
  assert client_update.signal(not_object) == Error(Nil)
}

fn guild_with(row: channel.Channel) -> data.GuildData {
  let assert Ok(guild) = id.guild_id(5)
  GuildData(
    facts: core_guild.facts(
      id: guild,
      owner: None,
      nsfw_level: core_guild.nsfw_level_from_int(0),
      features: [],
      member_count: 1,
    ),
    properties: dict.new(),
    roles: [],
    channels: [row],
    channel_patches: dict.new(),
    emojis: data_decode.raw_list(Error(Nil)),
    stickers: data_decode.raw_list(Error(Nil)),
  )
}

fn message(text: String) -> dynamic.Dynamic {
  let assert Ok(value) = json.parse(text, decode.dynamic)
  value
}

pub fn message_create_moves_last_message_forward_only_test() {
  let row = decoded_channel("{\"id\":\"9\",\"last_message_id\":\"500\"}")
  let guild_data =
    guild_with(row)
    |> channels.message_create(message(
      "{\"id\":\"700\",\"channel_id\":\"9\",\"type\":0}",
    ))
    |> channels.message_create(message(
      "{\"id\":\"600\",\"channel_id\":\"9\",\"type\":0}",
    ))
    |> channels.message_create(message(
      "{\"id\":\"900\",\"channel_id\":\"9\",\"type\":18}",
    ))
  assert passive_sync.last_messages(guild_data) == dict.from_list([#(9, 700)])
  let assert [stored] = guild_data.channels
  let assert Ok(wire) =
    raw_json.read(
      data.channel_wire(guild_data, stored),
      decode.at(["last_message_id"], decode.string),
    )
  assert wire == "700"
}

pub fn integer_message_ids_are_written_as_strings_test() {
  let row = decoded_channel("{\"id\":\"9\"}")
  let guild_data =
    guild_with(row)
    |> channels.message_create(message("{\"id\":800,\"channel_id\":\"9\"}"))
  let assert [stored] = guild_data.channels
  assert raw_json.to_bits(data.channel_wire(guild_data, stored))
    == <<"{\"id\":\"9\",\"last_message_id\":\"800\"}">>
}
