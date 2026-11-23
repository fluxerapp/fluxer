// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/voice/call/server
import fluxer_gateway/voice/call/state
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/token
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import support/voice_ids

fn fresh_call(value: Int) -> state.Call {
  let assert Ok(channel) = id.channel_id(value)
  let assert Ok(message) = id.message_id(value)
  state.new(
    state.Init(
      channel:,
      message:,
      region: None,
      ringing: [],
      recipients: [],
      caller: None,
    ),
    0,
  )
}

pub fn every_participant_gets_the_same_room_key_test() {
  let call = fresh_call(40)
  let #(first, call) = state.room_key(call)
  let #(second, call) = state.room_key(call)
  let #(third, _) = state.room_key(call)
  assert first != ""
  assert first == second
  assert second == third
}

pub fn a_new_call_gets_a_new_room_key_test() {
  let #(first, _) = state.room_key(fresh_call(41))
  let #(second, _) = state.room_key(fresh_call(41))
  assert first != second
}

fn dm_entry(capable: Bool) -> entry.Entry {
  let assert Ok(user) = id.user_id(7)
  let assert Ok(channel) = id.channel_id(42)
  entry.new(
    user:,
    guild: None,
    channel:,
    connection_id: voice_ids.conn("c1"),
    session_id: Some("s1"),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
  |> entry.set_e2ee_capable(capable)
}

pub fn p2p_conversion_reuses_the_call_room_key_test() {
  let #(existing, call) = state.room_key(fresh_call(42))
  let #(key, call) = server.conversion_key(call, [dm_entry(True)], True)
  assert key == Some(existing)
  let #(again, _) = state.room_key(call)
  assert again == existing
}

pub fn p2p_conversion_creates_and_stores_a_room_key_test() {
  let #(key, call) =
    server.conversion_key(fresh_call(43), [dm_entry(True)], True)
  let #(stored, _) = state.room_key(call)
  assert key == Some(stored)
}

pub fn conversion_without_capable_sessions_has_no_key_test() {
  let #(key, _) = server.conversion_key(fresh_call(44), [dm_entry(False)], True)
  assert key == None
  let #(key, _) = server.conversion_key(fresh_call(44), [dm_entry(True)], False)
  assert key == None
}

pub fn converted_server_update_carries_the_key_test() {
  let assert Ok(channel) = id.channel_id(45)
  let granted =
    token.Token(
      token: "t",
      endpoint: "e",
      connection_id: voice_ids.conn("c9"),
      region_id: None,
      server_id: None,
    )
  let read = fn(key) {
    server.server_update(channel, granted, key)
    |> raw_json.to_bits
    |> json.parse_bits({
      use found <- decode.optional_field(
        "e2ee_key",
        None,
        decode.optional(decode.string),
      )
      decode.success(found)
    })
  }
  assert read(Some("k")) == Ok(Some("k"))
  assert read(None) == Ok(None)
}
