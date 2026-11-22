// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/voice_key
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/store
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/voice_ids

fn connection(
  user: Int,
  channel: Int,
  conn: String,
) -> #(voice_key.VoiceKey, entry.Entry) {
  let assert Ok(user) = id.user_id(user)
  let assert Ok(channel) = id.channel_id(channel)
  let value =
    entry.new(
      user:,
      guild: None,
      channel:,
      connection_id: voice_ids.conn(conn),
      session_id: Some("s1"),
      flags: entry.no_flags(),
      mute: False,
      deaf: False,
    )
  let assert Ok(key) = entry.key(value)
  #(key, value)
}

fn targets(conn: option.Option(String)) -> List(String) {
  let assert Ok(user) = id.user_id(1)
  let assert Ok(channel) = id.channel_id(10)
  let states =
    store.from_list([
      connection(1, 10, "voice"),
      connection(1, 10, "stream"),
      connection(1, 11, "elsewhere"),
      connection(2, 10, "stranger"),
    ])
  dm_server.force_targets(
    states,
    user,
    channel,
    option.map(conn, voice_ids.conn),
  )
  |> list.map(fn(pair) { voice_ids.text(voice_key.connection(pair.0)) })
  |> list.sort(string.compare)
}

pub fn call_side_disconnect_drops_every_connection_in_the_channel_test() {
  assert targets(None) == ["stream", "voice"]
}

pub fn specific_disconnect_drops_only_that_connection_test() {
  assert targets(Some("stream")) == ["stream"]
}

pub fn specific_disconnect_ignores_a_connection_in_another_channel_test() {
  assert targets(Some("elsewhere")) == []
}
