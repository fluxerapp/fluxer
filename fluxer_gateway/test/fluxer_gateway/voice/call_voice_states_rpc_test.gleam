// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/voice/call/state
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/rpc
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/voice_ids

fn channel() -> id.ChannelId {
  let assert Ok(found) = id.channel_id(50)
  found
}

fn connection(user: Int, conn: String) -> entry.Entry {
  let assert Ok(user) = id.user_id(user)
  entry.new(
    user:,
    guild: None,
    channel: channel(),
    connection_id: voice_ids.conn(conn),
    session_id: Some("s" <> conn),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
}

fn call() -> state.Call {
  let assert Ok(message) = id.message_id(50)
  state.new(
    state.Init(
      channel: channel(),
      message:,
      region: None,
      ringing: [],
      recipients: [],
      caller: None,
    ),
    0,
  )
  |> state.put_entry(connection(7, "desktop"))
  |> state.put_entry(connection(7, "phone"))
  |> state.put_entry(connection(8, "other"))
}

pub fn every_connection_of_a_user_is_listed_test() {
  let view = state.view(call())
  let assert Ok(pairs) =
    rpc.call_voice_states(view, channel())
    |> json.to_string
    |> json.parse(decode.at(
      ["voice_states"],
      decode.list({
        use user <- decode.field("user_id", decode.string)
        use conn <- decode.field("connection_id", decode.string)
        decode.success(#(user, conn))
      }),
    ))
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.1, b.1) })
  assert sorted == [#("7", "desktop"), #("8", "other"), #("7", "phone")]
  assert list.map(view.voice_states, fn(value) {
      voice_ids.text(entry.connection_id(value))
    })
    == ["desktop", "phone", "other"]
}
