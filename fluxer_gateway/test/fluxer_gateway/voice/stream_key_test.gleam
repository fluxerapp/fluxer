// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/id
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/store
import fluxer_gateway/voice/stream_key
import gleam/option.{None, Some}
import gleeunit/should
import support/voice_ids

pub fn parse_guild_key_test() {
  let assert Ok(key) = stream_key.parse("1:2:conn")
  key.connection_id |> should.equal(voice_ids.conn("conn"))
  stream_key.parse("dm:2:conn") |> should.be_ok
  stream_key.parse("x:2:conn") |> should.be_error
  stream_key.parse("1::conn") |> should.be_error
  stream_key.parse("1:2") |> should.be_error
}

pub fn bound_dedups_test() {
  stream_key.bound(["a", "b", "a"]) |> should.equal(["a", "b"])
}

pub fn validate_test() {
  let assert Ok(user) = id.user_id(5)
  let assert Ok(channel) = id.channel_id(2)
  let assert Ok(guild) = id.guild_id(1)
  let owner =
    entry.new(
      user:,
      guild: Some(guild),
      channel:,
      connection_id: voice_ids.conn("conn"),
      session_id: None,
      flags: entry.no_flags(),
      mute: False,
      deaf: False,
    )
  let assert Ok(key) = entry.key(owner)
  let states = store.from_list([#(key, owner)])
  stream_key.validate(["1:2:conn"], Some(guild), channel, states)
  |> should.equal(Ok(["1:2:conn"]))
  stream_key.validate(["1:2:other"], Some(guild), channel, states)
  |> should.equal(Error(error.VoiceConnectionNotFound))
  stream_key.validate(["9:2:conn"], Some(guild), channel, states)
  |> should.equal(Error(error.VoiceInvalidState))
}
