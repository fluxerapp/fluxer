// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/token
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import support/voice_ids

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
    flags: entry.Flags(..entry.no_flags(), self_mute: True),
    mute: False,
    deaf: True,
  )
  |> entry.set_routing(Some("eu"), Some("srv"))
}

pub fn broadcast_hides_routing_test() {
  let text = sample() |> entry.to_json(entry.Broadcast) |> json.to_string
  string.contains(text, "region_id") |> should.be_false
  string.contains(text, "\"self_mute\":true") |> should.be_true
  string.contains(text, "\"deaf\":true") |> should.be_true
}

pub fn external_hides_geolocation_test() {
  let text = sample() |> entry.to_json(entry.External) |> json.to_string
  string.contains(text, "\"region_id\":\"eu\"") |> should.be_true
  string.contains(text, "latitude") |> should.be_false
  string.contains(text, "longitude") |> should.be_false
  let broadcast = sample() |> entry.to_json(entry.Broadcast) |> json.to_string
  string.contains(broadcast, "latitude") |> should.be_false
  string.contains(broadcast, "longitude") |> should.be_false
}

pub fn with_session_keeps_version_test() {
  let value = sample() |> entry.bump |> entry.bump
  let moved = entry.with_session(value, Some("s2"))
  entry.version(moved) |> should.equal(2)
  entry.session_id(moved) |> should.equal(Some("s2"))
  entry.region_id(moved) |> should.equal(Some("eu"))
}

pub fn patch_member_test() {
  let member = raw_json.from_json(json.object([#("mute", json.bool(False))]))
  entry.patch_member(member, True, True)
  |> entry.member_flag("deaf")
  |> should.be_true
}

pub fn account_error_test() {
  token.account_error("{\"code\":\"ACCOUNT_VERIFICATION_REQUIRED\"}")
  |> should.equal(error.VoiceAccountVerificationRequired)
  token.account_error(
    "{\"error\":{\"code\":\"UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS\"}}",
  )
  |> should.equal(error.VoiceUnclaimedAccount)
  token.account_error("nope") |> should.equal(error.VoiceTokenFailed)
}

pub fn disconnected_clears_channel_test() {
  sample() |> entry.disconnected |> entry.channel |> should.equal(None)
}
