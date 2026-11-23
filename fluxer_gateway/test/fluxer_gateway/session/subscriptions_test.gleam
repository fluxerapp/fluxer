// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/session/subscriptions
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}
import gleam/set
import gleeunit/should

fn raw(text: String) -> Dynamic {
  let assert Ok(value) = json.parse(text, decode.dynamic)
  value
}

pub fn active_flag_updates_active_guilds_test() {
  let incoming =
    subscriptions.decode_incoming(raw(
      "{\"subscriptions\":{\"100\":{\"active\":true,\"typing\":true}}}",
    ))
  let stored =
    subscriptions.store(
      subscriptions.new(set.new(), dict.new()),
      incoming,
      fn(_) { True },
    )
  subscriptions.is_active(stored.subscriptions, 100) |> should.be_true
}

pub fn member_list_guild_switch_reports_previous_test() {
  let first =
    subscriptions.decode_incoming(raw(
      "{\"subscriptions\":{\"100\":{\"member_list_channels\":{\"1\":[[0,99]]}}}}",
    ))
  let second =
    subscriptions.decode_incoming(raw(
      "{\"subscriptions\":{\"200\":{\"member_list_channels\":{\"2\":[[0,99]]}}}}",
    ))
  let empty = subscriptions.new(set.new(), dict.new())
  let stored = subscriptions.store(empty, first, fn(_) { True })
  stored.previous_member_list_guild |> should.equal(None)
  let switched =
    subscriptions.store(stored.subscriptions, second, fn(_) { True })
  switched.previous_member_list_guild |> should.equal(Some(100))
  subscriptions.member_list_guild(switched.subscriptions)
  |> should.equal(Some(200))
}

pub fn replay_rebuilds_the_guild_subscription_test() {
  let incoming =
    subscriptions.decode_incoming(raw(
      "{\"subscriptions\":{\"100\":{\"typing\":true,\"ignored\":1}}}",
    ))
  let stored =
    subscriptions.store(
      subscriptions.new(set.new(), dict.new()),
      incoming,
      fn(_) { True },
    )
  let assert Some(replay) = subscriptions.replay(stored.subscriptions, 100)
  raw_json.to_bits(replay)
  |> should.equal(<<"{\"subscriptions\":{\"100\":{\"typing\":true}}}">>)
}

pub fn removed_guild_is_no_longer_active_test() {
  subscriptions.new(set.from_list([5]), dict.new())
  |> subscriptions.remove(5)
  |> subscriptions.is_active(5)
  |> should.be_false
}
