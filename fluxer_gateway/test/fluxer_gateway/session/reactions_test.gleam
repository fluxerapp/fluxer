// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/reactions.{Buffered}
import gleam/bit_array
import gleam/list
import gleam/string
import gleeunit/should

fn reaction(message: String, user: String, emoji: String) -> raw_json.RawJson {
  raw_json.wrap(bit_array.from_string(
    "{\"channel_id\":\"10\",\"message_id\":\""
    <> message
    <> "\",\"user_id\":\""
    <> user
    <> "\",\"emoji\":{\"id\":null,\"name\":\""
    <> emoji
    <> "\"}}",
  ))
}

fn facts(raw: raw_json.RawJson) -> dispatch_facts.ReactionFacts {
  let assert dispatch_facts.Reaction(found) =
    dispatch_facts.scan("MESSAGE_REACTION_ADD", raw).detail
  found
}

fn hold(
  held: reactions.Reactions,
  raw: raw_json.RawJson,
) -> reactions.Reactions {
  let Buffered(next, _) = reactions.hold(held, facts(raw), raw)
  next
}

pub fn first_hold_arms_timer_test() {
  let raw = reaction("1", "2", "a")
  let Buffered(_, arm) = reactions.hold(reactions.new(), facts(raw), raw)
  arm |> should.be_true
}

pub fn single_reaction_flushes_as_add_test() {
  let #(_, frames) =
    reactions.new() |> hold(reaction("1", "2", "a")) |> reactions.flush
  frames |> list.map(fn(f) { f.0 }) |> should.equal(["MESSAGE_REACTION_ADD"])
}

pub fn same_message_groups_into_add_many_test() {
  let #(_, frames) =
    reactions.new()
    |> hold(reaction("1", "2", "a"))
    |> hold(reaction("1", "3", "b"))
    |> reactions.flush
  let assert [#(event, payload)] = frames
  event |> should.equal("MESSAGE_REACTION_ADD_MANY")
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(payload))
  string.contains(text, "\"reactions\"") |> should.be_true
}

pub fn remove_cancels_held_add_test() {
  let held = reactions.new() |> hold(reaction("1", "2", "a"))
  let assert Ok(remaining) =
    reactions.cancel(held, facts(reaction("1", "2", "a")))
  reactions.is_empty(remaining) |> should.be_true
}

pub fn remove_without_match_is_not_cancelled_test() {
  let held = reactions.new() |> hold(reaction("1", "2", "a"))
  reactions.cancel(held, facts(reaction("1", "2", "b")))
  |> should.equal(Error(Nil))
}
