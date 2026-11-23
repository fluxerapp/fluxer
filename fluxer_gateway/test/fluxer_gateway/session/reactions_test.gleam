// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/reactions.{Buffered}
import gleam/bit_array
import gleam/list
import gleam/result
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
  let removal = reaction("1", "2", "a")
  let assert Ok(remaining) = reactions.cancel(held, facts(removal), removal)
  reactions.is_empty(remaining) |> should.be_true
}

pub fn remove_without_match_is_not_cancelled_test() {
  let held = reactions.new() |> hold(reaction("1", "2", "a"))
  let removal = reaction("1", "2", "b")
  reactions.cancel(held, facts(removal), removal)
  |> should.equal(Error(Nil))
}

fn typed(
  message: String,
  user: String,
  answer: String,
  kind: String,
) -> raw_json.RawJson {
  raw_json.wrap(bit_array.from_string(
    "{\"channel_id\":\"10\",\"message_id\":\""
    <> message
    <> "\",\"user_id\":\""
    <> user
    <> "\",\"emoji\":{\"id\":\""
    <> answer
    <> "\",\"name\":\""
    <> answer
    <> "\"},\"reaction_type\":"
    <> kind
    <> "}",
  ))
}

fn untyped(message: String, user: String, answer: String) -> raw_json.RawJson {
  raw_json.wrap(bit_array.from_string(
    "{\"channel_id\":\"10\",\"message_id\":\""
    <> message
    <> "\",\"user_id\":\""
    <> user
    <> "\",\"emoji\":{\"id\":\""
    <> answer
    <> "\",\"name\":\""
    <> answer
    <> "\"}}",
  ))
}

pub fn emoji_remove_does_not_cancel_held_poll_vote_test() {
  let held = reactions.new() |> hold(typed("1", "2", "3", "2"))
  let emoji = typed("1", "2", "3", "0")
  reactions.cancel(held, facts(emoji), emoji) |> should.equal(Error(Nil))
  let bare = untyped("1", "2", "3")
  reactions.cancel(held, facts(bare), bare) |> should.equal(Error(Nil))
}

pub fn poll_vote_remove_cancels_held_poll_vote_test() {
  let held = reactions.new() |> hold(typed("1", "2", "3", "2"))
  let removal = typed("1", "2", "3", "2")
  let assert Ok(remaining) = reactions.cancel(held, facts(removal), removal)
  reactions.is_empty(remaining) |> should.be_true
}

pub fn poll_vote_remove_does_not_cancel_held_emoji_test() {
  let held = reactions.new() |> hold(untyped("1", "2", "3"))
  let removal = typed("1", "2", "3", "2")
  reactions.cancel(held, facts(removal), removal) |> should.equal(Error(Nil))
}

pub fn null_reaction_type_matches_absent_test() {
  let held = reactions.new() |> hold(untyped("1", "2", "3"))
  let removal = typed("1", "2", "3", "null")
  let assert Ok(remaining) = reactions.cancel(held, facts(removal), removal)
  reactions.is_empty(remaining) |> should.be_true
}

pub fn add_many_keeps_reaction_type_test() {
  let #(_, frames) =
    reactions.new()
    |> hold(typed("1", "2", "3", "2"))
    |> hold(typed("1", "4", "5", "2"))
    |> hold(untyped("1", "6", "7"))
    |> hold(typed("1", "8", "9", "null"))
    |> reactions.flush
  let assert [#("MESSAGE_REACTION_ADD_MANY", payload)] = frames
  let assert Ok(entries) =
    raw_json.field(payload, "reactions") |> result.try(raw_json.items)
  entries
  |> list.map(fn(entry) {
    raw_json.field(entry, "reaction_type")
    |> result.map(raw_json.to_bits)
  })
  |> should.equal([Ok(<<"2">>), Ok(<<"2">>), Error(Nil), Error(Nil)])
}
