// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/session/links.{NoHint, Slot, Waiting}
import gleam/option.{None, Some}
import gleeunit/should

fn guild(value: Int) -> id.GuildId {
  let assert Ok(guild) = id.guild_id(value)
  guild
}

pub fn new_links_wait_for_every_guild_test() {
  let table = links.new([guild(1), guild(2)])
  links.all_resolved(table) |> should.be_false
  links.get(table, guild(1)) |> should.equal(Some(Waiting(guild(1))))
}

pub fn token_matches_only_current_attempt_test() {
  let table = links.new([guild(1)])
  let #(table, first) = links.begin(table, guild(1), 0)
  let #(table, second) = links.begin(table, guild(1), 1)
  let #(table, stale) = links.settle(table, first)
  stale |> should.equal(None)
  let #(_, current) = links.settle(table, second)
  let assert Some(entry) = current
  entry.attempt |> should.equal(1)
}

pub fn session_budget_is_guilds_plus_three_test() {
  let table = links.new([guild(1)])
  links.tokens_left(table) |> should.equal(4)
  let assert Ok(table) = links.spend(table)
  let assert Ok(table) = links.spend(table)
  let assert Ok(table) = links.spend(table)
  let assert Ok(table) = links.spend(table)
  links.spend(table) |> should.equal(Error(Nil))
  links.tokens_left(links.refund(table)) |> should.equal(1)
}

pub fn slot_hint_sets_the_sleep_test() {
  let #(_, sleep) = links.next_sleep(links.new([guild(1)]), guild(1), Slot(700))
  sleep |> should.equal(700)
}

pub fn decorrelated_sleep_stays_in_bounds_test() {
  let #(_, sleep) = links.next_sleep(links.new([guild(1)]), guild(1), NoHint)
  { sleep >= links.base_sleep_ms() && sleep <= links.cap_sleep_ms() }
  |> should.be_true
}

pub fn backoff_is_capped_test() {
  { links.backoff_ms(40) <= 30_000 } |> should.be_true
  { links.backoff_ms(0) >= 1 } |> should.be_true
}
