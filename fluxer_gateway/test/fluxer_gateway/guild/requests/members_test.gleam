// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree as value
import fluxer_gateway/guild/requests/members
import fluxer_gateway/guild/requests/members_filter
import fluxer_gateway/guild/requests/members_search
import gleam/option.{None, Some}

pub fn parse_single_guild_keeps_nonce_test() {
  let data =
    value.Object([
      #("guild_id", value.String("10")),
      #("query", value.String("ab")),
      #("limit", value.Int(500)),
      #("nonce", value.String("n1")),
    ])
  let assert Ok(request) = members.parse(data)
  assert request.guild_ids == [10]
  assert request.limit == 100
  assert request.nonce == Some("n1")
  assert request.presences == False
}

pub fn parse_many_guilds_drops_nonce_and_dedupes_test() {
  let data =
    value.Object([
      #(
        "guild_ids",
        value.Array([value.String("1"), value.String("2"), value.String("1")]),
      ),
      #("nonce", value.String("n1")),
    ])
  let assert Ok(request) = members.parse(data)
  assert request.guild_ids == [1, 2]
  assert request.nonce == None
}

pub fn parse_rejects_bad_guild_ids_test() {
  let data = value.Object([#("guild_ids", value.Array([value.String("x")]))])
  assert members.parse(data) == Error(Nil)
  assert members.parse(value.Object([])) == Error(Nil)
}

pub fn sliding_window_test() {
  assert members_filter.admit([], 1000, 10_000, 2) == Ok([1000])
  assert members_filter.admit([900, 800], 1000, 10_000, 2) == Error(Nil)
  assert members_filter.admit([900, -20_000], 1000, 10_000, 2)
    == Ok([1000, 900])
}

pub fn full_list_and_limits_test() {
  assert members_filter.is_full_list("", 0, [])
  assert !members_filter.is_full_list("a", 0, [])
  assert members_search.resolve_limit("", 0) == 100_000
  assert members_search.resolve_limit("a", 0) == 25
  assert members_search.resolve_limit("a", 7) == 7
}
