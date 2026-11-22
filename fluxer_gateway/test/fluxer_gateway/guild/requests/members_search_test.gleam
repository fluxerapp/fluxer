// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members
import fluxer_gateway/guild/requests/members_search
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}

fn member_json(n: Int, username: json.Json, global_name: json.Json) -> String {
  json.object([
    #(
      "user",
      json.object([
        #("id", json.string(int.to_string(1000 + n))),
        #("username", username),
        #("global_name", global_name),
      ]),
    ),
    #("nick", json.null()),
  ])
  |> json.to_string
}

fn table(rows: List(String)) -> members.MembersTable {
  let found = members.new(None)
  members.put_many(
    found,
    list.map(rows, fn(text) {
      let assert Ok(value) = json.parse(text, decode.dynamic)
      let assert Ok(row) = data_decode.decode_member(value)
      row
    }),
  )
  found
}

pub fn search_matches_lowercased_name_prefixes_test() {
  let found =
    table([
      member_json(1, json.string("Alpha"), json.null()),
      member_json(2, json.string("beta"), json.string("ALbert")),
      member_json(3, json.int(123), json.string("gamma")),
    ])
  assert list.length(members_search.search(found, "al", 25)) == 2
  assert members_search.search(found, "12", 25) == []
  assert list.length(members_search.search(found, "gam", 25)) == 1
  members.drop(found)
}

pub fn search_stops_at_the_limit_in_table_order_test() {
  let rows =
    int.range(from: 40, to: 0, with: [], run: list.prepend)
    |> list.map(fn(n) {
      member_json(n, json.string("same" <> int.to_string(n)), json.null())
    })
  let found = table(rows)
  let all = members_search.search(found, "same", 100)
  assert list.length(all) == 40
  assert members_search.search(found, "same", 7) == list.take(all, 7)
  assert members_search.search(found, "same", 0) == []
  members.drop(found)
}
