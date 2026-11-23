// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/relations.{DmChannel}
import gleam/bit_array
import gleam/dict
import gleam/set
import gleeunit/should

fn raw(text: String) -> raw_json.RawJson {
  raw_json.wrap(bit_array.from_string(text))
}

fn apply(
  relations: relations.Relations,
  event: String,
  payload: raw_json.RawJson,
  me: Int,
) -> relations.Change {
  relations.apply(relations, event, dispatch_facts.scan(event, payload), me)
}

fn empty() -> relations.Relations {
  relations.new(relationships: dict.new(), channels: dict.new())
}

pub fn relationship_add_releases_friend_test() {
  let change =
    apply(empty(), "RELATIONSHIP_ADD", raw("{\"id\":\"5\",\"type\":1}"), 1)
  change.released |> should.equal([5])
  relations.friends(change.relations) |> should.equal([5])
}

pub fn blocked_relationship_is_not_a_friend_test() {
  let change =
    apply(empty(), "RELATIONSHIP_ADD", raw("{\"id\":\"5\",\"type\":2}"), 1)
  change.released |> should.equal([])
  relations.friends(change.relations) |> should.equal([])
}

pub fn group_dm_create_tracks_recipients_test() {
  let change =
    apply(
      empty(),
      "CHANNEL_CREATE",
      raw("{\"id\":\"9\",\"type\":3,\"recipient_ids\":[\"1\",\"7\"]}"),
      1,
    )
  change.released |> should.equal([7])
  relations.presence_targets(change.relations, 1, set.new())
  |> should.equal(set.from_list([7]))
}

pub fn direct_dm_partner_needs_shared_guild_test() {
  let rel =
    relations.new(
      relationships: dict.new(),
      channels: dict.from_list([
        #(9, DmChannel(kind: 1, recipients: set.from_list([1, 4]))),
      ]),
    )
  relations.presence_targets(rel, 1, set.new()) |> should.equal(set.new())
  relations.presence_targets(rel, 1, set.from_list([4]))
  |> should.equal(set.from_list([4]))
  relations.direct_partners(rel, 1) |> should.equal([4])
}

pub fn recipient_add_updates_group_dm_test() {
  let start =
    relations.new(
      relationships: dict.new(),
      channels: dict.from_list([
        #(9, DmChannel(kind: 3, recipients: set.from_list([1]))),
      ]),
    )
  let change =
    apply(
      start,
      "CHANNEL_RECIPIENT_ADD",
      raw("{\"channel_id\":\"9\",\"user\":{\"id\":\"8\"}}"),
      1,
    )
  change.released |> should.equal([8])
  relations.allows_presence(change.relations, 8, 1, set.new()) |> should.be_true
}
