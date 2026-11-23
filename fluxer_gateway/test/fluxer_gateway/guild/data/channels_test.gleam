// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/id
import fluxer_gateway/guild/data
import fluxer_gateway/guild/data/channels
import fluxer_gateway/guild/data/decode as data_decode
import gleam/dict
import gleam/list
import gleam/option.{Some}

fn loaded() -> data.Loaded {
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"7\",\"features\":[]}")),
      #(
        "roles",
        whole(
          "[{\"id\":\"100\",\"name\":\"@everyone\",\"permissions\":\"1024\",\"position\":0}]",
        ),
      ),
      #(
        "channels",
        whole(
          "[{\"id\":\"3\",\"type\":2,\"position\":0,\"name\":\"voice\"},"
          <> "{\"id\":\"2\",\"type\":0,\"position\":1,\"name\":\"general\",\"parent_id\":\"1\"},"
          <> "{\"id\":\"1\",\"type\":4,\"position\":0,\"name\":\"cat\"},"
          <> "{\"id\":\"4\",\"type\":0,\"position\":0,\"name\":\"hidden\","
          <> "\"permission_overwrites\":[{\"id\":\"100\",\"type\":0,\"allow\":\"0\",\"deny\":\"1024\"}]}]",
        ),
      ),
      #("emojis", whole("[]")),
      #("stickers", whole("[]")),
      #(
        "members",
        whole("[{\"user\":{\"id\":\"7\"},\"roles\":[],\"joined_at\":null}]"),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(found) = data_decode.collections(guild, collected)
  found
}

fn ids(rows: List(channel.Channel)) -> List(Int) {
  list.map(rows, fn(row) { id.channel_id_to_int(channel.id(row)) })
}

pub fn decodes_collections_test() {
  let found = loaded()
  let assert 4 = list.length(found.data.channels)
  let assert 1 = list.length(found.data.roles)
  let assert [_] = found.members
}

pub fn orders_roots_then_categories_test() {
  let found = loaded()
  let assert [4, 3, 1, 2] = ids(channels.sort_for_ordering(found.data.channels))
}

pub fn first_viewable_skips_denied_channel_test() {
  let found = loaded()
  let assert Some(first) = channels.first_everyone_viewable(found.data)
  let assert 3 = id.channel_id_to_int(first)
}

pub fn mentions_skip_categories_without_view_test() {
  let found = loaded()
  let assert Ok(hidden) = id.channel_id(4)
  let assert Ok(general) = id.channel_id(2)
  let assert [mention] =
    channels.mentions(found.data, [hidden, general, general], fn(_) {
      Some("name")
    })
  let assert True = mention.id == general
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
