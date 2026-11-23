// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const max_range_end = 100_000

const max_range_span = 99

const max_ranges_per_channel = 10

const max_member_ids = 1000

const max_thread_member_lists = 10

pub type GuildSubscription {
  GuildSubscription(
    active: Option(Bool),
    sync: Bool,
    typing: Option(Bool),
    channels: List(#(Int, List(#(Int, Int)))),
    members: Option(List(Int)),
    focus: Option(Option(Int)),
    threads: Option(Bool),
    thread_member_lists: Option(List(Int)),
  )
}

pub fn for_guild(data: JsonTree, guild: Int) -> Option(GuildSubscription) {
  case json_tree.field(data, "subscriptions") |> result.try(json_tree.fields) {
    Ok(entries) ->
      entries
      |> list.find(fn(pair) { id.parse(pair.0) == Ok(guild) })
      |> option.from_result
      |> option.map(fn(pair) { parse(pair.1) })
    Error(Nil) -> None
  }
}

pub fn parse(data: JsonTree) -> GuildSubscription {
  GuildSubscription(
    active: flag(data, "active"),
    sync: json_tree.field(data, "sync") |> result.try(json_tree.bool)
      == Ok(True),
    typing: flag(data, "typing"),
    channels: channels(json_tree.field(data, "member_list_channels")),
    members: capped_ids(json_tree.field(data, "members"), max_member_ids),
    focus: json_tree.field(data, "focused_channel_id")
      |> option.from_result
      |> option.map(fn(found) {
        json_tree.snowflake(found) |> option.from_result
      }),
    threads: flag(data, "threads"),
    thread_member_lists: capped_ids(
      json_tree.field(data, "thread_member_lists"),
      max_thread_member_lists,
    ),
  )
}

fn capped_ids(found: Result(JsonTree, Nil), limit: Int) -> Option(List(Int)) {
  case result.try(found, json_tree.items) {
    Ok(items) ->
      Some(list.take(list.filter_map(items, json_tree.snowflake), limit))
    Error(Nil) -> None
  }
}

fn flag(data: JsonTree, key: String) -> Option(Bool) {
  json_tree.field(data, key)
  |> result.try(json_tree.bool)
  |> option.from_result
}

fn channels(found: Result(JsonTree, Nil)) -> List(#(Int, List(#(Int, Int)))) {
  case result.try(found, json_tree.fields) {
    Ok(entries) ->
      list.filter_map(entries, fn(pair) {
        id.parse(pair.0)
        |> result.map(fn(channel_int) { #(channel_int, ranges(pair.1)) })
      })
    Error(Nil) -> []
  }
}

pub fn ranges(found: JsonTree) -> List(#(Int, Int)) {
  case json_tree.items(found) {
    Ok(items) ->
      items
      |> list.filter_map(range)
      |> list.take(max_ranges_per_channel)
    Error(Nil) -> []
  }
}

fn range(item: JsonTree) -> Result(#(Int, Int), Nil) {
  case json_tree.items(item) {
    Ok([first, second]) -> {
      use start <- result.try(json_tree.int(first))
      use stop <- result.try(json_tree.int(second))
      case
        start >= 0
        && stop >= start
        && stop <= max_range_end
        && stop - start <= max_range_span
      {
        True -> Ok(#(start, stop))
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
