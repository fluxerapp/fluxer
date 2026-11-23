// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/session/payload
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

const max_unconnected = 10

const max_member_list_channels = 50

const thread_keys = ["threads", "thread_member_lists"]

const stored_keys = [
  "active", "sync", "typing", "members", "thread_member_lists", "threads",
  "focused_channel_id",
]

pub type GuildSub {
  GuildSub(fields: Dict(String, RawJson), member_lists: Dict(String, RawJson))
}

pub type Incoming {
  Incoming(
    fields: Dict(String, RawJson),
    member_lists: Option(Dict(String, #(RawJson, Bool))),
    active: Option(Bool),
  )
}

pub opaque type Subscriptions {
  Subscriptions(
    guilds: Dict(Int, GuildSub),
    active: Set(Int),
    member_list_guild: Option(Int),
  )
}

pub fn new(active: Set(Int), guilds: Dict(Int, GuildSub)) -> Subscriptions {
  let from_subs =
    guilds
    |> dict.to_list
    |> list.filter_map(fn(entry) {
      let #(guild, sub) = entry
      case dict.get(sub.fields, "active") {
        Ok(raw) ->
          case raw_json.to_bits(raw) {
            <<"true">> -> Ok(guild)
            _ -> Error(Nil)
          }
        Error(Nil) -> Error(Nil)
      }
    })
    |> set.from_list
  let member_list_guild = case
    dict.filter(guilds, fn(_, sub) { !dict.is_empty(sub.member_lists) })
    |> dict.keys
  {
    [guild] -> Some(guild)
    _ -> None
  }
  Subscriptions(
    guilds:,
    active: set.union(active, from_subs),
    member_list_guild:,
  )
}

pub fn active(subscriptions: Subscriptions) -> Set(Int) {
  subscriptions.active
}

pub fn is_active(subscriptions: Subscriptions, guild: Int) -> Bool {
  set.contains(subscriptions.active, guild)
}

pub fn guilds(subscriptions: Subscriptions) -> Dict(Int, GuildSub) {
  subscriptions.guilds
}

pub fn member_list_guild(subscriptions: Subscriptions) -> Option(Int) {
  subscriptions.member_list_guild
}

pub fn remove(subscriptions: Subscriptions, guild: Int) -> Subscriptions {
  Subscriptions(
    guilds: dict.delete(subscriptions.guilds, guild),
    active: set.delete(subscriptions.active, guild),
    member_list_guild: case subscriptions.member_list_guild {
      Some(current) if current == guild -> None
      other -> other
    },
  )
}

pub type Stored {
  Stored(subscriptions: Subscriptions, previous_member_list_guild: Option(Int))
}

pub fn store(
  subscriptions: Subscriptions,
  incoming: List(#(Int, Incoming)),
  connected: fn(Int) -> Bool,
) -> Stored {
  let #(connected_subs, unconnected) =
    list.partition(incoming, fn(entry) { connected(entry.0) })
  let #(accepted, subscriptions) =
    accept_unconnected(subscriptions, connected_subs, unconnected, connected)
  let subscriptions =
    list.fold(accepted, subscriptions, fn(acc, entry) {
      let #(guild, sub) = entry
      let existing =
        dict.get(acc.guilds, guild)
        |> option.from_result
        |> option.unwrap(GuildSub(dict.new(), dict.new()))
      let active = case sub.active {
        Some(True) -> set.insert(acc.active, guild)
        Some(False) -> set.delete(acc.active, guild)
        None -> acc.active
      }
      Subscriptions(
        ..acc,
        guilds: dict.insert(acc.guilds, guild, merge(existing, sub)),
        active:,
      )
    })
  case list.find(accepted, fn(entry) { has_ranges(entry.1) }) {
    Ok(#(guild, _)) -> {
      let previous = case subscriptions.member_list_guild {
        Some(current) if current != guild -> Some(current)
        _ -> None
      }
      let guilds = case previous {
        Some(previous) ->
          case dict.get(subscriptions.guilds, previous) {
            Ok(sub) ->
              dict.insert(
                subscriptions.guilds,
                previous,
                GuildSub(..sub, member_lists: dict.new()),
              )
            Error(Nil) -> subscriptions.guilds
          }
        None -> subscriptions.guilds
      }
      Stored(
        Subscriptions(..subscriptions, guilds:, member_list_guild: Some(guild)),
        previous,
      )
    }
    Error(Nil) -> Stored(subscriptions, None)
  }
}

fn accept_unconnected(
  subscriptions: Subscriptions,
  connected_subs: List(#(Int, Incoming)),
  unconnected: List(#(Int, Incoming)),
  connected: fn(Int) -> Bool,
) -> #(List(#(Int, Incoming)), Subscriptions) {
  case unconnected {
    [] -> #(connected_subs, subscriptions)
    _ -> {
      let incoming_ids = list.map(unconnected, fn(entry) { entry.0 })
      let stored =
        subscriptions.guilds
        |> dict.keys
        |> list.filter(fn(guild) {
          !connected(guild) && !list.contains(incoming_ids, guild)
        })
      case list.length(stored) + list.length(unconnected) <= max_unconnected {
        True -> #(list.append(connected_subs, unconnected), subscriptions)
        False -> #(
          list.append(connected_subs, list.take(unconnected, max_unconnected)),
          list.fold(stored, subscriptions, remove),
        )
      }
    }
  }
}

fn merge(existing: GuildSub, incoming: Incoming) -> GuildSub {
  let fields = dict.merge(existing.fields, incoming.fields)
  let member_lists = case incoming.member_lists {
    None -> existing.member_lists
    Some(channels) -> {
      let fresh = with_ranges(channels)
      let merged =
        existing.member_lists
        |> dict.map_values(fn(_, raw) { #(raw, True) })
        |> dict.merge(channels)
        |> with_ranges
      case dict.size(merged) > max_member_list_channels {
        True -> fresh
        False -> merged
      }
    }
  }
  GuildSub(fields:, member_lists:)
}

fn with_ranges(
  channels: Dict(String, #(RawJson, Bool)),
) -> Dict(String, RawJson) {
  channels
  |> dict.filter(fn(_, entry) { entry.1 })
  |> dict.map_values(fn(_, entry) { entry.0 })
}

fn has_ranges(incoming: Incoming) -> Bool {
  case incoming.member_lists {
    Some(channels) -> list.any(dict.values(channels), fn(entry) { entry.1 })
    None -> False
  }
}

pub fn replay(subscriptions: Subscriptions, guild: Int) -> Option(RawJson) {
  case dict.get(subscriptions.guilds, guild) {
    Error(Nil) -> None
    Ok(sub) -> {
      let fields =
        sub.fields
        |> dict.to_list
        |> list.map(fn(entry) { #(entry.0, raw_json.to_json(entry.1)) })
      let fields = case dict.is_empty(sub.member_lists) {
        True -> fields
        False -> [
          #(
            "member_list_channels",
            json.dict(sub.member_lists, fn(key) { key }, raw_json.to_json),
          ),
          ..fields
        ]
      }
      Some(wrap(guild, fields))
    }
  }
}

pub fn replay_threads(
  subscriptions: Subscriptions,
  guild: Int,
) -> Option(RawJson) {
  use sub <- option.then(
    dict.get(subscriptions.guilds, guild) |> option.from_result,
  )
  case
    sub.fields
    |> dict.take(thread_keys)
    |> dict.to_list
  {
    [] -> None
    fields ->
      Some(wrap(
        guild,
        list.map(fields, fn(entry) { #(entry.0, raw_json.to_json(entry.1)) }),
      ))
  }
}

fn wrap(guild: Int, fields: List(#(String, json.Json))) -> RawJson {
  raw_json.from_json(
    json.object([
      #("subscriptions", json.object([#(id.write(guild), json.object(fields))])),
    ]),
  )
}

pub fn replay_member_lists(
  subscriptions: Subscriptions,
  guild: Int,
) -> Option(RawJson) {
  use sub <- option.then(
    dict.get(subscriptions.guilds, guild) |> option.from_result,
  )
  case dict.is_empty(sub.member_lists) {
    True -> None
    False ->
      Some(
        wrap(guild, [
          #(
            "member_list_channels",
            json.dict(sub.member_lists, fn(key) { key }, raw_json.to_json),
          ),
        ]),
      )
  }
}

pub fn decode_incoming(data: Dynamic) -> List(#(Int, Incoming)) {
  decode.run(data, incoming_decoder()) |> result.unwrap([])
}

fn incoming_decoder() -> Decoder(List(#(Int, Incoming))) {
  use guilds <- decode.field(
    "subscriptions",
    decode.dict(
      decode.string,
      decode.one_of(guild_decoder() |> decode.map(Some), [decode.success(None)]),
    ),
  )
  guilds
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    case id.parse(entry.0), entry.1 {
      Ok(guild), Some(sub) -> Ok(#(guild, sub))
      _, _ -> Error(Nil)
    }
  })
  |> decode.success
}

fn guild_decoder() -> Decoder(Incoming) {
  use all <- decode.then(decode.dict(decode.string, payload.raw()))
  use member_lists <- decode.then(decode.optional_field(
    "member_list_channels",
    None,
    decode.one_of(
      decode.dict(decode.string, ranges_decoder()) |> decode.map(Some),
      [decode.success(None)],
    ),
    decode.success,
  ))
  use active <- decode.then(decode.optional_field(
    "active",
    None,
    decode.one_of(decode.bool |> decode.map(Some), [decode.success(None)]),
    decode.success,
  ))
  decode.success(Incoming(
    fields: dict.filter(all, fn(key, _) { list.contains(stored_keys, key) }),
    member_lists:,
    active:,
  ))
}

fn ranges_decoder() -> Decoder(#(RawJson, Bool)) {
  use raw <- decode.then(payload.raw())
  use non_empty <- decode.then(
    decode.one_of(
      decode.list(decode.dynamic) |> decode.map(fn(items) { items != [] }),
      [decode.success(False)],
    ),
  )
  decode.success(#(raw, non_empty))
}
