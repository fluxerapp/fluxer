// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Series {
  Series(name: String, labels: List(#(String, String)))
}

type Counters

type CountersOption {
  WriteConcurrency
}

type Key {
  FluxerGatewayFixedCounters(group: String)
}

type GroupsKey {
  FluxerGatewayFixedCounterGroups
}

type Group {
  Group(counters: Counters, series: List(Series), size: Int)
}

type Stored

@external(erlang, "counters", "new")
fn counters_new(size: Int, options: List(CountersOption)) -> Counters

@external(erlang, "counters", "add")
fn counters_add(counters: Counters, index: Int, by: Int) -> Stored

@external(erlang, "counters", "get")
fn counters_get(counters: Counters, index: Int) -> Int

@external(erlang, "persistent_term", "get")
fn get_group(key: Key, default: Option(Group)) -> Option(Group)

@external(erlang, "persistent_term", "put")
fn put_group(key: Key, value: Option(Group)) -> Stored

@external(erlang, "persistent_term", "get")
fn get_groups(key: GroupsKey, default: List(String)) -> List(String)

@external(erlang, "persistent_term", "put")
fn put_groups(key: GroupsKey, value: List(String)) -> Stored

pub fn add(
  group: String,
  series: fn() -> List(Series),
  index: Int,
  by: Int,
) -> Nil {
  case by > 0 {
    False -> Nil
    True -> {
      let found = group_of(group, series)
      case index >= 1 && index <= found.size {
        True -> {
          let _ = counters_add(found.counters, index, by)
          Nil
        }
        False -> Nil
      }
    }
  }
}

pub fn incr(group: String, series: fn() -> List(Series), index: Int) -> Nil {
  add(group, series, index, 1)
}

pub fn rows() -> List(#(String, List(#(String, String)), Int)) {
  get_groups(FluxerGatewayFixedCounterGroups, [])
  |> list.sort(string.compare)
  |> list.flat_map(fn(group) {
    case get_group(FluxerGatewayFixedCounters(group), None) {
      None -> []
      Some(found) ->
        list.index_map(found.series, fn(entry, index) {
          #(entry.name, entry.labels, counters_get(found.counters, index + 1))
        })
    }
  })
}

fn group_of(group: String, series: fn() -> List(Series)) -> Group {
  case get_group(FluxerGatewayFixedCounters(group), None) {
    Some(found) -> found
    None -> {
      let series = series()
      let size = list.length(series)
      let created =
        Group(counters: counters_new(size, [WriteConcurrency]), series:, size:)
      let _ = put_group(FluxerGatewayFixedCounters(group), Some(created))
      let known = get_groups(FluxerGatewayFixedCounterGroups, [])
      let _ =
        put_groups(
          FluxerGatewayFixedCounterGroups,
          list.unique([group, ..known]),
        )
      created
    }
  }
}
