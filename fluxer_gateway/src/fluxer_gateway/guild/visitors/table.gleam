// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{type SessionRef}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type Tier {
  Verified
  Unverified
  Unclaimed
}

pub type Visitor {
  Visitor(
    ref: SessionRef,
    pid: Pid,
    monitor: Monitor,
    adult: Bool,
    thread_viewer: Bool,
    operator: Bool,
    tier: Tier,
    ip_key: Option(String),
    since_ms: Int,
    focus: Option(Int),
    view: Set(Int),
  )
}

pub opaque type Table {
  Table(
    visitors: Dict(String, Visitor),
    by_monitor: Dict(Monitor, String),
    by_user: Dict(Int, Int),
    by_ip: Dict(String, Int),
    focus: Dict(Int, Set(String)),
  )
}

pub fn new() -> Table {
  Table(
    visitors: dict.new(),
    by_monitor: dict.new(),
    by_user: dict.new(),
    by_ip: dict.new(),
    focus: dict.new(),
  )
}

pub fn tier_from_wire(text: Option(String)) -> Tier {
  case text {
    Some("verified") -> Verified
    Some("unverified") -> Unverified
    _ -> Unclaimed
  }
}

pub fn ip_key(text: Option(String)) -> Option(String) {
  case text {
    Some("") -> None
    other -> other
  }
}

pub fn count(table: Table) -> Int {
  dict.size(table.visitors)
}

pub fn visitors(table: Table) -> List(Visitor) {
  dict.values(table.visitors)
}

pub fn get(table: Table, key: String) -> Option(Visitor) {
  dict.get(table.visitors, key)
  |> option.from_result
}

pub fn find_by_monitor(table: Table, monitor: Monitor) -> Option(Visitor) {
  dict.get(table.by_monitor, monitor)
  |> result.try(dict.get(table.visitors, _))
  |> option.from_result
}

pub fn user_count(table: Table, user: Int) -> Int {
  dict.get(table.by_user, user)
  |> option.from_result
  |> option.unwrap(0)
}

pub fn ip_count(table: Table, ip: Option(String)) -> Int {
  ip
  |> option.then(fn(key) { dict.get(table.by_ip, key) |> option.from_result })
  |> option.unwrap(0)
}

pub fn put(table: Table, visitor: Visitor) -> Table {
  let key = key(visitor)
  let user = user_of(visitor)
  let #(table, _) = remove(table, key)
  let table = refocus(table, key, None, visitor.focus)
  Table(
    ..table,
    visitors: dict.insert(table.visitors, key, visitor),
    by_monitor: dict.insert(table.by_monitor, visitor.monitor, key),
    by_user: increment(table.by_user, user),
    by_ip: case visitor.ip_key {
      Some(ip) -> increment(table.by_ip, ip)
      None -> table.by_ip
    },
  )
}

pub fn remove(table: Table, key: String) -> #(Table, Option(Visitor)) {
  case dict.get(table.visitors, key) {
    Error(Nil) -> #(table, None)
    Ok(visitor) -> {
      let table = refocus(table, key, visitor.focus, None)
      let visitors = dict.delete(table.visitors, key)
      #(
        Table(
          ..table,
          visitors:,
          by_monitor: dict.delete(table.by_monitor, visitor.monitor),
          by_user: decrement(table.by_user, user_of(visitor)),
          by_ip: case visitor.ip_key {
            Some(ip) -> decrement(table.by_ip, ip)
            None -> table.by_ip
          },
        ),
        Some(visitor),
      )
    }
  }
}

pub fn rehome(table: Table, watch: fn(Pid) -> Monitor) -> Table {
  let visitors =
    dict.map_values(table.visitors, fn(_, visitor) {
      Visitor(..visitor, monitor: watch(visitor.pid))
    })
  Table(
    ..table,
    visitors:,
    by_monitor: dict.fold(visitors, dict.new(), fn(acc, key, visitor) {
      dict.insert(acc, visitor.monitor, key)
    }),
  )
}

pub fn replace(table: Table, visitor: Visitor) -> Table {
  let key = key(visitor)
  case dict.get(table.visitors, key) {
    Error(Nil) -> table
    Ok(old) -> {
      let table = refocus(table, key, old.focus, visitor.focus)
      Table(
        ..table,
        visitors: dict.insert(table.visitors, key, visitor),
        by_monitor: table.by_monitor
          |> dict.delete(old.monitor)
          |> dict.insert(visitor.monitor, key),
      )
    }
  }
}

pub fn focused(table: Table, channel: Int) -> List(Visitor) {
  case dict.get(table.focus, channel) {
    Error(Nil) -> []
    Ok(keys) ->
      set.to_list(keys) |> list.filter_map(dict.get(table.visitors, _))
  }
}

pub fn oldest_unverified(table: Table) -> Option(Visitor) {
  dict.values(table.visitors)
  |> list.fold(None, fn(best, visitor) {
    case visitor.tier, best {
      Verified, _ -> best
      _, None -> Some(visitor)
      _, Some(current) ->
        case visitor.since_ms < current.since_ms {
          True -> Some(visitor)
          False -> best
        }
    }
  })
}

pub fn key(visitor: Visitor) -> String {
  session.session_id_to_string(visitor.ref.id)
}

pub fn user_of(visitor: Visitor) -> Int {
  id.user_id_to_int(visitor.ref.user)
}

fn refocus(
  table: Table,
  key: String,
  old: Option(Int),
  new: Option(Int),
) -> Table {
  case old == new {
    True -> table
    False -> {
      let focus = case old {
        Some(channel) -> unfocus(table.focus, channel, key)
        None -> table.focus
      }
      let focus = case new {
        Some(channel) ->
          dict.upsert(focus, channel, fn(existing) {
            option.unwrap(existing, set.new()) |> set.insert(key)
          })
        None -> focus
      }
      Table(..table, focus:)
    }
  }
}

fn unfocus(
  focus: Dict(Int, Set(String)),
  channel: Int,
  key: String,
) -> Dict(Int, Set(String)) {
  case dict.get(focus, channel) {
    Error(Nil) -> focus
    Ok(keys) -> {
      let remaining = set.delete(keys, key)
      case set.size(remaining) {
        0 -> dict.delete(focus, channel)
        _ -> dict.insert(focus, channel, remaining)
      }
    }
  }
}

fn increment(counts: Dict(k, Int), key: k) -> Dict(k, Int) {
  dict.upsert(counts, key, fn(existing) { option.unwrap(existing, 0) + 1 })
}

fn decrement(counts: Dict(k, Int), key: k) -> Dict(k, Int) {
  case dict.get(counts, key) {
    Ok(count) if count > 1 -> dict.insert(counts, key, count - 1)
    _ -> dict.delete(counts, key)
  }
}
