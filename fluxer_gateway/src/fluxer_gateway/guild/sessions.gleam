// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/guild/dispatch/broadcast.{type Broadcaster}
import fluxer_gateway/guild/presence/row.{type Row}
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/visitors/table.{type Table}
import fluxer_gateway/perms/audience.{type Group}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub const max_sessions_per_user = 100

pub type HeadKey {
  HeadKey(
    properties: Dict(String, RawJson),
    roles: List(Role),
    emojis: RawJson,
    stickers: RawJson,
    masked: Bool,
  )
}

type CreateHead {
  CreateHead(key: HeadKey, bytes: BitArray)
}

type MaskedChannel {
  MaskedChannel(source: RawJson, masked: RawJson)
}

type ViewerCount {
  ViewerCount(expires_at_ms: Int, count: Int)
}

pub opaque type Sessions {
  Sessions(
    entries: Dict(String, Entry),
    passive: Int,
    by_monitor: Dict(Monitor, String),
    user_counts: Dict(Int, Int),
    subscribers: Dict(Int, Set(String)),
    subscriptions: Dict(String, Set(Int)),
    rows: Dict(Int, Row),
    engaged: Set(Int),
    interest: Set(Int),
    visitors: Table,
    broadcaster: Option(Broadcaster),
    heads: List(CreateHead),
    masked_channels: Dict(Int, MaskedChannel),
    viewer_counts: Dict(Int, ViewerCount),
    guild_window: List(Int),
    bot_full_list: Dict(Int, Int),
    group_revision: Int,
    groups: Dict(Int, Option(Group)),
  )
}

pub fn new() -> Sessions {
  Sessions(
    entries: dict.new(),
    passive: 0,
    by_monitor: dict.new(),
    user_counts: dict.new(),
    subscribers: dict.new(),
    subscriptions: dict.new(),
    rows: dict.new(),
    engaged: set.new(),
    interest: set.new(),
    visitors: table.new(),
    broadcaster: None,
    heads: [],
    masked_channels: dict.new(),
    viewer_counts: dict.new(),
    guild_window: [],
    bot_full_list: dict.new(),
    group_revision: -1,
    groups: dict.new(),
  )
}

pub fn rehome(sessions: Sessions, watch: fn(Pid) -> Monitor) -> Sessions {
  let entries =
    dict.map_values(sessions.entries, fn(_, found) {
      entry.rebind(
        found,
        ref: entry.ref(found),
        pid: entry.pid(found),
        monitor: watch(entry.pid(found)),
      )
    })
  Sessions(
    ..sessions,
    entries:,
    by_monitor: dict.fold(entries, dict.new(), fn(acc, key, found) {
      dict.insert(acc, entry.monitor(found), key)
    }),
    visitors: table.rehome(sessions.visitors, watch),
    broadcaster: None,
    group_revision: -1,
    groups: dict.new(),
  )
}

pub fn count(sessions: Sessions) -> Int {
  dict.size(sessions.entries)
}

pub fn session_refs(sessions: Sessions) -> List(SessionRef) {
  dict.values(sessions.entries)
  |> list.map(entry.ref)
}

pub fn entries(sessions: Sessions) -> List(Entry) {
  dict.values(sessions.entries)
}

pub fn get(sessions: Sessions, id: SessionId) -> Option(Entry) {
  dict.get(sessions.entries, session.session_id_to_string(id))
  |> option.from_result
}

pub fn get_key(sessions: Sessions, key: String) -> Option(Entry) {
  dict.get(sessions.entries, key)
  |> option.from_result
}

pub fn find_by_monitor(sessions: Sessions, monitor: Monitor) -> Option(Entry) {
  dict.get(sessions.by_monitor, monitor)
  |> result.try(dict.get(sessions.entries, _))
  |> option.from_result
}

pub fn put(sessions: Sessions, item: Entry) -> Sessions {
  let key = entry.key(item)
  let #(by_monitor, passive) = case dict.get(sessions.entries, key) {
    Ok(previous) -> #(
      dict.delete(sessions.by_monitor, entry.monitor(previous)),
      sessions.passive - passive_count(previous),
    )
    Error(Nil) -> #(sessions.by_monitor, sessions.passive)
  }
  Sessions(
    ..sessions,
    entries: dict.insert(sessions.entries, key, item),
    passive: passive + passive_count(item),
    by_monitor: dict.insert(by_monitor, entry.monitor(item), key),
  )
}

pub fn update(
  sessions: Sessions,
  id: SessionId,
  with: fn(Entry) -> Entry,
) -> Sessions {
  case get(sessions, id) {
    Some(found) -> put(sessions, with(found))
    None -> sessions
  }
}

pub fn delete(sessions: Sessions, id: SessionId) -> Sessions {
  let key = session.session_id_to_string(id)
  case dict.get(sessions.entries, key) {
    Error(Nil) -> sessions
    Ok(previous) ->
      Sessions(
        ..sessions,
        entries: dict.delete(sessions.entries, key),
        passive: sessions.passive - passive_count(previous),
        by_monitor: dict.delete(sessions.by_monitor, entry.monitor(previous)),
      )
  }
}

pub fn has_passive(sessions: Sessions) -> Bool {
  sessions.passive > 0
}

fn passive_count(item: Entry) -> Int {
  case entry.is_passive(item) {
    True -> 1
    False -> 0
  }
}

pub fn user_session_count(sessions: Sessions, user: Int) -> Int {
  dict.get(sessions.user_counts, user)
  |> option.from_result
  |> option.unwrap(0)
}

pub fn is_connected(sessions: Sessions, user: Int) -> Bool {
  dict.has_key(sessions.user_counts, user)
}

pub fn connected_list(sessions: Sessions) -> List(Int) {
  dict.keys(sessions.user_counts)
}

pub fn track_connected(sessions: Sessions, user: Int, delta: Int) -> Sessions {
  let current = user_session_count(sessions, user)
  let next = int.max(0, current + delta)
  let counts = case next {
    0 -> dict.delete(sessions.user_counts, user)
    _ -> dict.insert(sessions.user_counts, user, next)
  }
  Sessions(..sessions, user_counts: counts)
}

pub fn subscribers(sessions: Sessions, user: Int) -> List(String) {
  dict.get(sessions.subscribers, user)
  |> result.map(set.to_list)
  |> result.unwrap([])
}

pub fn has_subscribers(sessions: Sessions, user: Int) -> Bool {
  dict.has_key(sessions.subscribers, user)
}

fn subscriptions_of(sessions: Sessions, key: String) -> Set(Int) {
  dict.get(sessions.subscriptions, key)
  |> option.from_result
  |> option.unwrap(set.new())
}

pub fn replace_subscriptions(
  sessions: Sessions,
  key: String,
  users: List(Int),
) -> #(Sessions, List(Int), List(Int)) {
  let current = subscriptions_of(sessions, key)
  let wanted = set.from_list(users)
  let removed = set.difference(current, wanted)
  let added = set.difference(wanted, current)
  let subscribers =
    set.fold(removed, sessions.subscribers, fn(acc, user) {
      drop_subscriber(acc, user, key)
    })
  let subscribers =
    set.fold(added, subscribers, fn(acc, user) {
      dict.upsert(acc, user, fn(existing) {
        option.unwrap(existing, set.new()) |> set.insert(key)
      })
    })
  let subscriptions = case set.size(wanted) {
    0 -> dict.delete(sessions.subscriptions, key)
    _ -> dict.insert(sessions.subscriptions, key, wanted)
  }
  #(
    Sessions(..sessions, subscribers:, subscriptions:),
    set.to_list(added),
    set.to_list(removed),
  )
}

pub fn subscription_lists(sessions: Sessions) -> List(#(String, List(Int))) {
  dict.to_list(sessions.subscriptions)
  |> list.map(fn(pair) { #(pair.0, set.to_list(pair.1)) })
}

pub fn drop_subscriptions(sessions: Sessions, key: String) -> Sessions {
  let #(sessions, _, _) = replace_subscriptions(sessions, key, [])
  sessions
}

fn drop_subscriber(
  subscribers: Dict(Int, Set(String)),
  user: Int,
  key: String,
) -> Dict(Int, Set(String)) {
  case dict.get(subscribers, user) {
    Error(Nil) -> subscribers
    Ok(keys) -> {
      let remaining = set.delete(keys, key)
      case set.size(remaining) {
        0 -> dict.delete(subscribers, user)
        _ -> dict.insert(subscribers, user, remaining)
      }
    }
  }
}

pub fn presence_row(sessions: Sessions, user: Int) -> Option(Row) {
  dict.get(sessions.rows, user)
  |> option.from_result
}

pub fn presence_rows(sessions: Sessions) -> List(#(Int, Row)) {
  dict.to_list(sessions.rows)
}

pub fn presence_count(sessions: Sessions) -> Int {
  dict.size(sessions.rows)
}

pub fn put_row(sessions: Sessions, user: Int, stored: Row) -> Sessions {
  Sessions(..sessions, rows: dict.insert(sessions.rows, user, stored))
}

pub fn delete_row(sessions: Sessions, user: Int) -> Sessions {
  Sessions(..sessions, rows: dict.delete(sessions.rows, user))
}

pub fn is_engaged(sessions: Sessions, user: Int) -> Bool {
  set.contains(sessions.engaged, user)
}

pub fn engaged_users(sessions: Sessions) -> List(Int) {
  set.to_list(sessions.engaged)
}

pub fn set_engaged(sessions: Sessions, user: Int, engaged: Bool) -> Sessions {
  let users = case engaged {
    True -> set.insert(sessions.engaged, user)
    False -> set.delete(sessions.engaged, user)
  }
  Sessions(..sessions, engaged: users)
}

pub fn is_interested(sessions: Sessions, user: Int) -> Bool {
  set.contains(sessions.interest, user)
}

pub fn interested(sessions: Sessions) -> List(Int) {
  set.to_list(sessions.interest)
}

pub fn add_interest(sessions: Sessions, users: List(Int)) -> Sessions {
  Sessions(
    ..sessions,
    interest: list.fold(users, sessions.interest, set.insert),
  )
}

pub fn remove_interest(sessions: Sessions, users: List(Int)) -> Sessions {
  Sessions(
    ..sessions,
    interest: list.fold(users, sessions.interest, set.delete),
  )
}

pub fn visitors(sessions: Sessions) -> Table {
  sessions.visitors
}

pub fn set_visitors(sessions: Sessions, visitors: Table) -> Sessions {
  Sessions(..sessions, visitors:)
}

pub fn broadcaster(sessions: Sessions) -> Option(Broadcaster) {
  sessions.broadcaster
}

pub fn set_broadcaster(
  sessions: Sessions,
  broadcaster: Option(Broadcaster),
) -> Sessions {
  Sessions(..sessions, broadcaster:)
}

pub fn head(sessions: Sessions, key: HeadKey) -> Option(BitArray) {
  list.find(sessions.heads, fn(cached) { cached.key == key })
  |> option.from_result
  |> option.map(fn(cached) { cached.bytes })
}

pub fn set_head(sessions: Sessions, key: HeadKey, bytes: BitArray) -> Sessions {
  let kept =
    list.filter(sessions.heads, fn(cached) { cached.key.masked != key.masked })
  Sessions(..sessions, heads: [CreateHead(key, bytes), ..kept])
}

pub fn masked_channel(
  sessions: Sessions,
  channel: Int,
  source: RawJson,
) -> Option(RawJson) {
  case dict.get(sessions.masked_channels, channel) {
    Ok(cached) if cached.source == source -> Some(cached.masked)
    _ -> None
  }
}

pub fn set_masked_channels(
  sessions: Sessions,
  masked: List(#(Int, RawJson, RawJson)),
  live live: Int,
) -> Sessions {
  let base = case dict.size(sessions.masked_channels) > 2 * live {
    True -> dict.new()
    False -> sessions.masked_channels
  }
  Sessions(
    ..sessions,
    masked_channels: list.fold(masked, base, fn(cache, entry) {
      dict.insert(cache, entry.0, MaskedChannel(entry.1, entry.2))
    }),
  )
}

pub fn viewer_count(sessions: Sessions, user: Int, now_ms: Int) -> Option(Int) {
  case dict.get(sessions.viewer_counts, user) {
    Ok(ViewerCount(expires_at_ms:, count:)) if expires_at_ms > now_ms ->
      Some(count)
    _ -> None
  }
}

pub fn put_viewer_count(
  sessions: Sessions,
  user: Int,
  count: Int,
  now_ms: Int,
  ttl_ms: Int,
) -> Sessions {
  let live =
    dict.filter(sessions.viewer_counts, fn(_user, cached) {
      cached.expires_at_ms > now_ms
    })
  Sessions(
    ..sessions,
    viewer_counts: dict.insert(
      live,
      user,
      ViewerCount(expires_at_ms: now_ms + ttl_ms, count:),
    ),
  )
}

pub fn guild_window(sessions: Sessions) -> List(Int) {
  sessions.guild_window
}

pub fn set_guild_window(sessions: Sessions, stamps: List(Int)) -> Sessions {
  Sessions(..sessions, guild_window: stamps)
}

pub fn bot_full_list(sessions: Sessions, user: Int) -> Option(Int) {
  dict.get(sessions.bot_full_list, user)
  |> option.from_result
}

pub fn set_bot_full_list(
  sessions: Sessions,
  user: Int,
  at_ms: Int,
) -> Sessions {
  Sessions(
    ..sessions,
    bot_full_list: dict.insert(sessions.bot_full_list, user, at_ms),
  )
}

pub fn online_users(sessions: Sessions) -> List(Int) {
  dict.keys(sessions.user_counts)
  |> list.filter(fn(user) {
    dict.get(sessions.rows, user)
    |> result.map(row.is_online)
    |> result.unwrap(False)
  })
}

pub fn groups(sessions: Sessions, revision: Int) -> Dict(Int, Option(Group)) {
  case sessions.group_revision == revision {
    True -> sessions.groups
    False -> dict.new()
  }
}

pub fn set_groups(
  sessions: Sessions,
  revision: Int,
  groups: Dict(Int, Option(Group)),
) -> Sessions {
  Sessions(..sessions, group_revision: revision, groups:)
}

pub fn clear_groups(sessions: Sessions) -> Sessions {
  Sessions(..sessions, group_revision: -1, groups: dict.new())
}
