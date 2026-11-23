// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/push/settings.{type Settings}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}

const settings_table = "push_user_guild_settings"

const blocked_table = "push_blocked_ids"

const max_table_entries = 500_000

const evict_batch = 4096

fn reservation_ttl_ms() -> Int {
  durations.push().settings_cache_reservation_ttl_ms
}

fn blocked_ids_ttl_seconds() -> Int {
  durations.push().settings_cache_blocked_ids_ttl_seconds
}

fn failure_entry_ms() -> Int {
  durations.push().settings_cache_failure_entry_ms
}

fn settings_ttl_ms() -> Int {
  durations.push().settings_cache_settings_ttl_ms
}

pub type Lookup {
  Cached(Settings)
  Uncached
  FailedEntry
}

pub opaque type Fill {
  Fill(token: Int)
}

type SettingsEntry {
  Known(settings: Settings, expires_at_ms: Int)
  Pending(token: Int, reserved_at_ms: Int)
  Failed(until_ms: Int)
}

type SettingsRow {
  SettingsRow(key: #(Int, Int), entry: SettingsEntry)
}

type BlockedEntry {
  BlockedFetched(ids: List(Int), expires_at_s: Int)
  BlockedPending(token: Int, reserved_at_ms: Int)
}

type BlockedRow {
  BlockedRow(user: Int, entry: BlockedEntry)
}

type TableOption {
  NamedTable
  Public
  Set
  Keypos(Int)
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

type UniqueOption {
  Positive
}

type TimeUnit {
  Millisecond
  Second
}

type InfoItem {
  Size
}

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(name: String) -> Atom

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: key) -> List(row)

@external(erlang, "ets", "insert_new")
fn ets_insert_new(table: Atom, row: row) -> Bool

@external(erlang, "ets", "delete")
fn ets_delete(table: Atom, key: key) -> Bool

@external(erlang, "ets", "select_replace")
fn ets_select_replace(
  table: Atom,
  spec: List(#(head, List(guard), List(body))),
) -> Int

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Atom,
  spec: List(#(head, List(guard), List(Bool))),
) -> Int

@external(erlang, "ets", "first")
fn ets_first(table: Atom) -> Dynamic

@external(erlang, "ets", "next")
fn ets_next(table: Atom, key: Dynamic) -> Dynamic

@external(erlang, "ets", "safe_fixtable")
fn ets_safe_fixtable(table: Atom, fix: Bool) -> Bool

@external(erlang, "ets", "info")
fn ets_info(table: Atom, item: InfoItem) -> Int

@external(erlang, "ets", "whereis")
fn ets_whereis(table: Atom) -> Dynamic

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

fn settings_name() -> Atom {
  to_atom(settings_table)
}

fn blocked_name() -> Atom {
  to_atom(blocked_table)
}

fn wildcard() -> Atom {
  atom.create("_")
}

fn undefined() -> Dynamic {
  atom.to_dynamic(atom.create("undefined"))
}

pub fn init() -> Nil {
  list.each([settings_name(), blocked_name()], fn(name) {
    case ets_whereis(name) == undefined() {
      True -> {
        let _ =
          ets_new(name, [
            NamedTable,
            Public,
            Set,
            Keypos(2),
            ReadConcurrency(True),
            WriteConcurrency(True),
          ])
        Nil
      }
      False -> Nil
    }
  })
}

fn exists(name: Atom) -> Bool {
  ets_whereis(name) != undefined()
}

pub fn lookup_settings(user: Int, guild: Int) -> Lookup {
  case exists(settings_name()) {
    False -> Uncached
    True ->
      case ets_lookup(settings_name(), #(user, guild)) {
        [SettingsRow(entry: Known(settings, expires_at_ms), ..)] ->
          case monotonic_time(Millisecond) < expires_at_ms {
            True -> Cached(settings)
            False -> Uncached
          }
        [SettingsRow(entry: Failed(until_ms), ..)] ->
          case monotonic_time(Millisecond) < until_ms {
            True -> FailedEntry
            False -> Uncached
          }
        _ -> Uncached
      }
  }
}

pub fn delete_settings(user: Int, guild: Int) -> Nil {
  case exists(settings_name()) {
    False -> Nil
    True -> {
      let _ = ets_delete(settings_name(), #(user, guild))
      Nil
    }
  }
}

pub fn new_fill() -> Fill {
  Fill(unique_integer([Positive]))
}

pub fn reserve_settings(fill: Fill, users: List(Int), guild: Int) -> Nil {
  case exists(settings_name()) {
    False -> Nil
    True -> {
      guard_size(settings_name())
      let now = monotonic_time(Millisecond)
      list.each(users, fn(user) {
        let key = #(user, guild)
        let _ =
          ets_select_delete(settings_name(), [
            #(
              #(
                atom.create("settings_row"),
                key,
                #(atom.create("failed"), atom.create("$1")),
              ),
              [#(atom.create("=<"), atom.create("$1"), now)],
              [True],
            ),
          ])
        let _ =
          ets_select_delete(settings_name(), [
            #(
              #(
                atom.create("settings_row"),
                key,
                #(atom.create("known"), wildcard(), atom.create("$1")),
              ),
              [#(atom.create("=<"), atom.create("$1"), now)],
              [True],
            ),
          ])
        let _ =
          ets_insert_new(
            settings_name(),
            SettingsRow(key, Pending(fill.token, now)),
          )
        Nil
      })
    }
  }
}

pub fn fill_settings(
  fill: Fill,
  user: Int,
  guild: Int,
  settings: Settings,
) -> Nil {
  replace_pending(
    settings_name(),
    fill,
    #(user, guild),
    SettingsRow(
      #(user, guild),
      Known(settings, monotonic_time(Millisecond) + settings_ttl_ms()),
    ),
  )
}

pub fn fail_settings(fill: Fill, users: List(Int), guild: Int) -> Nil {
  let until = monotonic_time(Millisecond) + failure_entry_ms()
  list.each(users, fn(user) {
    replace_pending(
      settings_name(),
      fill,
      #(user, guild),
      SettingsRow(#(user, guild), Failed(until)),
    )
  })
}

pub fn release_settings(fill: Fill, users: List(Int), guild: Int) -> Nil {
  list.each(users, fn(user) {
    delete_pending(
      settings_name(),
      "settings_row",
      "pending",
      #(user, guild),
      fill,
    )
  })
}

fn replace_pending(table: Atom, fill: Fill, key: key, row: row) -> Nil {
  case exists(table) {
    False -> Nil
    True -> {
      let tag = case table == blocked_name() {
        True -> #("blocked_row", "blocked_pending")
        False -> #("settings_row", "pending")
      }
      let _ =
        ets_select_replace(table, [
          #(
            #(
              atom.create(tag.0),
              key,
              #(atom.create(tag.1), fill.token, wildcard()),
            ),
            [],
            [#(atom.create("const"), row)],
          ),
        ])
      Nil
    }
  }
}

fn delete_pending(
  table: Atom,
  row_tag: String,
  pending_tag: String,
  key: key,
  fill: Fill,
) -> Nil {
  case exists(table) {
    False -> Nil
    True -> {
      let _ =
        ets_select_delete(table, [
          #(
            #(
              atom.create(row_tag),
              key,
              #(atom.create(pending_tag), fill.token, wildcard()),
            ),
            [],
            [True],
          ),
        ])
      Nil
    }
  }
}

pub fn blocked_ids(user: Int) -> Option(List(Int)) {
  case exists(blocked_name()) {
    False -> None
    True ->
      case ets_lookup(blocked_name(), user) {
        [BlockedRow(entry: BlockedFetched(ids, expires_at_s), ..)] ->
          case system_time(Second) < expires_at_s {
            True -> Some(ids)
            False -> None
          }
        _ -> None
      }
  }
}

pub fn reserve_blocked(fill: Fill, users: List(Int)) -> Nil {
  case exists(blocked_name()) {
    False -> Nil
    True -> {
      guard_size(blocked_name())
      let now_ms = monotonic_time(Millisecond)
      let now_s = system_time(Second)
      list.each(users, fn(user) {
        let _ =
          ets_select_delete(blocked_name(), [
            #(
              #(
                atom.create("blocked_row"),
                user,
                #(atom.create("blocked_fetched"), wildcard(), atom.create("$1")),
              ),
              [#(atom.create("=<"), atom.create("$1"), now_s)],
              [True],
            ),
          ])
        let _ =
          ets_insert_new(
            blocked_name(),
            BlockedRow(user, BlockedPending(fill.token, now_ms)),
          )
        Nil
      })
    }
  }
}

pub fn fill_blocked(fill: Fill, user: Int, ids: List(Int)) -> Nil {
  let expires = system_time(Second) + blocked_ids_ttl_seconds()
  replace_pending(
    blocked_name(),
    fill,
    user,
    BlockedRow(user, BlockedFetched(ids, expires)),
  )
}

pub fn release_blocked(fill: Fill, users: List(Int)) -> Nil {
  list.each(users, fn(user) {
    delete_pending(blocked_name(), "blocked_row", "blocked_pending", user, fill)
  })
}

pub fn delete_blocked(user: Int) -> Nil {
  case exists(blocked_name()) {
    False -> Nil
    True -> {
      let _ = ets_delete(blocked_name(), user)
      Nil
    }
  }
}

fn table_size(table: Atom) -> Int {
  case exists(table) {
    True -> ets_info(table, Size)
    False -> 0
  }
}

pub fn evict() -> Nil {
  let now_s = system_time(Second)
  let cutoff = monotonic_time(Millisecond) - reservation_ttl_ms()
  let now_ms = monotonic_time(Millisecond)
  case exists(blocked_name()) {
    True -> {
      let _ =
        ets_select_delete(blocked_name(), [
          #(
            #(
              atom.create("blocked_row"),
              wildcard(),
              #(atom.create("blocked_fetched"), wildcard(), atom.create("$1")),
            ),
            [#(atom.create("=<"), atom.create("$1"), now_s)],
            [True],
          ),
        ])
      let _ =
        ets_select_delete(blocked_name(), [
          #(
            #(
              atom.create("blocked_row"),
              wildcard(),
              #(atom.create("blocked_pending"), wildcard(), atom.create("$1")),
            ),
            [#(atom.create("<"), atom.create("$1"), cutoff)],
            [True],
          ),
        ])
      Nil
    }
    False -> Nil
  }
  case exists(settings_name()) {
    True -> {
      let _ =
        ets_select_delete(settings_name(), [
          #(
            #(
              atom.create("settings_row"),
              wildcard(),
              #(atom.create("pending"), wildcard(), atom.create("$1")),
            ),
            [#(atom.create("<"), atom.create("$1"), cutoff)],
            [True],
          ),
        ])
      let _ =
        ets_select_delete(settings_name(), [
          #(
            #(
              atom.create("settings_row"),
              wildcard(),
              #(atom.create("failed"), atom.create("$1")),
            ),
            [#(atom.create("=<"), atom.create("$1"), now_ms)],
            [True],
          ),
        ])
      let _ =
        ets_select_delete(settings_name(), [
          #(
            #(
              atom.create("settings_row"),
              wildcard(),
              #(atom.create("known"), wildcard(), atom.create("$1")),
            ),
            [#(atom.create("=<"), atom.create("$1"), now_ms)],
            [True],
          ),
        ])
      Nil
    }
    False -> Nil
  }
  evict_table(settings_name(), max_table_entries)
  evict_table(blocked_name(), max_table_entries)
}

fn guard_size(table: Atom) -> Nil {
  case table_size(table) >= max_table_entries {
    True -> evict_table(table, max_table_entries - evict_batch)
    False -> Nil
  }
}

fn evict_table(table: Atom, max_entries: Int) -> Nil {
  let size = table_size(table)
  case size > max_entries {
    True -> {
      let _ = ets_safe_fixtable(table, True)
      evict_keys(table, ets_first(table), size - max_entries)
      let _ = ets_safe_fixtable(table, False)
      Nil
    }
    False -> Nil
  }
}

fn evict_keys(table: Atom, key: Dynamic, remaining: Int) -> Nil {
  case remaining <= 0 || key == atom.to_dynamic(atom.create("$end_of_table")) {
    True -> Nil
    False -> {
      let next = ets_next(table, key)
      let _ = ets_delete(table, key)
      evict_keys(table, next, remaining - 1)
    }
  }
}
