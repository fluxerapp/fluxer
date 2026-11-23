// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/push/permit
import fluxer_gateway/push/settings.{type Settings}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}

pub opaque type Profiles {
  Profiles(guild: Int, table: Table, synced: fn(Int) -> Nil)
}

type Table

fn profile_ttl_ms() -> Int {
  durations.guild().profiles_profile_ttl_ms
}

fn invalidation_ttl_ms() -> Int {
  durations.guild().profiles_invalidation_ttl_ms
}

type ProfileRow {
  ProfileRow(
    user: Int,
    settings: Settings,
    fetched_at_ms: Int,
    expires_at_ms: Int,
  )
  Invalidated(user: Int, at_ms: Int, expires_at_ms: Int)
}

type RegistryRow {
  RegistryRow(guild: Int, table: Table, synced: fn(Int) -> Nil)
}

type TableOption {
  Public
  Set
  Keypos(Int)
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

type InfoItem {
  Owner
}

type Head

type Guard =
  #(Atom, Atom, Int)

@external(erlang, "gleam_stdlib", "identity")
fn head(pattern: a) -> Head

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Table

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Table, key: Int) -> List(ProfileRow)

@external(erlang, "ets", "insert")
fn ets_insert(table: Table, row: ProfileRow) -> Bool

@external(erlang, "ets", "insert_new")
fn ets_insert_new(table: Table, row: ProfileRow) -> Bool

@external(erlang, "ets", "delete")
fn ets_drop(table: Table) -> Bool

@external(erlang, "ets", "tab2list")
fn ets_rows(table: Table) -> List(ProfileRow)

@external(erlang, "ets", "select")
fn ets_select(
  table: Table,
  spec: List(#(Head, List(Guard), List(Atom))),
) -> List(Int)

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Table,
  spec: List(#(Head, List(Guard), List(Bool))),
) -> Int

@external(erlang, "ets", "select_replace")
fn ets_select_replace(
  table: Table,
  spec: List(#(Head, List(Guard), List(#(Atom, ProfileRow)))),
) -> Int

@external(erlang, "ets", "info")
fn ets_info(table: Table, item: InfoItem) -> Atom

@external(erlang, "ets", "lookup")
fn registry_lookup(table: Atom, key: Int) -> List(RegistryRow)

@external(erlang, "ets", "insert")
fn registry_insert(table: Atom, row: RegistryRow) -> Bool

@external(erlang, "ets", "delete_object")
fn registry_delete_object(table: Atom, row: RegistryRow) -> Bool

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> Atom

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(name: String) -> Atom

fn registry() -> Atom {
  to_atom(permit.profiles_registry)
}

fn registry_exists() -> Bool {
  ets_whereis(registry()) != atom.create("undefined")
}

fn alive(table: Table) -> Bool {
  ets_info(table, Owner) != atom.create("undefined")
}

pub fn new(guild: GuildId, synced: fn(Int) -> Nil) -> Profiles {
  let raw = id.guild_id_to_int(guild)
  let table =
    ets_new(atom.create("fluxer_gateway_push_profiles"), [
      Public,
      Set,
      Keypos(2),
      ReadConcurrency(True),
      WriteConcurrency(True),
    ])
  let profiles = Profiles(guild: raw, table:, synced:)
  case registry_exists() {
    True -> {
      let _ = registry_insert(registry(), registry_row(profiles))
      Nil
    }
    False -> Nil
  }
  profiles
}

fn registry_row(profiles: Profiles) -> RegistryRow {
  RegistryRow(profiles.guild, profiles.table, profiles.synced)
}

pub fn ensure_registered(profiles: Profiles) -> List(Int) {
  case registry_exists() && alive(profiles.table) {
    False -> []
    True ->
      case registry_lookup(registry(), profiles.guild) {
        [RegistryRow(table:, ..)] if table == profiles.table -> []
        _ -> {
          let _ = registry_insert(registry(), registry_row(profiles))
          invalidate_all(profiles)
        }
      }
  }
}

fn invalidate_all(profiles: Profiles) -> List(Int) {
  let now = clock.monotonic_ms()
  ets_rows(profiles.table)
  |> list.map(fn(row) {
    let _ =
      ets_insert(
        profiles.table,
        Invalidated(row.user, now, now + invalidation_ttl_ms()),
      )
    row.user
  })
}

pub fn release(profiles: Profiles) -> Nil {
  case registry_exists() {
    True -> {
      let _ = registry_delete_object(registry(), registry_row(profiles))
      Nil
    }
    False -> Nil
  }
  case alive(profiles.table) {
    True -> {
      let _ = ets_drop(profiles.table)
      Nil
    }
    False -> Nil
  }
}

pub fn lookup(profiles: Profiles, user: Int) -> Option(Settings) {
  case alive(profiles.table) {
    False -> None
    True ->
      case ets_lookup(profiles.table, user) {
        [ProfileRow(settings:, expires_at_ms:, ..)] ->
          case clock.monotonic_ms() < expires_at_ms {
            True -> Some(settings)
            False -> None
          }
        _ -> None
      }
  }
}

pub fn all(profiles: Profiles) -> List(#(Int, Settings)) {
  case alive(profiles.table) {
    False -> []
    True -> {
      let now = clock.monotonic_ms()
      ets_rows(profiles.table)
      |> list.filter_map(fn(row) {
        case row {
          ProfileRow(user:, settings:, expires_at_ms:, ..)
            if now < expires_at_ms
          -> Ok(#(user, settings))
          _ -> Error(Nil)
        }
      })
    }
  }
}

pub fn put_fetched(
  profiles: Profiles,
  user: Int,
  settings: Settings,
  fetch_started_ms: Int,
) -> Bool {
  case alive(profiles.table) {
    False -> False
    True -> {
      let row =
        ProfileRow(
          user:,
          settings:,
          fetched_at_ms: fetch_started_ms,
          expires_at_ms: clock.monotonic_ms() + profile_ttl_ms(),
        )
      case ets_insert_new(profiles.table, row) {
        True -> True
        False -> {
          let any = atom.create("_")
          let since = atom.create("$1")
          let replace = [#(atom.create("const"), row)]
          let replaced =
            ets_select_replace(profiles.table, [
              #(
                head(#(atom.create("profile_row"), user, any, since, any)),
                [#(atom.create("=<"), since, fetch_started_ms)],
                replace,
              ),
              #(
                head(#(atom.create("invalidated"), user, since, any)),
                [#(atom.create("<"), since, fetch_started_ms)],
                replace,
              ),
            ])
          replaced > 0
        }
      }
    }
  }
}

pub fn expire(profiles: Profiles) -> List(Int) {
  case alive(profiles.table) {
    False -> []
    True -> {
      let now = clock.monotonic_ms()
      let any = atom.create("_")
      let expired = [#(atom.create("=<"), atom.create("$2"), now)]
      let profile_head =
        head(#(
          atom.create("profile_row"),
          atom.create("$1"),
          any,
          any,
          atom.create("$2"),
        ))
      let invalidated_head =
        head(#(
          atom.create("invalidated"),
          atom.create("$1"),
          any,
          atom.create("$2"),
        ))
      let users =
        ets_select(profiles.table, [
          #(profile_head, expired, [atom.create("$1")]),
          #(invalidated_head, expired, [atom.create("$1")]),
        ])
      let _ =
        ets_select_delete(profiles.table, [
          #(profile_head, expired, [True]),
          #(invalidated_head, expired, [True]),
        ])
      users
    }
  }
}

pub fn invalidate(guild: Int, user: Int) -> Nil {
  case guild != 0 && registry_exists() {
    False -> Nil
    True ->
      case registry_lookup(registry(), guild) {
        [RegistryRow(table:, synced:, ..)] -> {
          case alive(table) {
            True -> {
              let now = clock.monotonic_ms()
              let _ =
                ets_insert(
                  table,
                  Invalidated(user, now, now + invalidation_ttl_ms()),
                )
              Nil
            }
            False -> Nil
          }
          synced(user)
        }
        _ -> Nil
      }
  }
}
