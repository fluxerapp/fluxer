// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/list

pub type TableOption {
  NamedTable
  Public
  Set
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

type TableId

@external(erlang, "ets", "whereis")
fn ets_whereis(table: Atom) -> TableId

@external(erlang, "erlang", "is_reference")
fn is_reference(value: TableId) -> Bool

pub fn guilds_table() -> Atom {
  atom.create("fluxer_voice_guilds")
}

pub fn dm_sessions_table() -> Atom {
  atom.create("fluxer_voice_dm_sessions")
}

pub fn connections_table() -> Atom {
  atom.create("fluxer_voice_count_connections")
}

pub fn count_owners_table() -> Atom {
  atom.create("fluxer_voice_count_owners")
}

pub fn exists(table: Atom) -> Bool {
  is_reference(ets_whereis(table))
}

pub fn create_tables() -> Nil {
  [
    guilds_table(),
    dm_sessions_table(),
    connections_table(),
    count_owners_table(),
  ]
  |> list.each(fn(table) {
    case exists(table) {
      True -> Nil
      False -> {
        ets_new(table, [
          NamedTable,
          Public,
          Set,
          ReadConcurrency(True),
          WriteConcurrency(True),
        ])
        Nil
      }
    }
  })
}
