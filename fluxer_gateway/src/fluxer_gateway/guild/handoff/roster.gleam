// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table.{type Table}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

type Key =
  #(Int, SessionId)

type Pattern =
  #(#(Int, Atom), Pid)

@external(erlang, "ets", "select")
fn ets_select(
  table: Table(Key, Pid),
  spec: List(#(Pattern, List(Nil), List(Atom))),
) -> List(SessionId)

@external(erlang, "ets", "select_delete")
fn ets_select_delete(
  table: Table(Key, Pid),
  spec: List(#(Pattern, List(Nil), List(Bool))),
) -> Int

fn rows() -> Table(Key, Pid) {
  table.named(names.roster_table)
}

pub fn put(guild: GuildId, session: SessionId) -> Nil {
  put_many(guild, [session])
}

pub fn put_many(guild: GuildId, sessions: List(SessionId)) -> Nil {
  let rows = rows()
  let owner = process.self()
  let guild = id.guild_id_to_int(guild)
  case sessions != [] && table.exists(rows) {
    True ->
      table.insert_many(
        rows,
        list.map(sessions, fn(session) { #(#(guild, session), owner) }),
      )
    False -> Nil
  }
}

pub fn remove(guild: GuildId, session: SessionId) -> Nil {
  table.delete_row(
    rows(),
    #(id.guild_id_to_int(guild), session),
    process.self(),
  )
}

pub fn take(guild: GuildId, owner: Pid) -> List(SessionId) {
  let rows = rows()
  case table.exists(rows) {
    False -> []
    True -> {
      let key = #(id.guild_id_to_int(guild), atom.create("$1"))
      let found = ets_select(rows, [#(#(key, owner), [], [atom.create("$1")])])
      clear(guild, owner)
      found
    }
  }
}

pub fn clear(guild: GuildId, owner: Pid) -> Nil {
  let rows = rows()
  case table.exists(rows) {
    True -> {
      let key = #(id.guild_id_to_int(guild), atom.create("_"))
      let _ = ets_select_delete(rows, [#(#(key, owner), [], [True])])
      Nil
    }
    False -> Nil
  }
}

pub fn size() -> Int {
  table.size(rows())
}
