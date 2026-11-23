// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/presence/protocol.{type Input}
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/list

pub type Inbox =
  Subject(Input(Monitor))

fn shard_table(shard: Int) -> Atom {
  route.table(route.PidsTable, shard)
}

pub fn tables() -> List(Atom) {
  list.map(route.shards(), shard_table)
}

pub fn lookup(user: Int) -> Result(Inbox, Nil) {
  case table.lookup(shard_table(route.shard(user)), user) {
    [#(_, inbox), ..] -> Ok(inbox)
    [] -> Error(Nil)
  }
}

pub fn put(user: Int, inbox: Inbox) -> Nil {
  table.insert(shard_table(route.shard(user)), #(user, inbox))
}

pub fn delete(user: Int) -> Nil {
  table.delete(shard_table(route.shard(user)), user)
}

pub fn shard_entries(shard: Int) -> List(#(Int, Inbox)) {
  table.rows(shard_table(shard))
}

pub fn count() -> Int {
  list.fold(route.shards(), 0, fn(total, shard) {
    total + table.size(shard_table(shard))
  })
}
