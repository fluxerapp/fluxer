// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/push_buffer.{type Pending, type PushBuffer}
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

pub type Leftovers {
  Leftovers(temporary_guilds: List(Int), pending: List(Pending))
}

fn shard_table(shard: Int) -> Atom {
  route.table(route.LeftoversTable, shard)
}

pub fn tables() -> List(Atom) {
  list.map(route.shards(), shard_table)
}

pub fn record(user: Int, temporary_guilds: IdSet, push: PushBuffer) -> Nil {
  let owner = process.self()
  case id_set.is_empty(temporary_guilds), push_buffer.is_empty(push) {
    True, True -> release(user, owner)
    _, _ ->
      table.insert(shard_table(route.shard(user)), #(
        user,
        owner,
        Leftovers(
          temporary_guilds: id_set.to_list(temporary_guilds),
          pending: push_buffer.pending(push),
        ),
      ))
  }
}

pub fn release(user: Int, owner: Pid) -> Nil {
  let _ = take(user, owner)
  Nil
}

pub fn take(user: Int, owner: Pid) -> Leftovers {
  case table.lookup(shard_table(route.shard(user)), user) {
    [#(_, holder, leftovers), ..] if holder == owner -> {
      table.delete(shard_table(route.shard(user)), user)
      leftovers
    }
    _ -> Leftovers(temporary_guilds: [], pending: [])
  }
}
