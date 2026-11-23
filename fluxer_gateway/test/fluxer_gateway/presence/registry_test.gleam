// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleeunit/should

const guild_key = 914_000_000_000_000_001

fn guild_rows(key: Int) -> List(#(#(Int, Int), Subject(GuildRequest))) {
  table.rows(route.table(route.GuildInterestTable, 0))
  |> list.filter(fn(row: #(#(Int, Int), Subject(GuildRequest))) {
    row.0.0 == key
  })
}

fn row_members() -> List(Int) {
  let assert Ok(guild) = id.guild_id(guild_key)
  registry.guild_interest(guild)
}

fn as_other_process(work: fn() -> Nil) -> Nil {
  let done = process.new_subject()
  process.spawn(fn() {
    work()
    process.send(done, Nil)
  })
  let _ = process.receive(done, 1000)
  Nil
}

pub fn retiring_copy_cannot_drop_live_copy_interest_test() {
  table.create_ordered(route.table(route.GuildInterestTable, 0))
  let assert Ok(guild) = id.guild_id(guild_key)
  let live: Subject(GuildRequest) = process.new_subject()
  registry.register_guild(guild, packed_ids.from_list([1, 2, 3]), live)
  as_other_process(fn() {
    registry.unregister_guild(guild, packed_ids.from_list([2]))
    registry.drop_guild(guild)
  })
  row_members() |> should.equal([1, 2, 3])
  registry.unregister_guild(guild, packed_ids.from_list([2]))
  row_members() |> should.equal([1, 3])
  registry.drop_guild(guild)
  guild_rows(guild_key) |> should.equal([])
}

pub fn drop_guild_only_removes_interest_for_owner_test() {
  list.each(registry.tables(), table.create)
  list.each(registry.ordered_tables(), table.create_ordered)
  let assert Ok(guild) = id.guild_id(guild_key + 1)
  let registered = process.new_subject()
  let dropped = process.new_subject()
  process.spawn(fn() {
    let inbox: Subject(GuildRequest) = process.new_subject()
    let drop = process.new_subject()
    registry.register_guild(guild, packed_ids.empty(), inbox)
    process.send(registered, drop)
    let _ = process.receive(drop, 1000)
    registry.drop_guild(guild)
    process.send(dropped, Nil)
  })
  let assert Ok(drop) = process.receive(registered, 1000)
  let rows = fn() { guild_rows(guild_key + 1) }
  registry.drop_guild(guild)
  rows() |> list.length |> should.equal(1)
  process.send(drop, Nil)
  let assert Ok(Nil) = process.receive(dropped, 1000)
  rows() |> should.equal([])
}

@external(erlang, "erlang", "process_info")
fn process_info(pid: process.Pid, item: atom.Atom) -> #(atom.Atom, Int)

fn reductions() -> Int {
  process_info(process.self(), atom.create("reductions")).1
}

@external(erlang, "ets", "info")
fn ets_info(table: atom.Atom, item: atom.Atom) -> Int

fn interest_rows() -> Int {
  ets_info(route.table(route.GuildInterestTable, 0), atom.create("size"))
}

pub fn joining_a_large_guild_one_user_at_a_time_stays_cheap_test() {
  list.each(registry.tables(), table.create)
  list.each(registry.ordered_tables(), table.create_ordered)
  let assert Ok(guild) = id.guild_id(guild_key + 2)
  let inbox: Subject(GuildRequest) = process.new_subject()
  let users = int.range(1, 5001, [], list.prepend)
  let rows_before = interest_rows()
  let before = reductions()
  list.each(users, fn(user) {
    registry.register_guild(guild, packed_ids.from_list([user]), inbox)
  })
  let per_join = { reductions() - before } / 5000
  registry.drop_guild(guild)
  interest_rows() |> should.equal(rows_before)
  assert per_join < 500
}

pub fn a_large_guild_keeps_every_user_one_at_a_time_test() {
  list.each(registry.tables(), table.create)
  list.each(registry.ordered_tables(), table.create_ordered)
  let assert Ok(guild) = id.guild_id(guild_key + 3)
  let inbox: Subject(GuildRequest) = process.new_subject()
  int.range(1, 2001, [], list.prepend)
  |> list.each(fn(user) {
    registry.register_guild(guild, packed_ids.from_list([user]), inbox)
  })
  registry.guild_interest(guild) |> list.length |> should.equal(2000)
  registry.unregister_guild(guild, packed_ids.from_list([7, 9]))
  registry.guild_interest(guild) |> list.length |> should.equal(1998)
  registry.drop_guild(guild)
  guild_rows(guild_key + 3) |> should.equal([])
}
