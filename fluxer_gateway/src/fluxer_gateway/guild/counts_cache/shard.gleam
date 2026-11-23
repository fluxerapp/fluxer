// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/read_model
import fluxer_gateway/guild/table.{type Table}
import gleam/erlang/process.{type Pid}

fn rows() -> Table(Int, #(Int, Int)) {
  table.named(names.counts_table)
}

pub fn put(guild: GuildId, member_count: Int, online_count: Int) -> Nil {
  table.insert(rows(), id.guild_id_to_int(guild), #(member_count, online_count))
}

pub fn delete(guild: GuildId) -> Nil {
  table.delete(rows(), id.guild_id_to_int(guild))
}

pub fn delete_owned(guild: GuildId, owner: Pid) -> Nil {
  case read_model.get(guild) {
    Ok(view) if view.handle.pid != owner -> Nil
    _ -> delete(guild)
  }
}

pub fn get(guild: Int) -> Result(#(Int, Int), Nil) {
  table.lookup(rows(), guild)
}

pub fn guild_ints() -> List(Int) {
  table.fold(rows(), [], fn(acc, key, _value) { [key, ..acc] })
}

pub fn delete_int(guild: Int) -> Nil {
  table.delete(rows(), guild)
}

pub fn totals() -> #(Int, Int) {
  table.fold(rows(), #(0, 0), fn(acc, _guild, counts) {
    #(acc.0 + counts.0, acc.1 + counts.1)
  })
}
