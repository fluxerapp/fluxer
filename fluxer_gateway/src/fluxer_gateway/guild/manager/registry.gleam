// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table.{type Table}
import fluxer_gateway/telemetry/process_registry
import gleam/erlang/process
import gleam/list
import gleam/result

fn rows() -> Table(Int, Handle) {
  table.named(names.registry_table)
}

pub fn put(guild: GuildId, handle: Handle) -> Nil {
  table.insert(rows(), id.guild_id_to_int(guild), handle)
  let _ =
    process_registry.register(
      process_registry.guilds,
      id.guild_id_to_int(guild),
      handle.pid,
    )
  Nil
}

pub fn lookup(guild: GuildId) -> Result(Handle, Nil) {
  use handle <- result.try(table.lookup(rows(), id.guild_id_to_int(guild)))
  case process.is_alive(handle.pid) {
    True -> Ok(handle)
    False -> Error(Nil)
  }
}

pub fn remove(guild: GuildId, handle: Handle) -> Nil {
  table.delete_row(rows(), id.guild_id_to_int(guild), handle)
  process_registry.unregister_pid(
    process_registry.guilds,
    id.guild_id_to_int(guild),
    handle.pid,
  )
}

pub fn count() -> Int {
  table.size(rows())
}

pub fn handle_by_pid(pid: process.Pid) -> Result(Handle, Nil) {
  use value <- result.try(process_registry.id_of(process_registry.guilds, pid))
  use guild <- result.try(id.guild_id(value) |> result.replace_error(Nil))
  use found <- result.try(lookup(guild))
  case found.pid == pid {
    True -> Ok(found)
    False -> Error(Nil)
  }
}

pub fn entries() -> List(#(GuildId, Handle)) {
  rows()
  |> table.to_list
  |> list.filter_map(fn(row) {
    id.guild_id(row.0)
    |> result.map(fn(guild) { #(guild, row.1) })
  })
}

pub fn guild_ints() -> List(Int) {
  rows()
  |> table.to_list
  |> list.map(fn(row) { row.0 })
}
