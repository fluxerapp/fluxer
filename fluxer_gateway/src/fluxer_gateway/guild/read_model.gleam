// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/names
import fluxer_gateway/guild/table.{type Table}
import fluxer_gateway/member_list
import fluxer_gateway/perms
import gleam/erlang/process
import gleam/result

pub type View {
  View(
    handle: Handle,
    data: GuildData,
    members: MembersTable,
    reader: perms.Reader,
    counts: member_list.CountsHandle,
    public_online: Int,
    version: Int,
  )
}

fn rows() -> Table(Int, View) {
  table.named(names.read_model_table)
}

pub fn put(guild: GuildId, view: View) -> Nil {
  table.insert(rows(), id.guild_id_to_int(guild), view)
}

pub fn get(guild: GuildId) -> Result(View, Nil) {
  use view <- result.try(table.lookup(rows(), id.guild_id_to_int(guild)))
  case is_live(view) {
    True -> Ok(view)
    False -> Error(Nil)
  }
}

pub fn is_live(view: View) -> Bool {
  process.is_alive(view.handle.pid) && members.is_alive(view.members)
}

pub fn delete(guild: GuildId, owner: process.Pid) -> Nil {
  case table.lookup(rows(), id.guild_id_to_int(guild)) {
    Ok(view) if view.handle.pid == owner ->
      table.delete_row(rows(), id.guild_id_to_int(guild), view)
    _ -> Nil
  }
}

pub fn online_count(view: View) -> Int {
  member_list.counts(view.counts).1
}
