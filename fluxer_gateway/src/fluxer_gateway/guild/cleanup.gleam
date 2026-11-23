// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/counts_cache/shard as counts_shard
import fluxer_gateway/guild/health
import fluxer_gateway/guild/members
import fluxer_gateway/guild/push/dispatch as push_dispatch
import fluxer_gateway/guild/read_model
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/threads as guild_threads
import fluxer_gateway/member_list
import fluxer_gateway/perms
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/voice/guild_handler
import gleam/erlang/process
import gleam/list

pub fn release(state: GuildState) -> Nil {
  let guild = guild_state.guild_id(state)
  let self = process.self()
  read_model.delete(guild, self)
  presence_client.drop_guild_interest(guild)
  counts_shard.delete_owned(guild, self)
  health.unregister(guild)
  push_dispatch.release(guild_state.push(state))
  guild_threads.release(state)
  member_list.release(guild_state.member_list(state))
  perms.release(guild_state.perms_handle(state))
  case guild_state.repair(state) {
    guild_state.Rebuilt(stale) -> list.each(stale, perms.release)
    guild_state.Intact | guild_state.Unrepairable -> Nil
  }
  members.drop(guild_state.members_table(state))
  guild_handler.release(guild_state.voice(state))
}

pub fn release_staged(state: GuildState) -> Nil {
  push_dispatch.release(guild_state.push(state))
  member_list.release(guild_state.member_list(state))
  perms.release(guild_state.perms_handle(state))
  members.drop(guild_state.members_table(state))
  guild_handler.release(guild_state.voice(state))
}
