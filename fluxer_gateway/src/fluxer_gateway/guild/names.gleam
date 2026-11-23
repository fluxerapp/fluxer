// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/env
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/util/shard as shard_hash
import gleam/erlang/process.{type Name}
import gleam/int

pub const shard_count = 16

pub fn ets_owner() -> Name(message) {
  env.process_name("fluxer_gateway_guild_ets_owner")
}

pub fn health() -> Name(message) {
  env.process_name("fluxer_gateway_guild_health")
}

pub fn counts_cache() -> Name(message) {
  env.process_name("fluxer_gateway_guild_counts_cache")
}

pub fn shard(index: Int) -> Name(message) {
  env.process_name("fluxer_gateway_guild_shard_" <> int.to_string(index))
}

pub fn shard_of(guild: GuildId) -> Int {
  shard_hash.shard_index(id.guild_id_to_int(guild), shard_count)
}

pub fn factory(index: Int) -> Name(message) {
  env.process_name("fluxer_gateway_guild_factory_" <> int.to_string(index))
}

pub const registry_table = "fluxer_gateway_guild_registry"

pub const read_model_table = "fluxer_gateway_guild_read_model"

pub const unavailability_table = "fluxer_gateway_guild_unavailability"

pub const health_table = "fluxer_gateway_guild_health"

pub const counts_table = "fluxer_gateway_guild_counts"

pub const remains_table = "fluxer_gateway_guild_remains"

pub const roster_table = "fluxer_gateway_guild_roster"
