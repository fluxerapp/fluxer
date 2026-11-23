// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id
import fluxer_gateway/guild/counts_cache
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import gleam/json.{type Json}
import gleam/list
import gleam/result

pub fn handle(method: String, params: Params) -> Result(Json, RpcError) {
  case method {
    "guild.get_counts" -> {
      use guild <- result.try(rpc_params.guild(params))
      let #(member_count, presence_count) =
        counts_cache.counts(guild) |> result.unwrap(#(0, 0))
      Ok(
        json.object([
          #("member_count", json.int(member_count)),
          #("presence_count", json.int(presence_count)),
        ]),
      )
    }
    _ -> {
      use guilds <- result.try(rpc_params.guild_list(params, "guild_ids"))
      let unique = rpc_params.unique_sorted(guilds)
      use _ <- result.try(rpc_params.check_batch(list.length(unique)))
      Ok(
        json.object([
          #(
            "online_counts",
            json.array(counts_cache.bulk(unique), fn(row) {
              json.object([
                #("guild_id", id.json(row.0)),
                #("member_count", json.int(row.1)),
                #("online_count", json.int(row.2)),
              ])
            }),
          ),
        ]),
      )
    }
  }
}
