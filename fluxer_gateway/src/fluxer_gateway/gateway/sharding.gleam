// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/result

const max_shard_count = 16_384

pub type ShardError {
  InvalidShard
}

pub fn guild_in_shard(guild: GuildId, shard: #(Int, Int)) -> Bool {
  let value = id.guild_id_to_int(guild)
  case value <= 0, shard {
    True, _ -> False
    False, #(0, 1) -> True
    False, #(shard_id, shard_count) ->
      int.bitwise_shift_right(value, 22) % shard_count == shard_id
  }
}

pub fn shard_to_json(shard: #(Int, Int)) -> Json {
  json.preprocessed_array([json.int(shard.0), json.int(shard.1)])
}

pub fn decoder() -> Decoder(Result(Option(#(Int, Int)), ShardError)) {
  decode.optional(decode.list(decode.int))
  |> decode.map(fn(found) {
    case found {
      None -> Ok(None)
      Some([shard_id, shard_count]) ->
        validate_pair(shard_id, shard_count) |> result.map(Some)
      Some(_) -> Error(InvalidShard)
    }
  })
  |> decode.one_of([decode.success(Error(InvalidShard))])
}

fn validate_pair(
  shard_id: Int,
  shard_count: Int,
) -> Result(#(Int, Int), ShardError) {
  let valid =
    shard_id >= 0
    && shard_count > 0
    && shard_count <= max_shard_count
    && shard_id < shard_count
  case valid {
    True -> Ok(#(shard_id, shard_count))
    False -> Error(InvalidShard)
  }
}
