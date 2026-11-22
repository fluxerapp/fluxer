// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/guild/push/profiles
import fluxer_gateway/push/server
import fluxer_gateway/util/decode as edge
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/json.{type Json}
import gleam/list
import gleam/result
import logging

fn multicall_timeout_ms() -> Int {
  durations.push().rpc_multicall_timeout_ms
}

pub type SyncRequest {
  SyncRequest(user: Int, guild: Int)
}

@external(erlang, "rpc", "multicall")
fn multicall(
  nodes: List(Atom),
  module: Atom,
  function: Atom,
  args: List(SyncRequest),
  timeout: Int,
) -> #(List(Dynamic), List(Atom))

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(text: String) -> Atom

pub fn handle(method: String, params: Dynamic) -> Result(Json, RpcError) {
  case method {
    "push.sync_user_guild_settings" -> sync_user_guild_settings(params)
    "push.clear_channel_notifications" -> clear_channel_notifications(params)
    _ -> Error(error.UnknownMethod(method))
  }
}

fn sync_user_guild_settings(params: Dynamic) -> Result(Json, RpcError) {
  let decoder = {
    use user <- decode.field("user_id", edge.snowflake())
    use guild <- decode.field("guild_id", settings_guild_id())
    use _ <- decode.field("user_guild_settings", decode.dynamic)
    decode.success(SyncRequest(user:, guild:))
  }
  use request <- result.try(
    decode.run(params, decoder) |> result.replace_error(error.InvalidParams),
  )
  let nodes = list.map([node.self(), ..node.connected()], node.to_atom)
  let #(_, failed) =
    multicall(
      nodes,
      to_atom("fluxer_gateway@push@rpc"),
      to_atom("sync_local"),
      [request],
      multicall_timeout_ms(),
    )
  case failed {
    [] -> Nil
    _ -> logging.log(logging.Warning, "push cache mutation fanout failed")
  }
  Ok(json.bool(True))
}

pub fn sync_local(request: SyncRequest) -> Nil {
  server.invalidate_user_guild_settings_local(request.user, request.guild)
  profiles.invalidate(request.guild, request.user)
}

fn settings_guild_id() -> decode.Decoder(Int) {
  decode.one_of(
    decode.string
      |> decode.then(fn(text) {
        case text {
          "0" -> decode.success(0)
          _ -> decode.failure(0, "guild_id")
        }
      }),
    [edge.snowflake()],
  )
}

fn clear_channel_notifications(params: Dynamic) -> Result(Json, RpcError) {
  let decoder = {
    use user <- decode.field("user_id", edge.user_id())
    use channel <- decode.field("channel_id", edge.channel_id())
    use message <- decode.field("message_id", edge.snowflake())
    decode.success(#(user, channel, message))
  }
  use #(user, channel, raw_message) <- result.try(
    decode.run(params, decoder) |> result.replace_error(error.InvalidParams),
  )
  use message <- result.try(
    id.message_id(raw_message) |> result.replace_error(error.InvalidParams),
  )
  server.clear_channel_notifications(user, channel, message)
  Ok(json.bool(True))
}
