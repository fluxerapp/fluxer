// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError, RpcFailed}
import fluxer_gateway/core/id
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import gleam/json.{type Json}
import gleam/list
import gleam/result

pub fn handle(method: String, params: Params) -> Result(Json, RpcError) {
  case method {
    "guild.get_viewable_channels" -> viewable(params)
    "guild.resolve_channel_mentions" -> mentions(params)
    "guild.get_first_viewable_text_channel" -> first_viewable(params)
    "guild.get_category_channel_count" -> category_count(params)
    _ -> channel_count(params)
  }
}

fn viewable(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  infra.answer(guild, inbox.ViewableChannels(user))
  |> result.map_error(fn(_) { RpcFailed("viewable_channels_error") })
}

fn empty_mentions() -> Json {
  json.object([#("channels", json.preprocessed_array([]))])
}

fn mentions(params: Params) -> Result(Json, RpcError) {
  let guild_text =
    rpc_params.field(params, "guild_id")
    |> result.try(fn(value) {
      rpc_params.string(value)
      |> result.replace_error(error.InvalidParams)
    })
  let wanted =
    rpc_params.field(params, "channel_ids")
    |> result.try(fn(value) {
      rpc_params.items(value)
      |> result.replace_error(error.InvalidParams)
    })
  case guild_text, wanted {
    Ok(_), Ok(values) -> {
      use guild <- result.try(rpc_params.guild(params))
      let channels =
        list.try_map(values, rpc_params.validate_snowflake)
        |> result.unwrap([])
        |> list.filter_map(fn(found) {
          id.channel_id(found) |> result.replace_error(Nil)
        })
      case channels {
        [] -> Ok(empty_mentions())
        _ ->
          infra.answer(guild, inbox.ChannelMentions(channels))
          |> result.unwrap(empty_mentions())
          |> Ok
      }
    }
    _, _ -> Ok(empty_mentions())
  }
}

fn first_viewable(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  infra.ask(guild, inbox.FirstViewableTextChannel)
}

fn category_count(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use category <- result.try(rpc_params.channel(params, "category_id"))
  infra.ask(guild, inbox.CategoryChannelCount(category))
}

fn channel_count(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  infra.ask(guild, inbox.ChannelCount)
}
