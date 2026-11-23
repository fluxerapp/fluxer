// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError, RpcFailed}
import fluxer_gateway/core/id
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const default_limit = 1000

const max_limit = 5000

const validation_error = "resolve_mention_sources_page_validation_error"

type Request {
  Request(guild: id.GuildId, page: inbox.MentionPage)
}

pub fn handle(params: Params) -> Result(Json, RpcError) {
  case parse(params) {
    Error(Nil) -> Error(RpcFailed(validation_error))
    Ok(Request(guild:, page:)) ->
      infra.ask(guild, inbox.MentionSources(page))
      |> result.map_error(fn(reason) {
        case reason {
          RpcFailed("guild_not_found") | RpcFailed("guild_overloaded") -> reason
          error.RpcTimeout -> reason
          _ -> RpcFailed("resolve_mention_sources_page_error")
        }
      })
  }
}

fn parse(params: Params) -> Result(Request, Nil) {
  let snowflake = fn(key) {
    rpc_params.snowflake(params, key) |> result.replace_error(Nil)
  }
  use guild_int <- result.try(snowflake("guild_id"))
  use channel_int <- result.try(snowflake("channel_id"))
  use author_int <- result.try(snowflake("author_id"))
  use guild <- result.try(id.guild_id(guild_int) |> result.replace_error(Nil))
  use channel <- result.try(
    id.channel_id(channel_int) |> result.replace_error(Nil),
  )
  use author <- result.try(id.user_id(author_int) |> result.replace_error(Nil))
  use roles <- result.try(id_list(params, "role_ids", id.role_id))
  use users <- result.try(id_list(params, "user_ids", id.user_id))
  use cursor <- result.try(case rpc_params.optional(params, "cursor") {
    None -> Ok(None)
    Some(value) ->
      rpc_params.validate_snowflake(value)
      |> result.map(Some)
      |> result.replace_error(Nil)
  })
  let limit =
    rpc_params.optional_int(params, "limit")
    |> option.map(int.clamp(_, 1, max_limit))
    |> option.unwrap(default_limit)
  Ok(Request(
    guild:,
    page: inbox.MentionPage(
      channel:,
      author:,
      mention_everyone: rpc_params.truthy(rpc_params.optional(
        params,
        "mention_everyone",
      )),
      mention_here: rpc_params.truthy(rpc_params.optional(
        params,
        "mention_here",
      )),
      roles:,
      users:,
      limit:,
      cursor:,
    ),
  ))
}

fn id_list(
  params: Params,
  key: String,
  build: fn(Int) -> Result(a, id.IdError),
) -> Result(List(a), Nil) {
  case rpc_params.optional(params, key) {
    None -> Ok([])
    Some(value) ->
      rpc_params.validate_snowflake_list(value)
      |> result.replace_error(Nil)
      |> result.try(fn(ids) {
        list.try_map(ids, fn(found) {
          build(found) |> result.replace_error(Nil)
        })
      })
  }
}
