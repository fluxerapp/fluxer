// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError, RpcFailed}
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result

pub fn handle(method: String, params: Params) -> Result(Json, RpcError) {
  case method {
    "guild.get_member" -> get_member(params)
    "guild.has_member" -> has_member(params)
    "guild.list_members" -> list_members(params)
    "guild.list_members_cursor" -> list_members_cursor(params)
    "guild.get_members_with_role" -> members_with_role(params)
    "guild.check_target_member" -> check_target_member(params)
    _ -> evict_visitors(params)
  }
}

fn get_member(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  infra.answer(guild, inbox.GetMember(user))
  |> result.map_error(fn(_) { RpcFailed("guild_not_found") })
}

fn has_member(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  infra.answer(guild, inbox.HasMember(user))
  |> result.map_error(fn(_) { RpcFailed("guild_not_found") })
}

fn list_members(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use limit <- result.try(rpc_params.field(params, "limit"))
  use offset <- result.try(rpc_params.field(params, "offset"))
  case rpc_params.int(limit), rpc_params.int(offset) {
    Ok(limit), Ok(offset) ->
      infra.answer(guild, inbox.ListMembers(limit, int.max(offset, 0)))
    _, _ -> Error(RpcFailed("guild_members_error"))
  }
}

fn list_members_cursor(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  let limit =
    rpc_params.optional_int(params, "limit")
    |> option.map(int.clamp(_, 1, 1000))
    |> option.unwrap(1)
  use after <- result.try(case rpc_params.optional(params, "after") {
    None -> Ok(None)
    Some(_) -> rpc_params.user(params, "after") |> result.map(Some)
  })
  infra.answer(guild, inbox.ListMembersCursor(limit, after))
}

fn members_with_role(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use role <- result.try(rpc_params.role(params, "role_id"))
  infra.answer(guild, inbox.MembersWithRole(role))
  |> result.map_error(fn(_) { RpcFailed("members_with_role_error") })
}

fn check_target_member(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use target <- result.try(rpc_params.user(params, "target_user_id"))
  infra.answer(guild, inbox.CheckTargetMember(user, target))
}

fn evict_visitors(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  let keys =
    rpc_params.field(params, "ip_keys")
    |> result.try(fn(value) {
      rpc_params.items(value)
      |> result.replace_error(RpcFailed("invalid_ip_keys"))
    })
  use keys <- result.try(keys)
  let keys =
    list.filter_map(keys, fn(value) {
      case rpc_params.string(value) {
        Ok(text) if text != "" -> Ok(text)
        _ -> Error(Nil)
      }
    })
  let empty = json.object([#("user_ids", json.preprocessed_array([]))])
  case keys, registry.lookup(guild) {
    [], _ -> Ok(empty)
    _, Error(Nil) -> Ok(empty)
    _, Ok(_) ->
      infra.ask(guild, inbox.EvictVisitors(keys))
      |> result.map_error(fn(_) { RpcFailed("guild_visitors_error") })
  }
}
