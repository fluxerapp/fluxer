// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{
  type GatewayError, type RpcError, InvalidParams, RpcFailed,
}
import fluxer_gateway/core/id.{
  type ChannelId, type GuildId, type RoleId, type UserId,
}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type Params {
  Params(fields: Dict(String, RawJson))
}

pub fn of_object(raw: RawJson) -> Result(Params, Nil) {
  raw_json.fields(raw)
  |> result.map(fn(fields) { Params(dict.from_list(fields)) })
}

pub fn field(params: Params, key: String) -> Result(RawJson, RpcError) {
  dict.get(params.fields, key)
  |> result.replace_error(error.RpcInternalError)
}

pub fn optional(params: Params, key: String) -> Option(RawJson) {
  case dict.get(params.fields, key) {
    Ok(value) ->
      case is_null(value) {
        True -> None
        False -> Some(value)
      }
    Error(Nil) -> None
  }
}

pub fn optional_int(params: Params, key: String) -> Option(Int) {
  optional(params, key)
  |> option.then(fn(value) { flexible_int(value) |> option.from_result })
}

pub fn is_null(value: RawJson) -> Bool {
  tree(value) == Ok(json_tree.Null)
}

pub fn string(value: RawJson) -> Result(String, Nil) {
  tree(value) |> result.try(json_tree.string)
}

pub fn int(value: RawJson) -> Result(Int, Nil) {
  tree(value) |> result.try(json_tree.int)
}

pub fn items(value: RawJson) -> Result(List(RawJson), Nil) {
  raw_json.items(value)
}

pub fn flexible_int(value: RawJson) -> Result(Int, Nil) {
  tree(value) |> result.try(json_tree.loose_int)
}

fn tree(value: RawJson) -> Result(JsonTree, Nil) {
  json_tree.expand(json_tree.from_raw(value))
}

pub fn validate_snowflake(value: RawJson) -> Result(Int, GatewayError) {
  case tree(value) {
    Ok(json_tree.Null) -> Error(error.ValidationNullSnowflake)
    Ok(json_tree.Int(_) as found) | Ok(json_tree.String(_) as found) ->
      json_tree.snowflake(found)
      |> result.replace_error(error.ValidationInvalidSnowflake)
    _ -> Error(error.ValidationInvalidSnowflake)
  }
}

pub fn validate_snowflake_list(
  value: RawJson,
) -> Result(List(Int), GatewayError) {
  case items(value) {
    Error(Nil) -> Error(error.ValidationExpectedList)
    Ok(values) ->
      list.try_map(values, fn(item) {
        tree(item) |> result.try(json_tree.snowflake)
      })
      |> result.replace_error(error.ValidationInvalidSnowflakeList)
  }
}

pub fn snowflake(params: Params, key: String) -> Result(Int, RpcError) {
  use value <- result.try(field(params, key))
  validate_snowflake(value) |> result.replace_error(InvalidParams)
}

pub fn guild(params: Params) -> Result(GuildId, RpcError) {
  use value <- result.try(snowflake(params, "guild_id"))
  id.guild_id(value) |> result.replace_error(InvalidParams)
}

pub fn user(params: Params, key: String) -> Result(UserId, RpcError) {
  use value <- result.try(snowflake(params, key))
  id.user_id(value) |> result.replace_error(InvalidParams)
}

pub fn role(params: Params, key: String) -> Result(RoleId, RpcError) {
  use value <- result.try(snowflake(params, key))
  id.role_id(value) |> result.replace_error(InvalidParams)
}

pub fn channel(params: Params, key: String) -> Result(ChannelId, RpcError) {
  use value <- result.try(snowflake(params, key))
  id.channel_id(value) |> result.replace_error(InvalidParams)
}

pub fn guild_list(
  params: Params,
  key: String,
) -> Result(List(GuildId), RpcError) {
  use value <- result.try(field(params, key))
  use ids <- result.try(
    validate_snowflake_list(value)
    |> result.replace_error(InvalidParams),
  )
  list.try_map(ids, fn(found) {
    id.guild_id(found) |> result.replace_error(InvalidParams)
  })
}

pub fn unique_sorted(guilds: List(GuildId)) -> List(GuildId) {
  guilds
  |> list.map(id.guild_id_to_int)
  |> list.unique
  |> list.sort(int.compare)
  |> list.filter_map(fn(value) {
    id.guild_id(value) |> result.replace_error(Nil)
  })
}

const max_batch_size = 100

pub fn check_batch(size: Int) -> Result(Nil, RpcError) {
  case size > max_batch_size {
    True -> Error(RpcFailed("batch_too_large"))
    False -> Ok(Nil)
  }
}

pub fn optional_user(
  params: Params,
  key: String,
) -> Result(Option(UserId), RpcError) {
  use value <- result.try(field(params, key))
  optional_id(value, id.user_id)
}

pub fn optional_channel_param(
  params: Params,
  key: String,
) -> Result(Option(ChannelId), RpcError) {
  case optional(params, key) {
    None -> Ok(None)
    Some(value) -> optional_id(value, id.channel_id)
  }
}

fn optional_id(
  value: RawJson,
  build: fn(Int) -> Result(a, id.IdError),
) -> Result(Option(a), RpcError) {
  case is_null(value) {
    True -> Ok(None)
    False ->
      validate_snowflake(value)
      |> result.replace_error(Nil)
      |> result.try(fn(found) { build(found) |> result.replace_error(Nil) })
      |> result.map(Some)
      |> result.replace_error(RpcFailed("validation_invalid_params"))
  }
}

pub fn permission_channel(
  params: Params,
) -> Result(Option(ChannelId), RpcError) {
  use value <- result.try(field(params, "channel_id"))
  case string(value) {
    Ok("0") -> Ok(None)
    _ ->
      validate_snowflake(value)
      |> result.replace_error(InvalidParams)
      |> result.try(fn(found) {
        id.channel_id(found) |> result.replace_error(InvalidParams)
      })
      |> result.map(Some)
  }
}

pub fn permission_bits(params: Params) -> Result(Permissions, RpcError) {
  use value <- result.try(field(params, "permission"))
  case flexible_int(value) {
    Ok(bits) if bits >= 0 ->
      permissions.from_int(bits) |> result.replace_error(InvalidParams)
    _ -> Error(InvalidParams)
  }
}

pub fn truthy(value: Option(RawJson)) -> Bool {
  case option.then(value, fn(found) { tree(found) |> option.from_result }) {
    Some(json_tree.Bool(True)) -> True
    Some(json_tree.String("true")) -> True
    Some(json_tree.Int(1)) -> True
    _ -> False
  }
}
