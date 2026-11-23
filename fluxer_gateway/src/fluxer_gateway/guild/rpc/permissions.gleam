// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type RpcError, RpcFailed}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/node
import fluxer_gateway/core/permissions as core_permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/data
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/read_model.{type View}
import fluxer_gateway/guild/remote
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import fluxer_gateway/perms
import fluxer_gateway/perms/query as perms_query
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn batch_timeout_ms() -> Int {
  durations.guild().permissions_batch_timeout_ms
}

type GuildBits {
  GuildBits(guild: GuildId, bits: Int)
}

pub fn handle(method: String, params: Params) -> Result(Json, RpcError) {
  case method {
    "guild.get_user_permissions" -> user_permissions(params)
    "guild.get_user_permissions_batch" -> batch(params)
    "guild.check_permission" -> check(params)
    "guild.can_manage_roles" -> can_manage_roles(params)
    _ -> can_manage_role(params)
  }
}

fn user_permissions(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use channel <- result.try(rpc_params.permission_channel(params))
  use view <- result.try(infra.view(guild))
  case unknown_channel(view, channel) {
    Some(thread) ->
      thread_bits(guild, user, thread)
      |> result.map(fn(bits) {
        json.object([#("permissions", core_permissions.bits_json(bits))])
      })
    None ->
      perms.reply_json(view.reader, perms_query.UserPermissions(user, channel))
      |> result.map(raw_json.to_json)
      |> result.replace_error(RpcFailed("permissions_error"))
  }
}

fn unknown_channel(
  view: View,
  channel: Option(ChannelId),
) -> Option(ChannelId) {
  use found <- option.then(channel)
  case data.find_channel(view.data, found) {
    Some(_) -> None
    None -> Some(found)
  }
}

fn thread_bits(
  guild: GuildId,
  user: UserId,
  thread: ChannelId,
) -> Result(Int, RpcError) {
  infra.ask_thread_bits(guild, user, thread)
  |> result.replace_error(RpcFailed("permissions_error"))
}

fn check(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use required <- result.try(rpc_params.permission_bits(params))
  use channel <- result.try(rpc_params.permission_channel(params))
  use view <- result.try(infra.view(guild))
  case unknown_channel(view, channel) {
    Some(thread) ->
      thread_bits(guild, user, thread)
      |> result.map(fn(bits) {
        let wanted = core_permissions.to_int(required)
        json.object([
          #(
            "has_permission",
            json.bool(int.bitwise_and(bits, wanted) == wanted),
          ),
        ])
      })
      |> result.replace_error(RpcFailed("permission_check_error"))
    None ->
      perms.reply_json(
        view.reader,
        perms_query.CheckPermission(user, channel, required),
      )
      |> result.map(raw_json.to_json)
      |> result.replace_error(RpcFailed("permission_check_error"))
  }
}

fn can_manage_roles(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use _target <- result.try(rpc_params.user(params, "target_user_id"))
  use role <- result.try(rpc_params.role(params, "role_id"))
  infra.answer(guild, inbox.CanManageRoles(user, role))
}

fn can_manage_role(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use role <- result.try(rpc_params.role(params, "role_id"))
  infra.answer(guild, inbox.CanManageRole(user, role))
}

fn batch(params: Params) -> Result(Json, RpcError) {
  use guilds <- result.try(rpc_params.guild_list(params, "guild_ids"))
  use user <- result.try(rpc_params.user(params, "user_id"))
  use channel <- result.try(rpc_params.permission_channel(params))
  let unique = rpc_params.unique_sorted(guilds)
  use _ <- result.try(rpc_params.check_batch(list.length(unique)))
  let replies = process.new_subject()
  list.each(unique, fn(guild) {
    process.spawn(fn() { process.send(replies, one(guild, user, channel)) })
  })
  let results = collect(replies, list.length(unique), batch_timeout_ms(), [])
  Ok(
    json.object([
      #(
        "permissions",
        json.array(results, fn(row) {
          json.object([
            #("guild_id", id.guild_json(row.guild)),
            #("permissions", core_permissions.bits_json(row.bits)),
          ])
        }),
      ),
    ]),
  )
}

fn collect(
  replies: process.Subject(Result(GuildBits, Nil)),
  left: Int,
  timeout_ms: Int,
  acc: List(GuildBits),
) -> List(GuildBits) {
  case left {
    0 -> list.reverse(acc)
    _ ->
      case process.receive(replies, timeout_ms) {
        Ok(Ok(row)) -> collect(replies, left - 1, timeout_ms, [row, ..acc])
        Ok(Error(Nil)) -> collect(replies, left - 1, timeout_ms, acc)
        Error(Nil) -> list.reverse(acc)
      }
  }
}

fn one(
  guild: GuildId,
  user: UserId,
  channel: Option(ChannelId),
) -> Result(GuildBits, Nil) {
  case router.owner(router.GuildKey(guild), node.Guilds) {
    router.Local -> local_bits(guild, user, channel)
    router.Remote(owner) -> remote_bits(owner, guild, user, channel)
    router.OwnerUnavailable(_) -> Error(Nil)
  }
  |> result.map(fn(bits) { GuildBits(guild:, bits:) })
}

fn local_bits(
  guild: GuildId,
  user: UserId,
  channel: Option(ChannelId),
) -> Result(Int, Nil) {
  use view <- result.try(infra.view(guild) |> result.replace_error(Nil))
  case unknown_channel(view, channel) {
    Some(thread) ->
      thread_bits(guild, user, thread) |> result.replace_error(Nil)
    None -> {
      let scope = case channel {
        Some(found) -> perms.ChannelScope(found)
        None -> perms.GuildScope
      }
      perms.permissions(view.reader, user, scope)
      |> result.map(core_permissions.to_int)
      |> result.replace_error(Nil)
    }
  }
}

fn remote_bits(
  owner: node.Node,
  guild: GuildId,
  user: UserId,
  channel: Option(ChannelId),
) -> Result(Int, Nil) {
  let reply =
    remote.call(
      owner,
      "fluxer_gateway@guild@rpc@permissions",
      "owned_bits",
      [remote.arg(guild), remote.arg(user), remote.arg(channel)],
      durations.guild().infra_forward_timeout_ms,
      expecting: {
        use found <- decode.field(0, decode.bool)
        use bits <- decode.field(1, decode.int)
        decode.success(#(found, bits))
      },
    )
  case reply {
    Ok(#(True, bits)) -> Ok(bits)
    _ -> Error(Nil)
  }
}

pub fn owned_bits(
  guild: GuildId,
  user: UserId,
  channel: Option(ChannelId),
) -> #(Bool, Int) {
  case local_bits(guild, user, channel) {
    Ok(bits) -> #(True, bits)
    Error(Nil) -> #(False, 0)
  }
}
