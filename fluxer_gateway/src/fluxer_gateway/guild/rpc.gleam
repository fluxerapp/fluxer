// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/core/error.{type RpcError, RpcFailed, UnknownMethod}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/remote
import fluxer_gateway/guild/rpc/channels
import fluxer_gateway/guild/rpc/counts
import fluxer_gateway/guild/rpc/infra
import fluxer_gateway/guild/rpc/members
import fluxer_gateway/guild/rpc/mentions
import fluxer_gateway/guild/rpc/params.{type Params} as rpc_params
import fluxer_gateway/guild/rpc/permissions
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state.{type RpcCall}
import fluxer_gateway/voice/rpc_params as voice_params
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/result

pub fn handle_owned(method: String, raw: RawJson) -> #(Bool, BitArray) {
  case rpc_params.of_object(raw) {
    Ok(params) -> encoded(handle_local(method, params))
    Error(Nil) -> encoded(Error(error.RpcInternalError))
  }
}

pub fn encoded(answer: Result(Json, RpcError)) -> #(Bool, BitArray) {
  case answer {
    Ok(result) -> #(True, raw_json.from_json(result) |> raw_json.to_bits)
    Error(reason) -> #(False, <<error.rpc_error_message(reason):utf8>>)
  }
}

pub fn handle_local(method: String, params: Params) -> Result(Json, RpcError) {
  case method {
    "guild.dispatch" -> infra.dispatch(params)
    "guild.dispatch_many" -> infra.dispatch_many(params)
    "guild.get_data" -> get_data(params)
    "guild.get_auth_context" -> get_auth_context(params)
    "guild.start" -> infra.start(params)
    "guild.stop" -> infra.stop(params)
    "guild.reload" -> infra.reload(params)
    "guild.reload_all" -> infra.reload_all(params)
    "guild.shutdown" -> infra.shutdown(params)
    "guild.resync_sessions" -> infra.resync_sessions(params)
    "guild.get_counts" | "guild.get_online_counts_batch" ->
      counts.handle(method, params)
    "guild.get_member"
    | "guild.has_member"
    | "guild.list_members"
    | "guild.list_members_cursor"
    | "guild.get_members_with_role"
    | "guild.check_target_member"
    | "guild.evict_visitors" -> members.handle(method, params)
    "guild.get_user_permissions"
    | "guild.get_user_permissions_batch"
    | "guild.check_permission"
    | "guild.can_manage_roles"
    | "guild.can_manage_role" -> permissions.handle(method, params)
    "guild.get_viewable_channels"
    | "guild.resolve_channel_mentions"
    | "guild.get_first_viewable_text_channel"
    | "guild.get_category_channel_count"
    | "guild.get_channel_count" -> channels.handle(method, params)
    "guild.resolve_mention_sources_page" -> mentions.handle(params)
    "guild.update_member_voice"
    | "guild.disconnect_voice_user_if_in_channel"
    | "guild.confirm_voice_connection_from_livekit"
    | "guild.get_voice_states_for_channel"
    | "guild.move_member"
    | "guild.get_voice_state"
    | "guild.switch_voice_region" -> voice(method, params)
    _ -> Error(UnknownMethod(method))
  }
}

fn get_data(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.optional_user(params, "user_id"))
  infra.answer(guild, inbox.GetGuildData(user))
  |> result.map_error(data_error)
}

fn get_auth_context(params: Params) -> Result(Json, RpcError) {
  use guild <- result.try(rpc_params.guild(params))
  use user <- result.try(rpc_params.optional_user(params, "user_id"))
  use channel <- result.try(rpc_params.optional_channel_param(
    params,
    "channel_id",
  ))
  infra.answer(guild, inbox.GetAuthContext(user, channel))
  |> result.map_error(data_error)
}

fn data_error(reason: RpcError) -> RpcError {
  case reason {
    RpcFailed("forbidden") | RpcFailed("guild_not_found") -> reason
    RpcFailed("guild_overloaded") | error.RpcTimeout -> reason
    _ -> RpcFailed("guild_data_error")
  }
}

fn voice(method: String, params: Params) -> Result(Json, RpcError) {
  use parsed <- result.try(voice_params.guild_call(method, params))
  case parsed {
    voice_params.EmptyStates ->
      Ok(json.object([#("voice_states", json.preprocessed_array([]))]))
    voice_params.GuildCall(guild, call) -> voice_call(guild, call)
  }
}

pub fn voice_call(guild: GuildId, call: RpcCall) -> Result(Json, RpcError) {
  case router.owner(router.GuildKey(guild), node.Guilds) {
    router.Remote(owner) ->
      infra.forward_call(owner, "voice_owned_encoded", [
        remote.arg(guild),
        remote.arg(call),
      ])
      |> result.map(fn(body) { raw_json.wrap_string(body) |> raw_json.to_json })
    router.OwnerUnavailable(_) -> Error(error.RpcTimeout)
    router.Local -> voice_owned(guild, call)
  }
}

pub fn voice_owned_encoded(guild: GuildId, call: RpcCall) -> #(Bool, BitArray) {
  encoded(voice_owned(guild, call))
}

fn voice_owned(guild: GuildId, call: RpcCall) -> Result(Json, RpcError) {
  use found <- result.try(infra.handle(guild))
  let reply = process.new_subject()
  process.send(
    found.control,
    inbox.VoiceRpc(guild_handler.rpc_request(call, reply)),
  )
  case process.receive(reply, infra.guild_call_timeout_ms()) {
    Ok(answer) -> answer
    Error(Nil) -> Error(error.RpcTimeout)
  }
}
