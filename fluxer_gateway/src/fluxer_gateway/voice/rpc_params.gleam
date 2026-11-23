// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/guild/rpc/params.{type Params}
import fluxer_gateway/voice/guild_state.{type RpcCall}
import gleam/option.{type Option, None, Some}
import gleam/result

pub type GuildCall {
  GuildCall(guild: GuildId, call: RpcCall)
  EmptyStates
}

fn snowflake(params: Params, key: String) -> Result(Int, RpcError) {
  use value <- result.try(params.field(params, key))
  json_tree.snowflake(json_tree.from_raw(value))
  |> result.replace_error(error.InvalidParams)
}

fn guild_id(params: Params, key: String) -> Result(GuildId, RpcError) {
  use raw <- result.try(snowflake(params, key))
  id.guild_id(raw) |> result.replace_error(error.InvalidParams)
}

fn user_id(params: Params, key: String) -> Result(UserId, RpcError) {
  use raw <- result.try(snowflake(params, key))
  id.user_id(raw) |> result.replace_error(error.InvalidParams)
}

fn channel_id(params: Params, key: String) -> Result(ChannelId, RpcError) {
  use raw <- result.try(snowflake(params, key))
  id.channel_id(raw) |> result.replace_error(error.InvalidParams)
}

fn optional_channel(
  params: Params,
  key: String,
) -> Result(Option(ChannelId), RpcError) {
  use value <- result.try(params.field(params, key))
  case params.is_null(value) {
    True -> Ok(None)
    False ->
      json_tree.snowflake(json_tree.from_raw(value))
      |> result.try(fn(raw) { id.channel_id(raw) |> result.replace_error(Nil) })
      |> result.map(Some)
      |> result.replace_error(error.RpcInternalError)
  }
}

fn optional_text(params: Params, key: String) -> Option(String) {
  params.field(params, key)
  |> result.replace_error(Nil)
  |> result.try(params.string)
  |> option.from_result
}

fn optional_connection(params: Params) -> Option(ConnectionId) {
  optional_text(params, "connection_id")
  |> option.then(fn(text) {
    voice_key.connection_id(text) |> option.from_result
  })
}

fn bool_field(params: Params, key: String) -> Result(Bool, RpcError) {
  use value <- result.try(params.field(params, key))
  json_tree.bool(json_tree.from_raw(value))
  |> result.replace_error(error.RpcInternalError)
}

pub fn guild_call(
  method: String,
  params: Params,
) -> Result(GuildCall, RpcError) {
  case method {
    "guild.update_member_voice" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use user <- result.try(user_id(params, "user_id"))
      use mute <- result.try(bool_field(params, "mute"))
      use deaf <- result.try(bool_field(params, "deaf"))
      Ok(GuildCall(guild, guild_state.MemberVoice(user, mute, deaf)))
    }
    "guild.disconnect_voice_user_if_in_channel" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use user <- result.try(user_id(params, "user_id"))
      use channel <- result.try(channel_id(params, "expected_channel_id"))
      Ok(GuildCall(
        guild,
        guild_state.DisconnectIfInChannel(
          user,
          channel,
          optional_connection(params),
        ),
      ))
    }
    "guild.confirm_voice_connection_from_livekit" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use _ <- result.try(params.field(params, "connection_id"))
      Ok(GuildCall(
        guild,
        guild_state.Confirm(
          optional_connection(params),
          optional_text(params, "token_nonce"),
        ),
      ))
    }
    "guild.get_voice_states_for_channel" ->
      case guild_id(params, "guild_id"), channel_id(params, "channel_id") {
        Ok(guild), Ok(channel) ->
          Ok(GuildCall(guild, guild_state.ChannelStates(channel)))
        _, _ -> Ok(EmptyStates)
      }
    "guild.move_member" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use user <- result.try(user_id(params, "user_id"))
      use moderator <- result.try(user_id(params, "moderator_id"))
      use channel <- result.try(optional_channel(params, "channel_id"))
      Ok(GuildCall(
        guild,
        guild_state.MoveMember(
          user,
          moderator,
          channel,
          optional_connection(params),
        ),
      ))
    }
    "guild.get_voice_state" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use user <- result.try(user_id(params, "user_id"))
      Ok(GuildCall(guild, guild_state.GetVoiceState(user)))
    }
    "guild.switch_voice_region" -> {
      use guild <- result.try(guild_id(params, "guild_id"))
      use channel <- result.try(channel_id(params, "channel_id"))
      Ok(GuildCall(guild, guild_state.SwitchRegion(channel)))
    }
    _ -> Error(error.UnknownMethod(method))
  }
}
