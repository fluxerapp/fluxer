// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/guild/rpc as guild_rpc
import fluxer_gateway/util/decode as edge
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/guild_state
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn call_timeout_ms() -> Int {
  durations.voice().rpc_call_timeout_ms
}

type Params =
  Dict(String, RawJson)

pub fn handle(method: String, params: RawJson) -> Result(Json, RpcError) {
  let params = case raw_json.fields(params) {
    Ok(fields) -> dict.from_list(fields)
    Error(Nil) -> dict.new()
  }
  case method {
    "voice.confirm_connection" -> voice_confirm(params)
    "voice.disconnect_user_if_in_channel" -> voice_disconnect(params)
    "voice.get_voice_states_for_channel" -> voice_states(params)
    "call.get" -> call_get(params)
    "call.create" -> call_create(params)
    "call.update_region" -> call_update_region(params)
    "call.ring" -> call_ring(params)
    "call.stop_ringing" -> call_stop_ringing(params)
    "call.confirm_connection" -> call_confirm(params)
    "call.disconnect_user_if_in_channel" -> call_disconnect(params)
    _ -> Error(error.UnknownMethod(method))
  }
}

fn field(params: Params, key: String) -> Result(RawJson, RpcError) {
  dict.get(params, key) |> result.replace_error(error.RpcInternalError)
}

fn optional_field(params: Params, key: String) -> Option(RawJson) {
  dict.get(params, key) |> option.from_result
}

fn optional_text(params: Params, key: String) -> Option(String) {
  optional_field(params, key)
  |> option.then(fn(value) { raw_json.string(value) |> option.from_result })
}

fn optional_connection(params: Params) -> Option(ConnectionId) {
  optional_text(params, "connection_id")
  |> option.then(fn(text) {
    voice_key.connection_id(text) |> option.from_result
  })
}

fn read(value: RawJson, decoder: Decoder(a)) -> Result(a, RpcError) {
  raw_json.read(value, decoder) |> result.replace_error(error.InvalidParams)
}

fn typed(
  value: RawJson,
  make: fn(Int) -> Result(a, id.IdError),
) -> Result(a, RpcError) {
  use raw <- result.try(read(value, edge.snowflake()))
  make(raw) |> result.replace_error(error.InvalidParams)
}

fn channel_of(value: RawJson) -> Result(ChannelId, RpcError) {
  typed(value, id.channel_id)
}

fn user_of(value: RawJson) -> Result(UserId, RpcError) {
  typed(value, id.user_id)
}

fn channel_param(params: Params) -> Result(ChannelId, RpcError) {
  field(params, "channel_id") |> result.try(channel_of)
}

fn user_param(params: Params, key: String) -> Result(UserId, RpcError) {
  field(params, key) |> result.try(user_of)
}

fn empty_states() -> Json {
  json.object([#("voice_states", json.preprocessed_array([]))])
}

fn optional_guild(params: Params) -> Result(Option(GuildId), RpcError) {
  case optional_field(params, "guild_id") {
    None -> Ok(None)
    Some(value) -> {
      let empty =
        raw_json.read(
          value,
          decode.one_of(
            decode.map(decode.optional(decode.int), fn(number) {
              number == None || number == Some(0)
            }),
            [decode.map(decode.string, fn(text) { text == "0" })],
          ),
        )
        |> result.unwrap(False)
      case empty {
        True -> Ok(None)
        False -> typed(value, id.guild_id) |> result.map(Some)
      }
    }
  }
}

fn voice_confirm(params: Params) -> Result(Json, RpcError) {
  use channel_raw <- result.try(field(params, "channel_id"))
  use _ <- result.try(field(params, "connection_id"))
  use guild <- result.try(optional_guild(params))
  let conn = optional_connection(params)
  case guild {
    None -> {
      use channel <- result.try(channel_of(channel_raw))
      confirm_call(channel, conn)
    }
    Some(guild) ->
      guild_rpc.voice_call(
        guild,
        guild_state.Confirm(conn, optional_text(params, "token_nonce")),
      )
  }
}

fn voice_disconnect(params: Params) -> Result(Json, RpcError) {
  use channel_raw <- result.try(field(params, "channel_id"))
  use user_raw <- result.try(field(params, "user_id"))
  use guild <- result.try(optional_guild(params))
  use channel <- result.try(channel_of(channel_raw))
  use user <- result.try(user_of(user_raw))
  let conn = optional_connection(params)
  case guild {
    None -> disconnect_call(channel, user, conn)
    Some(guild) ->
      guild_rpc.voice_call(
        guild,
        guild_state.DisconnectIfInChannel(user, channel, conn),
      )
  }
}

fn voice_states(params: Params) -> Result(Json, RpcError) {
  let channel =
    optional_field(params, "channel_id")
    |> option.to_result(Nil)
    |> result.try(fn(value) { channel_of(value) |> result.replace_error(Nil) })
  case channel, optional_guild(params) {
    Ok(channel), Ok(None) ->
      case lookup_view(channel) {
        Ok(Some(view)) -> Ok(call_voice_states(view, channel))
        Ok(None) -> Ok(empty_states())
        Error(reason) -> Error(reason)
      }
    Ok(channel), Ok(Some(guild)) ->
      guild_rpc.voice_call(guild, guild_state.ChannelStates(channel))
    _, _ -> Ok(empty_states())
  }
}

pub fn call_voice_states(view: call_state.View, channel: ChannelId) -> Json {
  json.object([
    #(
      "voice_states",
      json.array(view.connections, call_voice_state(_, channel)),
    ),
  ])
}

fn call_voice_state(value: entry.Entry, channel: ChannelId) -> Json {
  json.object(
    list.flatten([
      [
        #(
          "connection_id",
          voice_key.connection_json(entry.connection_id(value)),
        ),
        #("user_id", id.user_json(entry.user(value))),
        #(
          "channel_id",
          id.channel_json(option.unwrap(entry.channel(value), channel)),
        ),
      ],
      optional_pair("region_id", entry.region_id(value)),
      optional_pair("server_id", entry.server_id(value)),
    ]),
  )
}

fn optional_pair(key: String, value: Option(String)) -> List(#(String, Json)) {
  case value {
    Some(text) -> [#(key, json.string(text))]
    None -> []
  }
}

fn lookup_view(
  channel: ChannelId,
) -> Result(Option(call_state.View), RpcError) {
  case manager.lookup(channel) {
    Ok(call) ->
      case call_server.fetch_state(call, call_timeout_ms()) {
        Ok(view) -> Ok(Some(view))
        Error(call_server.CallGone) -> Ok(None)
        Error(call_server.CallFailed) ->
          Error(error.RpcFailed("call_state_error"))
      }
    Error(manager.NotFound) -> Ok(None)
    Error(manager.Unavailable) -> Error(error.RpcFailed("call_lookup_error"))
  }
}

fn call_get(params: Params) -> Result(Json, RpcError) {
  use channel <- result.try(channel_param(params))
  use view <- result.try(lookup_view(channel))
  view
  |> option.map(call_state.view_json)
  |> option.unwrap(json.null())
  |> Ok
}

fn snowflake_list(
  params: Params,
  key: String,
) -> Result(List(UserId), RpcError) {
  use value <- result.try(field(params, key))
  use raws <- result.try(read(value, edge.snowflake_list()))
  list.try_map(raws, fn(raw) {
    id.user_id(raw) |> result.replace_error(error.InvalidParams)
  })
}

fn caller(params: Params) -> Option(call_state.Caller) {
  let caller_id =
    optional_field(params, "caller_id")
    |> option.then(fn(value) {
      raw_json.read(value, edge.snowflake()) |> option.from_result
    })
    |> option.then(fn(raw) { id.user_id(raw) |> option.from_result })
  let non_empty = fn(key) {
    case optional_text(params, key) {
      Some("") -> None
      other -> other
    }
  }
  case caller_id, non_empty("caller_name") {
    Some(caller_id), Some(name) ->
      Some(call_state.Caller(
        id: caller_id,
        name:,
        avatar: non_empty("caller_avatar"),
      ))
    _, _ -> None
  }
}

fn call_create(params: Params) -> Result(Json, RpcError) {
  use channel_raw <- result.try(field(params, "channel_id"))
  use message_raw <- result.try(field(params, "message_id"))
  use region_raw <- result.try(field(params, "region"))
  use _ <- result.try(field(params, "ringing"))
  use _ <- result.try(field(params, "recipients"))
  use channel <- result.try(channel_of(channel_raw))
  use message <- result.try(typed(message_raw, id.message_id))
  use ringing <- result.try(snowflake_list(params, "ringing"))
  use recipients <- result.try(snowflake_list(params, "recipients"))
  let init =
    call_state.Init(
      channel:,
      message:,
      region: raw_json.string(region_raw) |> option.from_result,
      ringing:,
      recipients:,
      caller: caller(params),
    )
  case manager.lookup(channel) {
    Ok(call) -> state_of(call, "call_state_error")
    Error(manager.Unavailable) -> Error(error.RpcFailed("call_lookup_error"))
    Error(manager.NotFound) ->
      case manager.create(channel, init) {
        Ok(call) -> state_of(call, "call_state_error")
        Error(manager.AlreadyExists) ->
          case manager.lookup(channel) {
            Ok(call) -> state_of(call, "call_state_error")
            Error(_) -> Error(error.RpcFailed("create_call_error"))
          }
        Error(_) -> Error(error.RpcFailed("create_call_error"))
      }
  }
}

fn state_of(call: manager.CallRef, failure: String) -> Result(Json, RpcError) {
  call_server.get_state(call, call_timeout_ms())
  |> result.map(call_state.view_json)
  |> result.replace_error(error.RpcFailed(failure))
}

fn with_call(
  params: Params,
  run: fn(manager.CallRef) -> Result(Json, RpcError),
) -> Result(Json, RpcError) {
  use channel <- result.try(channel_param(params))
  case manager.lookup(channel) {
    Ok(call) -> run(call)
    Error(manager.NotFound) -> Error(error.RpcFailed("call_not_found"))
    Error(manager.Unavailable) -> Error(error.RpcFailed("call_lookup_error"))
  }
}

fn call_update_region(params: Params) -> Result(Json, RpcError) {
  use region_raw <- result.try(field(params, "region"))
  use call <- with_call(params)
  case raw_json.string(region_raw) {
    Error(_) -> Ok(json.bool(True))
    Ok(region) ->
      case call_server.update_region(call, Some(region), call_timeout_ms()) {
        Ok(Nil) -> Ok(json.bool(True))
        Error(Nil) -> Error(error.RpcFailed("update_region_error"))
      }
  }
}

fn call_ring(params: Params) -> Result(Json, RpcError) {
  use _ <- result.try(field(params, "recipients"))
  use _ <- result.try(channel_param(params))
  use recipients <- result.try(snowflake_list(params, "recipients"))
  use call <- with_call(params)
  case call_server.ring(call, recipients, caller(params), call_timeout_ms()) {
    Ok(Nil) -> Ok(json.bool(True))
    Error(Nil) -> Error(error.RpcFailed("ring_recipients_error"))
  }
}

fn call_stop_ringing(params: Params) -> Result(Json, RpcError) {
  use _ <- result.try(field(params, "recipients"))
  use _ <- result.try(channel_param(params))
  use recipients <- result.try(snowflake_list(params, "recipients"))
  use call <- with_call(params)
  case call_server.stop_ringing(call, recipients, call_timeout_ms()) {
    Ok(Nil) -> Ok(json.bool(True))
    Error(Nil) -> Error(error.RpcFailed("stop_ringing_error"))
  }
}

fn call_not_found() -> Json {
  json.object([
    #("success", json.bool(True)),
    #("call_not_found", json.bool(True)),
  ])
}

fn call_confirm(params: Params) -> Result(Json, RpcError) {
  use _ <- result.try(field(params, "connection_id"))
  use channel <- result.try(channel_param(params))
  confirm_call(channel, optional_connection(params))
}

fn confirm_call(
  channel: ChannelId,
  conn: Option(ConnectionId),
) -> Result(Json, RpcError) {
  case manager.lookup(channel) {
    Error(manager.NotFound) -> Ok(call_not_found())
    Error(manager.Unavailable) -> Error(error.RpcFailed("call_lookup_error"))
    Ok(call) ->
      case conn {
        None -> Ok(json.string("ok"))
        Some(conn) ->
          case call_server.confirm(call, conn, call_timeout_ms()) {
            Ok(reply) -> Ok(reply)
            Error(call_server.CallGone) -> Ok(call_not_found())
            Error(call_server.CallFailed) ->
              Error(error.RpcFailed("confirm_connection_error"))
          }
      }
  }
}

fn call_disconnect(params: Params) -> Result(Json, RpcError) {
  use channel <- result.try(channel_param(params))
  use user <- result.try(user_param(params, "user_id"))
  disconnect_call(channel, user, optional_connection(params))
}

fn disconnect_call(
  channel: ChannelId,
  user: UserId,
  conn: Option(ConnectionId),
) -> Result(Json, RpcError) {
  case manager.lookup(channel) {
    Error(manager.NotFound) -> Ok(call_not_found())
    Error(manager.Unavailable) -> Error(error.RpcFailed("call_lookup_error"))
    Ok(call) ->
      case
        call_server.disconnect_user(
          call,
          user,
          channel,
          conn,
          call_timeout_ms(),
        )
      {
        Ok(reply) -> Ok(reply)
        Error(call_server.CallGone) -> Ok(call_not_found())
        Error(call_server.CallFailed) ->
          Error(error.RpcFailed("disconnect_user_error"))
      }
  }
}
