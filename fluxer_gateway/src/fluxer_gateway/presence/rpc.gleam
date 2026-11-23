// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{
  type RpcError, InvalidParams, RpcFailed, RpcInternalError, UnknownMethod,
}
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/presence/custom_status
import fluxer_gateway/presence/dispatch_id.{type DispatchId}
import fluxer_gateway/presence/manager
import fluxer_gateway/presence/pids.{type Inbox}
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol.{type Input, type UserEvent}
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/push_hook
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/status
import fluxer_gateway/util/decode as id_decode
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Monitor}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

fn lookup_timeout_ms() -> Int {
  durations.presence().rpc_lookup_timeout_ms
}

const max_moved_hops = 3

const suppress_notifications_flag = 4096

pub fn handle_raw(
  method: String,
  payload: RawJson,
) -> Result(Result(Json, RpcError), Nil) {
  case method {
    "presence.dispatch" -> raw_json.fields(payload) |> result.map(dispatch)
    _ -> Error(Nil)
  }
}

pub fn handle(method: String, params: Dynamic) -> Result(Json, RpcError) {
  case method {
    "presence.join_guild" ->
      guild_forward(params, fn(guild) {
        protocol.Guild(protocol.JoinGuild(guild))
      })
    "presence.leave_guild" ->
      guild_forward(params, fn(guild) {
        protocol.Guild(protocol.LeaveGuild(guild))
      })
    "presence.add_temporary_guild" ->
      guild_forward(params, fn(guild) {
        protocol.Guild(protocol.AddTemporaryGuild(guild))
      })
    "presence.remove_temporary_guild" ->
      guild_forward(params, fn(guild) {
        protocol.Guild(protocol.RemoveTemporaryGuild(guild))
      })
    "presence.start_visit" -> start_visit(params)
    "presence.end_visit" ->
      guild_forward(params, fn(guild) { protocol.VisitEnd(guild) })
    "presence.terminate_sessions" -> terminate_sessions(params)
    "presence.terminate_all_sessions" -> terminate_all(params)
    "presence.has_active" -> has_active(params)
    "presence.sync_group_dm_recipients" -> sync_group_dm_recipients(params)
    _ -> Error(UnknownMethod(method))
  }
}

fn field(params: Dynamic, name: String) -> Result(Dynamic, RpcError) {
  decode.run(params, decode.at([name], decode.dynamic))
  |> result.replace_error(RpcInternalError)
}

fn checked(value: Dynamic, decoder: decode.Decoder(a)) -> Result(a, RpcError) {
  decode.run(value, decoder) |> result.replace_error(InvalidParams)
}

fn user_param(params: Dynamic) -> Result(UserId, RpcError) {
  use raw <- result.try(field(params, "user_id"))
  checked(raw, id_decode.user_id())
}

fn guild_param(params: Dynamic) -> Result(Int, RpcError) {
  use raw <- result.try(field(params, "guild_id"))
  checked(raw, id_decode.guild_id()) |> result.map(id.guild_id_to_int)
}

fn lookup_error() -> RpcError {
  RpcFailed("presence_lookup_error")
}

fn forward(user: UserId, input: Input(Monitor)) -> Result(Json, RpcError) {
  case manager.lookup_all(id.user_id_to_int(user), lookup_timeout_ms()) {
    Ok(inboxes) -> {
      list.each(inboxes, process.send(_, input))
      Ok(json.bool(True))
    }
    Error(manager.NotFound) -> Ok(json.bool(True))
    Error(manager.LookupFailed(_)) | Error(manager.OwnerUnavailable) ->
      Error(lookup_error())
  }
}

fn guild_forward(
  params: Dynamic,
  build: fn(Int) -> Input(Monitor),
) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  use guild <- result.try(guild_param(params))
  forward(user, build(guild))
}

fn start_visit(params: Dynamic) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  use guild <- result.try(guild_param(params))
  let operator =
    decode.run(params, decode.at(["operator"], decode.bool))
    |> result.unwrap(False)
  forward(user, protocol.VisitStart(guild, operator))
}

fn terminate_all(params: Dynamic) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  forward(user, protocol.TerminateAll)
}

fn terminate_sessions(params: Dynamic) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  use raw <- result.try(field(params, "session_id_hashes"))
  use items <- result.try(
    decode.run(raw, decode.list(decode.dynamic))
    |> result.replace_error(RpcInternalError),
  )
  let hashes =
    list.filter_map(items, fn(item) {
      decode.run(item, decode.string) |> result.replace_error(Nil)
    })
  let failed = RpcFailed("presence_terminate_sessions_error")
  case manager.lookup_all(id.user_id_to_int(user), lookup_timeout_ms()) {
    Error(manager.NotFound) -> Ok(json.bool(True))
    Error(manager.LookupFailed(_)) | Error(manager.OwnerUnavailable) ->
      Error(failed)
    Ok(inboxes) -> {
      let outcomes = list.map(inboxes, terminate_at(_, hashes, max_moved_hops))
      case
        list.all(outcomes, fn(outcome) { outcome == Ok(protocol.TerminateOk) })
      {
        True -> Ok(json.bool(True))
        False -> Error(failed)
      }
    }
  }
}

fn terminate_at(
  inbox: Inbox,
  hashes: List(String),
  hops_left: Int,
) -> Result(protocol.TerminateOutcome, remote.CallError) {
  let outcome =
    remote.call_subject(inbox, lookup_timeout_ms(), fn(reply) {
      protocol.TerminateSessions(hashes, reply)
    })
  case outcome {
    Ok(protocol.TerminateMoved(to)) if hops_left > 0 ->
      terminate_at(to, hashes, hops_left - 1)
    other -> other
  }
}

fn has_active(params: Dynamic) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  case manager.lookup_all(id.user_id_to_int(user), lookup_timeout_ms()) {
    Ok(_) -> Ok(json.object([#("has_active", json.bool(True))]))
    Error(manager.NotFound) ->
      Ok(json.object([#("has_active", json.bool(False))]))
    Error(manager.LookupFailed(_)) | Error(manager.OwnerUnavailable) ->
      Error(lookup_error())
  }
}

fn sync_group_dm_recipients(params: Dynamic) -> Result(Json, RpcError) {
  use user <- result.try(user_param(params))
  use raw <- result.try(field(params, "recipients_by_channel"))
  use channels <- result.try(
    decode.run(raw, decode.dict(decode.dynamic, decode.list(decode.dynamic)))
    |> result.replace_error(RpcInternalError),
  )
  use _ <- result.try(
    dict.to_list(channels)
    |> list.try_map(fn(entry) {
      use _ <- result.try(checked(entry.0, id_decode.channel_id()))
      list.try_map(entry.1, checked(_, id_decode.user_id()))
    }),
  )
  forward(user, protocol.Rejoin)
}

fn raw_field(
  params: List(#(String, RawJson)),
  name: String,
) -> Result(RawJson, RpcError) {
  list.key_find(params, name) |> result.replace_error(RpcInternalError)
}

fn dispatch(params: List(#(String, RawJson))) -> Result(Json, RpcError) {
  use user <- result.try(
    raw_field(params, "user_id")
    |> result.try(fn(raw) {
      raw_json.read(raw, id_decode.user_id())
      |> result.replace_error(InvalidParams)
    }),
  )
  use raw_event <- result.try(raw_field(params, "event"))
  use data <- result.try(raw_field(params, "data"))
  let failed = RpcFailed("presence_dispatch_error")
  use name <- result.try(
    raw_json.read(raw_event, decode.string)
    |> result.try(event_name)
    |> result.replace_error(failed),
  )
  case name {
    "RELATIONSHIP_ADD" | "RELATIONSHIP_REMOVE" ->
      push_hook.invalidate_blocked_ids(user)
    _ -> Nil
  }
  let fields = json_tree.fields(json_tree.from_raw(data))
  case manager.lookup_all(id.user_id_to_int(user), lookup_timeout_ms()) {
    Ok(inboxes) -> {
      let parsed = user_event(name, fields, data)
      let dispatch = dispatch_id.mint()
      list.each(inboxes, send_dispatch(_, dispatch, name, data, parsed))
      Ok(json.bool(True))
    }
    Error(manager.NotFound) -> {
      offline_dispatch(user, name, fields, data)
      Ok(json.bool(True))
    }
    Error(manager.OwnerUnavailable) -> {
      offline_dispatch(user, name, fields, data)
      Error(failed)
    }
    Error(manager.LookupFailed(_)) -> Error(failed)
  }
}

fn send_dispatch(
  inbox: Inbox,
  id: DispatchId,
  name: String,
  raw: RawJson,
  parsed: UserEvent,
) -> Nil {
  process.send(
    inbox,
    protocol.UserDispatch(id:, event: name, data: raw, parsed:),
  )
}

fn event_name(text: String) -> Result(String, Nil) {
  let upper = string.uppercase(text)
  case upper != "" && name_bytes(<<upper:utf8>>) {
    True -> Ok(upper)
    False -> Error(Nil)
  }
}

fn name_bytes(bytes: BitArray) -> Bool {
  case bytes {
    <<>> -> True
    <<byte, rest:bits>>
      if byte == 95
      || { byte >= 65 && byte <= 90 }
      || { byte >= 48 && byte <= 57 }
    -> name_bytes(rest)
    _ -> False
  }
}

type Fields =
  Result(List(#(String, JsonTree)), Nil)

fn data_field(fields: Fields, name: String) -> Result(JsonTree, Nil) {
  result.try(fields, list.key_find(_, name))
}

fn offline_dispatch(
  user: UserId,
  name: String,
  fields: Fields,
  raw: RawJson,
) -> Nil {
  case name {
    "MESSAGE_CREATE" ->
      case author_field(fields) |> result.try(json_tree.expand) {
        Ok(json_tree.Int(0)) | Ok(json_tree.String("0")) -> Nil
        Ok(author) ->
          case json_tree.snowflake(author) {
            Ok(author_id) -> push_hook.direct_message(raw, user, author_id)
            Error(Nil) -> Nil
          }
        Error(Nil) -> Nil
      }
    _ -> Nil
  }
}

fn author_field(fields: Fields) -> Result(JsonTree, Nil) {
  data_field(fields, "author")
  |> result.try(json_tree.field(_, "id"))
}

fn snowflake_field(fields: Fields, name: String) -> option.Option(Int) {
  data_field(fields, name)
  |> result.try(json_tree.snowflake)
  |> option.from_result
}

fn user_event(name: String, fields: Fields, data: RawJson) -> UserEvent {
  case name, fields {
    "USER_UPDATE", Ok(found) -> protocol.UserUpdate(profile.from_fields(found))
    "USER_SETTINGS_UPDATE", Ok(_) ->
      protocol.UserSettingsUpdate(
        custom_status: custom_status.settings_value(data_field(
          fields,
          "custom_status",
        )),
        invisible: case
          data_field(fields, "status") |> result.try(json_tree.string)
        {
          Ok(text) -> status.settings_invisible(Some(text))
          Error(Nil) -> None
        },
      )
    "MESSAGE_CREATE", Ok(_) -> protocol.MessageCreate(dm_message(fields, data))
    "MESSAGE_ACK", Ok(_) ->
      protocol.MessageAck(
        channel: snowflake_field(fields, "channel_id"),
        message: snowflake_field(fields, "message_id"),
      )
    _, _ -> protocol.OtherEvent
  }
}

fn dm_message(
  fields: Fields,
  data: RawJson,
) -> option.Option(push_buffer.DmMessage) {
  case author_field(fields) |> result.try(json_tree.snowflake) {
    Error(Nil) -> None
    Ok(author) ->
      Some(push_buffer.DmMessage(
        author:,
        channel: snowflake_field(fields, "channel_id"),
        message: snowflake_field(fields, "id"),
        suppressed: case
          data_field(fields, "flags") |> result.try(json_tree.int)
        {
          Ok(flags) -> int.bitwise_and(flags, suppress_notifications_flag) != 0
          Error(Nil) -> False
        },
        data:,
      ))
  }
}
