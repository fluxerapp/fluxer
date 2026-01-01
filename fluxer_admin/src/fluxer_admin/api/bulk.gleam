//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/api/common.{
  type ApiError, Forbidden, NetworkError, NotFound, ServerError, Unauthorized,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type BulkOperationError {
  BulkOperationError(id: String, error: String)
}

pub type BulkOperationResponse {
  BulkOperationResponse(
    successful: List(String),
    failed: List(BulkOperationError),
  )
}

pub fn bulk_update_user_flags(
  ctx: web.Context,
  session: web.Session,
  user_ids: List(String),
  add_flags: List(String),
  remove_flags: List(String),
  audit_log_reason: option.Option(String),
) -> Result(BulkOperationResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/bulk-update-flags"
  let body =
    json.object([
      #("user_ids", json.array(user_ids, json.string)),
      #("add_flags", json.array(add_flags, json.string)),
      #("remove_flags", json.array(remove_flags, json.string)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  let req = case audit_log_reason {
    option.Some(reason) -> request.set_header(req, "x-audit-log-reason", reason)
    option.None -> req
  }

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let error_decoder = {
        use id <- decode.field("id", decode.string)
        use error <- decode.field("error", decode.string)
        decode.success(BulkOperationError(id: id, error: error))
      }

      let decoder = {
        use successful <- decode.field("successful", decode.list(decode.string))
        use failed <- decode.field("failed", decode.list(error_decoder))
        decode.success(BulkOperationResponse(
          successful: successful,
          failed: failed,
        ))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> {
      let message_decoder = {
        use message <- decode.field("message", decode.string)
        decode.success(message)
      }

      let message = case json.parse(resp.body, message_decoder) {
        Ok(msg) -> msg
        Error(_) ->
          "Missing required permissions. Contact an administrator to request access."
      }

      Error(Forbidden(message))
    }
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn bulk_update_guild_features(
  ctx: web.Context,
  session: web.Session,
  guild_ids: List(String),
  add_features: List(String),
  remove_features: List(String),
  audit_log_reason: option.Option(String),
) -> Result(BulkOperationResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/guilds/bulk-update-features"
  let body =
    json.object([
      #("guild_ids", json.array(guild_ids, json.string)),
      #("add_features", json.array(add_features, json.string)),
      #("remove_features", json.array(remove_features, json.string)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  let req = case audit_log_reason {
    option.Some(reason) -> request.set_header(req, "x-audit-log-reason", reason)
    option.None -> req
  }

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let error_decoder = {
        use id <- decode.field("id", decode.string)
        use error <- decode.field("error", decode.string)
        decode.success(BulkOperationError(id: id, error: error))
      }

      let decoder = {
        use successful <- decode.field("successful", decode.list(decode.string))
        use failed <- decode.field("failed", decode.list(error_decoder))
        decode.success(BulkOperationResponse(
          successful: successful,
          failed: failed,
        ))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> {
      let message_decoder = {
        use message <- decode.field("message", decode.string)
        decode.success(message)
      }

      let message = case json.parse(resp.body, message_decoder) {
        Ok(msg) -> msg
        Error(_) ->
          "Missing required permissions. Contact an administrator to request access."
      }

      Error(Forbidden(message))
    }
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn bulk_add_guild_members(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  user_ids: List(String),
  audit_log_reason: option.Option(String),
) -> Result(BulkOperationResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/bulk/add-guild-members"
  let body =
    json.object([
      #("guild_id", json.string(guild_id)),
      #("user_ids", json.array(user_ids, json.string)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  let req = case audit_log_reason {
    option.Some(reason) -> request.set_header(req, "x-audit-log-reason", reason)
    option.None -> req
  }

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let error_decoder = {
        use id <- decode.field("id", decode.string)
        use error <- decode.field("error", decode.string)
        decode.success(BulkOperationError(id: id, error: error))
      }

      let decoder = {
        use successful <- decode.field("successful", decode.list(decode.string))
        use failed <- decode.field("failed", decode.list(error_decoder))
        decode.success(BulkOperationResponse(
          successful: successful,
          failed: failed,
        ))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> {
      let message_decoder = {
        use message <- decode.field("message", decode.string)
        decode.success(message)
      }

      let message = case json.parse(resp.body, message_decoder) {
        Ok(msg) -> msg
        Error(_) ->
          "Missing required permissions. Contact an administrator to request access."
      }

      Error(Forbidden(message))
    }
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn bulk_schedule_user_deletion(
  ctx: web.Context,
  session: web.Session,
  user_ids: List(String),
  reason_code: Int,
  public_reason: option.Option(String),
  days_until_deletion: Int,
  audit_log_reason: option.Option(String),
) -> Result(BulkOperationResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/bulk/schedule-user-deletion"
  let fields = [
    #("user_ids", json.array(user_ids, json.string)),
    #("reason_code", json.int(reason_code)),
    #("days_until_deletion", json.int(days_until_deletion)),
  ]
  let fields = case public_reason {
    option.Some(r) -> [#("public_reason", json.string(r)), ..fields]
    option.None -> fields
  }
  let body = json.object(fields) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  let req = case audit_log_reason {
    option.Some(reason) -> request.set_header(req, "x-audit-log-reason", reason)
    option.None -> req
  }

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let error_decoder = {
        use id <- decode.field("id", decode.string)
        use error <- decode.field("error", decode.string)
        decode.success(BulkOperationError(id: id, error: error))
      }

      let decoder = {
        use successful <- decode.field("successful", decode.list(decode.string))
        use failed <- decode.field("failed", decode.list(error_decoder))
        decode.success(BulkOperationResponse(
          successful: successful,
          failed: failed,
        ))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> {
      let message_decoder = {
        use message <- decode.field("message", decode.string)
        decode.success(message)
      }

      let message = case json.parse(resp.body, message_decoder) {
        Ok(msg) -> msg
        Error(_) ->
          "Missing required permissions. Contact an administrator to request access."
      }

      Error(Forbidden(message))
    }
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}
