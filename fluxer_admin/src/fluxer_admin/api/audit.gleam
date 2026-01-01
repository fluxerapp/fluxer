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
import gleam/dict
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type AuditLog {
  AuditLog(
    log_id: String,
    admin_user_id: String,
    target_type: String,
    target_id: String,
    action: String,
    audit_log_reason: option.Option(String),
    metadata: List(#(String, String)),
    created_at: String,
  )
}

pub type ListAuditLogsResponse {
  ListAuditLogsResponse(logs: List(AuditLog), total: Int)
}

pub fn search_audit_logs(
  ctx: web.Context,
  session: web.Session,
  query: option.Option(String),
  admin_user_id_filter: option.Option(String),
  target_type: option.Option(String),
  target_id: option.Option(String),
  action: option.Option(String),
  limit: Int,
  offset: Int,
) -> Result(ListAuditLogsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/audit-logs/search"

  let mut_fields = [#("limit", json.int(limit)), #("offset", json.int(offset))]

  let mut_fields = case query {
    option.Some(q) if q != "" -> [#("query", json.string(q)), ..mut_fields]
    _ -> mut_fields
  }
  let mut_fields = case admin_user_id_filter {
    option.Some(id) if id != "" -> [
      #("admin_user_id", json.string(id)),
      ..mut_fields
    ]
    _ -> mut_fields
  }
  let mut_fields = case target_type {
    option.Some(tt) if tt != "" -> [
      #("target_type", json.string(tt)),
      ..mut_fields
    ]
    _ -> mut_fields
  }
  let mut_fields = case target_id {
    option.Some(tid) if tid != "" -> [
      #("target_id", json.string(tid)),
      ..mut_fields
    ]
    _ -> mut_fields
  }
  let mut_fields = case action {
    option.Some(act) if act != "" -> [
      #("action", json.string(act)),
      ..mut_fields
    ]
    _ -> mut_fields
  }

  let body = json.object(mut_fields) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let audit_log_decoder = {
        use log_id <- decode.field("log_id", decode.string)
        use admin_user_id <- decode.field("admin_user_id", decode.string)
        use target_type_val <- decode.field("target_type", decode.string)
        use target_id_val <- decode.field("target_id", decode.string)
        use action <- decode.field("action", decode.string)
        use audit_log_reason <- decode.field(
          "audit_log_reason",
          decode.optional(decode.string),
        )
        use metadata <- decode.field(
          "metadata",
          decode.dict(decode.string, decode.string),
        )
        use created_at <- decode.field("created_at", decode.string)

        let metadata_list =
          metadata
          |> dict.to_list

        decode.success(AuditLog(
          log_id: log_id,
          admin_user_id: admin_user_id,
          target_type: target_type_val,
          target_id: target_id_val,
          action: action,
          audit_log_reason: audit_log_reason,
          metadata: metadata_list,
          created_at: created_at,
        ))
      }

      let decoder = {
        use logs <- decode.field("logs", decode.list(audit_log_decoder))
        use total <- decode.field("total", decode.int)
        decode.success(ListAuditLogsResponse(logs: logs, total: total))
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
