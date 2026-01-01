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
import fluxer_admin/web.{type Context, type Session}
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type RefreshSearchIndexResponse {
  RefreshSearchIndexResponse(job_id: String)
}

pub type IndexRefreshStatus {
  IndexRefreshStatus(
    status: String,
    total: option.Option(Int),
    indexed: option.Option(Int),
    started_at: option.Option(String),
    completed_at: option.Option(String),
    error: option.Option(String),
  )
}

pub fn refresh_search_index(
  ctx: Context,
  session: Session,
  index_type: String,
  audit_log_reason: option.Option(String),
) -> Result(RefreshSearchIndexResponse, ApiError) {
  refresh_search_index_with_guild(
    ctx,
    session,
    index_type,
    option.None,
    audit_log_reason,
  )
}

pub fn refresh_search_index_with_guild(
  ctx: Context,
  session: Session,
  index_type: String,
  guild_id: option.Option(String),
  audit_log_reason: option.Option(String),
) -> Result(RefreshSearchIndexResponse, ApiError) {
  let fields = case guild_id {
    option.Some(id) -> [
      #("index_type", json.string(index_type)),
      #("guild_id", json.string(id)),
    ]
    option.None -> [#("index_type", json.string(index_type))]
  }
  let url = ctx.api_endpoint <> "/admin/search/refresh-index"
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
      let decoder = {
        use job_id <- decode.field("job_id", decode.string)
        decode.success(RefreshSearchIndexResponse(job_id: job_id))
      }

      case json.parse(resp.body, decoder) {
        Ok(result) -> Ok(result)
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

pub fn get_index_refresh_status(
  ctx: Context,
  session: Session,
  job_id: String,
) -> Result(IndexRefreshStatus, ApiError) {
  let fields = [#("job_id", json.string(job_id))]
  let url = ctx.api_endpoint <> "/admin/search/refresh-status"
  let body = json.object(fields) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let decoder = {
        use status <- decode.field("status", decode.string)
        use total <- decode.optional_field(
          "total",
          option.None,
          decode.optional(decode.int),
        )
        use indexed <- decode.optional_field(
          "indexed",
          option.None,
          decode.optional(decode.int),
        )
        use started_at <- decode.optional_field(
          "started_at",
          option.None,
          decode.optional(decode.string),
        )
        use completed_at <- decode.optional_field(
          "completed_at",
          option.None,
          decode.optional(decode.string),
        )
        use error <- decode.optional_field(
          "error",
          option.None,
          decode.optional(decode.string),
        )
        decode.success(IndexRefreshStatus(
          status: status,
          total: total,
          indexed: indexed,
          started_at: started_at,
          completed_at: completed_at,
          error: error,
        ))
      }

      case json.parse(resp.body, decoder) {
        Ok(result) -> Ok(result)
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
