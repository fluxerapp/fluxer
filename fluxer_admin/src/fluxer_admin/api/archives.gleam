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
  admin_post_with_audit,
}
import fluxer_admin/web.{type Context, type Session}
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option}

pub type Archive {
  Archive(
    archive_id: String,
    subject_type: String,
    subject_id: String,
    requested_by: String,
    requested_at: String,
    started_at: Option(String),
    completed_at: Option(String),
    failed_at: Option(String),
    file_size: Option(String),
    progress_percent: Int,
    progress_step: Option(String),
    error_message: Option(String),
    download_url_expires_at: Option(String),
    expires_at: Option(String),
  )
}

pub type ListArchivesResponse {
  ListArchivesResponse(archives: List(Archive))
}

pub fn trigger_user_archive(
  ctx: Context,
  session: Session,
  user_id: String,
  audit_log_reason: Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/archives/user",
    [#("user_id", json.string(user_id))],
    audit_log_reason,
  )
}

pub fn trigger_guild_archive(
  ctx: Context,
  session: Session,
  guild_id: String,
  audit_log_reason: Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/archives/guild",
    [#("guild_id", json.string(guild_id))],
    audit_log_reason,
  )
}

fn archive_decoder() {
  use archive_id <- decode.field("archive_id", decode.string)
  use subject_type <- decode.field("subject_type", decode.string)
  use subject_id <- decode.field("subject_id", decode.string)
  use requested_by <- decode.field("requested_by", decode.string)
  use requested_at <- decode.field("requested_at", decode.string)
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
  use failed_at <- decode.optional_field(
    "failed_at",
    option.None,
    decode.optional(decode.string),
  )
  use file_size <- decode.optional_field(
    "file_size",
    option.None,
    decode.optional(decode.string),
  )
  use progress_percent <- decode.field("progress_percent", decode.int)
  use progress_step <- decode.optional_field(
    "progress_step",
    option.None,
    decode.optional(decode.string),
  )
  use error_message <- decode.optional_field(
    "error_message",
    option.None,
    decode.optional(decode.string),
  )
  use download_url_expires_at <- decode.optional_field(
    "download_url_expires_at",
    option.None,
    decode.optional(decode.string),
  )
  use expires_at <- decode.optional_field(
    "expires_at",
    option.None,
    decode.optional(decode.string),
  )
  decode.success(Archive(
    archive_id: archive_id,
    subject_type: subject_type,
    subject_id: subject_id,
    requested_by: requested_by,
    requested_at: requested_at,
    started_at: started_at,
    completed_at: completed_at,
    failed_at: failed_at,
    file_size: file_size,
    progress_percent: progress_percent,
    progress_step: progress_step,
    error_message: error_message,
    download_url_expires_at: download_url_expires_at,
    expires_at: expires_at,
  ))
}

pub fn list_archives(
  ctx: Context,
  session: Session,
  subject_type: String,
  subject_id: Option(String),
  include_expired: Bool,
) -> Result(ListArchivesResponse, ApiError) {
  let fields = [
    #("subject_type", json.string(subject_type)),
    #("include_expired", json.bool(include_expired)),
  ]
  let fields = case subject_id {
    option.Some(id) -> fields |> list.append([#("subject_id", json.string(id))])
    option.None -> fields
  }

  let url = ctx.api_endpoint <> "/admin/archives/list"
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
        use archives <- decode.field("archives", decode.list(archive_decoder()))
        decode.success(ListArchivesResponse(archives: archives))
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

pub fn get_archive_download_url(
  ctx: Context,
  session: Session,
  subject_type: String,
  subject_id: String,
  archive_id: String,
) -> Result(#(String, String), ApiError) {
  let url =
    ctx.api_endpoint
    <> "/admin/archives/"
    <> subject_type
    <> "/"
    <> subject_id
    <> "/"
    <> archive_id
    <> "/download"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let decoder = {
        use download_url <- decode.field("downloadUrl", decode.string)
        use expires_at <- decode.field("expiresAt", decode.string)
        decode.success(#(download_url, expires_at))
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
