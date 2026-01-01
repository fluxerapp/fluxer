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
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type MessageAttachment {
  MessageAttachment(filename: String, url: String)
}

pub type Message {
  Message(
    id: String,
    channel_id: String,
    author_id: String,
    author_username: String,
    content: String,
    timestamp: String,
    attachments: List(MessageAttachment),
  )
}

pub type LookupMessageResponse {
  LookupMessageResponse(messages: List(Message), message_id: String)
}

pub type MessageShredResponse {
  MessageShredResponse(job_id: String, requested: option.Option(Int))
}

pub type DeleteAllUserMessagesResponse {
  DeleteAllUserMessagesResponse(
    dry_run: Bool,
    channel_count: Int,
    message_count: Int,
    job_id: option.Option(String),
  )
}

pub type MessageShredStatus {
  MessageShredStatus(
    status: String,
    requested: option.Option(Int),
    total: option.Option(Int),
    processed: option.Option(Int),
    skipped: option.Option(Int),
    started_at: option.Option(String),
    completed_at: option.Option(String),
    failed_at: option.Option(String),
    error: option.Option(String),
  )
}

pub fn delete_message(
  ctx: web.Context,
  session: web.Session,
  channel_id: String,
  message_id: String,
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let fields = [
    #("channel_id", json.string(channel_id)),
    #("message_id", json.string(message_id)),
  ]
  admin_post_with_audit(
    ctx,
    session,
    "/admin/messages/delete",
    fields,
    audit_log_reason,
  )
}

pub fn lookup_message(
  ctx: web.Context,
  session: web.Session,
  channel_id: String,
  message_id: String,
  context_limit: Int,
) -> Result(LookupMessageResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/messages/lookup"
  let body =
    json.object([
      #("channel_id", json.string(channel_id)),
      #("message_id", json.string(message_id)),
      #("context_limit", json.int(context_limit)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let attachment_decoder = {
        use filename <- decode.field("filename", decode.string)
        use url <- decode.field("url", decode.string)
        decode.success(MessageAttachment(filename: filename, url: url))
      }

      let message_decoder = {
        use id <- decode.field("id", decode.string)
        use channel_id <- decode.field("channel_id", decode.string)
        use author_id <- decode.field("author_id", decode.string)
        use author_username <- decode.field("author_username", decode.string)
        use content <- decode.field("content", decode.string)
        use timestamp <- decode.field("timestamp", decode.string)
        use attachments <- decode.optional_field(
          "attachments",
          [],
          decode.list(attachment_decoder),
        )
        decode.success(Message(
          id: id,
          channel_id: channel_id,
          author_id: author_id,
          author_username: author_username,
          content: content,
          timestamp: timestamp,
          attachments: attachments,
        ))
      }

      let decoder = {
        use messages <- decode.field("messages", decode.list(message_decoder))
        use message_id <- decode.field("message_id", decode.string)
        decode.success(LookupMessageResponse(
          messages: messages,
          message_id: message_id,
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

pub fn queue_message_shred(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  entries: json.Json,
) -> Result(MessageShredResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/messages/shred"
  let body =
    json.object([
      #("user_id", json.string(user_id)),
      #("entries", entries),
    ])
    |> json.to_string

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
        use job_id <- decode.field("job_id", decode.string)
        use requested <- decode.optional_field(
          "requested",
          option.None,
          decode.optional(decode.int),
        )
        decode.success(MessageShredResponse(
          job_id: job_id,
          requested: requested,
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

pub fn delete_all_user_messages(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  dry_run: Bool,
) -> Result(DeleteAllUserMessagesResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/messages/delete-all"
  let body =
    json.object([
      #("user_id", json.string(user_id)),
      #("dry_run", json.bool(dry_run)),
    ])
    |> json.to_string

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
        use dry_run <- decode.field("dry_run", decode.bool)
        use channel_count <- decode.field("channel_count", decode.int)
        use message_count <- decode.field("message_count", decode.int)
        use job_id <- decode.optional_field(
          "job_id",
          option.None,
          decode.optional(decode.string),
        )
        decode.success(DeleteAllUserMessagesResponse(
          dry_run: dry_run,
          channel_count: channel_count,
          message_count: message_count,
          job_id: job_id,
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

pub fn get_message_shred_status(
  ctx: web.Context,
  session: web.Session,
  job_id: String,
) -> Result(MessageShredStatus, ApiError) {
  let url = ctx.api_endpoint <> "/admin/messages/shred-status"
  let body =
    json.object([#("job_id", json.string(job_id))])
    |> json.to_string

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
        use requested <- decode.optional_field(
          "requested",
          option.None,
          decode.optional(decode.int),
        )
        use total <- decode.optional_field(
          "total",
          option.None,
          decode.optional(decode.int),
        )
        use processed <- decode.optional_field(
          "processed",
          option.None,
          decode.optional(decode.int),
        )
        use skipped <- decode.optional_field(
          "skipped",
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
        use failed_at <- decode.optional_field(
          "failed_at",
          option.None,
          decode.optional(decode.string),
        )
        use error <- decode.optional_field(
          "error",
          option.None,
          decode.optional(decode.string),
        )
        decode.success(MessageShredStatus(
          status: status,
          requested: requested,
          total: total,
          processed: processed,
          skipped: skipped,
          started_at: started_at,
          completed_at: completed_at,
          failed_at: failed_at,
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

pub fn lookup_message_by_attachment(
  ctx: web.Context,
  session: web.Session,
  channel_id: String,
  attachment_id: String,
  filename: String,
  context_limit: Int,
) -> Result(LookupMessageResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/messages/lookup-by-attachment"
  let body =
    json.object([
      #("channel_id", json.string(channel_id)),
      #("attachment_id", json.string(attachment_id)),
      #("filename", json.string(filename)),
      #("context_limit", json.int(context_limit)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let attachment_decoder = {
        use filename <- decode.field("filename", decode.string)
        use url <- decode.field("url", decode.string)
        decode.success(MessageAttachment(filename: filename, url: url))
      }

      let message_decoder = {
        use id <- decode.field("id", decode.string)
        use channel_id <- decode.field("channel_id", decode.string)
        use author_id <- decode.field("author_id", decode.string)
        use author_username <- decode.field("author_username", decode.string)
        use content <- decode.field("content", decode.string)
        use timestamp <- decode.field("timestamp", decode.string)
        use attachments <- decode.optional_field(
          "attachments",
          [],
          decode.list(attachment_decoder),
        )
        decode.success(Message(
          id: id,
          channel_id: channel_id,
          author_id: author_id,
          author_username: author_username,
          content: content,
          timestamp: timestamp,
          attachments: attachments,
        ))
      }

      let decoder = {
        use messages <- decode.field("messages", decode.list(message_decoder))
        use message_id <- decode.field("message_id", decode.string)
        decode.success(LookupMessageResponse(
          messages: messages,
          message_id: message_id,
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
