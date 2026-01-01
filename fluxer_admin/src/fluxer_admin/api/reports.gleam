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
import fluxer_admin/api/messages.{type Message, Message, MessageAttachment}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type Report {
  Report(
    report_id: String,
    reporter_id: option.Option(String),
    reporter_tag: option.Option(String),
    reporter_username: option.Option(String),
    reporter_discriminator: option.Option(String),
    reporter_email: option.Option(String),
    reporter_full_legal_name: option.Option(String),
    reporter_country_of_residence: option.Option(String),
    reported_at: String,
    status: Int,
    report_type: Int,
    category: String,
    additional_info: option.Option(String),
    reported_user_id: option.Option(String),
    reported_user_tag: option.Option(String),
    reported_user_username: option.Option(String),
    reported_user_discriminator: option.Option(String),
    reported_user_avatar_hash: option.Option(String),
    reported_guild_id: option.Option(String),
    reported_guild_name: option.Option(String),
    reported_guild_icon_hash: option.Option(String),
    reported_message_id: option.Option(String),
    reported_channel_id: option.Option(String),
    reported_channel_name: option.Option(String),
    reported_guild_invite_code: option.Option(String),
    resolved_at: option.Option(String),
    resolved_by_admin_id: option.Option(String),
    public_comment: option.Option(String),
    message_context: List(Message),
  )
}

pub type ListReportsResponse {
  ListReportsResponse(reports: List(Report))
}

pub type SearchReportResult {
  SearchReportResult(
    report_id: String,
    reporter_id: option.Option(String),
    reporter_tag: option.Option(String),
    reporter_username: option.Option(String),
    reporter_discriminator: option.Option(String),
    reporter_email: option.Option(String),
    reporter_full_legal_name: option.Option(String),
    reporter_country_of_residence: option.Option(String),
    reported_at: String,
    status: Int,
    report_type: Int,
    category: String,
    additional_info: option.Option(String),
    reported_user_id: option.Option(String),
    reported_user_tag: option.Option(String),
    reported_user_username: option.Option(String),
    reported_user_discriminator: option.Option(String),
    reported_user_avatar_hash: option.Option(String),
    reported_guild_id: option.Option(String),
    reported_guild_name: option.Option(String),
    reported_guild_invite_code: option.Option(String),
  )
}

pub type SearchReportsResponse {
  SearchReportsResponse(
    reports: List(SearchReportResult),
    total: Int,
    offset: Int,
    limit: Int,
  )
}

pub fn list_reports(
  ctx: web.Context,
  session: web.Session,
  status: Int,
  limit: Int,
  offset: option.Option(Int),
) -> Result(ListReportsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/reports/list"

  let mut_fields = [#("status", json.int(status)), #("limit", json.int(limit))]
  let mut_fields = case offset {
    option.Some(o) -> [#("offset", json.int(o)), ..mut_fields]
    option.None -> mut_fields
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
      let report_decoder = {
        use report_id <- decode.field("report_id", decode.string)
        use reporter_id <- decode.field(
          "reporter_id",
          decode.optional(decode.string),
        )
        use reporter_tag <- decode.field(
          "reporter_tag",
          decode.optional(decode.string),
        )
        use reporter_username <- decode.field(
          "reporter_username",
          decode.optional(decode.string),
        )
        use reporter_discriminator <- decode.field(
          "reporter_discriminator",
          decode.optional(decode.string),
        )
        use reporter_email <- decode.field(
          "reporter_email",
          decode.optional(decode.string),
        )
        use reporter_full_legal_name <- decode.field(
          "reporter_full_legal_name",
          decode.optional(decode.string),
        )
        use reporter_country_of_residence <- decode.field(
          "reporter_country_of_residence",
          decode.optional(decode.string),
        )
        use reported_at <- decode.field("reported_at", decode.string)
        use status_val <- decode.field("status", decode.int)
        use report_type <- decode.field("report_type", decode.int)
        use category <- decode.field("category", decode.string)
        use additional_info <- decode.field(
          "additional_info",
          decode.optional(decode.string),
        )
        use reported_user_id <- decode.field(
          "reported_user_id",
          decode.optional(decode.string),
        )
        use reported_user_tag <- decode.field(
          "reported_user_tag",
          decode.optional(decode.string),
        )
        use reported_user_username <- decode.field(
          "reported_user_username",
          decode.optional(decode.string),
        )
        use reported_user_discriminator <- decode.field(
          "reported_user_discriminator",
          decode.optional(decode.string),
        )
        use reported_user_avatar_hash <- decode.field(
          "reported_user_avatar_hash",
          decode.optional(decode.string),
        )
        use reported_guild_id <- decode.field(
          "reported_guild_id",
          decode.optional(decode.string),
        )
        use reported_guild_name <- decode.field(
          "reported_guild_name",
          decode.optional(decode.string),
        )
        use reported_guild_icon_hash <- decode.field(
          "reported_guild_icon_hash",
          decode.optional(decode.string),
        )
        use reported_guild_invite_code <- decode.field(
          "reported_guild_invite_code",
          decode.optional(decode.string),
        )
        use reported_message_id <- decode.field(
          "reported_message_id",
          decode.optional(decode.string),
        )
        use reported_channel_id <- decode.field(
          "reported_channel_id",
          decode.optional(decode.string),
        )
        use reported_channel_name <- decode.field(
          "reported_channel_name",
          decode.optional(decode.string),
        )
        use resolved_at <- decode.field(
          "resolved_at",
          decode.optional(decode.string),
        )
        use resolved_by_admin_id <- decode.field(
          "resolved_by_admin_id",
          decode.optional(decode.string),
        )
        use public_comment <- decode.field(
          "public_comment",
          decode.optional(decode.string),
        )

        decode.success(
          Report(
            report_id: report_id,
            reporter_id: reporter_id,
            reporter_tag: reporter_tag,
            reporter_username: reporter_username,
            reporter_discriminator: reporter_discriminator,
            reporter_email: reporter_email,
            reporter_full_legal_name: reporter_full_legal_name,
            reporter_country_of_residence: reporter_country_of_residence,
            reported_at: reported_at,
            status: status_val,
            report_type: report_type,
            category: category,
            additional_info: additional_info,
            reported_user_id: reported_user_id,
            reported_user_tag: reported_user_tag,
            reported_user_username: reported_user_username,
            reported_user_discriminator: reported_user_discriminator,
            reported_user_avatar_hash: reported_user_avatar_hash,
            reported_guild_id: reported_guild_id,
            reported_guild_name: reported_guild_name,
            reported_guild_icon_hash: reported_guild_icon_hash,
            reported_message_id: reported_message_id,
            reported_channel_id: reported_channel_id,
            reported_channel_name: reported_channel_name,
            reported_guild_invite_code: reported_guild_invite_code,
            resolved_at: resolved_at,
            resolved_by_admin_id: resolved_by_admin_id,
            public_comment: public_comment,
            message_context: [],
          ),
        )
      }

      let decoder = {
        use reports <- decode.field("reports", decode.list(report_decoder))
        decode.success(ListReportsResponse(reports: reports))
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

pub fn resolve_report(
  ctx: web.Context,
  session: web.Session,
  report_id: String,
  public_comment: option.Option(String),
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let fields = [#("report_id", json.string(report_id))]
  let fields = case public_comment {
    option.Some(comment) -> [
      #("public_comment", json.string(comment)),
      ..fields
    ]
    option.None -> fields
  }
  admin_post_with_audit(
    ctx,
    session,
    "/admin/reports/resolve",
    fields,
    audit_log_reason,
  )
}

pub fn search_reports(
  ctx: web.Context,
  session: web.Session,
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  limit: Int,
  offset: Int,
) -> Result(SearchReportsResponse, ApiError) {
  let mut_fields = [#("limit", json.int(limit)), #("offset", json.int(offset))]

  let mut_fields = case query {
    option.Some(q) if q != "" -> [#("query", json.string(q)), ..mut_fields]
    _ -> mut_fields
  }

  let mut_fields = case status_filter {
    option.Some(s) -> [#("status", json.int(s)), ..mut_fields]
    option.None -> mut_fields
  }

  let mut_fields = case type_filter {
    option.Some(t) -> [#("report_type", json.int(t)), ..mut_fields]
    option.None -> mut_fields
  }

  let mut_fields = case category_filter {
    option.Some(c) if c != "" -> [#("category", json.string(c)), ..mut_fields]
    _ -> mut_fields
  }

  let url = ctx.api_endpoint <> "/admin/reports/search"
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
      let report_decoder = {
        use report_id <- decode.field("report_id", decode.string)
        use reporter_id <- decode.field(
          "reporter_id",
          decode.optional(decode.string),
        )
        use reporter_tag <- decode.field(
          "reporter_tag",
          decode.optional(decode.string),
        )
        use reporter_username <- decode.field(
          "reporter_username",
          decode.optional(decode.string),
        )
        use reporter_discriminator <- decode.field(
          "reporter_discriminator",
          decode.optional(decode.string),
        )
        use reporter_email <- decode.field(
          "reporter_email",
          decode.optional(decode.string),
        )
        use reporter_full_legal_name <- decode.field(
          "reporter_full_legal_name",
          decode.optional(decode.string),
        )
        use reporter_country_of_residence <- decode.field(
          "reporter_country_of_residence",
          decode.optional(decode.string),
        )
        use reported_at <- decode.field("reported_at", decode.string)
        use status_val <- decode.field("status", decode.int)
        use report_type <- decode.field("report_type", decode.int)
        use category <- decode.field("category", decode.string)
        use additional_info <- decode.field(
          "additional_info",
          decode.optional(decode.string),
        )
        use reported_user_id <- decode.field(
          "reported_user_id",
          decode.optional(decode.string),
        )
        use reported_user_tag <- decode.field(
          "reported_user_tag",
          decode.optional(decode.string),
        )
        use reported_user_username <- decode.field(
          "reported_user_username",
          decode.optional(decode.string),
        )
        use reported_user_discriminator <- decode.field(
          "reported_user_discriminator",
          decode.optional(decode.string),
        )
        use reported_user_avatar_hash <- decode.field(
          "reported_user_avatar_hash",
          decode.optional(decode.string),
        )
        use reported_guild_id <- decode.field(
          "reported_guild_id",
          decode.optional(decode.string),
        )
        use reported_guild_name <- decode.field(
          "reported_guild_name",
          decode.optional(decode.string),
        )
        use reported_guild_invite_code <- decode.field(
          "reported_guild_invite_code",
          decode.optional(decode.string),
        )
        decode.success(SearchReportResult(
          report_id: report_id,
          reporter_id: reporter_id,
          reporter_tag: reporter_tag,
          reporter_username: reporter_username,
          reporter_discriminator: reporter_discriminator,
          reporter_email: reporter_email,
          reporter_full_legal_name: reporter_full_legal_name,
          reporter_country_of_residence: reporter_country_of_residence,
          reported_at: reported_at,
          status: status_val,
          report_type: report_type,
          category: category,
          additional_info: additional_info,
          reported_user_id: reported_user_id,
          reported_user_tag: reported_user_tag,
          reported_user_username: reported_user_username,
          reported_user_discriminator: reported_user_discriminator,
          reported_user_avatar_hash: reported_user_avatar_hash,
          reported_guild_id: reported_guild_id,
          reported_guild_name: reported_guild_name,
          reported_guild_invite_code: reported_guild_invite_code,
        ))
      }

      let decoder = {
        use reports <- decode.field("reports", decode.list(report_decoder))
        use total <- decode.field("total", decode.int)
        use offset_val <- decode.field("offset", decode.int)
        use limit_val <- decode.field("limit", decode.int)
        decode.success(SearchReportsResponse(
          reports: reports,
          total: total,
          offset: offset_val,
          limit: limit_val,
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

pub fn get_report_detail(
  ctx: web.Context,
  session: web.Session,
  report_id: String,
) -> Result(Report, ApiError) {
  let url = ctx.api_endpoint <> "/admin/reports/" <> report_id
  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let attachment_decoder = {
        use filename <- decode.field("filename", decode.string)
        use url <- decode.field("url", decode.string)
        decode.success(MessageAttachment(filename: filename, url: url))
      }

      let context_message_decoder = {
        use id <- decode.field("id", decode.string)
        use channel_id <- decode.field(
          "channel_id",
          decode.optional(decode.string),
        )
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
          channel_id: option.unwrap(channel_id, ""),
          author_id: author_id,
          author_username: author_username,
          content: content,
          timestamp: timestamp,
          attachments: attachments,
        ))
      }

      let report_decoder = {
        use report_id <- decode.field("report_id", decode.string)
        use reporter_id <- decode.field(
          "reporter_id",
          decode.optional(decode.string),
        )
        use reporter_tag <- decode.field(
          "reporter_tag",
          decode.optional(decode.string),
        )
        use reporter_username <- decode.field(
          "reporter_username",
          decode.optional(decode.string),
        )
        use reporter_discriminator <- decode.field(
          "reporter_discriminator",
          decode.optional(decode.string),
        )
        use reporter_email <- decode.field(
          "reporter_email",
          decode.optional(decode.string),
        )
        use reporter_full_legal_name <- decode.field(
          "reporter_full_legal_name",
          decode.optional(decode.string),
        )
        use reporter_country_of_residence <- decode.field(
          "reporter_country_of_residence",
          decode.optional(decode.string),
        )
        use reported_at <- decode.field("reported_at", decode.string)
        use status_val <- decode.field("status", decode.int)
        use report_type <- decode.field("report_type", decode.int)
        use category <- decode.field("category", decode.string)
        use additional_info <- decode.field(
          "additional_info",
          decode.optional(decode.string),
        )
        use reported_user_id <- decode.field(
          "reported_user_id",
          decode.optional(decode.string),
        )
        use reported_user_tag <- decode.field(
          "reported_user_tag",
          decode.optional(decode.string),
        )
        use reported_user_username <- decode.field(
          "reported_user_username",
          decode.optional(decode.string),
        )
        use reported_user_discriminator <- decode.field(
          "reported_user_discriminator",
          decode.optional(decode.string),
        )
        use reported_user_avatar_hash <- decode.field(
          "reported_user_avatar_hash",
          decode.optional(decode.string),
        )
        use reported_guild_id <- decode.field(
          "reported_guild_id",
          decode.optional(decode.string),
        )
        use reported_guild_name <- decode.field(
          "reported_guild_name",
          decode.optional(decode.string),
        )
        use reported_guild_icon_hash <- decode.field(
          "reported_guild_icon_hash",
          decode.optional(decode.string),
        )
        use reported_guild_invite_code <- decode.field(
          "reported_guild_invite_code",
          decode.optional(decode.string),
        )
        use reported_message_id <- decode.field(
          "reported_message_id",
          decode.optional(decode.string),
        )
        use reported_channel_id <- decode.field(
          "reported_channel_id",
          decode.optional(decode.string),
        )
        use reported_channel_name <- decode.field(
          "reported_channel_name",
          decode.optional(decode.string),
        )
        use resolved_at <- decode.field(
          "resolved_at",
          decode.optional(decode.string),
        )
        use resolved_by_admin_id <- decode.field(
          "resolved_by_admin_id",
          decode.optional(decode.string),
        )
        use public_comment <- decode.field(
          "public_comment",
          decode.optional(decode.string),
        )
        use message_context <- decode.optional_field(
          "message_context",
          [],
          decode.list(context_message_decoder),
        )
        decode.success(Report(
          report_id: report_id,
          reporter_id: reporter_id,
          reporter_tag: reporter_tag,
          reporter_username: reporter_username,
          reporter_discriminator: reporter_discriminator,
          reporter_email: reporter_email,
          reporter_full_legal_name: reporter_full_legal_name,
          reporter_country_of_residence: reporter_country_of_residence,
          reported_at: reported_at,
          status: status_val,
          report_type: report_type,
          category: category,
          additional_info: additional_info,
          reported_user_id: reported_user_id,
          reported_user_tag: reported_user_tag,
          reported_user_username: reported_user_username,
          reported_user_discriminator: reported_user_discriminator,
          reported_user_avatar_hash: reported_user_avatar_hash,
          reported_guild_id: reported_guild_id,
          reported_guild_name: reported_guild_name,
          reported_guild_icon_hash: reported_guild_icon_hash,
          reported_message_id: reported_message_id,
          reported_channel_id: reported_channel_id,
          reported_channel_name: reported_channel_name,
          reported_guild_invite_code: reported_guild_invite_code,
          resolved_at: resolved_at,
          resolved_by_admin_id: resolved_by_admin_id,
          public_comment: public_comment,
          message_context: message_context,
        ))
      }

      case json.parse(resp.body, report_decoder) {
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
