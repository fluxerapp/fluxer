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
  admin_post_simple, admin_post_with_audit,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type CheckBanResponse {
  CheckBanResponse(banned: Bool)
}

pub fn ban_email(
  ctx: web.Context,
  session: web.Session,
  email: String,
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/bans/email/add",
    [#("email", json.string(email))],
    audit_log_reason,
  )
}

pub fn unban_email(
  ctx: web.Context,
  session: web.Session,
  email: String,
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/bans/email/remove",
    [#("email", json.string(email))],
    audit_log_reason,
  )
}

pub fn check_email_ban(
  ctx: web.Context,
  session: web.Session,
  email: String,
) -> Result(CheckBanResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/bans/email/check"
  let body = json.object([#("email", json.string(email))]) |> json.to_string

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
        use banned <- decode.field("banned", decode.bool)
        decode.success(CheckBanResponse(banned: banned))
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

pub fn ban_ip(
  ctx: web.Context,
  session: web.Session,
  ip: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/bans/ip/add", [
    #("ip", json.string(ip)),
  ])
}

pub fn unban_ip(
  ctx: web.Context,
  session: web.Session,
  ip: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/bans/ip/remove", [
    #("ip", json.string(ip)),
  ])
}

pub fn check_ip_ban(
  ctx: web.Context,
  session: web.Session,
  ip: String,
) -> Result(CheckBanResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/bans/ip/check"
  let body = json.object([#("ip", json.string(ip))]) |> json.to_string

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
        use banned <- decode.field("banned", decode.bool)
        decode.success(CheckBanResponse(banned: banned))
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

pub fn ban_phone(
  ctx: web.Context,
  session: web.Session,
  phone: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/bans/phone/add", [
    #("phone", json.string(phone)),
  ])
}

pub fn unban_phone(
  ctx: web.Context,
  session: web.Session,
  phone: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/bans/phone/remove", [
    #("phone", json.string(phone)),
  ])
}

pub fn check_phone_ban(
  ctx: web.Context,
  session: web.Session,
  phone: String,
) -> Result(CheckBanResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/bans/phone/check"
  let body = json.object([#("phone", json.string(phone))]) |> json.to_string

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
        use banned <- decode.field("banned", decode.bool)
        decode.success(CheckBanResponse(banned: banned))
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
