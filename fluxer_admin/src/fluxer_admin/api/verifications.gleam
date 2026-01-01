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
  type ApiError, type UserLookupResult, Forbidden, NetworkError, NotFound,
  ServerError, Unauthorized, admin_post_simple, user_lookup_decoder,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json

pub type PendingVerificationMetadata {
  PendingVerificationMetadata(key: String, value: String)
}

pub type PendingVerification {
  PendingVerification(
    user_id: String,
    created_at: String,
    user: UserLookupResult,
    metadata: List(PendingVerificationMetadata),
  )
}

pub type PendingVerificationsResponse {
  PendingVerificationsResponse(pending_verifications: List(PendingVerification))
}

pub fn list_pending_verifications(
  ctx: web.Context,
  session: web.Session,
  limit: Int,
) -> Result(PendingVerificationsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/pending-verifications/list"
  let body = json.object([#("limit", json.int(limit))]) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let pending_verification_metadata_decoder = {
        use key <- decode.field("key", decode.string)
        use value <- decode.field("value", decode.string)
        decode.success(PendingVerificationMetadata(key: key, value: value))
      }

      let pending_verification_decoder = {
        use user_id <- decode.field("user_id", decode.string)
        use created_at <- decode.field("created_at", decode.string)
        use user <- decode.field("user", user_lookup_decoder())
        use metadata <- decode.field(
          "metadata",
          decode.list(pending_verification_metadata_decoder),
        )
        decode.success(PendingVerification(
          user_id: user_id,
          created_at: created_at,
          user: user,
          metadata: metadata,
        ))
      }

      let decoder = {
        use pending_verifications <- decode.field(
          "pending_verifications",
          decode.list(pending_verification_decoder),
        )
        decode.success(PendingVerificationsResponse(
          pending_verifications: pending_verifications,
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

pub fn approve_registration(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/pending-verifications/approve", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn reject_registration(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/pending-verifications/reject", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn bulk_approve_registrations(
  ctx: web.Context,
  session: web.Session,
  user_ids: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/pending-verifications/bulk-approve", [
    #("user_ids", json.array(user_ids, json.string)),
  ])
}

pub fn bulk_reject_registrations(
  ctx: web.Context,
  session: web.Session,
  user_ids: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/pending-verifications/bulk-reject", [
    #("user_ids", json.array(user_ids, json.string)),
  ])
}
