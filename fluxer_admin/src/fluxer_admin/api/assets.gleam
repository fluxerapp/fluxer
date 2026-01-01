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

pub type AssetPurgeResult {
  AssetPurgeResult(
    id: String,
    asset_type: String,
    found_in_db: Bool,
    guild_id: option.Option(String),
  )
}

pub type AssetPurgeError {
  AssetPurgeError(id: String, error: String)
}

pub type AssetPurgeResponse {
  AssetPurgeResponse(
    processed: List(AssetPurgeResult),
    errors: List(AssetPurgeError),
  )
}

pub fn purge_assets(
  ctx: web.Context,
  session: web.Session,
  ids: List(String),
  audit_log_reason: option.Option(String),
) -> Result(AssetPurgeResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/assets/purge"
  let body =
    json.object([#("ids", json.array(ids, json.string))]) |> json.to_string

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
      let result_decoder = {
        use processed <- decode.field(
          "processed",
          decode.list({
            use id <- decode.field("id", decode.string)
            use asset_type <- decode.field("asset_type", decode.string)
            use found_in_db <- decode.field("found_in_db", decode.bool)
            use guild_id <- decode.field(
              "guild_id",
              decode.optional(decode.string),
            )
            decode.success(AssetPurgeResult(
              id: id,
              asset_type: asset_type,
              found_in_db: found_in_db,
              guild_id: guild_id,
            ))
          }),
        )
        use errors <- decode.field(
          "errors",
          decode.list({
            use id <- decode.field("id", decode.string)
            use error <- decode.field("error", decode.string)
            decode.success(AssetPurgeError(id: id, error: error))
          }),
        )
        decode.success(AssetPurgeResponse(processed: processed, errors: errors))
      }

      case json.parse(resp.body, result_decoder) {
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
