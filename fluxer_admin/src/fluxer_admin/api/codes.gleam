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

fn parse_codes(body: String) -> Result(List(String), ApiError) {
  let decoder = {
    use codes <- decode.field("codes", decode.list(decode.string))
    decode.success(codes)
  }

  case json.parse(body, decoder) {
    Ok(codes) -> Ok(codes)
    Error(_) -> Error(ServerError)
  }
}

pub fn generate_beta_codes(
  ctx: web.Context,
  session: web.Session,
  count: Int,
) -> Result(List(String), ApiError) {
  let url = ctx.api_endpoint <> "/admin/codes/beta"
  let body = json.object([#("count", json.int(count))]) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) ->
      case resp.status {
        200 -> parse_codes(resp.body)
        401 -> Error(Unauthorized)
        403 -> {
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
        404 -> Error(NotFound)
        _ -> Error(ServerError)
      }
    Error(_) -> Error(NetworkError)
  }
}

pub fn generate_gift_codes(
  ctx: web.Context,
  session: web.Session,
  count: Int,
  product_type: String,
) -> Result(List(String), ApiError) {
  let url = ctx.api_endpoint <> "/admin/codes/gift"
  let body =
    json.object([
      #("count", json.int(count)),
      #("product_type", json.string(product_type)),
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
    Ok(resp) ->
      case resp.status {
        200 -> parse_codes(resp.body)
        401 -> Error(Unauthorized)
        403 -> {
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
        404 -> Error(NotFound)
        _ -> Error(ServerError)
      }
    Error(_) -> Error(NetworkError)
  }
}
