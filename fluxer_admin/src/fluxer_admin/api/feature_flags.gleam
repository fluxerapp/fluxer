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
  type ApiError, Forbidden, NetworkError, ServerError, Unauthorized,
}
import fluxer_admin/web.{type Context, type Session}
import gleam/dict
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/string

pub type FeatureFlagConfig {
  FeatureFlagConfig(guild_ids: List(String))
}

pub fn get_feature_flags(
  ctx: Context,
  session: Session,
) -> Result(List(#(String, FeatureFlagConfig)), ApiError) {
  let url = ctx.api_endpoint <> "/admin/feature-flags/get"
  let body = json.object([]) |> json.to_string

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
        use feature_flags <- decode.field(
          "feature_flags",
          decode.dict(decode.string, decode.list(decode.string)),
        )
        decode.success(feature_flags)
      }
      case json.parse(resp.body, decoder) {
        Ok(flags_dict) -> {
          let entries =
            dict.to_list(flags_dict)
            |> list.map(fn(entry) {
              let #(flag, guild_ids) = entry
              #(flag, FeatureFlagConfig(guild_ids:))
            })
          Ok(entries)
        }
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn update_feature_flag(
  ctx: Context,
  session: Session,
  flag_id: String,
  guild_ids: List(String),
) -> Result(FeatureFlagConfig, ApiError) {
  let url = ctx.api_endpoint <> "/admin/feature-flags/update"
  let guild_ids_str = string.join(guild_ids, ",")
  let body =
    json.object([
      #("flag", json.string(flag_id)),
      #("guild_ids", json.string(guild_ids_str)),
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
    Ok(resp) if resp.status == 200 -> Ok(FeatureFlagConfig(guild_ids:))
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}
