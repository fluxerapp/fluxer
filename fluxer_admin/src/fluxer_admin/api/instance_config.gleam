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
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type InstanceConfig {
  InstanceConfig(
    manual_review_enabled: Bool,
    manual_review_schedule_enabled: Bool,
    manual_review_schedule_start_hour_utc: Int,
    manual_review_schedule_end_hour_utc: Int,
    manual_review_active_now: Bool,
    registration_alerts_webhook_url: String,
    system_alerts_webhook_url: String,
  )
}

fn instance_config_decoder() {
  use manual_review_enabled <- decode.field(
    "manual_review_enabled",
    decode.bool,
  )
  use manual_review_schedule_enabled <- decode.field(
    "manual_review_schedule_enabled",
    decode.bool,
  )
  use manual_review_schedule_start_hour_utc <- decode.field(
    "manual_review_schedule_start_hour_utc",
    decode.int,
  )
  use manual_review_schedule_end_hour_utc <- decode.field(
    "manual_review_schedule_end_hour_utc",
    decode.int,
  )
  use manual_review_active_now <- decode.field(
    "manual_review_active_now",
    decode.bool,
  )
  use registration_alerts_webhook_url <- decode.field(
    "registration_alerts_webhook_url",
    decode.optional(decode.string),
  )
  use system_alerts_webhook_url <- decode.field(
    "system_alerts_webhook_url",
    decode.optional(decode.string),
  )
  decode.success(InstanceConfig(
    manual_review_enabled:,
    manual_review_schedule_enabled:,
    manual_review_schedule_start_hour_utc:,
    manual_review_schedule_end_hour_utc:,
    manual_review_active_now:,
    registration_alerts_webhook_url: option.unwrap(
      registration_alerts_webhook_url,
      "",
    ),
    system_alerts_webhook_url: option.unwrap(system_alerts_webhook_url, ""),
  ))
}

pub fn get_instance_config(
  ctx: Context,
  session: Session,
) -> Result(InstanceConfig, ApiError) {
  let url = ctx.api_endpoint <> "/admin/instance-config/get"
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
      case json.parse(resp.body, instance_config_decoder()) {
        Ok(config) -> Ok(config)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn update_instance_config(
  ctx: Context,
  session: Session,
  manual_review_enabled: Bool,
  manual_review_schedule_enabled: Bool,
  manual_review_schedule_start_hour_utc: Int,
  manual_review_schedule_end_hour_utc: Int,
  registration_alerts_webhook_url: String,
  system_alerts_webhook_url: String,
) -> Result(InstanceConfig, ApiError) {
  let url = ctx.api_endpoint <> "/admin/instance-config/update"
  let registration_webhook_json = case registration_alerts_webhook_url {
    "" -> json.null()
    url -> json.string(url)
  }
  let system_webhook_json = case system_alerts_webhook_url {
    "" -> json.null()
    url -> json.string(url)
  }
  let body =
    json.object([
      #("manual_review_enabled", json.bool(manual_review_enabled)),
      #(
        "manual_review_schedule_enabled",
        json.bool(manual_review_schedule_enabled),
      ),
      #(
        "manual_review_schedule_start_hour_utc",
        json.int(manual_review_schedule_start_hour_utc),
      ),
      #(
        "manual_review_schedule_end_hour_utc",
        json.int(manual_review_schedule_end_hour_utc),
      ),
      #("registration_alerts_webhook_url", registration_webhook_json),
      #("system_alerts_webhook_url", system_webhook_json),
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
      case json.parse(resp.body, instance_config_decoder()) {
        Ok(config) -> Ok(config)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}
