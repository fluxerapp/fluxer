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
import gleam/option

pub type VoiceRegion {
  VoiceRegion(
    id: String,
    name: String,
    emoji: String,
    latitude: Float,
    longitude: Float,
    is_default: Bool,
    vip_only: Bool,
    required_guild_features: List(String),
    allowed_guild_ids: List(String),
    allowed_user_ids: List(String),
    created_at: option.Option(String),
    updated_at: option.Option(String),
    servers: option.Option(List(VoiceServer)),
  )
}

pub type VoiceServer {
  VoiceServer(
    region_id: String,
    server_id: String,
    endpoint: String,
    is_active: Bool,
    vip_only: Bool,
    required_guild_features: List(String),
    allowed_guild_ids: List(String),
    allowed_user_ids: List(String),
    created_at: option.Option(String),
    updated_at: option.Option(String),
  )
}

pub type ListVoiceRegionsResponse {
  ListVoiceRegionsResponse(regions: List(VoiceRegion))
}

pub type GetVoiceRegionResponse {
  GetVoiceRegionResponse(region: option.Option(VoiceRegion))
}

pub type ListVoiceServersResponse {
  ListVoiceServersResponse(servers: List(VoiceServer))
}

pub type GetVoiceServerResponse {
  GetVoiceServerResponse(server: option.Option(VoiceServer))
}

fn voice_region_decoder() {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use emoji <- decode.field("emoji", decode.string)
  use latitude <- decode.field("latitude", decode.float)
  use longitude <- decode.field("longitude", decode.float)
  use is_default <- decode.field("is_default", decode.bool)
  use vip_only <- decode.field("vip_only", decode.bool)
  use required_guild_features <- decode.field(
    "required_guild_features",
    decode.list(decode.string),
  )
  use allowed_guild_ids <- decode.field(
    "allowed_guild_ids",
    decode.list(decode.string),
  )
  use allowed_user_ids <- decode.field(
    "allowed_user_ids",
    decode.list(decode.string),
  )
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))

  decode.success(VoiceRegion(
    id: id,
    name: name,
    emoji: emoji,
    latitude: latitude,
    longitude: longitude,
    is_default: is_default,
    vip_only: vip_only,
    required_guild_features: required_guild_features,
    allowed_guild_ids: allowed_guild_ids,
    allowed_user_ids: allowed_user_ids,
    created_at: created_at,
    updated_at: updated_at,
    servers: option.None,
  ))
}

fn voice_region_with_servers_decoder() {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use emoji <- decode.field("emoji", decode.string)
  use latitude <- decode.field("latitude", decode.float)
  use longitude <- decode.field("longitude", decode.float)
  use is_default <- decode.field("is_default", decode.bool)
  use vip_only <- decode.field("vip_only", decode.bool)
  use required_guild_features <- decode.field(
    "required_guild_features",
    decode.list(decode.string),
  )
  use allowed_guild_ids <- decode.field(
    "allowed_guild_ids",
    decode.list(decode.string),
  )
  use allowed_user_ids <- decode.field(
    "allowed_user_ids",
    decode.list(decode.string),
  )
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))
  use servers <- decode.field("servers", decode.list(voice_server_decoder()))

  decode.success(VoiceRegion(
    id: id,
    name: name,
    emoji: emoji,
    latitude: latitude,
    longitude: longitude,
    is_default: is_default,
    vip_only: vip_only,
    required_guild_features: required_guild_features,
    allowed_guild_ids: allowed_guild_ids,
    allowed_user_ids: allowed_user_ids,
    created_at: created_at,
    updated_at: updated_at,
    servers: option.Some(servers),
  ))
}

fn voice_server_decoder() {
  use region_id <- decode.field("region_id", decode.string)
  use server_id <- decode.field("server_id", decode.string)
  use endpoint <- decode.field("endpoint", decode.string)
  use is_active <- decode.field("is_active", decode.bool)
  use vip_only <- decode.field("vip_only", decode.bool)
  use required_guild_features <- decode.field(
    "required_guild_features",
    decode.list(decode.string),
  )
  use allowed_guild_ids <- decode.field(
    "allowed_guild_ids",
    decode.list(decode.string),
  )
  use allowed_user_ids <- decode.field(
    "allowed_user_ids",
    decode.list(decode.string),
  )
  use created_at <- decode.field("created_at", decode.optional(decode.string))
  use updated_at <- decode.field("updated_at", decode.optional(decode.string))

  decode.success(VoiceServer(
    region_id: region_id,
    server_id: server_id,
    endpoint: endpoint,
    is_active: is_active,
    vip_only: vip_only,
    required_guild_features: required_guild_features,
    allowed_guild_ids: allowed_guild_ids,
    allowed_user_ids: allowed_user_ids,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

pub fn list_voice_regions(
  ctx: Context,
  session: Session,
  include_servers: Bool,
) -> Result(ListVoiceRegionsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/voice/regions/list"
  let body =
    json.object([#("include_servers", json.bool(include_servers))])
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
      let decoder_fn = case include_servers {
        True -> voice_region_with_servers_decoder
        False -> voice_region_decoder
      }

      let decoder = {
        use regions <- decode.field("regions", decode.list(decoder_fn()))
        decode.success(ListVoiceRegionsResponse(regions: regions))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn get_voice_region(
  ctx: Context,
  session: Session,
  region_id: String,
  include_servers: Bool,
) -> Result(GetVoiceRegionResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/voice/regions/get"
  let body =
    json.object([
      #("id", json.string(region_id)),
      #("include_servers", json.bool(include_servers)),
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
      let decoder_fn = case include_servers {
        True -> voice_region_with_servers_decoder
        False -> voice_region_decoder
      }

      let decoder = {
        use region <- decode.field("region", decode.optional(decoder_fn()))
        decode.success(GetVoiceRegionResponse(region: region))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn create_voice_region(
  ctx: Context,
  session: Session,
  id: String,
  name: String,
  emoji: String,
  latitude: Float,
  longitude: Float,
  is_default: Bool,
  vip_only: Bool,
  required_guild_features: List(String),
  allowed_guild_ids: List(String),
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/regions/create",
    [
      #("id", json.string(id)),
      #("name", json.string(name)),
      #("emoji", json.string(emoji)),
      #("latitude", json.float(latitude)),
      #("longitude", json.float(longitude)),
      #("is_default", json.bool(is_default)),
      #("vip_only", json.bool(vip_only)),
      #(
        "required_guild_features",
        json.array(required_guild_features, json.string),
      ),
      #("allowed_guild_ids", json.array(allowed_guild_ids, json.string)),
    ],
    audit_log_reason,
  )
}

pub fn update_voice_region(
  ctx: Context,
  session: Session,
  id: String,
  name: option.Option(String),
  emoji: option.Option(String),
  latitude: option.Option(Float),
  longitude: option.Option(Float),
  is_default: option.Option(Bool),
  vip_only: option.Option(Bool),
  required_guild_features: option.Option(List(String)),
  allowed_guild_ids: option.Option(List(String)),
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let base_fields = [#("id", json.string(id))]

  let fields = case name {
    option.Some(n) -> [#("name", json.string(n)), ..base_fields]
    option.None -> base_fields
  }

  let fields = case emoji {
    option.Some(e) -> [#("emoji", json.string(e)), ..fields]
    option.None -> fields
  }

  let fields = case latitude {
    option.Some(lat) -> [#("latitude", json.float(lat)), ..fields]
    option.None -> fields
  }

  let fields = case longitude {
    option.Some(lng) -> [#("longitude", json.float(lng)), ..fields]
    option.None -> fields
  }

  let fields = case is_default {
    option.Some(d) -> [#("is_default", json.bool(d)), ..fields]
    option.None -> fields
  }

  let fields = case vip_only {
    option.Some(v) -> [#("vip_only", json.bool(v)), ..fields]
    option.None -> fields
  }

  let fields = case required_guild_features {
    option.Some(features) -> [
      #("required_guild_features", json.array(features, json.string)),
      ..fields
    ]
    option.None -> fields
  }

  let fields = case allowed_guild_ids {
    option.Some(ids) -> [
      #("allowed_guild_ids", json.array(ids, json.string)),
      ..fields
    ]
    option.None -> fields
  }

  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/regions/update",
    fields,
    audit_log_reason,
  )
}

pub fn delete_voice_region(
  ctx: Context,
  session: Session,
  id: String,
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/regions/delete",
    [#("id", json.string(id))],
    audit_log_reason,
  )
}

pub fn list_voice_servers(
  ctx: Context,
  session: Session,
  region_id: String,
) -> Result(ListVoiceServersResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/voice/servers/list"
  let body =
    json.object([#("region_id", json.string(region_id))]) |> json.to_string

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
        use servers <- decode.field(
          "servers",
          decode.list(voice_server_decoder()),
        )
        decode.success(ListVoiceServersResponse(servers: servers))
      }

      case json.parse(resp.body, decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Access denied"))
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn create_voice_server(
  ctx: Context,
  session: Session,
  region_id: String,
  server_id: String,
  endpoint: String,
  api_key: String,
  api_secret: String,
  is_active: Bool,
  vip_only: Bool,
  required_guild_features: List(String),
  allowed_guild_ids: List(String),
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/servers/create",
    [
      #("region_id", json.string(region_id)),
      #("server_id", json.string(server_id)),
      #("endpoint", json.string(endpoint)),
      #("api_key", json.string(api_key)),
      #("api_secret", json.string(api_secret)),
      #("is_active", json.bool(is_active)),
      #("vip_only", json.bool(vip_only)),
      #(
        "required_guild_features",
        json.array(required_guild_features, json.string),
      ),
      #("allowed_guild_ids", json.array(allowed_guild_ids, json.string)),
    ],
    audit_log_reason,
  )
}

pub fn update_voice_server(
  ctx: Context,
  session: Session,
  region_id: String,
  server_id: String,
  endpoint: option.Option(String),
  api_key: option.Option(String),
  api_secret: option.Option(String),
  is_active: option.Option(Bool),
  vip_only: option.Option(Bool),
  required_guild_features: option.Option(List(String)),
  allowed_guild_ids: option.Option(List(String)),
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let base_fields = [
    #("region_id", json.string(region_id)),
    #("server_id", json.string(server_id)),
  ]

  let fields = case endpoint {
    option.Some(e) -> [#("endpoint", json.string(e)), ..base_fields]
    option.None -> base_fields
  }

  let fields = case api_key {
    option.Some(k) -> [#("api_key", json.string(k)), ..fields]
    option.None -> fields
  }

  let fields = case api_secret {
    option.Some(s) -> [#("api_secret", json.string(s)), ..fields]
    option.None -> fields
  }

  let fields = case is_active {
    option.Some(a) -> [#("is_active", json.bool(a)), ..fields]
    option.None -> fields
  }

  let fields = case vip_only {
    option.Some(v) -> [#("vip_only", json.bool(v)), ..fields]
    option.None -> fields
  }

  let fields = case required_guild_features {
    option.Some(features) -> [
      #("required_guild_features", json.array(features, json.string)),
      ..fields
    ]
    option.None -> fields
  }

  let fields = case allowed_guild_ids {
    option.Some(ids) -> [
      #("allowed_guild_ids", json.array(ids, json.string)),
      ..fields
    ]
    option.None -> fields
  }

  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/servers/update",
    fields,
    audit_log_reason,
  )
}

pub fn delete_voice_server(
  ctx: Context,
  session: Session,
  region_id: String,
  server_id: String,
  audit_log_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  admin_post_with_audit(
    ctx,
    session,
    "/admin/voice/servers/delete",
    [
      #("region_id", json.string(region_id)),
      #("server_id", json.string(server_id)),
    ],
    audit_log_reason,
  )
}
