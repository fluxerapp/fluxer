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
  ServerError, Unauthorized, admin_post_simple, admin_post_with_audit,
  user_lookup_decoder,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option.{type Option}

pub type ContactChangeLogEntry {
  ContactChangeLogEntry(
    event_id: String,
    field: String,
    old_value: Option(String),
    new_value: Option(String),
    reason: String,
    actor_user_id: Option(String),
    event_at: String,
  )
}

pub type ListUserChangeLogResponse {
  ListUserChangeLogResponse(
    entries: List(ContactChangeLogEntry),
    next_page_token: Option(String),
  )
}

pub type UserSession {
  UserSession(
    session_id_hash: String,
    created_at: String,
    approx_last_used_at: String,
    client_ip: String,
    client_os: String,
    client_platform: String,
    client_location: Option(String),
  )
}

pub type ListUserSessionsResponse {
  ListUserSessionsResponse(sessions: List(UserSession))
}

pub type SearchUsersResponse {
  SearchUsersResponse(users: List(UserLookupResult), total: Int)
}

pub type UserGuild {
  UserGuild(
    id: String,
    owner_id: String,
    name: String,
    features: List(String),
    icon: option.Option(String),
    banner: option.Option(String),
    member_count: Int,
  )
}

pub type ListUserGuildsResponse {
  ListUserGuildsResponse(guilds: List(UserGuild))
}

pub fn list_user_guilds(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(ListUserGuildsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/list-guilds"
  let body = json.object([#("user_id", json.string(user_id))]) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let guild_decoder = {
        use id <- decode.field("id", decode.string)
        use owner_id <- decode.optional_field("owner_id", "", decode.string)
        use name <- decode.field("name", decode.string)
        use features <- decode.field("features", decode.list(decode.string))
        use icon <- decode.optional_field(
          "icon",
          option.None,
          decode.optional(decode.string),
        )
        use banner <- decode.optional_field(
          "banner",
          option.None,
          decode.optional(decode.string),
        )
        use member_count <- decode.optional_field("member_count", 0, decode.int)
        decode.success(UserGuild(
          id: id,
          owner_id: owner_id,
          name: name,
          features: features,
          icon: icon,
          banner: banner,
          member_count: member_count,
        ))
      }

      let decoder = {
        use guilds <- decode.field("guilds", decode.list(guild_decoder))
        decode.success(ListUserGuildsResponse(guilds: guilds))
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

pub fn list_user_change_log(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(ListUserChangeLogResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/change-log"
  let body =
    json.object([
      #("user_id", json.string(user_id)),
      #("limit", json.int(50)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  let entry_decoder = {
    use event_id <- decode.field("event_id", decode.string)
    use field <- decode.field("field", decode.string)
    use old_value <- decode.field("old_value", decode.optional(decode.string))
    use new_value <- decode.field("new_value", decode.optional(decode.string))
    use reason <- decode.field("reason", decode.string)
    use actor_user_id <- decode.field(
      "actor_user_id",
      decode.optional(decode.string),
    )
    use event_at <- decode.field("event_at", decode.string)
    decode.success(ContactChangeLogEntry(
      event_id: event_id,
      field: field,
      old_value: old_value,
      new_value: new_value,
      reason: reason,
      actor_user_id: actor_user_id,
      event_at: event_at,
    ))
  }

  let decoder = {
    use entries <- decode.field("entries", decode.list(entry_decoder))
    use next_page_token <- decode.field(
      "next_page_token",
      decode.optional(decode.string),
    )
    decode.success(ListUserChangeLogResponse(
      entries: entries,
      next_page_token: next_page_token,
    ))
  }

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 ->
      case json.parse(resp.body, decoder) {
        Ok(result) -> Ok(result)
        Error(_) -> Error(ServerError)
      }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 -> Error(Forbidden("Missing permission"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(_resp) -> Error(ServerError)
    Error(_) -> Error(NetworkError)
  }
}

pub fn lookup_user(
  ctx: web.Context,
  session: web.Session,
  query: String,
) -> Result(Option(UserLookupResult), ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/lookup"
  let body = json.object([#("query", json.string(query))]) |> json.to_string

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
        use user <- decode.field("user", decode.optional(user_lookup_decoder()))
        decode.success(user)
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

pub fn update_user_flags(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  add_flags: List(String),
  remove_flags: List(String),
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/update-flags"
  let body =
    json.object([
      #("user_id", json.string(user_id)),
      #("add_flags", json.array(add_flags, json.string)),
      #("remove_flags", json.array(remove_flags, json.string)),
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
    Ok(resp) if resp.status == 200 -> Ok(Nil)
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

pub fn disable_mfa(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/disable-mfa", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn verify_email(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/verify-email", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn unlink_phone(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/unlink-phone", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn terminate_sessions(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/terminate-sessions", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn temp_ban_user(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  duration_hours: Int,
  reason: option.Option(String),
  private_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let fields = [
    #("user_id", json.string(user_id)),
    #("duration_hours", json.int(duration_hours)),
  ]
  let fields = case reason {
    option.Some(r) -> [#("reason", json.string(r)), ..fields]
    option.None -> fields
  }
  admin_post_with_audit(
    ctx,
    session,
    "/admin/users/temp-ban",
    fields,
    private_reason,
  )
}

pub fn unban_user(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/unban", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn schedule_deletion(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  reason_code: Int,
  public_reason: option.Option(String),
  days_until_deletion: Int,
  private_reason: option.Option(String),
) -> Result(Nil, ApiError) {
  let fields = [
    #("user_id", json.string(user_id)),
    #("reason_code", json.int(reason_code)),
    #("days_until_deletion", json.int(days_until_deletion)),
  ]
  let fields = case public_reason {
    option.Some(r) -> [#("public_reason", json.string(r)), ..fields]
    option.None -> fields
  }
  admin_post_with_audit(
    ctx,
    session,
    "/admin/users/schedule-deletion",
    fields,
    private_reason,
  )
}

pub fn cancel_deletion(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/cancel-deletion", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn cancel_bulk_message_deletion(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/cancel-bulk-message-deletion", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn change_email(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  email: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/change-email", [
    #("user_id", json.string(user_id)),
    #("email", json.string(email)),
  ])
}

pub fn send_password_reset(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/send-password-reset", [
    #("user_id", json.string(user_id)),
  ])
}

pub fn update_suspicious_activity_flags(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  flags: Int,
) -> Result(Nil, ApiError) {
  admin_post_simple(
    ctx,
    session,
    "/admin/users/update-suspicious-activity-flags",
    [#("user_id", json.string(user_id)), #("flags", json.int(flags))],
  )
}

pub fn get_current_admin(
  ctx: web.Context,
  session: web.Session,
) -> Result(Option(UserLookupResult), ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/me"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let decoder = {
        use user <- decode.field("user", decode.optional(user_lookup_decoder()))
        decode.success(user)
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

pub fn set_user_acls(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  acls: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/set-acls", [
    #("user_id", json.string(user_id)),
    #("acls", json.array(acls, json.string)),
  ])
}

pub fn clear_user_fields(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  fields: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/clear-fields", [
    #("user_id", json.string(user_id)),
    #("fields", json.array(fields, json.string)),
  ])
}

pub fn set_bot_status(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  bot: Bool,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/set-bot-status", [
    #("user_id", json.string(user_id)),
    #("bot", json.bool(bot)),
  ])
}

pub fn set_system_status(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  system: Bool,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/set-system-status", [
    #("user_id", json.string(user_id)),
    #("system", json.bool(system)),
  ])
}

pub fn change_username(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  username: String,
  discriminator: Option(Int),
) -> Result(Nil, ApiError) {
  let fields = case discriminator {
    option.Some(disc) -> [
      #("user_id", json.string(user_id)),
      #("username", json.string(username)),
      #("discriminator", json.int(disc)),
    ]
    option.None -> [
      #("user_id", json.string(user_id)),
      #("username", json.string(username)),
    ]
  }
  admin_post_simple(ctx, session, "/admin/users/change-username", fields)
}

pub fn change_dob(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  date_of_birth: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/users/change-dob", [
    #("user_id", json.string(user_id)),
    #("date_of_birth", json.string(date_of_birth)),
  ])
}

pub fn list_user_sessions(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
) -> Result(ListUserSessionsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/list-sessions"
  let body = json.object([#("user_id", json.string(user_id))]) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let session_decoder = {
        use session_id_hash <- decode.field("session_id_hash", decode.string)
        use created_at <- decode.field("created_at", decode.string)
        use approx_last_used_at <- decode.field(
          "approx_last_used_at",
          decode.string,
        )
        use client_ip <- decode.field("client_ip", decode.string)
        use client_os <- decode.field("client_os", decode.string)
        use client_platform <- decode.field("client_platform", decode.string)
        use client_location <- decode.field("client_location", decode.optional(decode.string))
        decode.success(UserSession(
          session_id_hash: session_id_hash,
          created_at: created_at,
          approx_last_used_at: approx_last_used_at,
          client_ip: client_ip,
          client_os: client_os,
          client_platform: client_platform,
          client_location: client_location,
        ))
      }

      let decoder = {
        use sessions <- decode.field("sessions", decode.list(session_decoder))
        decode.success(ListUserSessionsResponse(sessions: sessions))
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

pub fn search_users(
  ctx: web.Context,
  session: web.Session,
  query: String,
  limit: Int,
  offset: Int,
) -> Result(SearchUsersResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/users/search"
  let body =
    json.object([
      #("query", json.string(query)),
      #("limit", json.int(limit)),
      #("offset", json.int(offset)),
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
        use users <- decode.field("users", decode.list(user_lookup_decoder()))
        use total <- decode.field("total", decode.int)
        decode.success(SearchUsersResponse(users: users, total: total))
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
