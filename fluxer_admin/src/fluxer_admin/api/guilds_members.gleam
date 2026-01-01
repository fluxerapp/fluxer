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

pub type GuildMember {
  GuildMember(
    user: GuildMemberUser,
    nick: option.Option(String),
    avatar: option.Option(String),
    roles: List(String),
    joined_at: String,
    premium_since: option.Option(String),
    deaf: Bool,
    mute: Bool,
    flags: Int,
    pending: Bool,
    communication_disabled_until: option.Option(String),
  )
}

pub type GuildMemberUser {
  GuildMemberUser(
    id: String,
    username: String,
    discriminator: String,
    avatar: option.Option(String),
    bot: Bool,
    system: Bool,
    public_flags: Int,
  )
}

pub type ListGuildMembersResponse {
  ListGuildMembersResponse(
    members: List(GuildMember),
    total: Int,
    limit: Int,
    offset: Int,
  )
}

pub fn list_guild_members(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  limit: Int,
  offset: Int,
) -> Result(ListGuildMembersResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/guilds/list-members"
  let body =
    json.object([
      #("guild_id", json.string(guild_id)),
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
      let user_decoder = {
        use id <- decode.field("id", decode.string)
        use username <- decode.field("username", decode.string)
        use discriminator <- decode.field("discriminator", decode.string)
        use avatar <- decode.field("avatar", decode.optional(decode.string))
        use bot <- decode.optional_field("bot", False, decode.bool)
        use system <- decode.optional_field("system", False, decode.bool)
        use public_flags <- decode.optional_field("public_flags", 0, decode.int)
        decode.success(GuildMemberUser(
          id: id,
          username: username,
          discriminator: discriminator,
          avatar: avatar,
          bot: bot,
          system: system,
          public_flags: public_flags,
        ))
      }

      let member_decoder = {
        use user <- decode.field("user", user_decoder)
        use nick <- decode.optional_field(
          "nick",
          option.None,
          decode.optional(decode.string),
        )
        use avatar <- decode.optional_field(
          "avatar",
          option.None,
          decode.optional(decode.string),
        )
        use roles <- decode.field("roles", decode.list(decode.string))
        use joined_at <- decode.field("joined_at", decode.string)
        use premium_since <- decode.optional_field(
          "premium_since",
          option.None,
          decode.optional(decode.string),
        )
        use deaf <- decode.optional_field("deaf", False, decode.bool)
        use mute <- decode.optional_field("mute", False, decode.bool)
        use flags <- decode.optional_field("flags", 0, decode.int)
        use pending <- decode.optional_field("pending", False, decode.bool)
        use communication_disabled_until <- decode.optional_field(
          "communication_disabled_until",
          option.None,
          decode.optional(decode.string),
        )
        decode.success(GuildMember(
          user: user,
          nick: nick,
          avatar: avatar,
          roles: roles,
          joined_at: joined_at,
          premium_since: premium_since,
          deaf: deaf,
          mute: mute,
          flags: flags,
          pending: pending,
          communication_disabled_until: communication_disabled_until,
        ))
      }

      let decoder = {
        use members <- decode.field("members", decode.list(member_decoder))
        use total <- decode.field("total", decode.int)
        use limit <- decode.field("limit", decode.int)
        use offset <- decode.field("offset", decode.int)
        decode.success(ListGuildMembersResponse(
          members: members,
          total: total,
          limit: limit,
          offset: offset,
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
