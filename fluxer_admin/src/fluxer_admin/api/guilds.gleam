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
  admin_post_simple,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option

pub type GuildChannel {
  GuildChannel(
    id: String,
    name: String,
    type_: Int,
    position: Int,
    parent_id: option.Option(String),
  )
}

pub type GuildRole {
  GuildRole(
    id: String,
    name: String,
    color: Int,
    position: Int,
    permissions: String,
    hoist: Bool,
    mentionable: Bool,
  )
}

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

pub type GuildLookupResult {
  GuildLookupResult(
    id: String,
    owner_id: String,
    name: String,
    vanity_url_code: option.Option(String),
    icon: option.Option(String),
    banner: option.Option(String),
    splash: option.Option(String),
    features: List(String),
    verification_level: Int,
    mfa_level: Int,
    nsfw_level: Int,
    explicit_content_filter: Int,
    default_message_notifications: Int,
    afk_channel_id: option.Option(String),
    afk_timeout: Int,
    system_channel_id: option.Option(String),
    system_channel_flags: Int,
    rules_channel_id: option.Option(String),
    disabled_operations: Int,
    member_count: Int,
    channels: List(GuildChannel),
    roles: List(GuildRole),
  )
}

pub type GuildSearchResult {
  GuildSearchResult(
    id: String,
    owner_id: String,
    name: String,
    features: List(String),
    icon: option.Option(String),
    banner: option.Option(String),
    member_count: Int,
  )
}

pub type SearchGuildsResponse {
  SearchGuildsResponse(guilds: List(GuildSearchResult), total: Int)
}

pub fn lookup_guild(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
) -> Result(option.Option(GuildLookupResult), ApiError) {
  let url = ctx.api_endpoint <> "/admin/guilds/lookup"
  let body =
    json.object([#("guild_id", json.string(guild_id))]) |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let channel_decoder = {
        use id <- decode.field("id", decode.string)
        use name <- decode.field("name", decode.string)
        use type_ <- decode.field("type", decode.int)
        use position <- decode.field("position", decode.int)
        use parent_id <- decode.field(
          "parent_id",
          decode.optional(decode.string),
        )
        decode.success(GuildChannel(
          id: id,
          name: name,
          type_: type_,
          position: position,
          parent_id: parent_id,
        ))
      }

      let role_decoder = {
        use id <- decode.field("id", decode.string)
        use name <- decode.field("name", decode.string)
        use color <- decode.field("color", decode.int)
        use position <- decode.field("position", decode.int)
        use permissions <- decode.field("permissions", decode.string)
        use hoist <- decode.field("hoist", decode.bool)
        use mentionable <- decode.field("mentionable", decode.bool)
        decode.success(GuildRole(
          id: id,
          name: name,
          color: color,
          position: position,
          permissions: permissions,
          hoist: hoist,
          mentionable: mentionable,
        ))
      }

      let guild_decoder = {
        use id <- decode.field("id", decode.string)
        use owner_id <- decode.field("owner_id", decode.string)
        use name <- decode.field("name", decode.string)
        use vanity_url_code <- decode.field(
          "vanity_url_code",
          decode.optional(decode.string),
        )
        use icon <- decode.field("icon", decode.optional(decode.string))
        use banner <- decode.field("banner", decode.optional(decode.string))
        use splash <- decode.field("splash", decode.optional(decode.string))
        use features <- decode.field("features", decode.list(decode.string))
        use verification_level <- decode.field("verification_level", decode.int)
        use mfa_level <- decode.field("mfa_level", decode.int)
        use nsfw_level <- decode.field("nsfw_level", decode.int)
        use explicit_content_filter <- decode.field(
          "explicit_content_filter",
          decode.int,
        )
        use default_message_notifications <- decode.field(
          "default_message_notifications",
          decode.int,
        )
        use afk_channel_id <- decode.field(
          "afk_channel_id",
          decode.optional(decode.string),
        )
        use afk_timeout <- decode.field("afk_timeout", decode.int)
        use system_channel_id <- decode.field(
          "system_channel_id",
          decode.optional(decode.string),
        )
        use system_channel_flags <- decode.field(
          "system_channel_flags",
          decode.int,
        )
        use rules_channel_id <- decode.field(
          "rules_channel_id",
          decode.optional(decode.string),
        )
        use disabled_operations <- decode.field(
          "disabled_operations",
          decode.int,
        )
        use member_count <- decode.field("member_count", decode.int)
        use channels <- decode.field("channels", decode.list(channel_decoder))
        use roles <- decode.field("roles", decode.list(role_decoder))
        decode.success(GuildLookupResult(
          id: id,
          owner_id: owner_id,
          name: name,
          vanity_url_code: vanity_url_code,
          icon: icon,
          banner: banner,
          splash: splash,
          features: features,
          verification_level: verification_level,
          mfa_level: mfa_level,
          nsfw_level: nsfw_level,
          explicit_content_filter: explicit_content_filter,
          default_message_notifications: default_message_notifications,
          afk_channel_id: afk_channel_id,
          afk_timeout: afk_timeout,
          system_channel_id: system_channel_id,
          system_channel_flags: system_channel_flags,
          rules_channel_id: rules_channel_id,
          disabled_operations: disabled_operations,
          member_count: member_count,
          channels: channels,
          roles: roles,
        ))
      }

      let decoder = {
        use guild <- decode.field("guild", decode.optional(guild_decoder))
        decode.success(guild)
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

pub fn clear_guild_fields(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  fields: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/clear-fields", [
    #("guild_id", json.string(guild_id)),
    #("fields", json.array(fields, json.string)),
  ])
}

pub fn update_guild_features(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  add_features: List(String),
  remove_features: List(String),
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/update-features", [
    #("guild_id", json.string(guild_id)),
    #("add_features", json.array(add_features, json.string)),
    #("remove_features", json.array(remove_features, json.string)),
  ])
}

pub fn update_guild_settings(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  verification_level: option.Option(Int),
  mfa_level: option.Option(Int),
  nsfw_level: option.Option(Int),
  explicit_content_filter: option.Option(Int),
  default_message_notifications: option.Option(Int),
  disabled_operations: option.Option(Int),
) -> Result(Nil, ApiError) {
  let mut_fields = [#("guild_id", json.string(guild_id))]
  let mut_fields = case verification_level {
    option.Some(vl) -> [#("verification_level", json.int(vl)), ..mut_fields]
    option.None -> mut_fields
  }
  let mut_fields = case mfa_level {
    option.Some(ml) -> [#("mfa_level", json.int(ml)), ..mut_fields]
    option.None -> mut_fields
  }
  let mut_fields = case nsfw_level {
    option.Some(nl) -> [#("nsfw_level", json.int(nl)), ..mut_fields]
    option.None -> mut_fields
  }
  let mut_fields = case explicit_content_filter {
    option.Some(ecf) -> [
      #("explicit_content_filter", json.int(ecf)),
      ..mut_fields
    ]
    option.None -> mut_fields
  }
  let mut_fields = case default_message_notifications {
    option.Some(dmn) -> [
      #("default_message_notifications", json.int(dmn)),
      ..mut_fields
    ]
    option.None -> mut_fields
  }
  let mut_fields = case disabled_operations {
    option.Some(dops) -> [
      #("disabled_operations", json.int(dops)),
      ..mut_fields
    ]
    option.None -> mut_fields
  }
  admin_post_simple(ctx, session, "/admin/guilds/update-settings", mut_fields)
}

pub fn update_guild_name(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  name: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/update-name", [
    #("guild_id", json.string(guild_id)),
    #("name", json.string(name)),
  ])
}

pub fn update_guild_vanity(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  vanity_url_code: option.Option(String),
) -> Result(Nil, ApiError) {
  let fields = [#("guild_id", json.string(guild_id))]
  let fields = case vanity_url_code {
    option.Some(code) -> [#("vanity_url_code", json.string(code)), ..fields]
    option.None -> fields
  }
  admin_post_simple(ctx, session, "/admin/guilds/update-vanity", fields)
}

pub fn transfer_guild_ownership(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
  new_owner_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/transfer-ownership", [
    #("guild_id", json.string(guild_id)),
    #("new_owner_id", json.string(new_owner_id)),
  ])
}

pub fn reload_guild(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/reload", [
    #("guild_id", json.string(guild_id)),
  ])
}

pub fn shutdown_guild(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/shutdown", [
    #("guild_id", json.string(guild_id)),
  ])
}

pub fn delete_guild(
  ctx: web.Context,
  session: web.Session,
  guild_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/delete", [
    #("guild_id", json.string(guild_id)),
  ])
}

pub fn force_add_user_to_guild(
  ctx: web.Context,
  session: web.Session,
  user_id: String,
  guild_id: String,
) -> Result(Nil, ApiError) {
  admin_post_simple(ctx, session, "/admin/guilds/force-add-user", [
    #("user_id", json.string(user_id)),
    #("guild_id", json.string(guild_id)),
  ])
}

pub fn search_guilds(
  ctx: web.Context,
  session: web.Session,
  query: String,
  limit: Int,
  offset: Int,
) -> Result(SearchGuildsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/admin/guilds/search"
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
        decode.success(GuildSearchResult(
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
        use total <- decode.field("total", decode.int)
        decode.success(SearchGuildsResponse(guilds: guilds, total: total))
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
