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

import fluxer_admin/api/assets
import fluxer_admin/api/guilds
import fluxer_admin/api/search
import fluxer_admin/components/flash
import fluxer_admin/web.{type Context, type Session}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import wisp.{type Request, type Response}

pub fn handle_clear_fields(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let fields = case list.key_find(form_data.values, "fields") {
    Ok(value) -> [value]
    Error(_) -> []
  }

  case guilds.clear_guild_fields(ctx, session, guild_id, fields) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild fields cleared successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to clear guild fields",
      )
  }
}

pub fn handle_update_features(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let guild_result = guilds.lookup_guild(ctx, session, guild_id)

  case guild_result {
    Error(_) -> flash.redirect_with_error(ctx, redirect_url, "Guild not found")
    Ok(option.None) ->
      flash.redirect_with_error(ctx, redirect_url, "Guild not found")
    Ok(option.Some(current_guild)) -> {
      let submitted_features =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "features[]" -> Ok(field.1)
            _ -> Error(Nil)
          }
        })

      let custom_features_input =
        list.key_find(form_data.values, "custom_features")
        |> result.unwrap("")

      let custom_features =
        string.split(custom_features_input, ",")
        |> list.map(string.trim)
        |> list.filter(fn(s) { s != "" })

      let submitted_features = list.append(submitted_features, custom_features)

      let submitted_features = case
        list.contains(submitted_features, "UNAVAILABLE_FOR_EVERYONE")
        && list.contains(
          submitted_features,
          "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF",
        )
      {
        True ->
          list.filter(submitted_features, fn(f) {
            f != "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF"
          })
        False -> submitted_features
      }

      let add_features =
        list.filter(submitted_features, fn(feature) {
          !list.contains(current_guild.features, feature)
        })

      let remove_features =
        list.filter(current_guild.features, fn(feature) {
          !list.contains(submitted_features, feature)
        })

      case
        guilds.update_guild_features(
          ctx,
          session,
          guild_id,
          add_features,
          remove_features,
        )
      {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "Guild features updated successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "Failed to update guild features",
          )
      }
    }
  }
}

pub fn handle_update_disabled_operations(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let checked_ops =
    list.filter_map(form_data.values, fn(field) {
      case field.0 {
        "disabled_operations[]" -> Ok(field.1)
        _ -> Error(Nil)
      }
    })

  let disabled_ops_value =
    list.fold(checked_ops, 0, fn(acc, op_str) {
      case int.parse(op_str) {
        Ok(val) -> int.bitwise_or(acc, val)
        Error(_) -> acc
      }
    })

  case
    guilds.update_guild_settings(
      ctx,
      session,
      guild_id,
      option.None,
      option.None,
      option.None,
      option.None,
      option.None,
      option.Some(disabled_ops_value),
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Disabled operations updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update disabled operations",
      )
  }
}

pub fn handle_update_name(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let name = list.key_find(form_data.values, "name") |> result.unwrap("")

  case guilds.update_guild_name(ctx, session, guild_id, name) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild name updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update guild name",
      )
  }
}

pub fn handle_update_vanity(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let vanity = case list.key_find(form_data.values, "vanity_url_code") {
    Ok("") -> option.None
    Ok(code) -> option.Some(code)
    Error(_) -> option.None
  }

  case guilds.update_guild_vanity(ctx, session, guild_id, vanity) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Vanity URL updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update vanity URL",
      )
  }
}

pub fn handle_transfer_ownership(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let new_owner_id =
    list.key_find(form_data.values, "new_owner_id") |> result.unwrap("")

  case guilds.transfer_guild_ownership(ctx, session, guild_id, new_owner_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild ownership transferred successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to transfer guild ownership",
      )
  }
}

pub fn handle_reload(
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  case guilds.reload_guild(ctx, session, guild_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild reloaded successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to reload guild")
  }
}

pub fn handle_shutdown(
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  case guilds.shutdown_guild(ctx, session, guild_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild shutdown successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to shutdown guild")
  }
}

pub fn handle_delete_guild(
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  case guilds.delete_guild(ctx, session, guild_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild deleted successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to delete guild")
  }
}

pub fn handle_update_settings(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let verification_level =
    list.key_find(form_data.values, "verification_level")
    |> result.try(int.parse)
    |> option.from_result

  let mfa_level =
    list.key_find(form_data.values, "mfa_level")
    |> result.try(int.parse)
    |> option.from_result

  let nsfw_level =
    list.key_find(form_data.values, "nsfw_level")
    |> result.try(int.parse)
    |> option.from_result

  let explicit_content_filter =
    list.key_find(form_data.values, "explicit_content_filter")
    |> result.try(int.parse)
    |> option.from_result

  let default_message_notifications =
    list.key_find(form_data.values, "default_message_notifications")
    |> result.try(int.parse)
    |> option.from_result

  case
    guilds.update_guild_settings(
      ctx,
      session,
      guild_id,
      verification_level,
      mfa_level,
      nsfw_level,
      explicit_content_filter,
      default_message_notifications,
      option.None,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Guild settings updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update guild settings",
      )
  }
}

pub fn handle_force_add_user(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let user_id = list.key_find(form_data.values, "user_id") |> result.unwrap("")

  case guilds.force_add_user_to_guild(ctx, session, user_id, guild_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "User added to guild successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to add user to guild",
      )
  }
}

pub fn handle_refresh_search_index(
  req: Request,
  ctx: Context,
  session: Session,
  guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let index_type =
    list.key_find(form_data.values, "index_type") |> result.unwrap("")

  case
    search.refresh_search_index_with_guild(
      ctx,
      session,
      index_type,
      option.Some(guild_id),
      option.None,
    )
  {
    Ok(response) ->
      flash.redirect_with_success(
        ctx,
        "/search-index?job_id=" <> response.job_id,
        "Search index refresh started successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to start search index refresh",
      )
  }
}

pub fn handle_delete_emoji(
  req: Request,
  ctx: Context,
  session: Session,
  _guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let emoji_id =
    list.key_find(form_data.values, "emoji_id")
    |> result.unwrap("")
    |> string.trim

  case emoji_id {
    "" -> flash.redirect_with_error(ctx, redirect_url, "Emoji ID is required.")
    _ -> handle_delete_asset(ctx, session, redirect_url, emoji_id, "Emoji")
  }
}

pub fn handle_delete_sticker(
  req: Request,
  ctx: Context,
  session: Session,
  _guild_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let sticker_id =
    list.key_find(form_data.values, "sticker_id")
    |> result.unwrap("")
    |> string.trim

  case sticker_id {
    "" ->
      flash.redirect_with_error(ctx, redirect_url, "Sticker ID is required.")
    _ -> handle_delete_asset(ctx, session, redirect_url, sticker_id, "Sticker")
  }
}

fn handle_delete_asset(
  ctx: Context,
  session: Session,
  redirect_url: String,
  asset_id: String,
  asset_label: String,
) -> Response {
  case assets.purge_assets(ctx, session, [asset_id], option.None) {
    Ok(response) -> {
      case list.find(response.errors, fn(err) { err.id == asset_id }) {
        Ok(err) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            asset_label <> " deletion failed: " <> err.error,
          )
        Error(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            asset_label <> " deleted successfully.",
          )
      }
    }
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        asset_label <> " deletion failed.",
      )
  }
}
