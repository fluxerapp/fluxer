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

import fluxer_admin/acl
import fluxer_admin/api/common
import fluxer_admin/api/instance_config
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/pages/instance_config_sections as sections
import fluxer_admin/web.{type Context, type Session}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
) -> Response {
  let result = instance_config.get_instance_config(ctx, session)
  let reservation_view_acl = case current_admin {
    option.Some(admin) ->
      acl.has_permission(
        admin.acls,
        constants.acl_instance_snowflake_reservation_view,
      )
    option.None -> False
  }
  let reservation_manage_acl = case current_admin {
    option.Some(admin) ->
      acl.has_permission(
        admin.acls,
        constants.acl_instance_snowflake_reservation_manage,
      )
    option.None -> False
  }

  let content = case result {
    Ok(config) -> {
      let base_children = [
        ui.heading_page("Instance Configuration"),
        sections.render_status_card(config),
        sections.render_config_form(ctx, config),
      ]

      case reservation_view_acl {
        True ->
          case instance_config.list_snowflake_reservations(ctx, session) {
            Ok(reservations) -> {
              let children =
                list.append(base_children, [
                  sections.render_snowflake_reservation_section(
                    ctx,
                    reservations,
                    reservation_manage_acl,
                  ),
                ])
              h.div([a.class("space-y-6")], children)
            }
            Error(err) -> errors.error_view(err)
          }
        False -> h.div([a.class("space-y-6")], base_children)
      }
    }
    Error(err) -> errors.error_view(err)
  }

  let html =
    layout.page(
      "Instance Configuration",
      "instance-config",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  action_name: String,
) -> Response {
  case action_name {
    "update" -> handle_update(req, ctx, session)
    "add-reservation" -> handle_add_snowflake_reservation(req, ctx, session)
    "delete-reservation" ->
      handle_delete_snowflake_reservation(req, ctx, session)
    _ -> flash.redirect_with_error(ctx, "/instance-config", "Unknown action")
  }
}

fn handle_update(req: Request, ctx: Context, session: Session) -> Response {
  use form_data <- wisp.require_form(req)

  let manual_review_enabled =
    list.key_find(form_data.values, "manual_review_enabled")
    |> result.map(fn(v) { v == "true" })
    |> result.unwrap(False)

  let schedule_enabled =
    list.key_find(form_data.values, "schedule_enabled")
    |> result.map(fn(v) { v == "true" })
    |> result.unwrap(False)

  let start_hour =
    list.key_find(form_data.values, "start_hour")
    |> result.try(int.parse)
    |> result.unwrap(0)

  let end_hour =
    list.key_find(form_data.values, "end_hour")
    |> result.try(int.parse)
    |> result.unwrap(23)

  let registration_alerts_webhook_url =
    list.key_find(form_data.values, "registration_alerts_webhook_url")
    |> result.unwrap("")

  let system_alerts_webhook_url =
    list.key_find(form_data.values, "system_alerts_webhook_url")
    |> result.unwrap("")

  case
    instance_config.update_instance_config(
      ctx,
      session,
      manual_review_enabled,
      schedule_enabled,
      start_hour,
      end_hour,
      registration_alerts_webhook_url,
      system_alerts_webhook_url,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        "/instance-config",
        "Configuration updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/instance-config",
        "Failed to update configuration",
      )
  }
}

fn handle_add_snowflake_reservation(
  req: Request,
  ctx: Context,
  session: Session,
) -> Response {
  use form_data <- wisp.require_form(req)

  let email =
    list.key_find(form_data.values, "reservation_email")
    |> result.unwrap("")

  let snowflake =
    list.key_find(form_data.values, "reservation_snowflake")
    |> result.unwrap("")

  case email == "" {
    True ->
      flash.redirect_with_error(
        ctx,
        "/instance-config",
        "Email and Snowflake ID are required.",
      )
    False ->
      case snowflake == "" {
        True ->
          flash.redirect_with_error(
            ctx,
            "/instance-config",
            "Email and Snowflake ID are required.",
          )
        False ->
          case
            instance_config.add_snowflake_reservation(
              ctx,
              session,
              email,
              snowflake,
            )
          {
            Ok(_) ->
              flash.redirect_with_success(
                ctx,
                "/instance-config",
                "Reservation added successfully.",
              )
            Error(_) ->
              flash.redirect_with_error(
                ctx,
                "/instance-config",
                "Failed to add reservation.",
              )
          }
      }
  }
}

fn handle_delete_snowflake_reservation(
  req: Request,
  ctx: Context,
  session: Session,
) -> Response {
  use form_data <- wisp.require_form(req)

  let email =
    list.key_find(form_data.values, "reservation_email")
    |> result.unwrap("")

  case email == "" {
    True ->
      flash.redirect_with_error(
        ctx,
        "/instance-config",
        "Reservation email is required.",
      )
    False ->
      case instance_config.delete_snowflake_reservation(ctx, session, email) {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            "/instance-config",
            "Reservation removed successfully.",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            "/instance-config",
            "Failed to remove reservation.",
          )
      }
  }
}
