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

import fluxer_admin/api/bans
import fluxer_admin/api/common
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session}
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub type BanType {
  IpBan
  EmailBan
  PhoneBan
}

type BanConfig {
  BanConfig(
    title: String,
    route: String,
    input_label: String,
    input_name: String,
    input_type: ui.InputType,
    placeholder: String,
    entity_name: String,
    active_page: String,
  )
}

fn get_config(ban_type: BanType) -> BanConfig {
  case ban_type {
    IpBan ->
      BanConfig(
        title: "IP Bans",
        route: "/ip-bans",
        input_label: "IP Address or CIDR",
        input_name: "ip",
        input_type: ui.Text,
        placeholder: "192.168.1.1 or 192.168.0.0/16",
        entity_name: "IP/CIDR",
        active_page: "ip-bans",
      )
    EmailBan ->
      BanConfig(
        title: "Email Bans",
        route: "/email-bans",
        input_label: "Email Address",
        input_name: "email",
        input_type: ui.Email,
        placeholder: "user@example.com",
        entity_name: "Email",
        active_page: "email-bans",
      )
    PhoneBan ->
      BanConfig(
        title: "Phone Bans",
        route: "/phone-bans",
        input_label: "Phone Number",
        input_name: "phone",
        input_type: ui.Tel,
        placeholder: "+1234567890",
        entity_name: "Phone",
        active_page: "phone-bans",
      )
  }
}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  ban_type: BanType,
) -> Response {
  let config = get_config(ban_type)

  let content =
    ui.stack("6", [
      ui.heading_page(config.title),
      ui.grid("1 md:grid-cols-2", "6", [
        ui.card(ui.PaddingMedium, [
          ui.stack("4", [
            ui.heading_card("Ban " <> config.input_label),
            h.form([a.method("POST"), a.action("?action=ban")], [
              ui.stack("4", [
                ui.input(
                  config.input_label,
                  config.input_name,
                  config.input_type,
                  option.None,
                  True,
                  option.Some(config.placeholder),
                ),
                ui.button(
                  "Ban " <> config.entity_name,
                  "submit",
                  ui.Primary,
                  ui.Medium,
                  ui.Full,
                  [],
                ),
              ]),
            ]),
          ]),
        ]),
        ui.card(ui.PaddingMedium, [
          ui.stack("4", [
            ui.heading_card("Check " <> config.input_label <> " Ban Status"),
            h.form([a.method("POST"), a.action("?action=check")], [
              ui.stack("4", [
                ui.input(
                  config.input_label,
                  config.input_name,
                  config.input_type,
                  option.None,
                  True,
                  option.Some(config.placeholder),
                ),
                ui.button(
                  "Check Status",
                  "submit",
                  ui.Primary,
                  ui.Medium,
                  ui.Full,
                  [],
                ),
              ]),
            ]),
          ]),
        ]),
      ]),
      ui.card(ui.PaddingMedium, [
        ui.stack("4", [
          ui.heading_card("Remove " <> config.input_label <> " Ban"),
          h.form([a.method("POST"), a.action("?action=unban")], [
            ui.stack("4", [
              ui.input(
                config.input_label,
                config.input_name,
                config.input_type,
                option.None,
                True,
                option.Some(config.placeholder),
              ),
              ui.button(
                "Unban " <> config.entity_name,
                "submit",
                ui.Danger,
                ui.Medium,
                ui.Full,
                [],
              ),
            ]),
          ]),
        ]),
      ]),
    ])

  let html =
    layout.page(
      config.title,
      config.active_page,
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
  ban_type: BanType,
  action: option.Option(String),
) -> Response {
  use form_data <- wisp.require_form(req)

  let config = get_config(ban_type)
  let value_result = list.key_find(form_data.values, config.input_name)

  case action, value_result {
    option.Some("ban"), Ok(value) -> {
      let result = case ban_type {
        IpBan -> bans.ban_ip(ctx, session, value)
        EmailBan -> bans.ban_email(ctx, session, value, option.None)
        PhoneBan -> bans.ban_phone(ctx, session, value)
      }

      case result {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            config.route,
            config.entity_name <> " " <> value <> " banned successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            config.route,
            "Failed to ban " <> config.entity_name <> " " <> value,
          )
      }
    }
    option.Some("unban"), Ok(value) -> {
      let result = case ban_type {
        IpBan -> bans.unban_ip(ctx, session, value)
        EmailBan -> bans.unban_email(ctx, session, value, option.None)
        PhoneBan -> bans.unban_phone(ctx, session, value)
      }

      case result {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            config.route,
            config.entity_name <> " " <> value <> " unbanned successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            config.route,
            "Failed to unban " <> config.entity_name <> " " <> value,
          )
      }
    }
    option.Some("check"), Ok(value) -> {
      let result = case ban_type {
        IpBan -> bans.check_ip_ban(ctx, session, value)
        EmailBan -> bans.check_email_ban(ctx, session, value)
        PhoneBan -> bans.check_phone_ban(ctx, session, value)
      }

      case result {
        Ok(response) if response.banned ->
          flash.redirect_with_info(
            ctx,
            config.route,
            config.entity_name <> " " <> value <> " is banned",
          )
        Ok(_) ->
          flash.redirect_with_info(
            ctx,
            config.route,
            config.entity_name <> " " <> value <> " is NOT banned",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            config.route,
            "Error checking ban status",
          )
      }
    }
    _, _ -> wisp.redirect(web.prepend_base_path(ctx, config.route))
  }
}
