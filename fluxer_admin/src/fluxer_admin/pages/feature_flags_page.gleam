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
import fluxer_admin/api/feature_flags
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session, action}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  admin_acls: List(String),
) -> Response {
  let can_view = acl.has_permission(admin_acls, constants.acl_feature_flag_view)

  let content = case can_view {
    False ->
      errors.api_error_view(
        ctx,
        common.Forbidden("Access denied"),
        option.None,
        option.None,
      )
    True -> {
      case feature_flags.get_feature_flags(ctx, session) {
        Ok(entries) -> render_page(ctx, flash_data, entries)
        Error(err) -> errors.api_error_view(ctx, err, option.None, option.None)
      }
    }
  }

  let html =
    layout.page(
      "Feature Flags",
      "feature-flags",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )

  wisp.html_response(element.to_document_string(html), 200)
}

fn render_page(
  ctx: Context,
  flash_data: option.Option(flash.Flash),
  entries: List(#(String, feature_flags.FeatureFlagConfig)),
) -> element.Element(a) {
  let config_map = build_config_map(entries)

  h.div([a.class("space-y-6")], [
    ui.heading_page("Feature Flags"),
    flash.view(flash_data),
    h.div(
      [a.class("space-y-6")],
      list.map(constants.get_feature_flags(), fn(flag) {
        let guild_ids = case dict.get(config_map, flag.id) {
          Ok(ids) -> ids
          Error(_) -> []
        }

        render_flag_card(ctx, flag, guild_ids)
      }),
    ),
  ])
}

fn render_flag_card(
  ctx: Context,
  flag: constants.FeatureFlag,
  guild_ids: List(String),
) -> element.Element(a) {
  let guild_text = format_guild_list(guild_ids)

  h.div([a.class("space-y-4")], [
    ui.card(ui.PaddingMedium, [
      h.div([a.class("space-y-4")], [
        h.div([a.class("flex items-center justify-between")], [
          h.div([a.class("space-y-1")], [
            h.h3([a.class("text-lg font-semibold text-neutral-900")], [
              element.text(flag.name),
            ]),
            h.p([a.class("text-sm text-neutral-600")], [
              element.text(flag.description),
            ]),
          ]),
          h.span([a.class("text-xs uppercase tracking-wide text-neutral-500")], [
            element.text(
              "Guilds "
              <> int.to_string(list.length(guild_ids))
              <> " configured",
            ),
          ]),
        ]),
        h.form(
          [
            a.method("POST"),
            action(ctx, "/feature-flags?action=update"),
            a.class("space-y-4"),
          ],
          [
            h.input([
              a.type_("hidden"),
              a.name("flag_id"),
              a.value(flag.id),
            ]),
            h.div([a.class("space-y-1")], [
              h.label([a.class("text-sm font-medium text-neutral-700")], [
                element.text("Guild IDs"),
              ]),
              h.textarea(
                [
                  a.name("guild_ids"),
                  a.attribute("rows", "3"),
                  a.class(
                    "w-full border border-neutral-300 rounded text-sm px-3 py-2",
                  ),
                ],
                guild_text,
              ),
              h.p([a.class("text-xs text-neutral-500")], [
                element.text(
                  "Comma-separated guild IDs that receive this feature.",
                ),
              ]),
            ]),
            h.div([a.class("text-right")], [
              ui.button_primary("Save", "submit", []),
            ]),
          ],
        ),
      ]),
    ]),
  ])
}

fn build_config_map(
  entries: List(#(String, feature_flags.FeatureFlagConfig)),
) -> dict.Dict(String, List(String)) {
  list.fold(entries, dict.new(), fn(acc, entry) {
    let #(flag, config) = entry
    dict.insert(acc, flag, config.guild_ids)
  })
}

fn format_guild_list(ids: List(String)) -> String {
  case ids {
    [] -> ""
    [first, ..rest] -> list.fold(rest, first, fn(acc, id) { acc <> ", " <> id })
  }
}

fn parse_guild_ids(raw: String) -> List(String) {
  list.flatten(
    list.map(string.split(string.replace(raw, "\r", ""), "\n"), fn(line) {
      list.filter(
        list.map(string.split(line, ","), fn(item) { string.trim(item) }),
        fn(item) { item != "" },
      )
    }),
  )
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  admin_acls: List(String),
  action_name: Result(String, Nil),
) -> Response {
  case acl.has_permission(admin_acls, constants.acl_feature_flag_manage) {
    False -> flash.redirect_with_error(ctx, "/feature-flags", "Access denied")
    True ->
      case action_name {
        Ok("update") -> handle_update(req, ctx, session)
        _ -> flash.redirect_with_error(ctx, "/feature-flags", "Unknown action")
      }
  }
}

fn handle_update(req: Request, ctx: Context, session: Session) -> Response {
  use form_data <- wisp.require_form(req)

  let flag_id =
    list.key_find(form_data.values, "flag_id")
    |> result.unwrap("")

  let guild_input =
    list.key_find(form_data.values, "guild_ids")
    |> result.unwrap("")

  let guild_ids = parse_guild_ids(guild_input)

  case feature_flags.update_feature_flag(ctx, session, flag_id, guild_ids) {
    Ok(_) ->
      flash.redirect_with_success(ctx, "/feature-flags", "Feature flag updated")
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        "/feature-flags",
        "Failed to update feature flag",
      )
  }
}
