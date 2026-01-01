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
import fluxer_admin/api/assets
import fluxer_admin/api/common
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  ctx: web.Context,
  session: web.Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  result: option.Option(assets.AssetPurgeResponse),
) -> Response {
  let has_permission = case current_admin {
    option.Some(admin) ->
      acl.has_permission(admin.acls, constants.acl_asset_purge)
    option.None -> False
  }

  let content =
    h.div([a.class("space-y-6")], [
      h.div([a.class("mb-6")], [ui.heading_page("Asset Purge")]),
      h.div([a.class("text-sm text-neutral-600")], [
        element.text(
          "Purge emojis or stickers from the storage and CDN. Provide one or more IDs (comma-separated).",
        ),
      ]),
      case result {
        option.Some(response) -> render_result(response)
        option.None -> element.none()
      },
      case has_permission {
        True -> render_form()
        False -> render_permission_notice()
      },
    ])

  let html =
    layout.page(
      "Asset Purge",
      "asset-purge",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_form() {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Purge Assets"),
    h.p([a.class("text-sm text-neutral-500 mb-4")], [
      element.text(
        "Enter the emoji or sticker IDs that should be removed from S3 and CDN caches.",
      ),
    ]),
    h.form(
      [
        a.method("POST"),
        a.action("?action=purge-assets"),
        a.class("space-y-4"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("IDs (comma or newline separated)"),
          ]),
          h.textarea(
            [
              a.name("asset_ids"),
              a.required(True),
              a.placeholder("123456789012345678\n876543210987654321"),
              a.attribute("rows", "4"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ],
            "",
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Audit Log Reason (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("audit_log_reason"),
            a.placeholder("DMCA takedown request"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        ui.button("Purge Assets", "submit", ui.Danger, ui.Medium, ui.Full, []),
      ],
    ),
  ])
}

fn render_permission_notice() {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Permission required"),
    h.p([a.class("text-sm text-neutral-600")], [
      element.text("You need the asset:purge ACL to use this tool."),
    ]),
  ])
}

fn render_result(result: assets.AssetPurgeResponse) {
  h.div([a.class("space-y-4")], [
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin("Purge Result"),
      h.div([a.class("text-sm text-neutral-600 mb-4")], [
        element.text(
          "Processed "
          <> int.to_string(list.length(result.processed))
          <> " ID(s); "
          <> int.to_string(list.length(result.errors))
          <> " error(s).",
        ),
      ]),
      render_processed_table(result.processed),
      case list.is_empty(result.errors) {
        True -> element.none()
        False -> render_errors(result.errors)
      },
    ]),
  ])
}

fn render_processed_table(items: List(assets.AssetPurgeResult)) {
  h.div([a.class("overflow-x-auto border border-neutral-200 rounded-lg")], [
    h.table([a.class("min-w-full text-left text-sm text-neutral-700")], [
      h.thead([a.class("bg-neutral-50 text-xs uppercase text-neutral-500")], [
        h.tr([], [
          h.th([a.class("px-4 py-2 font-medium")], [element.text("ID")]),
          h.th([a.class("px-4 py-2 font-medium")], [element.text("Type")]),
          h.th([a.class("px-4 py-2 font-medium")], [element.text("In DB")]),
          h.th([a.class("px-4 py-2 font-medium")], [element.text("Guild ID")]),
        ]),
      ]),
      h.tbody([], {
        list.map(items, fn(item) {
          h.tr([a.class("border-t border-neutral-100")], [
            h.td([a.class("px-4 py-3 break-words")], [element.text(item.id)]),
            h.td([a.class("px-4 py-3")], [element.text(item.asset_type)]),
            h.td([a.class("px-4 py-3")], [
              element.text(case item.found_in_db {
                True -> "Yes"
                False -> "No"
              }),
            ]),
            h.td([a.class("px-4 py-3")], [
              element.text(option.unwrap(item.guild_id, "—")),
            ]),
          ])
        })
      }),
    ]),
  ])
}

fn render_errors(errors: List(assets.AssetPurgeError)) {
  h.div([a.class("mt-4 space-y-2")], {
    list.map(errors, fn(err) {
      h.div([a.class("text-sm text-red-600")], [
        element.text(err.id <> ": " <> err.error),
      ])
    })
  })
}

pub fn handle_action(
  req: Request,
  ctx: web.Context,
  session: web.Session,
  current_admin: option.Option(common.UserLookupResult),
) -> Response {
  use form_data <- wisp.require_form(req)

  let ids_input =
    list.key_find(form_data.values, "asset_ids")
    |> option.from_result
    |> option.unwrap("")

  let normalized =
    string.replace(ids_input, "\n", ",")
    |> string.replace("\r", ",")

  let ids =
    string.split(normalized, ",")
    |> list.map(string.trim)
    |> list.filter(fn(id) { !string.is_empty(id) })

  let audit_log_reason =
    list.key_find(form_data.values, "audit_log_reason")
    |> option.from_result

  case list.is_empty(ids) {
    True ->
      flash.redirect_with_error(ctx, "/asset-purge", "Provide at least one ID.")
    False ->
      case assets.purge_assets(ctx, session, ids, audit_log_reason) {
        Ok(response) ->
          view(ctx, session, current_admin, option.None, option.Some(response))
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            "/asset-purge",
            "Failed to purge assets.",
          )
      }
  }
}
