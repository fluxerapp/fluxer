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
import fluxer_admin/api/bulk
import fluxer_admin/api/common
import fluxer_admin/components/deletion_days_script
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session}
import gleam/int
import gleam/list
import gleam/option
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
  result: option.Option(bulk.BulkOperationResponse),
) -> Response {
  let content =
    h.div([a.class("space-y-6")], [
      h.div([a.class("mb-6")], [ui.heading_page("Bulk Actions")]),
      case result {
        option.Some(response) -> render_result(response)
        option.None -> element.none()
      },
      case acl.has_permission(admin_acls, "bulk:update_user_flags") {
        True -> render_bulk_update_user_flags()
        False -> element.none()
      },
      case acl.has_permission(admin_acls, "bulk:update_guild_features") {
        True -> render_bulk_update_guild_features()
        False -> element.none()
      },
      case acl.has_permission(admin_acls, "bulk:add_guild_members") {
        True -> render_bulk_add_guild_members()
        False -> element.none()
      },
      case acl.has_permission(admin_acls, "bulk:delete_users") {
        True -> render_bulk_schedule_user_deletion()
        False -> element.none()
      },
    ])

  let html =
    layout.page(
      "Bulk Actions",
      "bulk-actions",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_custom_checkbox(
  name: String,
  value: String,
  label: String,
) -> element.Element(a) {
  ui.custom_checkbox(name, value, label, False, option.None)
}

fn render_result(response: bulk.BulkOperationResponse) {
  let success_count = list.length(response.successful)
  let fail_count = list.length(response.failed)

  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6 mb-6")], [
    ui.heading_card_with_margin("Operation Result"),
    h.div([a.class("space-y-3")], [
      h.div([a.class("text-sm")], [
        h.span([a.class("text-sm font-medium text-green-600")], [
          element.text("Successful: "),
        ]),
        element.text(int.to_string(success_count)),
      ]),
      h.div([a.class("text-sm")], [
        h.span([a.class("text-sm font-medium text-red-600")], [
          element.text("Failed: "),
        ]),
        element.text(int.to_string(fail_count)),
      ]),
      case list.is_empty(response.failed) {
        True -> element.none()
        False ->
          h.div([a.class("mt-4")], [
            h.h3([a.class("text-sm font-medium text-neutral-900 mb-2")], [
              element.text("Errors:"),
            ]),
            h.ul([a.class("space-y-1")], {
              list.map(response.failed, fn(error) {
                h.li([a.class("text-sm text-red-600")], [
                  element.text(error.id <> ": " <> error.error),
                ])
              })
            }),
          ])
      },
    ]),
  ])
}

fn render_bulk_update_user_flags() {
  let all_flags = constants.get_patchable_flags()

  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Bulk Update User Flags"),
    h.form(
      [
        a.method("POST"),
        a.action("?action=bulk-update-user-flags"),
        a.class("space-y-4"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("User IDs (one per line)"),
          ]),
          h.textarea(
            [
              a.name("user_ids"),
              a.placeholder("123456789\n987654321"),
              a.required(True),
              a.attribute("rows", "5"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ],
            "",
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Flags to Add"),
          ]),
          h.div([a.class("grid grid-cols-2 gap-3")], {
            list.map(all_flags, fn(flag) {
              render_custom_checkbox(
                "add_flags[]",
                int.to_string(flag.value),
                flag.name,
              )
            })
          }),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Flags to Remove"),
          ]),
          h.div([a.class("grid grid-cols-2 gap-3")], {
            list.map(all_flags, fn(flag) {
              render_custom_checkbox(
                "remove_flags[]",
                int.to_string(flag.value),
                flag.name,
              )
            })
          }),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Audit Log Reason (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("audit_log_reason"),
            a.placeholder("Reason for this bulk operation"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        ui.button(
          "Update User Flags",
          "submit",
          ui.Primary,
          ui.Medium,
          ui.Full,
          [],
        ),
      ],
    ),
  ])
}

fn render_bulk_update_guild_features() {
  let all_features = constants.get_guild_features()

  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Bulk Update Guild Features"),
    h.form(
      [
        a.method("POST"),
        a.action("?action=bulk-update-guild-features"),
        a.class("space-y-4"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Guild IDs (one per line)"),
          ]),
          h.textarea(
            [
              a.name("guild_ids"),
              a.placeholder("123456789\n987654321"),
              a.required(True),
              a.attribute("rows", "5"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ],
            "",
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Features to Add"),
          ]),
          h.div([a.class("grid grid-cols-2 gap-3")], {
            list.map(all_features, fn(feature) {
              render_custom_checkbox(
                "add_features[]",
                feature.value,
                feature.value,
              )
            })
          }),
          h.div([a.class("mt-3")], [
            h.label([a.class("text-xs text-neutral-600 mb-1 block")], [
              element.text("Custom features (comma-separated):"),
            ]),
            h.input([
              a.type_("text"),
              a.name("custom_add_features"),
              a.placeholder("CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ]),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Features to Remove"),
          ]),
          h.div([a.class("grid grid-cols-2 gap-3")], {
            list.map(all_features, fn(feature) {
              render_custom_checkbox(
                "remove_features[]",
                feature.value,
                feature.value,
              )
            })
          }),
          h.div([a.class("mt-3")], [
            h.label([a.class("text-xs text-neutral-600 mb-1 block")], [
              element.text("Custom features (comma-separated):"),
            ]),
            h.input([
              a.type_("text"),
              a.name("custom_remove_features"),
              a.placeholder("CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ]),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Audit Log Reason (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("audit_log_reason"),
            a.placeholder("Reason for this bulk operation"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        ui.button(
          "Update Guild Features",
          "submit",
          ui.Primary,
          ui.Medium,
          ui.Full,
          [],
        ),
      ],
    ),
  ])
}

fn render_bulk_add_guild_members() {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Bulk Add Guild Members"),
    h.form(
      [
        a.method("POST"),
        a.action("?action=bulk-add-guild-members"),
        a.class("space-y-4"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Guild ID"),
          ]),
          h.input([
            a.type_("text"),
            a.name("guild_id"),
            a.placeholder("123456789"),
            a.required(True),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("User IDs (one per line)"),
          ]),
          h.textarea(
            [
              a.name("user_ids"),
              a.placeholder("123456789\n987654321"),
              a.required(True),
              a.attribute("rows", "5"),
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
            a.placeholder("Reason for this bulk operation"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        h.button(
          [
            a.type_("submit"),
            a.class(
              "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Add Members")],
        ),
      ],
    ),
  ])
}

fn render_bulk_schedule_user_deletion() {
  let deletion_reasons = constants.get_deletion_reasons()

  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Bulk Schedule User Deletion"),
    h.form(
      [
        a.method("POST"),
        a.action("?action=bulk-schedule-user-deletion"),
        a.class("space-y-4"),
        a.attribute(
          "onsubmit",
          "return confirm('Are you sure you want to schedule these users for deletion?')",
        ),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("User IDs (one per line)"),
          ]),
          h.textarea(
            [
              a.name("user_ids"),
              a.placeholder("123456789\n987654321"),
              a.required(True),
              a.attribute("rows", "5"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ],
            "",
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Deletion Reason"),
          ]),
          h.select(
            [
              a.id("bulk-deletion-reason"),
              a.name("reason_code"),
              a.required(True),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
              ),
            ],
            list.map(deletion_reasons, fn(reason) {
              h.option([a.value(int.to_string(reason.0))], reason.1)
            }),
          ),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Public Reason (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("public_reason"),
            a.placeholder("Terms of service violation"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Days Until Deletion"),
          ]),
          h.input([
            a.type_("number"),
            a.id("bulk-deletion-days"),
            a.name("days_until_deletion"),
            a.value("14"),
            a.min("14"),
            a.required(True),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        h.div([a.class("space-y-2")], [
          h.label([a.class("text-sm font-medium text-neutral-700 mb-2 block")], [
            element.text("Audit Log Reason (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("audit_log_reason"),
            a.placeholder("Reason for this bulk operation"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
            ),
          ]),
        ]),
        ui.button(
          "Schedule Deletion",
          "submit",
          ui.Danger,
          ui.Medium,
          ui.Full,
          [],
        ),
        deletion_days_script.render(),
      ],
    ),
  ])
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  admin_acls: List(String),
  action: option.Option(String),
) -> Response {
  use form_data <- wisp.require_form(req)

  case action {
    option.Some("bulk-update-user-flags") -> {
      let user_ids_text =
        list.key_find(form_data.values, "user_ids") |> option.from_result
      let add_flags =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "add_flags[]" -> Ok(field.1)
            _ -> Error(Nil)
          }
        })
      let remove_flags =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "remove_flags[]" -> Ok(field.1)
            _ -> Error(Nil)
          }
        })
      let audit_log_reason =
        list.key_find(form_data.values, "audit_log_reason")
        |> option.from_result

      case user_ids_text {
        option.Some(text) -> {
          let user_ids =
            string.split(text, "\n")
            |> list.map(string.trim)
            |> list.filter(fn(id) { !string.is_empty(id) })

          case
            bulk.bulk_update_user_flags(
              ctx,
              session,
              user_ids,
              add_flags,
              remove_flags,
              audit_log_reason,
            )
          {
            Ok(result) ->
              view(
                ctx,
                session,
                current_admin,
                option.None,
                admin_acls,
                option.Some(result),
              )
            Error(_) ->
              wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
          }
        }
        option.None ->
          wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
      }
    }

    option.Some("bulk-update-guild-features") -> {
      let guild_ids_text =
        list.key_find(form_data.values, "guild_ids") |> option.from_result
      let add_features =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "add_features[]" -> Ok(field.1)
            _ -> Error(Nil)
          }
        })
      let remove_features =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "remove_features[]" -> Ok(field.1)
            _ -> Error(Nil)
          }
        })

      let custom_add_features =
        list.key_find(form_data.values, "custom_add_features")
        |> option.from_result
        |> option.unwrap("")
        |> string.split(",")
        |> list.map(string.trim)
        |> list.filter(fn(s) { !string.is_empty(s) })

      let custom_remove_features =
        list.key_find(form_data.values, "custom_remove_features")
        |> option.from_result
        |> option.unwrap("")
        |> string.split(",")
        |> list.map(string.trim)
        |> list.filter(fn(s) { !string.is_empty(s) })

      let add_features = list.append(add_features, custom_add_features)
      let remove_features = list.append(remove_features, custom_remove_features)

      let audit_log_reason =
        list.key_find(form_data.values, "audit_log_reason")
        |> option.from_result

      case guild_ids_text {
        option.Some(text) -> {
          let guild_ids =
            string.split(text, "\n")
            |> list.map(string.trim)
            |> list.filter(fn(id) { !string.is_empty(id) })

          case
            bulk.bulk_update_guild_features(
              ctx,
              session,
              guild_ids,
              add_features,
              remove_features,
              audit_log_reason,
            )
          {
            Ok(result) ->
              view(
                ctx,
                session,
                current_admin,
                option.None,
                admin_acls,
                option.Some(result),
              )
            Error(_) ->
              wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
          }
        }
        option.None ->
          wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
      }
    }

    option.Some("bulk-add-guild-members") -> {
      let guild_id =
        list.key_find(form_data.values, "guild_id") |> option.from_result
      let user_ids_text =
        list.key_find(form_data.values, "user_ids") |> option.from_result
      let audit_log_reason =
        list.key_find(form_data.values, "audit_log_reason")
        |> option.from_result

      case guild_id, user_ids_text {
        option.Some(gid), option.Some(text) -> {
          let user_ids =
            string.split(text, "\n")
            |> list.map(string.trim)
            |> list.filter(fn(id) { !string.is_empty(id) })

          case
            bulk.bulk_add_guild_members(
              ctx,
              session,
              gid,
              user_ids,
              audit_log_reason,
            )
          {
            Ok(result) ->
              view(
                ctx,
                session,
                current_admin,
                option.None,
                admin_acls,
                option.Some(result),
              )
            Error(_) ->
              wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
          }
        }
        _, _ -> wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
      }
    }

    option.Some("bulk-schedule-user-deletion") -> {
      let user_ids_text =
        list.key_find(form_data.values, "user_ids") |> option.from_result
      let reason_code =
        list.key_find(form_data.values, "reason_code")
        |> option.from_result
        |> option.then(fn(s) { int.parse(s) |> option.from_result })
      let public_reason =
        list.key_find(form_data.values, "public_reason") |> option.from_result
      let days_until_deletion =
        list.key_find(form_data.values, "days_until_deletion")
        |> option.from_result
        |> option.then(fn(s) { int.parse(s) |> option.from_result })
        |> option.unwrap(30)
      let audit_log_reason =
        list.key_find(form_data.values, "audit_log_reason")
        |> option.from_result

      case user_ids_text, reason_code {
        option.Some(text), option.Some(code) -> {
          let user_ids =
            string.split(text, "\n")
            |> list.map(string.trim)
            |> list.filter(fn(id) { !string.is_empty(id) })

          case
            bulk.bulk_schedule_user_deletion(
              ctx,
              session,
              user_ids,
              code,
              public_reason,
              days_until_deletion,
              audit_log_reason,
            )
          {
            Ok(result) ->
              view(
                ctx,
                session,
                current_admin,
                option.None,
                admin_acls,
                option.Some(result),
              )
            Error(_) ->
              wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
          }
        }
        _, _ -> wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
      }
    }

    _ -> wisp.redirect(web.prepend_base_path(ctx, "/bulk-actions"))
  }
}
