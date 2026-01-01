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

import fluxer_admin/api/audit
import fluxer_admin/api/common
import fluxer_admin/components/date_time
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  query: option.Option(String),
  admin_user_id_filter: option.Option(String),
  target_type: option.Option(String),
  target_id: option.Option(String),
  action: option.Option(String),
  current_page: Int,
) -> Response {
  let limit = 50
  let offset = { current_page - 1 } * limit

  let result =
    audit.search_audit_logs(
      ctx,
      session,
      query,
      admin_user_id_filter,
      target_type,
      target_id,
      action,
      limit,
      offset,
    )

  let content = case result {
    Ok(response) -> {
      let total_pages = { response.total + limit - 1 } / limit

      h.div([a.class("max-w-7xl mx-auto")], [
        ui.flex_row_between([
          ui.heading_page("Audit Logs"),
          h.div([a.class("flex items-center gap-4")], [
            h.span([a.class("text-sm text-neutral-600")], [
              element.text(
                "Showing "
                <> int.to_string(list.length(response.logs))
                <> " of "
                <> int.to_string(response.total)
                <> " entries",
              ),
            ]),
          ]),
        ]),
        render_filters(
          ctx,
          query,
          admin_user_id_filter,
          target_type,
          target_id,
          action,
        ),
        case list.is_empty(response.logs) {
          True -> empty_state()
          False -> render_logs_table(ctx, response.logs)
        },
        case response.total > limit {
          True ->
            render_pagination(
              ctx,
              current_page,
              total_pages,
              query,
              admin_user_id_filter,
              target_type,
              target_id,
              action,
            )
          False -> element.none()
        },
      ])
    }
    Error(err) -> errors.api_error_view(ctx, err, option.None, option.None)
  }

  let html =
    layout.page(
      "Audit Logs",
      "audit-logs",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_filters(
  ctx: Context,
  query: option.Option(String),
  admin_user_id_filter: option.Option(String),
  target_type: option.Option(String),
  target_id: option.Option(String),
  action: option.Option(String),
) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4 mb-6")], [
    h.form([a.method("get"), a.class("space-y-4")], [
      h.div([a.class("w-full")], [
        h.label([a.class("block text-sm text-neutral-700 mb-2")], [
          element.text("Search"),
        ]),
        h.input([
          a.type_("text"),
          a.name("q"),
          a.value(option.unwrap(query, "")),
          a.placeholder("Search audit logs by action, reason, or metadata..."),
          a.class(
            "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
          ),
        ]),
      ]),
      h.div([a.class("grid grid-cols-1 md:grid-cols-4 gap-4")], [
        h.div([a.class("flex-1")], [
          h.label([a.class("block text-sm text-neutral-700 mb-2")], [
            element.text("Action"),
          ]),
          h.select(
            [
              a.name("action"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
              ),
            ],
            [
              h.option([a.value(""), a.selected(option.is_none(action))], "All"),
              h.option(
                [
                  a.value("temp_ban"),
                  a.selected(action == option.Some("temp_ban")),
                ],
                "Temp Ban",
              ),
              h.option(
                [a.value("unban"), a.selected(action == option.Some("unban"))],
                "Unban",
              ),
              h.option(
                [
                  a.value("schedule_deletion"),
                  a.selected(action == option.Some("schedule_deletion")),
                ],
                "Schedule Deletion",
              ),
              h.option(
                [
                  a.value("cancel_deletion"),
                  a.selected(action == option.Some("cancel_deletion")),
                ],
                "Cancel Deletion",
              ),
              h.option(
                [
                  a.value("update_flags"),
                  a.selected(action == option.Some("update_flags")),
                ],
                "Update Flags",
              ),
              h.option(
                [
                  a.value("update_features"),
                  a.selected(action == option.Some("update_features")),
                ],
                "Update Features",
              ),
              h.option(
                [
                  a.value("delete_message"),
                  a.selected(action == option.Some("delete_message")),
                ],
                "Delete Message",
              ),
              h.option(
                [a.value("ban_ip"), a.selected(action == option.Some("ban_ip"))],
                "Ban IP",
              ),
              h.option(
                [
                  a.value("ban_email"),
                  a.selected(action == option.Some("ban_email")),
                ],
                "Ban Email",
              ),
              h.option(
                [
                  a.value("ban_phone"),
                  a.selected(action == option.Some("ban_phone")),
                ],
                "Ban Phone",
              ),
            ],
          ),
        ]),
        h.div([a.class("flex-1")], [
          h.label([a.class("block text-sm text-neutral-700 mb-2")], [
            element.text("Target Type"),
          ]),
          h.select(
            [
              a.name("target_type"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
              ),
            ],
            [
              h.option([a.value("")], "All types"),
              h.option(
                [
                  a.value("user"),
                  a.selected(target_type == option.Some("user")),
                ],
                "User",
              ),
              h.option(
                [
                  a.value("guild"),
                  a.selected(target_type == option.Some("guild")),
                ],
                "Guild",
              ),
              h.option(
                [
                  a.value("message"),
                  a.selected(target_type == option.Some("message")),
                ],
                "Message",
              ),
              h.option(
                [a.value("ip"), a.selected(target_type == option.Some("ip"))],
                "IP",
              ),
              h.option(
                [
                  a.value("email"),
                  a.selected(target_type == option.Some("email")),
                ],
                "Email",
              ),
              h.option(
                [
                  a.value("phone"),
                  a.selected(target_type == option.Some("phone")),
                ],
                "Phone",
              ),
            ],
          ),
        ]),
        h.div([a.class("flex-1")], [
          h.label([a.class("block text-sm text-neutral-700 mb-2")], [
            element.text("Target ID"),
          ]),
          h.input([
            a.type_("text"),
            a.name("target_id"),
            a.value(option.unwrap(target_id, "")),
            a.placeholder("Filter by target ID..."),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
            ),
          ]),
        ]),
        h.div([a.class("flex-1")], [
          h.label([a.class("block text-sm text-neutral-700 mb-2")], [
            element.text("Admin User ID (optional)"),
          ]),
          h.input([
            a.type_("text"),
            a.name("admin_user_id"),
            a.value(option.unwrap(admin_user_id_filter, "")),
            a.placeholder("Specific admin user ID..."),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
            ),
          ]),
        ]),
      ]),
      h.div([a.class("flex gap-2")], [
        h.button(
          [
            a.type_("submit"),
            a.class(
              "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Search & Filter")],
        ),
        h.a(
          [
            href(ctx, "/audit-logs"),
            a.class(
              "px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg text-sm font-medium hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Clear")],
        ),
      ]),
    ]),
  ])
}

fn render_logs_table(ctx: Context, logs: List(audit.AuditLog)) {
  ui.table_container([
    h.table([a.class("min-w-full divide-y divide-neutral-200")], [
      h.thead([a.class("bg-neutral-50")], [
        h.tr([], [
          ui.table_header_cell("Timestamp"),
          ui.table_header_cell("Action"),
          ui.table_header_cell("Admin"),
          ui.table_header_cell("Target"),
          ui.table_header_cell("Reason"),
          ui.table_header_cell("Details"),
        ]),
      ]),
      h.tbody(
        [a.class("bg-white divide-y divide-neutral-200")],
        list.map(logs, fn(log) { render_log_row(ctx, log) }),
      ),
    ]),
  ])
}

fn render_log_row(ctx: Context, log: audit.AuditLog) {
  let expanded_id = "expanded-" <> log.log_id

  case list.is_empty(log.metadata) {
    True ->
      h.tr([a.class("hover:bg-neutral-50 transition-colors")], [
        h.td([a.class(ui.table_cell_class <> " whitespace-nowrap")], [
          element.text(date_time.format_timestamp(log.created_at)),
        ]),
        h.td([a.class("px-6 py-4 whitespace-nowrap")], [
          action_pill(log.action),
        ]),
        render_admin_cell(ctx, log.admin_user_id),
        render_target_cell(ctx, log.target_type, log.target_id),
        h.td([a.class(ui.table_cell_muted_class)], [
          case log.audit_log_reason {
            option.Some(reason) -> element.text(reason)
            option.None ->
              h.span([a.class("text-neutral-400 italic")], [element.text("—")])
          },
        ]),
        h.td([a.class(ui.table_cell_muted_class)], [
          h.span([a.class("text-neutral-400 italic")], [element.text("—")]),
        ]),
      ])
    False ->
      element.fragment([
        h.tr([a.class("hover:bg-neutral-50 transition-colors")], [
          h.td([a.class(ui.table_cell_class <> " whitespace-nowrap")], [
            element.text(date_time.format_timestamp(log.created_at)),
          ]),
          h.td([a.class("px-6 py-4 whitespace-nowrap")], [
            action_pill(log.action),
          ]),
          render_admin_cell(ctx, log.admin_user_id),
          render_target_cell(ctx, log.target_type, log.target_id),
          h.td([a.class(ui.table_cell_muted_class)], [
            case log.audit_log_reason {
              option.Some(reason) -> element.text(reason)
              option.None ->
                h.span([a.class("text-neutral-400 italic")], [element.text("—")])
            },
          ]),
          h.td([a.class(ui.table_cell_muted_class)], [
            h.button(
              [
                a.class(
                  "cursor-pointer text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
                ),
                a.attribute(
                  "onclick",
                  "document.getElementById('"
                    <> expanded_id
                    <> "').classList.toggle('hidden')",
                ),
              ],
              [element.text("Toggle details")],
            ),
          ]),
        ]),
        h.tr([a.id(expanded_id), a.class("hidden bg-neutral-50")], [
          h.td([a.attribute("colspan", "6"), a.class("px-6 py-4")], [
            render_metadata_expanded(log.metadata),
          ]),
        ]),
      ])
  }
}

fn render_admin_cell(ctx: Context, admin_user_id: String) {
  h.td([a.class(ui.table_cell_class <> " whitespace-nowrap")], [
    case string.is_empty(admin_user_id) {
      True -> h.span([a.class("text-neutral-400 italic")], [element.text("—")])
      False ->
        h.a(
          [
            href(ctx, "/users/" <> admin_user_id),
            a.class(
              "text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
            ),
          ],
          [element.text("User " <> admin_user_id)],
        )
    },
  ])
}

fn render_target_cell(ctx: Context, target_type: String, target_id: String) {
  h.td([a.class(ui.table_cell_class <> " whitespace-nowrap")], [
    case target_type, target_id {
      "user", id -> {
        h.a(
          [
            href(ctx, "/users/" <> id),
            a.class(
              "text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
            ),
          ],
          [element.text("User " <> id)],
        )
      }
      "guild", id -> {
        h.a(
          [
            href(ctx, "/guilds/" <> id),
            a.class(
              "text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
            ),
          ],
          [element.text("Guild " <> id)],
        )
      }
      type_, id ->
        h.span([a.class("text-neutral-900")], [
          element.text(string.capitalise(type_) <> " " <> id),
        ])
    },
  ])
}

fn render_metadata_expanded(metadata: List(#(String, String))) {
  h.div(
    [a.class("grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3")],
    list.map(metadata, fn(entry) {
      let #(key, value) = entry
      h.div([a.class("bg-white border border-neutral-200 rounded-lg p-3")], [
        h.div([a.class("text-xs text-neutral-500 uppercase mb-1")], [
          element.text(key),
        ]),
        h.div([a.class("text-sm text-neutral-900 break-all")], [
          element.text(value),
        ]),
      ])
    }),
  )
}

fn format_action(action: String) -> String {
  action
  |> string.replace("_", " ")
  |> string.capitalise
}

fn action_pill(action: String) {
  ui.pill(format_action(action), action_tone(action))
}

fn action_tone(action: String) -> ui.PillTone {
  case action {
    "temp_ban"
    | "disable_suspicious_activity"
    | "schedule_deletion"
    | "ban_ip"
    | "ban_email"
    | "ban_phone" -> ui.PillDanger
    "unban" | "cancel_deletion" | "unban_ip" | "unban_email" | "unban_phone" ->
      ui.PillSuccess
    "update_flags" | "update_features" | "set_acls" | "update_settings" ->
      ui.PillInfo
    "delete_message" -> ui.PillOrange
    _ -> ui.PillNeutral
  }
}

fn empty_state() {
  ui.card_empty([
    ui.text_muted("No audit logs found"),
    ui.text_small_muted("Try adjusting your filters or check back later"),
  ])
}

fn render_pagination(
  ctx: Context,
  current_page: Int,
  total_pages: Int,
  query: option.Option(String),
  admin_user_id_filter: option.Option(String),
  target_type: option.Option(String),
  target_id: option.Option(String),
  action: option.Option(String),
) {
  let build_url = fn(page: Int) {
    let base = "/audit-logs?page=" <> int.to_string(page)
    let with_query = case query {
      option.Some(q) if q != "" -> base <> "&q=" <> q
      _ -> base
    }
    let with_admin_user = case admin_user_id_filter {
      option.Some(id) if id != "" -> with_query <> "&admin_user_id=" <> id
      _ -> with_query
    }
    let with_target_type = case target_type {
      option.Some(tt) if tt != "" -> with_admin_user <> "&target_type=" <> tt
      _ -> with_admin_user
    }
    let with_target_id = case target_id {
      option.Some(tid) if tid != "" -> with_target_type <> "&target_id=" <> tid
      _ -> with_target_type
    }
    let with_action = case action {
      option.Some(act) if act != "" -> with_target_id <> "&action=" <> act
      _ -> with_target_id
    }
    with_action
  }

  h.div(
    [
      a.class(
        "mt-6 flex items-center justify-between border-t border-neutral-200 bg-white px-4 py-3 sm:px-6 rounded-b-lg",
      ),
    ],
    [
      h.div([a.class("flex flex-1 justify-between sm:hidden")], [
        case current_page > 1 {
          True ->
            h.a(
              [
                href(ctx, build_url(current_page - 1)),
                a.class(
                  "relative inline-flex items-center rounded-md border border-neutral-300 bg-white px-4 py-2 text-sm font-medium text-neutral-700 hover:bg-neutral-50",
                ),
              ],
              [element.text("Previous")],
            )
          False ->
            h.span(
              [
                a.class(
                  "relative inline-flex items-center rounded-md border border-neutral-300 bg-neutral-100 px-4 py-2 text-sm font-medium text-neutral-400 cursor-not-allowed",
                ),
              ],
              [element.text("Previous")],
            )
        },
        case current_page < total_pages {
          True ->
            h.a(
              [
                href(ctx, build_url(current_page + 1)),
                a.class(
                  "relative ml-3 inline-flex items-center rounded-md border border-neutral-300 bg-white px-4 py-2 text-sm font-medium text-neutral-700 hover:bg-neutral-50",
                ),
              ],
              [element.text("Next")],
            )
          False ->
            h.span(
              [
                a.class(
                  "relative ml-3 inline-flex items-center rounded-md border border-neutral-300 bg-neutral-100 px-4 py-2 text-sm font-medium text-neutral-400 cursor-not-allowed",
                ),
              ],
              [element.text("Next")],
            )
        },
      ]),
      h.div(
        [a.class("hidden sm:flex sm:flex-1 sm:items-center sm:justify-between")],
        [
          h.div([], [
            h.p([a.class("text-sm text-neutral-700")], [
              element.text(
                "Page "
                <> int.to_string(current_page)
                <> " of "
                <> int.to_string(total_pages),
              ),
            ]),
          ]),
          h.div([], [
            h.nav(
              [a.class("isolate inline-flex -space-x-px rounded-md shadow-sm")],
              [
                case current_page > 1 {
                  True ->
                    h.a(
                      [
                        href(ctx, build_url(current_page - 1)),
                        a.class(
                          "relative inline-flex items-center rounded-l-md px-4 py-2 text-neutral-900 ring-1 ring-inset ring-neutral-300 hover:bg-neutral-50 focus:z-20 focus:outline-offset-0",
                        ),
                      ],
                      [element.text("Previous")],
                    )
                  False ->
                    h.span(
                      [
                        a.class(
                          "relative inline-flex items-center rounded-l-md px-4 py-2 text-neutral-400 ring-1 ring-inset ring-neutral-300 bg-neutral-100 cursor-not-allowed",
                        ),
                      ],
                      [element.text("Previous")],
                    )
                },
                case current_page < total_pages {
                  True ->
                    h.a(
                      [
                        href(ctx, build_url(current_page + 1)),
                        a.class(
                          "relative inline-flex items-center rounded-r-md px-4 py-2 text-neutral-900 ring-1 ring-inset ring-neutral-300 hover:bg-neutral-50 focus:z-20 focus:outline-offset-0",
                        ),
                      ],
                      [element.text("Next")],
                    )
                  False ->
                    h.span(
                      [
                        a.class(
                          "relative inline-flex items-center rounded-r-md px-4 py-2 text-neutral-400 ring-1 ring-inset ring-neutral-300 bg-neutral-100 cursor-not-allowed",
                        ),
                      ],
                      [element.text("Next")],
                    )
                },
              ],
            ),
          ]),
        ],
      ),
    ],
  )
}
