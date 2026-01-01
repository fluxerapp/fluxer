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

import fluxer_admin/api/common
import fluxer_admin/api/reports
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/message_list
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, href}
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
  report_id: String,
) -> Response {
  let result = reports.get_report_detail(ctx, session, report_id)

  let #(content, include_script) = case result {
    Ok(report) -> {
      let has_message_context =
        report.report_type == 0 && !list.is_empty(report.message_context)

      #(
        h.div([a.class("max-w-5xl mx-auto")], [
          h.div([a.class("mb-6")], [
            ui.flex_row("4", [
              h.a(
                [
                  href(ctx, "/reports"),
                  a.class(
                    "px-3 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors",
                  ),
                ],
                [element.text("← Back to Reports")],
              ),
              ui.heading_page("Report Details"),
            ]),
          ]),
          h.div([a.class("grid grid-cols-1 lg:grid-cols-3 gap-6")], [
            h.div([a.class("lg:col-span-2 space-y-6")], [
              render_basic_info(ctx, report),
              render_reported_entity(ctx, report),
              render_message_context(ctx, report),
              case report.additional_info {
                option.Some(info) -> render_additional_info(info)
                option.None -> element.none()
              },
            ]),
            h.div([a.class("space-y-6")], [
              render_status_card(ctx, report),
              render_actions_card(ctx, report),
            ]),
          ]),
        ]),
        has_message_context,
      )
    }
    Error(err) -> #(error_view(err), False)
  }

  let html =
    layout.page(
      "Report Details",
      "reports",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  let html_string = element.to_document_string(html)
  let final_html = case include_script {
    True ->
      string.replace(
        html_string,
        "</body>",
        message_list.deletion_script() <> "</body>",
      )
    False -> html_string
  }

  wisp.html_response(final_html, 200)
}

pub fn fragment(ctx: Context, session: Session, report_id: String) -> Response {
  let result = reports.get_report_detail(ctx, session, report_id)
  let content = case result {
    Ok(report) ->
      h.div([a.attribute("data-report-fragment", ""), a.class("space-y-4")], [
        render_basic_info(ctx, report),
        render_reported_entity(ctx, report),
        render_message_context(ctx, report),
        case report.additional_info {
          option.Some(info) -> render_additional_info(info)
          option.None -> element.none()
        },
      ])
    Error(err) ->
      h.div([a.attribute("data-report-fragment", "")], [
        h.div(
          [
            a.class(
              "bg-red-50 border border-red-200 rounded-xl p-4 text-sm text-red-800",
            ),
          ],
          [
            element.text("Failed to load report: " <> api_error_message(err)),
          ],
        ),
      ])
  }

  wisp.html_response(element.to_document_string(content), 200)
}

fn api_error_message(err: common.ApiError) -> String {
  case err {
    common.Unauthorized -> "Unauthorized"
    common.Forbidden(message) -> message
    common.NotFound -> "Not Found"
    common.NetworkError -> "Network error"
    common.ServerError -> "Server error"
  }
}

fn render_basic_info(ctx: Context, report: reports.Report) {
  let reporter_primary = case report.reporter_tag {
    option.Some(tag) -> tag
    option.None ->
      case report.reporter_email {
        option.Some(email) -> email
        option.None -> "Anonymous"
      }
  }

  let reporter_row = case report.reporter_id {
    option.Some(id) ->
      render_info_row_with_link(
        ctx,
        "Reporter",
        reporter_primary,
        "/users/" <> id,
        True,
      )
    option.None -> render_info_row("Reporter", reporter_primary, False)
  }

  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("title-sm text-neutral-900 mb-4")], [
      element.text("Basic Information"),
    ]),
    h.dl([a.class("grid grid-cols-1 sm:grid-cols-2 gap-4")], [
      render_info_row("Report ID", report.report_id, True),
      render_info_row(
        "Reported At",
        format_timestamp(report.reported_at),
        False,
      ),
      render_info_row("Type", format_report_type(report.report_type), False),
      render_info_row("Category", report.category, False),
      reporter_row,
      render_info_row_opt("Reporter Email", report.reporter_email, False),
      render_info_row_opt(
        "Full Legal Name",
        report.reporter_full_legal_name,
        False,
      ),
      render_info_row_opt(
        "Country of Residence",
        report.reporter_country_of_residence,
        False,
      ),
      render_info_row(
        "Status",
        case report.status {
          0 -> "Pending"
          1 -> "Resolved"
          _ -> "Unknown"
        },
        False,
      ),
    ]),
  ])
}

fn render_reported_entity(ctx: Context, report: reports.Report) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("title-sm text-neutral-900 mb-4")], [
      element.text("Reported Entity"),
    ]),
    h.dl([a.class("grid grid-cols-1 gap-4")], case report.report_type {
      0 -> [
        render_info_row_opt_with_link(
          ctx,
          "User",
          report.reported_user_id,
          option.Some(format_reported_user_label(report)),
          fn(id) { "/users/" <> id },
          True,
        ),
        render_info_row_opt("Message ID", report.reported_message_id, True),
        render_info_row_opt("Channel ID", report.reported_channel_id, True),
        render_info_row_opt("Channel Name", report.reported_channel_name, False),
        render_info_row_opt(
          "Guild Invite Code",
          report.reported_guild_invite_code,
          False,
        ),
      ]
      1 -> [
        render_info_row_opt_with_link(
          ctx,
          "User",
          report.reported_user_id,
          option.Some(format_reported_user_label(report)),
          fn(id) { "/users/" <> id },
          True,
        ),
        render_info_row_opt("Guild Name", report.reported_guild_name, False),
        render_info_row_opt("Guild ID", report.reported_guild_id, True),
        render_info_row_opt(
          "Guild Invite Code",
          report.reported_guild_invite_code,
          False,
        ),
      ]
      2 -> [
        render_info_row_opt_with_link(
          ctx,
          "Guild",
          report.reported_guild_id,
          report.reported_guild_name,
          fn(id) { "/guilds/" <> id },
          True,
        ),
        render_info_row_opt(
          "Guild Invite Code",
          report.reported_guild_invite_code,
          False,
        ),
      ]
      _ -> [element.text("Unknown report type")]
    }),
  ])
}

fn render_message_context(ctx: Context, report: reports.Report) {
  case report.report_type {
    0 -> {
      case list.is_empty(report.message_context) {
        True -> element.none()
        False ->
          h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
            h.h2([a.class("title-sm text-neutral-900 mb-4")], [
              element.text("Message Context"),
            ]),
            message_list.render(ctx, report.message_context, True),
          ])
      }
    }
    _ -> element.none()
  }
}

fn render_additional_info(info: String) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("title-sm text-neutral-900 mb-4")], [
      element.text("Additional Information"),
    ]),
    h.p([a.class("text-neutral-700 whitespace-pre-wrap")], [element.text(info)]),
  ])
}

fn render_status_card(ctx: Context, report: reports.Report) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("title-sm text-neutral-900 mb-4")], [
      element.text("Status"),
    ]),
    h.div([a.class("space-y-3")], [
      h.div([a.class("text-center")], [
        case report.status {
          0 ->
            h.span(
              [
                a.class(
                  "px-4 py-2 subtitle rounded-lg bg-yellow-100 text-yellow-700",
                ),
              ],
              [element.text("Pending")],
            )
          1 ->
            h.span(
              [
                a.class(
                  "px-4 py-2 subtitle rounded-lg bg-green-100 text-green-700",
                ),
              ],
              [element.text("Resolved")],
            )
          _ ->
            h.span(
              [
                a.class(
                  "px-4 py-2 subtitle rounded-lg bg-neutral-100 text-neutral-700",
                ),
              ],
              [element.text("Unknown")],
            )
        },
      ]),
      case report.resolved_at {
        option.Some(timestamp) ->
          h.div([a.class("body-sm text-neutral-600")], [
            h.span([a.class("label")], [element.text("Resolved At: ")]),
            element.text(format_timestamp(timestamp)),
          ])
        option.None -> element.none()
      },
      case report.resolved_by_admin_id {
        option.Some(admin_id) ->
          h.div([a.class("body-sm text-neutral-600")], [
            h.span([a.class("label")], [element.text("Resolved By: ")]),
            h.a(
              [
                href(ctx, "/users/" <> admin_id),
                a.class("underline hover:text-neutral-900"),
              ],
              [element.text(admin_id)],
            ),
          ])
        option.None -> element.none()
      },
      case report.public_comment {
        option.Some(comment) ->
          h.div([a.class("pt-3 border-t border-neutral-200")], [
            h.p([a.class("body-sm text-neutral-700 mb-2")], [
              element.text("Public Comment:"),
            ]),
            h.p([a.class("body-sm text-neutral-600 whitespace-pre-wrap")], [
              element.text(comment),
            ]),
          ])
        option.None -> element.none()
      },
    ]),
  ])
}

fn render_actions_card(ctx: Context, report: reports.Report) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-6")], [
    h.h2([a.class("title-sm text-neutral-900 mb-4")], [
      element.text("Actions"),
    ]),
    h.div([a.class("space-y-3")], [
      case report.status {
        0 ->
          h.button(
            [
              a.class(
                "w-full px-4 py-2 bg-neutral-900 text-white rounded-lg label hover:bg-neutral-800 transition-colors",
              ),
              a.attribute(
                "onclick",
                "if(confirm('Resolve this report?')) { fetch('/reports/"
                  <> report.report_id
                  <> "/resolve', { method: 'POST', headers: { 'Content-Type': 'application/json' } }).then(() => location.reload()) }",
              ),
            ],
            [element.text("Resolve Report")],
          )
        _ -> element.none()
      },
      case report.report_type {
        0 -> {
          case report.reported_user_id {
            option.Some(user_id) ->
              h.a(
                [
                  href(ctx, "/users/" <> user_id),
                  a.class(
                    "block w-full px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors text-center",
                  ),
                ],
                [element.text("View Reported User")],
              )
            option.None -> element.none()
          }
        }
        1 -> {
          case report.reported_user_id {
            option.Some(user_id) ->
              h.a(
                [
                  href(ctx, "/users/" <> user_id),
                  a.class(
                    "block w-full px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors text-center",
                  ),
                ],
                [element.text("View Reported User")],
              )
            option.None -> element.none()
          }
        }
        2 -> {
          case report.reported_guild_id {
            option.Some(guild_id) ->
              h.a(
                [
                  href(ctx, "/guilds/" <> guild_id),
                  a.class(
                    "block w-full px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors text-center",
                  ),
                ],
                [element.text("View Reported Guild")],
              )
            option.None -> element.none()
          }
        }
        _ -> element.none()
      },
      case report.reporter_id {
        option.Some(user_id) ->
          h.a(
            [
              href(ctx, "/users/" <> user_id),
              a.class(
                "block w-full px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors text-center",
              ),
            ],
            [element.text("View Reporter")],
          )
        option.None -> element.none()
      },
    ]),
  ])
}

fn render_info_row(label: String, value: String, mono: Bool) {
  h.div([], [
    h.dt([a.class("body-sm text-neutral-500 mb-1")], [
      element.text(label),
    ]),
    h.dd(
      [
        a.class(case mono {
          True -> "body-sm text-neutral-900"
          False -> "body-sm text-neutral-900"
        }),
      ],
      [element.text(value)],
    ),
  ])
}

fn render_info_row_with_link(
  ctx: Context,
  label: String,
  value: String,
  path: String,
  mono: Bool,
) {
  h.div([], [
    h.dt([a.class("body-sm text-neutral-500 mb-1")], [
      element.text(label),
    ]),
    h.dd([], [
      h.a(
        [
          href(ctx, path),
          a.class(
            "body-sm text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500 "
            <> case mono {
              True -> ""
              False -> ""
            },
          ),
        ],
        [element.text(value)],
      ),
    ]),
  ])
}

fn render_info_row_opt(label: String, value: option.Option(String), mono: Bool) {
  h.div([], [
    h.dt([a.class("body-sm text-neutral-500 mb-1")], [
      element.text(label),
    ]),
    h.dd(
      [
        a.class(case mono {
          True -> "body-sm text-neutral-900"
          False -> "body-sm text-neutral-900"
        }),
      ],
      [
        element.text(case value {
          option.Some(v) -> v
          option.None -> "—"
        }),
      ],
    ),
  ])
}

fn render_info_row_opt_with_link(
  ctx: Context,
  label: String,
  id: option.Option(String),
  name: option.Option(String),
  path_fn: fn(String) -> String,
  mono: Bool,
) {
  h.div([], [
    h.dt([a.class("body-sm text-neutral-500 mb-1")], [
      element.text(label),
    ]),
    h.dd([], [
      case id {
        option.Some(id_val) -> {
          let display = case name {
            option.Some(n) -> n
            option.None -> id_val
          }
          h.a(
            [
              href(ctx, path_fn(id_val)),
              a.class(
                "body-sm text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500 "
                <> case mono {
                  True -> ""
                  False -> ""
                },
              ),
            ],
            [element.text(display)],
          )
        }
        option.None ->
          h.span([a.class("body-sm text-neutral-400 italic")], [
            element.text("—"),
          ])
      },
    ]),
  ])
}

fn format_timestamp(timestamp: String) -> String {
  case string.split(timestamp, "T") {
    [date_part, time_part] -> {
      let time_clean = case string.split(time_part, ".") {
        [hms, _] -> hms
        _ -> time_part
      }
      let time_clean = string.replace(time_clean, "Z", "")

      case string.split(time_clean, ":") {
        [hour, minute, _] -> date_part <> " " <> hour <> ":" <> minute
        _ -> timestamp
      }
    }
    _ -> timestamp
  }
}

fn format_report_type(report_type: Int) -> String {
  case report_type {
    0 -> "Message"
    1 -> "User"
    2 -> "Guild"
    _ -> "Unknown"
  }
}

fn format_reported_user_label(report: reports.Report) -> String {
  case report.reported_user_tag {
    option.Some(tag) -> tag
    option.None ->
      case report.reported_user_username {
        option.Some(username) -> {
          let discriminator =
            option.unwrap(report.reported_user_discriminator, "0000")
          username <> "#" <> discriminator
        }
        option.None ->
          "User " <> option.unwrap(report.reported_user_id, "unknown")
      }
  }
}

fn error_view(err: common.ApiError) {
  let #(title, message) = case err {
    common.Unauthorized -> #(
      "Authentication Required",
      "Your session has expired. Please log in again.",
    )
    common.Forbidden(msg) -> #("Permission Denied", msg)
    common.NotFound -> #("Not Found", "Report not found.")
    common.ServerError -> #(
      "Server Error",
      "An internal server error occurred. Please try again later.",
    )
    common.NetworkError -> #(
      "Network Error",
      "Could not connect to the API. Please try again later.",
    )
  }

  h.div([a.class("max-w-4xl mx-auto")], [
    h.div([a.class("bg-red-50 border border-red-200 rounded-lg p-8")], [
      h.div([a.class("flex items-start gap-4")], [
        h.div(
          [
            a.class(
              "flex-shrink-0 w-12 h-12 bg-red-100 rounded-full flex items-center justify-center",
            ),
          ],
          [
            h.span([a.class("text-red-600 title-sm")], [
              element.text("!"),
            ]),
          ],
        ),
        h.div([a.class("flex-1")], [
          h.h2([a.class("title-sm text-red-900 mb-2")], [
            element.text(title),
          ]),
          h.p([a.class("text-red-700")], [element.text(message)]),
        ]),
      ]),
    ]),
  ])
}
