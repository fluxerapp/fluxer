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
import fluxer_admin/components/date_time
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/review_deck
import fluxer_admin/components/review_hintbar
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import gleam/uri
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Response}

const report_category_options = [
  #("harassment", "Harassment or Bullying"),
  #("hate_speech", "Hate Speech"),
  #("spam", "Spam or Scam"),
  #("illegal_activity", "Illegal Activity"),
  #("impersonation", "Impersonation"),
  #("child_safety", "Child Safety Concerns"),
  #("other", "Other"),
  #("violent_content", "Violent or Graphic Content"),
  #("nsfw_violation", "NSFW Policy Violation"),
  #("doxxing", "Sharing Personal Information"),
  #("self_harm", "Self-Harm or Suicide"),
  #("malicious_links", "Malicious Links"),
  #("spam_account", "Spam Account"),
  #("underage_user", "Underage User"),
  #("inappropriate_profile", "Inappropriate Profile"),
  #("raid_coordination", "Raid Coordination"),
  #("malware_distribution", "Malware Distribution"),
  #("extremist_community", "Extremist Community"),
]

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  page: Int,
) -> Response {
  view_with_mode(
    ctx,
    session,
    current_admin,
    flash_data,
    query,
    status_filter,
    type_filter,
    category_filter,
    page,
    True,
  )
}

pub fn view_fragment(
  ctx: Context,
  session: Session,
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  page: Int,
) -> Response {
  let limit = 50
  let offset = page * limit

  let result =
    reports.search_reports(
      ctx,
      session,
      query,
      status_filter,
      type_filter,
      category_filter,
      limit,
      offset,
    )

  let content = case result {
    Ok(response) -> {
      h.div([a.attribute("data-review-fragment", "true")], [
        h.div(
          [a.class("max-w-7xl mx-auto")],
          list.map(response.reports, fn(report) {
            render_report_card(ctx, report)
          }),
        ),
      ])
    }
    Error(err) -> {
      h.div(
        [
          a.attribute("data-review-fragment", "true"),
          a.attribute("data-fragment-error", "true"),
        ],
        [h.span([], [element.text(api_error_message(err))])],
      )
    }
  }

  wisp.html_response(element.to_document_string(content), 200)
}

pub fn view_with_mode(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  page: Int,
  table_view: Bool,
) -> Response {
  let limit = 50
  let offset = page * limit

  let result =
    reports.search_reports(
      ctx,
      session,
      query,
      status_filter,
      type_filter,
      category_filter,
      limit,
      offset,
    )

  let content = case result {
    Ok(response) -> {
      h.div([a.class("max-w-7xl mx-auto")], [
        ui.flex_row_between([
          ui.heading_page("Reports"),
          h.div([a.class("flex items-center gap-4")], [
            h.span([a.class("body-sm text-neutral-600")], [
              element.text(
                "Found "
                <> int.to_string(response.total)
                <> " results (showing "
                <> int.to_string(list.length(response.reports))
                <> ")",
              ),
            ]),
          ]),
        ]),
        render_filters(
          ctx,
          query,
          status_filter,
          type_filter,
          category_filter,
          table_view,
        ),
        case list.is_empty(response.reports) {
          True -> empty_state()
          False ->
            case table_view {
              True ->
                h.div([a.class("mt-4")], [
                  render_reports_table(ctx, response.reports),
                  render_pagination(
                    ctx,
                    response.total,
                    response.offset,
                    response.limit,
                    page,
                    query,
                    status_filter,
                    type_filter,
                    category_filter,
                  ),
                ])
              False ->
                h.div([a.class("mt-4")], [
                  render_review_deck(
                    ctx,
                    response.reports,
                    response.total,
                    page,
                    query,
                    status_filter,
                    type_filter,
                    category_filter,
                  ),
                ])
            }
        },
      ])
    }
    Error(err) -> error_view(err)
  }

  let html =
    layout.page(
      "Reports",
      "reports",
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
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  table_view: Bool,
) {
  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4 mb-6")], [
    h.form([a.method("get"), a.class("space-y-4")], [
      h.div([a.class("w-full")], [
        h.label([a.class("block body-sm text-neutral-700 mb-2")], [
          element.text("Search"),
        ]),
        h.input([
          a.type_("text"),
          a.name("q"),
          a.value(option.unwrap(query, "")),
          a.placeholder("Search by ID, reporter, category, or description..."),
          a.class(
            "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
          ),
        ]),
      ]),
      h.div([a.class("grid grid-cols-1 md:grid-cols-3 gap-4")], [
        h.div([a.class("flex-1")], [
          h.label([a.class("block body-sm text-neutral-700 mb-2")], [
            element.text("Status"),
          ]),
          h.select(
            [
              a.name("status"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
              ),
            ],
            [
              h.option(
                [a.value(""), a.selected(option.is_none(status_filter))],
                "All",
              ),
              h.option(
                [a.value("0"), a.selected(status_filter == option.Some(0))],
                "Pending",
              ),
              h.option(
                [a.value("1"), a.selected(status_filter == option.Some(1))],
                "Resolved",
              ),
            ],
          ),
        ]),
        h.div([a.class("flex-1")], [
          h.label([a.class("block body-sm text-neutral-700 mb-2")], [
            element.text("Type"),
          ]),
          h.select(
            [
              a.name("type"),
              a.class(
                "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
              ),
            ],
            [
              h.option(
                [a.value(""), a.selected(option.is_none(type_filter))],
                "All",
              ),
              h.option(
                [a.value("0"), a.selected(type_filter == option.Some(0))],
                "Message",
              ),
              h.option(
                [a.value("1"), a.selected(type_filter == option.Some(1))],
                "User",
              ),
              h.option(
                [a.value("2"), a.selected(type_filter == option.Some(2))],
                "Guild",
              ),
            ],
          ),
        ]),
        {
          let selected_category = option.unwrap(category_filter, "")
          let category_select_children =
            list.append(
              [
                h.option(
                  [a.value(""), a.selected(option.is_none(category_filter))],
                  "All",
                ),
              ],
              list.map(report_category_options, fn(option_pair) {
                let #(value, label) = option_pair
                h.option(
                  [a.value(value), a.selected(selected_category == value)],
                  label,
                )
              }),
            )

          h.div([a.class("flex-1")], [
            h.label([a.class("block body-sm text-neutral-700 mb-2")], [
              element.text("Category"),
            ]),
            h.select(
              [
                a.name("category"),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
                ),
              ],
              category_select_children,
            ),
          ])
        },
      ]),
      h.div([a.class("flex gap-2")], [
        h.button(
          [
            a.type_("submit"),
            a.class(
              "px-4 py-2 bg-neutral-900 text-white rounded-lg label hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Search & Filter")],
        ),
        h.a(
          [
            href(ctx, "/reports"),
            a.class(
              "px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Clear")],
        ),
        h.a(
          [
            href(
              ctx,
              build_table_view_url(
                query,
                status_filter,
                type_filter,
                category_filter,
                False,
              ),
            ),
            a.class(
              "px-4 py-2 "
              <> case table_view {
                True ->
                  "bg-neutral-100 text-neutral-600 border border-neutral-200"
                False -> "bg-white text-neutral-900 border border-neutral-300"
              }
              <> " rounded-lg label hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Deck")],
        ),
        h.a(
          [
            href(
              ctx,
              build_table_view_url(
                query,
                status_filter,
                type_filter,
                category_filter,
                True,
              ),
            ),
            a.class(
              "px-4 py-2 "
              <> case table_view {
                True -> "bg-white text-neutral-900 border border-neutral-300"
                False ->
                  "bg-neutral-100 text-neutral-600 border border-neutral-200"
              }
              <> " rounded-lg label hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Table view")],
        ),
      ]),
    ]),
  ])
}

fn render_reports_table(ctx: Context, reports: List(reports.SearchReportResult)) {
  let base_cell = ui.table_cell_class

  let columns = [
    ui.TableColumn(
      "Reported At",
      base_cell <> " whitespace-nowrap",
      fn(report: reports.SearchReportResult) {
        element.text(date_time.format_timestamp(report.reported_at))
      },
    ),
    ui.TableColumn(
      "Type",
      "px-6 py-4 whitespace-nowrap",
      fn(report: reports.SearchReportResult) {
        report_type_pill(report.report_type)
      },
    ),
    ui.TableColumn(
      "Category",
      base_cell,
      fn(report: reports.SearchReportResult) { element.text(report.category) },
    ),
    ui.TableColumn(
      "Reporter",
      "px-6 py-4 whitespace-nowrap text-sm",
      fn(report: reports.SearchReportResult) {
        render_reporter_cell(ctx, report)
      },
    ),
    ui.TableColumn(
      "Reported",
      "px-6 py-4 whitespace-nowrap text-sm",
      fn(report: reports.SearchReportResult) {
        render_reported_cell(ctx, report)
      },
    ),
    ui.TableColumn(
      "Status",
      "px-6 py-4 whitespace-nowrap",
      fn(report: reports.SearchReportResult) { status_pill(report.status) },
    ),
    ui.TableColumn(
      "Actions",
      "px-6 py-4 whitespace-nowrap text-sm",
      fn(report: reports.SearchReportResult) {
        render_actions_cell(ctx, report)
      },
    ),
  ]

  ui.data_table(columns, reports)
}

fn render_reported_cell(ctx: Context, report: reports.SearchReportResult) {
  case report.report_type {
    0 -> render_reported_user_cell(ctx, report)
    1 -> render_reported_user_cell(ctx, report)
    2 -> render_reported_guild_cell(ctx, report)
    _ ->
      h.span([a.class("text-sm text-neutral-400 italic")], [
        element.text("Unknown"),
      ])
  }
}

fn render_reporter_cell(ctx: Context, report: reports.SearchReportResult) {
  let primary = case report.reporter_tag {
    option.Some(tag) -> tag
    option.None ->
      case report.reporter_email {
        option.Some(email) -> email
        option.None -> "Anonymous"
      }
  }

  let primary_element = case report.reporter_id {
    option.Some(id) ->
      h.a(
        [
          href(ctx, "/users/" <> id),
          a.class(
            "text-sm text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
          ),
        ],
        [element.text(primary)],
      )
    option.None ->
      h.span([a.class("text-sm text-neutral-900")], [element.text(primary)])
  }

  let detail_fragments = []
  let detail_fragments = case report.reporter_full_legal_name {
    option.Some(full_name) ->
      list.append(detail_fragments, [element.text(full_name)])
    option.None -> detail_fragments
  }

  let detail_fragments = case report.reporter_country_of_residence {
    option.Some(country) ->
      list.append(detail_fragments, [element.text(country)])
    option.None -> detail_fragments
  }

  let secondary = case list.is_empty(detail_fragments) {
    True -> element.none()
    False -> h.div([a.class("text-xs text-neutral-500")], detail_fragments)
  }

  h.div([a.class("flex flex-col gap-1")], [
    primary_element,
    secondary,
  ])
}

fn render_reported_user_cell(ctx: Context, report: reports.SearchReportResult) {
  let primary_text = format_user_tag(report)
  case report.reported_user_id {
    option.Some(id) ->
      h.a(
        [
          href(ctx, "/users/" <> id),
          a.class(
            "text-sm text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
          ),
        ],
        [element.text(primary_text)],
      )
    option.None ->
      h.span([a.class("text-sm text-neutral-900")], [element.text(primary_text)])
  }
}

fn render_reported_guild_cell(ctx: Context, report: reports.SearchReportResult) {
  case report.reported_guild_id {
    option.Some(guild_id) -> {
      let primary_name = case report.reported_guild_name {
        option.Some(name) -> name
        option.None -> "Guild " <> guild_id
      }
      let primary_element =
        h.a(
          [
            href(ctx, "/guilds/" <> guild_id),
            a.class(
              "text-sm text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500",
            ),
          ],
          [element.text(primary_name)],
        )
      let detail_lines = case report.reported_guild_invite_code {
        option.Some(code) -> [element.text("Invite: " <> code)]
        option.None -> []
      }
      let secondary = case list.is_empty(detail_lines) {
        True -> element.none()
        False -> h.div([a.class("text-xs text-neutral-500")], detail_lines)
      }
      h.div([a.class("flex flex-col gap-1")], [primary_element, secondary])
    }
    option.None ->
      h.span([a.class("text-sm text-neutral-400 italic")], [element.text("—")])
  }
}

fn format_user_tag(report: reports.SearchReportResult) -> String {
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

fn render_actions_cell(ctx: Context, report: reports.SearchReportResult) {
  h.a(
    [
      href(ctx, "/reports/" <> report.report_id),
      a.class(
        "inline-flex items-center px-3 py-1.5 bg-neutral-900 text-white rounded text-xs font-medium hover:bg-neutral-800 transition-colors",
      ),
    ],
    [element.text("View Details")],
  )
}

fn render_pagination(
  ctx: Context,
  total: Int,
  _offset: Int,
  limit: Int,
  current_page: Int,
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
) {
  let total_pages = { total + limit - 1 } / limit
  let has_previous = current_page > 0
  let has_next = current_page < total_pages - 1

  h.div([a.class("mt-6 flex justify-center gap-3 items-center")], [
    case has_previous {
      True -> {
        let prev_url =
          build_pagination_url(
            current_page - 1,
            query,
            status_filter,
            type_filter,
            category_filter,
          )

        h.a(
          [
            href(ctx, prev_url),
            a.class(
              "px-6 py-2 bg-white text-neutral-900 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("← Previous")],
        )
      }
      False ->
        h.div(
          [
            a.class(
              "px-6 py-2 bg-neutral-100 text-neutral-400 border border-neutral-200 rounded-lg label cursor-not-allowed",
            ),
          ],
          [element.text("← Previous")],
        )
    },
    h.span([a.class("body-sm text-neutral-600")], [
      element.text(
        "Page "
        <> int.to_string(current_page + 1)
        <> " of "
        <> int.to_string(total_pages),
      ),
    ]),
    case has_next {
      True -> {
        let next_url =
          build_pagination_url(
            current_page + 1,
            query,
            status_filter,
            type_filter,
            category_filter,
          )

        h.a(
          [
            href(ctx, next_url),
            a.class(
              "px-6 py-2 bg-neutral-900 text-white rounded-lg label hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Next →")],
        )
      }
      False ->
        h.div(
          [
            a.class(
              "px-6 py-2 bg-neutral-100 text-neutral-400 rounded-lg label cursor-not-allowed",
            ),
          ],
          [element.text("Next →")],
        )
    },
  ])
}

fn build_pagination_url(
  page: Int,
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
) -> String {
  let base = "/reports"
  let mut_params = [#("page", int.to_string(page))]

  let mut_params = case query {
    option.Some(q) ->
      case string.trim(q) {
        "" -> mut_params
        q -> [#("q", q), ..mut_params]
      }
    option.None -> mut_params
  }

  let mut_params = case status_filter {
    option.Some(s) -> [#("status", int.to_string(s)), ..mut_params]
    option.None -> mut_params
  }

  let mut_params = case type_filter {
    option.Some(t) -> [#("type", int.to_string(t)), ..mut_params]
    option.None -> mut_params
  }

  let mut_params = case category_filter {
    option.Some(c) ->
      case string.trim(c) {
        "" -> mut_params
        c -> [#("category", c), ..mut_params]
      }
    option.None -> mut_params
  }

  case mut_params {
    [] -> base
    params -> {
      let query_string =
        params
        |> list.map(fn(pair) {
          let #(key, value) = pair
          key <> "=" <> uri.percent_encode(value)
        })
        |> string.join("&")
      base <> "?" <> query_string
    }
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

fn report_type_pill(report_type: Int) {
  let tone = case report_type {
    0 -> ui.PillInfo
    1 -> ui.PillPurple
    2 -> ui.PillOrange
    _ -> ui.PillNeutral
  }

  ui.pill(format_report_type(report_type), tone)
}

fn status_pill(status: Int) {
  let #(label, tone) = case status {
    0 -> #("Pending", ui.PillWarning)
    1 -> #("Resolved", ui.PillSuccess)
    _ -> #("Unknown", ui.PillNeutral)
  }

  ui.pill(label, tone)
}

fn empty_state() {
  ui.card_empty([
    ui.text_muted("No reports found"),
    ui.text_small_muted("Try adjusting your filters or check back later"),
  ])
}

fn api_error_message(err: common.ApiError) -> String {
  case err {
    common.Unauthorized -> "Authentication Required"
    common.Forbidden(msg) -> msg
    common.NotFound -> "Reports could not be retrieved."
    common.ServerError ->
      "An internal server error occurred. Please try again later."
    common.NetworkError ->
      "Could not connect to the API. Please try again later."
  }
}

fn build_table_view_url(
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
  table_view: Bool,
) -> String {
  let base = "/reports"
  let mut_params = [
    #("table", case table_view {
      True -> "1"
      False -> "0"
    }),
  ]

  let mut_params = case query {
    option.Some(q) ->
      case string.trim(q) {
        "" -> mut_params
        q -> [#("q", q), ..mut_params]
      }
    option.None -> mut_params
  }

  let mut_params = case status_filter {
    option.Some(s) -> [#("status", int.to_string(s)), ..mut_params]
    option.None -> mut_params
  }

  let mut_params = case type_filter {
    option.Some(t) -> [#("type", int.to_string(t)), ..mut_params]
    option.None -> mut_params
  }

  let mut_params = case category_filter {
    option.Some(c) ->
      case string.trim(c) {
        "" -> mut_params
        c -> [#("category", c), ..mut_params]
      }
    option.None -> mut_params
  }

  case mut_params {
    [] -> base
    params -> {
      let query_string =
        params
        |> list.map(fn(pair) {
          let #(key, value) = pair
          key <> "=" <> uri.percent_encode(value)
        })
        |> string.join("&")
      base <> "?" <> query_string
    }
  }
}

fn prepend_fragment_base(ctx: Context, path: String) -> String {
  ctx.base_path <> path
}

fn render_review_deck(
  ctx: Context,
  reports: List(reports.SearchReportResult),
  _total: Int,
  page: Int,
  query: option.Option(String),
  status_filter: option.Option(Int),
  type_filter: option.Option(Int),
  category_filter: option.Option(String),
) {
  let fragment_base =
    prepend_fragment_base(
      ctx,
      "/reports/fragment?"
        <> build_table_view_url(
        query,
        status_filter,
        type_filter,
        category_filter,
        True,
      ),
    )

  let deck_attrs = [
    a.attribute("data-review-deck", "true"),
    a.attribute("data-fragment-base", fragment_base),
    a.attribute("data-next-page", int.to_string(page + 1)),
    a.attribute("data-can-paginate", "true"),
    a.attribute("data-empty-url", "/reports"),
    a.attribute("data-prefetch-when-remaining", "6"),
    a.tabindex(0),
  ]

  h.div(deck_attrs, [
    review_deck.styles(),
    h.div(
      [a.class("max-w-7xl mx-auto")],
      list.map(reports, fn(report) { render_report_card(ctx, report) }),
    ),
    h.div(
      [
        a.attribute("data-review-progress", "true"),
        a.class("text-center mt-4 body-sm text-neutral-600"),
      ],
      [
        element.text(int.to_string(list.length(reports)) <> " remaining"),
      ],
    ),
    review_hintbar.view(
      "←",
      "Skip",
      "→",
      "Resolve",
      "Esc",
      "Exit",
      option.Some("Swipe cards on touch devices"),
    ),
    ..review_deck.script_tags()
  ])
}

fn render_report_card(ctx: Context, report: reports.SearchReportResult) {
  let card_attrs = [
    a.attribute("data-review-card", "true"),
    a.attribute("data-left-mode", "skip"),
    a.attribute("data-direct-url", "/reports/" <> report.report_id),
    a.attribute("data-expand-url", "/reports/" <> report.report_id),
    a.attribute("data-expand-target", "[data-report-context]"),
    a.tabindex(0),
    a.class(
      "review-card bg-white border border-neutral-200 rounded-xl shadow-sm p-6 mb-4 focus:outline-none focus:ring-2 focus:ring-neutral-900",
    ),
  ]

  h.div(card_attrs, [
    h.div([a.class("flex items-start justify-between gap-4 mb-4")], [
      h.div([a.class("flex items-center gap-3")], [
        h.a(
          [
            href(ctx, "/reports/" <> report.report_id),
            a.class("hover:underline"),
          ],
          [element.text("#" <> report.report_id)],
        ),
        status_pill(report.status),
        report_type_pill(report.report_type),
      ]),
      h.span([a.class("body-sm text-neutral-500 whitespace-nowrap")], [
        element.text(date_time.format_timestamp(report.reported_at)),
      ]),
    ]),
    h.div([a.class("space-y-2 mb-4")], [
      h.div([a.class("flex items-center gap-2")], [
        h.span([a.class("label-sm text-neutral-600")], [
          element.text("Category:"),
        ]),
        h.span([a.class("body-sm text-neutral-900")], [
          element.text(report.category),
        ]),
      ]),
      case report.additional_info {
        option.Some(info) if info != "" ->
          h.div([a.class("flex items-start gap-2")], [
            h.span([a.class("label-sm text-neutral-600")], [
              element.text("Details:"),
            ]),
            h.span([a.class("body-sm text-neutral-900 flex-1")], [
              element.text(info),
            ]),
          ])
        _ -> element.none()
      },
    ]),
    h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4 mb-4")], [
      h.div([a.class("space-y-1")], [
        h.div([a.class("label-sm text-neutral-600")], [element.text("Reporter")]),
        render_reporter_compact(ctx, report),
      ]),
      h.div([a.class("space-y-1")], [
        h.div([a.class("label-sm text-neutral-600")], [element.text("Reported")]),
        render_reported_compact(ctx, report),
      ]),
    ]),
    h.div([a.attribute("data-report-context", "true"), a.hidden(True)], []),
    h.form(
      [
        a.attribute("data-review-submit", "left"),
        a.method("post"),
        a.attribute("action", "/reports/" <> report.report_id <> "/skip"),
      ],
      [h.input([a.type_("hidden"), a.name("_method"), a.value("post")])],
    ),
    h.form(
      [
        a.attribute("data-review-submit", "right"),
        a.method("post"),
        a.attribute("action", "/reports/" <> report.report_id <> "/resolve"),
      ],
      [
        h.input([a.type_("hidden"), a.name("_method"), a.value("post")]),
        h.input([
          a.type_("hidden"),
          a.name("public_comment"),
          a.value("Resolved via review deck"),
        ]),
      ],
    ),
    h.div(
      [
        a.class(
          "flex items-center justify-between pt-4 border-t border-neutral-200",
        ),
      ],
      [
        h.button(
          [
            a.attribute("data-review-action", "left"),
            a.class(
              "px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg label hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Skip")],
        ),
        h.button(
          [
            a.attribute("data-review-action", "right"),
            a.class(
              "px-4 py-2 bg-green-600 text-white rounded-lg label hover:bg-green-700 transition-colors",
            ),
          ],
          [element.text("Resolve")],
        ),
      ],
    ),
  ])
}

fn render_reporter_compact(ctx: Context, report: reports.SearchReportResult) {
  let primary = case report.reporter_tag {
    option.Some(tag) -> tag
    option.None ->
      case report.reporter_email {
        option.Some(email) -> email
        option.None -> "Anonymous"
      }
  }

  let primary_element = case report.reporter_id {
    option.Some(id) ->
      h.a(
        [
          href(ctx, "/users/" <> id),
          a.class("body-sm text-neutral-900 hover:text-neutral-600 underline"),
        ],
        [element.text(primary)],
      )
    option.None ->
      h.span([a.class("body-sm text-neutral-900")], [element.text(primary)])
  }

  primary_element
}

fn render_reported_compact(ctx: Context, report: reports.SearchReportResult) {
  case report.report_type {
    0 ->
      case report.reported_user_id {
        option.Some(id) ->
          h.a(
            [
              href(ctx, "/users/" <> id),
              a.class(
                "body-sm text-neutral-900 hover:text-neutral-600 underline",
              ),
            ],
            [element.text(format_user_tag(report))],
          )
        option.None ->
          h.span([a.class("body-sm text-neutral-400 italic")], [
            element.text("—"),
          ])
      }
    1 ->
      case report.reported_user_id {
        option.Some(id) ->
          h.a(
            [
              href(ctx, "/users/" <> id),
              a.class(
                "body-sm text-neutral-900 hover:text-neutral-600 underline",
              ),
            ],
            [element.text(format_user_tag(report))],
          )
        option.None ->
          h.span([a.class("body-sm text-neutral-400 italic")], [
            element.text("—"),
          ])
      }
    2 ->
      case report.reported_guild_id {
        option.Some(guild_id) -> {
          let primary_name = case report.reported_guild_name {
            option.Some(name) -> name
            option.None -> "Guild " <> guild_id
          }
          h.a(
            [
              href(ctx, "/guilds/" <> guild_id),
              a.class(
                "body-sm text-neutral-900 hover:text-neutral-600 underline",
              ),
            ],
            [element.text(primary_name)],
          )
        }
        option.None ->
          h.span([a.class("body-sm text-neutral-400 italic")], [
            element.text("—"),
          ])
      }
    _ ->
      h.span([a.class("body-sm text-neutral-400 italic")], [element.text("—")])
  }
}

fn error_view(err: common.ApiError) {
  let #(title, message) = case err {
    common.Unauthorized -> #(
      "Authentication Required",
      "Your session has expired. Please log in again.",
    )
    common.Forbidden(msg) -> #("Permission Denied", msg)
    common.NotFound -> #("Not Found", "Reports could not be retrieved.")
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
