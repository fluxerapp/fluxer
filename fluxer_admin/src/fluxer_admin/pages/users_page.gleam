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
import fluxer_admin/api/users
import fluxer_admin/avatar
import fluxer_admin/badge
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/pagination
import fluxer_admin/components/ui
import fluxer_admin/components/url_builder
import fluxer_admin/user
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
  page: Int,
) -> Response {
  let limit = 50
  let offset = page * limit

  let result = case query {
    option.Some(q) ->
      case string.trim(q) {
        "" -> Ok(users.SearchUsersResponse(users: [], total: 0))
        trimmed_query ->
          users.search_users(ctx, session, trimmed_query, limit, offset)
      }
    option.None -> Ok(users.SearchUsersResponse(users: [], total: 0))
  }

  let content = case result {
    Ok(response) -> {
      h.div([a.class("max-w-7xl mx-auto space-y-6")], [
        ui.flex_row_between([
          ui.heading_page("Users"),
          case query {
            option.Some(_) ->
              h.div([a.class("flex items-center gap-4")], [
                h.span([a.class("text-sm text-neutral-600")], [
                  element.text(
                    "Found "
                    <> int.to_string(response.total)
                    <> " results (showing "
                    <> int.to_string(list.length(response.users))
                    <> ")",
                  ),
                ]),
              ])
            option.None -> element.none()
          },
        ]),
        render_search_form(ctx, query),
        case query {
          option.Some(_) ->
            case list.is_empty(response.users) {
              True -> empty_search_results()
              False ->
                h.div([], [
                  render_users_grid(ctx, response.users),
                  pagination.pagination(ctx, response.total, limit, page, fn(p) {
                    build_pagination_url(p, query)
                  }),
                ])
            }
          option.None -> empty_state()
        },
      ])
    }
    Error(err) -> errors.api_error_view(ctx, err, option.None, option.None)
  }

  let html =
    layout.page(
      "Users",
      "users",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_search_form(ctx: Context, query: option.Option(String)) {
  ui.card(ui.PaddingSmall, [
    h.form([a.method("get"), a.class("flex flex-col gap-4")], [
      h.div([a.class("flex gap-2")], [
        h.input([
          a.type_("text"),
          a.name("q"),
          a.value(option.unwrap(query, "")),
          a.placeholder("Search by ID, username, email, or phone..."),
          a.class(
            "flex-1 px-4 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
          ),
          a.attribute("autocomplete", "off"),
        ]),
        ui.button_primary("Search", "submit", []),
        h.a(
          [
            href(ctx, "/users"),
            a.class(
              "px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg text-sm font-medium hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Clear")],
        ),
      ]),
      h.p([a.class("text-xs text-neutral-500")], [
        element.text(
          "Search supports: User ID, Username, Email, Phone number, and more",
        ),
      ]),
    ]),
  ])
}

fn render_users_grid(ctx: Context, users: List(common.UserLookupResult)) {
  h.div(
    [a.class("grid grid-cols-1 gap-4")],
    list.map(users, fn(user) { render_user_card(ctx, user) }),
  )
}

fn render_user_card(ctx: Context, user: common.UserLookupResult) {
  let badges = badge.get_user_badges(ctx.cdn_endpoint, user.flags)

  h.div(
    [
      a.class(
        "bg-white border border-neutral-200 rounded-lg overflow-hidden hover:border-neutral-300 transition-colors",
      ),
    ],
    [
      h.div([a.class("p-5")], [
        h.div([a.class("flex items-center gap-4")], [
          h.img([
            a.src(avatar.get_user_avatar_url(
              ctx.media_endpoint,
              ctx.cdn_endpoint,
              user.id,
              user.avatar,
              True,
              ctx.asset_version,
            )),
            a.alt(user.username),
            a.class("w-16 h-16 rounded-full flex-shrink-0"),
          ]),
          h.div([a.class("flex-1 min-w-0")], [
            h.div([a.class("flex items-center gap-2 mb-1")], [
              h.h2([a.class("text-base font-medium text-neutral-900")], [
                element.text(
                  user.username
                  <> "#"
                  <> user.format_discriminator(user.discriminator),
                ),
              ]),
              case user.bot {
                True ->
                  h.span(
                    [
                      a.class("px-2 py-0.5 bg-blue-100 text-blue-700 rounded"),
                    ],
                    [element.text("Bot")],
                  )
                False -> element.none()
              },
            ]),
            case list.is_empty(badges) {
              False ->
                h.div(
                  [a.class("flex items-center gap-1.5 mb-2")],
                  list.map(badges, fn(b) {
                    h.img([
                      a.src(b.icon),
                      a.alt(b.name),
                      a.title(b.name),
                      a.class("w-5 h-5"),
                    ])
                  }),
                )
              True -> element.none()
            },
            h.div([a.class("space-y-0.5")], [
              h.div([a.class("text-sm text-neutral-600")], [
                element.text("ID: " <> user.id),
              ]),
              case user.extract_timestamp(user.id) {
                Ok(created_at) ->
                  h.div([a.class("text-sm text-neutral-500")], [
                    element.text("Created: " <> created_at),
                  ])
                Error(_) -> element.none()
              },
            ]),
          ]),
          h.a(
            [
              href(ctx, "/users/" <> user.id),
              a.class(
                "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors flex-shrink-0 no-underline",
              ),
            ],
            [element.text("View Details")],
          ),
        ]),
      ]),
    ],
  )
}

fn build_pagination_url(page: Int, query: option.Option(String)) -> String {
  url_builder.build_url("/users", [
    #("page", option.Some(int.to_string(page))),
    #("q", query),
  ])
}

fn empty_state() {
  ui.card_empty([
    ui.text_muted("Enter a search query to find users"),
    ui.text_small_muted(
      "Search by User ID, Username, Email, Phone, or other attributes",
    ),
  ])
}

fn empty_search_results() {
  ui.card_empty([
    ui.text_muted("No users found"),
    ui.text_small_muted("Try adjusting your search query"),
  ])
}
