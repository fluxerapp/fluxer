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
import fluxer_admin/api/guilds
import fluxer_admin/avatar
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/pagination
import fluxer_admin/components/ui
import fluxer_admin/components/url_builder
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
        "" -> Ok(guilds.SearchGuildsResponse(guilds: [], total: 0))
        trimmed_query ->
          guilds.search_guilds(ctx, session, trimmed_query, limit, offset)
      }
    option.None -> Ok(guilds.SearchGuildsResponse(guilds: [], total: 0))
  }

  let content = case result {
    Ok(response) -> {
      h.div([a.class("max-w-7xl mx-auto space-y-6")], [
        ui.flex_row_between([
          ui.heading_page("Guilds"),
          case query {
            option.Some(_) ->
              h.div([a.class("flex items-center gap-4")], [
                h.span([a.class("text-sm text-neutral-600")], [
                  element.text(
                    "Found "
                    <> int.to_string(response.total)
                    <> " results (showing "
                    <> int.to_string(list.length(response.guilds))
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
            case list.is_empty(response.guilds) {
              True -> empty_search_results()
              False ->
                h.div([], [
                  render_guilds_grid(ctx, response.guilds),
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
      "Guilds",
      "guilds",
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
          a.placeholder("Search by ID, guild name, or vanity URL..."),
          a.class(
            "flex-1 px-4 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:border-transparent",
          ),
          a.attribute("autocomplete", "off"),
        ]),
        ui.button_primary("Search", "submit", []),
        h.a(
          [
            href(ctx, "/guilds"),
            a.class(
              "px-4 py-2 bg-white text-neutral-700 border border-neutral-300 rounded-lg text-sm font-medium hover:bg-neutral-50 transition-colors",
            ),
          ],
          [element.text("Clear")],
        ),
      ]),
      h.p([a.class("text-xs text-neutral-500")], [
        element.text(
          "Search supports: Guild ID, Guild Name, Vanity URL, and more",
        ),
      ]),
    ]),
  ])
}

fn render_guilds_grid(ctx: Context, guilds: List(guilds.GuildSearchResult)) {
  h.div(
    [a.class("grid grid-cols-1 gap-4")],
    list.map(guilds, fn(guild) { render_guild_card(ctx, guild) }),
  )
}

fn render_guild_card(ctx: Context, guild: guilds.GuildSearchResult) {
  h.div(
    [
      a.class(
        "bg-white border border-neutral-200 rounded-lg overflow-hidden hover:border-neutral-300 transition-colors",
      ),
    ],
    [
      h.div([a.class("p-5")], [
        h.div([a.class("flex items-center gap-4")], [
          case
            avatar.get_guild_icon_url(
              ctx.media_endpoint,
              guild.id,
              guild.icon,
              True,
            )
          {
            option.Some(icon_url) ->
              h.div([a.class("flex-shrink-0")], [
                h.img([
                  a.src(icon_url),
                  a.alt(guild.name),
                  a.class("w-16 h-16 rounded-full"),
                ]),
              ])
            option.None ->
              h.div([a.class("flex-shrink-0")], [
                h.div(
                  [
                    a.class(
                      "w-16 h-16 rounded-full bg-neutral-200 flex items-center justify-center text-base font-medium text-neutral-600",
                    ),
                  ],
                  [element.text(avatar.get_initials_from_name(guild.name))],
                ),
              ])
          },
          h.div([a.class("flex-1 min-w-0")], [
            h.div([a.class("flex items-center gap-2 mb-2")], [
              h.h2([a.class("text-base font-medium text-neutral-900")], [
                element.text(guild.name),
              ]),
              case list.is_empty(guild.features) {
                False ->
                  h.span(
                    [
                      a.class(
                        "px-2 py-0.5 bg-purple-100 text-purple-700 text-xs rounded uppercase",
                      ),
                    ],
                    [element.text("Featured")],
                  )
                True -> element.none()
              },
            ]),
            h.div([a.class("space-y-0.5")], [
              h.div([a.class("text-sm text-neutral-600")], [
                element.text("ID: " <> guild.id),
              ]),
              h.div([a.class("text-sm text-neutral-600")], [
                element.text("Members: " <> int.to_string(guild.member_count)),
              ]),
              h.div([a.class("text-sm text-neutral-600")], [
                element.text("Owner: "),
                h.a(
                  [
                    href(ctx, "/users/" <> guild.owner_id),
                    a.class(
                      "hover:text-blue-600 hover:underline transition-colors",
                    ),
                  ],
                  [element.text(guild.owner_id)],
                ),
              ]),
            ]),
          ]),
          h.a(
            [
              href(ctx, "/guilds/" <> guild.id),
              a.class(
                "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm hover:bg-neutral-800 transition-colors flex-shrink-0 no-underline",
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
  url_builder.build_url("/guilds", [
    #("page", option.Some(int.to_string(page))),
    #("q", query),
  ])
}

fn empty_state() {
  ui.card_empty([
    ui.text_muted("Enter a search query to find guilds"),
    ui.text_small_muted(
      "Search by Guild ID, Guild Name, Vanity URL, or other attributes",
    ),
  ])
}

fn empty_search_results() {
  ui.card_empty([
    ui.text_muted("No guilds found"),
    ui.text_small_muted("Try adjusting your search query"),
  ])
}
