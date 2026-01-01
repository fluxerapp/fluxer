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
import fluxer_admin/api/guilds_members
import fluxer_admin/avatar
import fluxer_admin/badge
import fluxer_admin/components/ui
import fluxer_admin/user
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn members_tab(
  ctx: Context,
  session: Session,
  guild_id: String,
  admin_acls: List(String),
  page: Int,
) {
  let limit = 50
  let offset = page * limit

  case acl.has_permission(admin_acls, "guild:list:members") {
    True -> {
      case
        guilds_members.list_guild_members(ctx, session, guild_id, limit, offset)
      {
        Ok(response) ->
          render_members_list(ctx, guild_id, response, page, limit)
        Error(common.Forbidden(message)) ->
          render_error("Permission Denied", message)
        Error(common.NotFound) -> render_error("Not Found", "Guild not found.")
        Error(_) ->
          render_error(
            "Error",
            "Failed to load guild members. Please try again.",
          )
      }
    }
    False ->
      render_error(
        "Permission Denied",
        "You don't have permission to view guild members.",
      )
  }
}

fn render_error(title: String, message: String) {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin(title),
    h.p([a.class("text-sm text-neutral-600")], [element.text(message)]),
  ])
}

fn render_members_list(
  ctx: Context,
  guild_id: String,
  response: guilds_members.ListGuildMembersResponse,
  page: Int,
  limit: Int,
) {
  h.div([a.class("space-y-6")], [
    ui.card(ui.PaddingMedium, [
      h.div([a.class("flex justify-between items-center mb-4")], [
        ui.heading_card(
          "Guild Members (" <> int.to_string(response.total) <> ")",
        ),
        render_pagination_info(response.offset, response.limit, response.total),
      ]),
      case list.is_empty(response.members) {
        True ->
          h.p([a.class("text-sm text-neutral-600")], [
            element.text("No members found."),
          ])
        False ->
          h.div([a.class("space-y-2")], {
            list.map(response.members, render_member(ctx, _))
          })
      },
      render_pagination(ctx, guild_id, page, response.total, limit),
    ]),
  ])
}

fn render_pagination_info(offset: Int, limit: Int, total: Int) {
  let start = offset + 1
  let end = case offset + limit > total {
    True -> total
    False -> offset + limit
  }
  h.p([a.class("text-sm text-neutral-600")], [
    element.text(
      "Showing "
      <> int.to_string(start)
      <> "-"
      <> int.to_string(end)
      <> " of "
      <> int.to_string(total),
    ),
  ])
}

fn render_pagination(
  ctx: Context,
  guild_id: String,
  current_page: Int,
  total: Int,
  limit: Int,
) {
  let total_pages = { total + limit - 1 } / limit
  let has_previous = current_page > 0
  let has_next = current_page < total_pages - 1

  case total_pages > 1 {
    False -> element.none()
    True ->
      h.div([a.class("flex justify-between items-center mt-4 pt-4 border-t")], [
        case has_previous {
          True ->
            h.a(
              [
                href(
                  ctx,
                  "/guilds/"
                    <> guild_id
                    <> "?tab=members&page="
                    <> int.to_string(current_page - 1),
                ),
                a.class(
                  "px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                ),
              ],
              [element.text("← Previous")],
            )
          False ->
            h.div(
              [
                a.class(
                  "px-4 py-2 bg-neutral-300 text-neutral-500 rounded text-sm font-medium cursor-not-allowed",
                ),
              ],
              [element.text("← Previous")],
            )
        },
        h.div([a.class("text-sm text-neutral-600")], [
          element.text(
            "Page "
            <> int.to_string(current_page + 1)
            <> " of "
            <> int.to_string(total_pages),
          ),
        ]),
        case has_next {
          True ->
            h.a(
              [
                href(
                  ctx,
                  "/guilds/"
                    <> guild_id
                    <> "?tab=members&page="
                    <> int.to_string(current_page + 1),
                ),
                a.class(
                  "px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                ),
              ],
              [element.text("Next →")],
            )
          False ->
            h.div(
              [
                a.class(
                  "px-4 py-2 bg-neutral-300 text-neutral-500 rounded text-sm font-medium cursor-not-allowed",
                ),
              ],
              [element.text("Next →")],
            )
        },
      ])
  }
}

fn render_member(ctx: Context, member: guilds_members.GuildMember) {
  let badges =
    badge.get_user_badges(
      ctx.cdn_endpoint,
      int.to_string(member.user.public_flags),
    )

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
              member.user.id,
              member.user.avatar,
              True,
              ctx.asset_version,
            )),
            a.alt(member.user.username),
            a.class("w-16 h-16 rounded-full flex-shrink-0"),
          ]),
          h.div([a.class("flex-1 min-w-0")], [
            h.div([a.class("flex items-center gap-2 mb-1")], [
              h.h2([a.class("text-base font-medium text-neutral-900")], [
                element.text(
                  member.user.username
                  <> "#"
                  <> case int.parse(member.user.discriminator) {
                    Ok(disc_int) -> user.format_discriminator(disc_int)
                    Error(_) -> member.user.discriminator
                  },
                ),
              ]),
              case member.user.bot {
                True ->
                  h.span(
                    [
                      a.class("px-2 py-0.5 bg-blue-100 text-blue-700 rounded"),
                    ],
                    [element.text("Bot")],
                  )
                False -> element.none()
              },
              case member.nick {
                option.Some(nick) ->
                  h.span(
                    [
                      a.class("text-sm text-neutral-600 ml-2"),
                    ],
                    [
                      element.text("(" <> nick <> ")"),
                    ],
                  )
                option.None -> element.none()
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
                element.text("ID: " <> member.user.id),
              ]),
              case user.extract_timestamp(member.user.id) {
                Ok(created_at) ->
                  h.div([a.class("text-sm text-neutral-500")], [
                    element.text("Created: " <> created_at),
                  ])
                Error(_) -> element.none()
              },
              h.div([a.class("text-sm text-neutral-500")], [
                element.text("Joined: " <> format_date(member.joined_at)),
              ]),
              case member.roles != [] {
                True ->
                  h.div([a.class("text-sm text-neutral-500")], [
                    element.text(
                      int.to_string(list.length(member.roles)) <> " roles",
                    ),
                  ])
                False -> element.none()
              },
            ]),
          ]),
          h.a(
            [
              href(ctx, "/users/" <> member.user.id),
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

fn format_date(iso_date: String) -> String {
  case iso_date {
    _ ->
      case string.split(iso_date, "T") {
        [date, ..] -> date
        _ -> iso_date
      }
  }
}
