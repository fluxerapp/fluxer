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
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn guilds_tab(
  ctx: Context,
  session: Session,
  _user: common.UserLookupResult,
  user_id: String,
) {
  let guilds_result = users.list_user_guilds(ctx, session, user_id)

  case guilds_result {
    Ok(guilds_response) ->
      h.div([a.class("space-y-6")], [
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin(
            "Guilds ("
            <> int.to_string(list.length(guilds_response.guilds))
            <> ")",
          ),
          case list.is_empty(guilds_response.guilds) {
            True ->
              h.p([a.class("text-sm text-neutral-600")], [
                element.text("No guilds"),
              ])
            False -> render_guilds_grid(ctx, guilds_response.guilds)
          },
        ]),
      ])
    Error(_) -> element.none()
  }
}

fn render_guilds_grid(ctx: Context, guilds: List(users.UserGuild)) {
  h.div(
    [a.class("grid grid-cols-1 gap-4")],
    list.map(guilds, fn(guild) { render_guild_card(ctx, guild) }),
  )
}

fn render_guild_card(ctx: Context, guild: users.UserGuild) {
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
                "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm hover:bg-neutral-800 transition-colors flex-shrink-0",
              ),
            ],
            [element.text("View Details")],
          ),
        ]),
      ]),
    ],
  )
}
