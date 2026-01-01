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
import fluxer_admin/api/guilds
import fluxer_admin/components/ui
import fluxer_admin/pages/guild_detail/forms
import fluxer_admin/web.{type Context}
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn features_tab(
  ctx: Context,
  guild: guilds.GuildLookupResult,
  guild_id: String,
  admin_acls: List(String),
) {
  h.div([a.class("space-y-6")], [
    case acl.has_permission(admin_acls, "guild:update:features") {
      True ->
        ui.card(ui.PaddingMedium, [
          h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
            element.text("Guild Features"),
          ]),
          h.p([a.class("text-sm text-neutral-600 mb-4")], [
            element.text("Select which features are enabled for this guild."),
          ]),
          forms.render_features_form(ctx, guild.features, guild_id),
        ])
      False ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Guild Features"),
          case list.is_empty(guild.features) {
            True ->
              h.p([a.class("text-sm text-neutral-600")], [
                element.text("No features enabled"),
              ])
            False ->
              h.div([a.class("flex flex-wrap gap-2")], {
                list.map(guild.features, fn(feature) {
                  h.span(
                    [
                      a.class(
                        "px-3 py-1 bg-purple-100 text-purple-700 text-sm rounded",
                      ),
                    ],
                    [element.text(feature)],
                  )
                })
              })
          },
        ])
    },
  ])
}
