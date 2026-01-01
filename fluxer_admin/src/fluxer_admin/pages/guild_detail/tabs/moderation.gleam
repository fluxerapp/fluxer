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
import fluxer_admin/constants
import fluxer_admin/web.{type Context, action}
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn moderation_tab(
  ctx: Context,
  _guild: guilds.GuildLookupResult,
  guild_id: String,
  admin_acls: List(String),
) {
  h.div([a.class("space-y-6")], [
    case acl.has_permission(admin_acls, "guild:update:name") {
      True ->
        ui.card(ui.PaddingMedium, [
          h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
            element.text("Update Guild Name"),
          ]),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/" <> guild_id <> "?action=update-name&tab=moderation",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to change this guild\\'s name?')",
              ),
            ],
            [
              h.input([
                a.type_("text"),
                a.name("name"),
                a.placeholder("New guild name"),
                a.required(True),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm mb-3",
                ),
              ]),
              ui.button_primary("Update Name", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
    case acl.has_permission(admin_acls, "guild:update:vanity") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Update Vanity URL"),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/" <> guild_id <> "?action=update-vanity&tab=moderation",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to change this guild\\'s vanity URL?')",
              ),
            ],
            [
              h.input([
                a.type_("text"),
                a.name("vanity_url_code"),
                a.placeholder("vanity-code (leave empty to remove)"),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm mb-3",
                ),
              ]),
              ui.button_primary("Update Vanity URL", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
    case acl.has_permission(admin_acls, "guild:transfer_ownership") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Transfer Ownership"),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/"
                  <> guild_id
                  <> "?action=transfer-ownership&tab=moderation",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to transfer ownership of this guild? This action cannot be easily undone.')",
              ),
            ],
            [
              h.input([
                a.type_("text"),
                a.name("new_owner_id"),
                a.placeholder("New owner user ID"),
                a.required(True),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm mb-3",
                ),
              ]),
              ui.button_danger("Transfer Ownership", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
    case acl.has_permission(admin_acls, "guild:force_add_user") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Force Add User to Guild"),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/"
                  <> guild_id
                  <> "?action=force-add-user&tab=moderation",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to force add this user to the guild?')",
              ),
            ],
            [
              h.input([
                a.type_("text"),
                a.name("user_id"),
                a.placeholder("User ID to add"),
                a.required(True),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm mb-3",
                ),
              ]),
              ui.button_primary("Add User", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
    case
      acl.has_permission(admin_acls, "guild:reload")
      || acl.has_permission(admin_acls, "guild:shutdown")
    {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Guild Process Controls"),
          h.div([a.class("flex flex-wrap gap-3")], [
            case acl.has_permission(admin_acls, "guild:reload") {
              True ->
                h.form(
                  [
                    a.method("POST"),
                    action(
                      ctx,
                      "/guilds/" <> guild_id <> "?action=reload&tab=moderation",
                    ),
                    a.attribute(
                      "onsubmit",
                      "return confirm('Are you sure you want to reload this guild process?')",
                    ),
                  ],
                  [
                    ui.button_success("Reload Guild", "submit", []),
                  ],
                )
              False -> element.none()
            },
            case acl.has_permission(admin_acls, "guild:shutdown") {
              True ->
                h.form(
                  [
                    a.method("POST"),
                    action(
                      ctx,
                      "/guilds/"
                        <> guild_id
                        <> "?action=shutdown&tab=moderation",
                    ),
                    a.attribute(
                      "onsubmit",
                      "return confirm('Are you sure you want to shutdown this guild process?')",
                    ),
                  ],
                  [
                    ui.button_danger("Shutdown Guild", "submit", []),
                  ],
                )
              False -> element.none()
            },
          ]),
        ])
      False -> element.none()
    },
    case acl.has_permission(admin_acls, constants.acl_guild_delete) {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Delete Guild"),
          h.p([a.class("text-sm text-neutral-600 mb-4")], [
            element.text(
              "Deleting a guild permanently removes it and all associated data. This action cannot be undone.",
            ),
          ]),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/" <> guild_id <> "?action=delete-guild&tab=moderation",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to permanently delete this guild? This action cannot be undone.')",
              ),
            ],
            [
              ui.button_danger("Delete Guild", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
  ])
}
