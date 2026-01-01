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
import fluxer_admin/web.{type Context, action}
import gleam/int
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn settings_tab(
  ctx: Context,
  guild: guilds.GuildLookupResult,
  guild_id: String,
  admin_acls: List(String),
) {
  h.div([a.class("space-y-6")], [
    case acl.has_permission(admin_acls, "guild:update:settings") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Guild Settings"),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/" <> guild_id <> "?action=update-settings&tab=settings",
              ),
            ],
            [
              h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4")], [
                h.div([], [
                  h.label(
                    [
                      a.class("block text-sm font-medium text-neutral-600 mb-1"),
                    ],
                    [element.text("Verification Level")],
                  ),
                  h.select(
                    [
                      a.name("verification_level"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                      ),
                    ],
                    [
                      option_element("0", "None", guild.verification_level == 0),
                      option_element(
                        "1",
                        "Low (verified email)",
                        guild.verification_level == 1,
                      ),
                      option_element(
                        "2",
                        "Medium (5+ minutes)",
                        guild.verification_level == 2,
                      ),
                      option_element(
                        "3",
                        "High (10+ minutes)",
                        guild.verification_level == 3,
                      ),
                      option_element(
                        "4",
                        "Very High (verified phone)",
                        guild.verification_level == 4,
                      ),
                    ],
                  ),
                ]),
                h.div([], [
                  h.label(
                    [
                      a.class("block text-sm font-medium text-neutral-600 mb-1"),
                    ],
                    [element.text("MFA Level")],
                  ),
                  h.select(
                    [
                      a.name("mfa_level"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                      ),
                    ],
                    [
                      option_element("0", "None", guild.mfa_level == 0),
                      option_element("1", "Elevated", guild.mfa_level == 1),
                    ],
                  ),
                ]),
                h.div([], [
                  h.label(
                    [
                      a.class("block text-sm font-medium text-neutral-600 mb-1"),
                    ],
                    [element.text("NSFW Level")],
                  ),
                  h.select(
                    [
                      a.name("nsfw_level"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                      ),
                    ],
                    [
                      option_element("0", "Default", guild.nsfw_level == 0),
                      option_element("1", "Explicit", guild.nsfw_level == 1),
                      option_element("2", "Safe", guild.nsfw_level == 2),
                      option_element(
                        "3",
                        "Age Restricted",
                        guild.nsfw_level == 3,
                      ),
                    ],
                  ),
                ]),
                h.div([], [
                  h.label(
                    [
                      a.class("block text-sm font-medium text-neutral-600 mb-1"),
                    ],
                    [element.text("Explicit Content Filter")],
                  ),
                  h.select(
                    [
                      a.name("explicit_content_filter"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                      ),
                    ],
                    [
                      option_element(
                        "0",
                        "Disabled",
                        guild.explicit_content_filter == 0,
                      ),
                      option_element(
                        "1",
                        "Members without roles",
                        guild.explicit_content_filter == 1,
                      ),
                      option_element(
                        "2",
                        "All members",
                        guild.explicit_content_filter == 2,
                      ),
                    ],
                  ),
                ]),
                h.div([], [
                  h.label(
                    [
                      a.class("block text-sm font-medium text-neutral-600 mb-1"),
                    ],
                    [element.text("Default Notifications")],
                  ),
                  h.select(
                    [
                      a.name("default_message_notifications"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                      ),
                    ],
                    [
                      option_element(
                        "0",
                        "All messages",
                        guild.default_message_notifications == 0,
                      ),
                      option_element(
                        "1",
                        "Only mentions",
                        guild.default_message_notifications == 1,
                      ),
                    ],
                  ),
                ]),
              ]),
              h.div([a.class("mt-6 pt-6 border-t border-neutral-200")], [
                ui.button_primary("Save Settings", "submit", []),
              ]),
            ],
          ),
        ])
      False ->
        ui.card(ui.PaddingMedium, [
          h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
            element.text("Guild Settings"),
          ]),
          info_grid([
            #(
              "Verification Level",
              verification_level_to_string(guild.verification_level),
            ),
            #("MFA Level", mfa_level_to_string(guild.mfa_level)),
            #("NSFW Level", nsfw_level_to_string(guild.nsfw_level)),
            #(
              "Explicit Content Filter",
              content_filter_to_string(guild.explicit_content_filter),
            ),
            #(
              "Default Notifications",
              notification_level_to_string(guild.default_message_notifications),
            ),
            #("AFK Timeout", int.to_string(guild.afk_timeout) <> " seconds"),
          ]),
        ])
    },
    case acl.has_permission(admin_acls, "guild:update:settings") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Disabled Operations"),
          forms.render_disabled_operations_form(
            ctx,
            guild.disabled_operations,
            guild_id,
          ),
        ])
      False ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Disabled Operations"),
          h.p([a.class("text-sm text-neutral-600")], [
            element.text(
              "Bitfield value: " <> int.to_string(guild.disabled_operations),
            ),
          ]),
        ])
    },
    case acl.has_permission(admin_acls, "guild:update:settings") {
      True ->
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Clear Guild Fields"),
          h.form(
            [
              a.method("POST"),
              action(
                ctx,
                "/guilds/" <> guild_id <> "?action=clear-fields&tab=settings",
              ),
              a.attribute(
                "onsubmit",
                "return confirm('Are you sure you want to clear these fields?')",
              ),
            ],
            [
              h.div([a.class("space-y-2 mb-3")], [
                h.label([a.class("flex items-center gap-2")], [
                  h.input([
                    a.type_("checkbox"),
                    a.name("fields"),
                    a.value("icon"),
                  ]),
                  h.span([a.class("text-sm")], [element.text("Icon")]),
                ]),
                h.label([a.class("flex items-center gap-2")], [
                  h.input([
                    a.type_("checkbox"),
                    a.name("fields"),
                    a.value("banner"),
                  ]),
                  h.span([a.class("text-sm")], [element.text("Banner")]),
                ]),
                h.label([a.class("flex items-center gap-2")], [
                  h.input([
                    a.type_("checkbox"),
                    a.name("fields"),
                    a.value("splash"),
                  ]),
                  h.span([a.class("text-sm")], [element.text("Splash")]),
                ]),
              ]),
              ui.button_danger("Clear Selected Fields", "submit", []),
            ],
          ),
        ])
      False -> element.none()
    },
  ])
}

fn info_grid(items: List(#(String, String))) {
  let info_items =
    list.map(items, fn(item) {
      let #(label, value) = item
      ui.info_item_text(label, value)
    })

  ui.info_grid(info_items)
}

fn verification_level_to_string(level: Int) -> String {
  case level {
    0 -> "None"
    1 -> "Low (verified email)"
    2 -> "Medium (registered for 5 minutes)"
    3 -> "High (member for 10 minutes)"
    4 -> "Very High (verified phone)"
    _ -> "Unknown (" <> int.to_string(level) <> ")"
  }
}

fn mfa_level_to_string(level: Int) -> String {
  case level {
    0 -> "None"
    1 -> "Elevated"
    _ -> "Unknown (" <> int.to_string(level) <> ")"
  }
}

fn nsfw_level_to_string(level: Int) -> String {
  case level {
    0 -> "Default"
    1 -> "Explicit"
    2 -> "Safe"
    3 -> "Age Restricted"
    _ -> "Unknown (" <> int.to_string(level) <> ")"
  }
}

fn content_filter_to_string(level: Int) -> String {
  case level {
    0 -> "Disabled"
    1 -> "Members without roles"
    2 -> "All members"
    _ -> "Unknown (" <> int.to_string(level) <> ")"
  }
}

fn notification_level_to_string(level: Int) -> String {
  case level {
    0 -> "All messages"
    1 -> "Only mentions"
    _ -> "Unknown (" <> int.to_string(level) <> ")"
  }
}

fn option_element(value: String, label: String, selected: Bool) {
  element.element("option", [a.value(value), a.selected(selected)], [
    element.text(label),
  ])
}
