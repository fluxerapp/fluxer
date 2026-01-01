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

import birl
import fluxer_admin/api/guilds
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, action, href}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

const fluxer_epoch = 1_420_070_400_000

fn get_current_snowflake() -> String {
  let now = birl.now() |> birl.to_unix_milli
  let timestamp_offset = now - fluxer_epoch
  let snowflake = timestamp_offset * 4_194_304
  int.to_string(snowflake)
}

pub fn overview_tab(ctx: Context, guild: guilds.GuildLookupResult) {
  h.div([a.class("space-y-6")], [
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin("Guild Information"),
      info_grid(ctx, guild, [
        #("Guild ID", guild.id),
        #("Name", guild.name),
        #("Member Count", int.to_string(guild.member_count)),
        #("Vanity URL", case guild.vanity_url_code {
          option.Some(vanity) -> vanity
          option.None -> "None"
        }),
      ]),
    ]),
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin("Features"),
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
    ]),
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin(
        "Channels (" <> int.to_string(list.length(guild.channels)) <> ")",
      ),
      case list.is_empty(guild.channels) {
        True ->
          h.p([a.class("text-sm text-neutral-600")], [
            element.text("No channels"),
          ])
        False ->
          h.div([a.class("space-y-2")], {
            list.map(
              list.sort(guild.channels, fn(a, b) {
                int.compare(a.position, b.position)
              }),
              fn(channel) { render_channel(ctx, channel) },
            )
          })
      },
    ]),
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin(
        "Roles (" <> int.to_string(list.length(guild.roles)) <> ")",
      ),
      case list.is_empty(guild.roles) {
        True ->
          h.p([a.class("text-sm text-neutral-600")], [element.text("No roles")])
        False ->
          h.div([a.class("space-y-2")], {
            list.map(
              list.sort(guild.roles, fn(a, b) {
                int.compare(b.position, a.position)
              }),
              render_role,
            )
          })
      },
    ]),
    ui.card(ui.PaddingMedium, [
      ui.heading_card_with_margin("Search Index Management"),
      h.p([a.class("text-sm text-neutral-600 mb-4")], [
        element.text("Refresh search indexes for this guild."),
      ]),
      h.div([a.class("space-y-3")], [
        render_search_index_button(
          ctx,
          guild.id,
          "Channel Messages",
          "channel_messages",
        ),
      ]),
    ]),
  ])
}

fn info_grid(
  ctx: Context,
  guild: guilds.GuildLookupResult,
  items: List(#(String, String)),
) {
  let owner_item =
    ui.info_item(
      "Owner ID",
      h.a(
        [
          href(ctx, "/users/" <> guild.owner_id),
          a.class(
            "text-sm text-neutral-900 hover:text-blue-600 hover:underline",
          ),
        ],
        [element.text(guild.owner_id)],
      ),
    )

  let other_items =
    list.map(items, fn(item) {
      let #(label, value) = item
      ui.info_item_text(label, value)
    })

  ui.info_grid([owner_item, ..other_items])
}

fn render_channel(ctx: Context, channel: guilds.GuildChannel) {
  let current_snowflake = get_current_snowflake()
  h.a(
    [
      href(
        ctx,
        "/messages?channel_id="
          <> channel.id
          <> "&message_id="
          <> current_snowflake
          <> "&context_limit=50",
      ),
      a.class(
        "flex items-center gap-3 p-3 bg-neutral-50 rounded border border-neutral-200 hover:bg-neutral-100 transition-colors",
      ),
    ],
    [
      h.div([a.class("flex-1")], [
        h.div([a.class("text-sm font-medium text-neutral-900")], [
          element.text(channel.name),
        ]),
        h.div([a.class("text-sm text-neutral-600")], [
          element.text(channel.id),
        ]),
      ]),
      h.div([a.class("text-sm text-neutral-600")], [
        element.text(channel_type_to_string(channel.type_)),
      ]),
    ],
  )
}

fn render_role(role: guilds.GuildRole) {
  let color_hex = int_to_hex(role.color)
  h.div(
    [
      a.class(
        "flex items-center gap-3 p-3 bg-neutral-50 rounded border border-neutral-200",
      ),
    ],
    [
      h.div(
        [
          a.class("w-4 h-4 rounded"),
          a.attribute("style", "background-color: #" <> color_hex),
        ],
        [],
      ),
      h.div([a.class("flex-1")], [
        h.div([a.class("text-sm font-medium text-neutral-900")], [
          element.text(role.name),
        ]),
        h.div([a.class("text-sm text-neutral-600")], [
          element.text(role.id),
        ]),
      ]),
      h.div([a.class("flex gap-2")], [
        case role.hoist {
          True ->
            h.span(
              [
                a.class("px-2 py-0.5 bg-blue-100 text-blue-700 text-xs rounded"),
              ],
              [element.text("Hoisted")],
            )
          False -> element.none()
        },
        case role.mentionable {
          True ->
            h.span(
              [
                a.class(
                  "px-2 py-0.5 bg-green-100 text-green-700 text-xs rounded",
                ),
              ],
              [element.text("Mentionable")],
            )
          False -> element.none()
        },
      ]),
    ],
  )
}

fn channel_type_to_string(type_: Int) -> String {
  case type_ {
    0 -> "Text"
    2 -> "Voice"
    4 -> "Category"
    _ -> "Unknown (" <> int.to_string(type_) <> ")"
  }
}

fn int_to_hex(i: Int) -> String {
  let hex_digits = "0123456789ABCDEF"
  case i {
    0 -> "000000"
    _ -> {
      let r = i / 65_536 % 256
      let g = i / 256 % 256
      let b = i % 256
      byte_to_hex(r, hex_digits)
      <> byte_to_hex(g, hex_digits)
      <> byte_to_hex(b, hex_digits)
    }
  }
}

fn byte_to_hex(byte: Int, _hex_digits: String) -> String {
  let high = byte / 16
  let low = byte % 16
  let high_str = case high {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    15 -> "F"
    _ -> "0"
  }
  let low_str = case low {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    15 -> "F"
    _ -> "0"
  }
  high_str <> low_str
}

fn render_search_index_button(
  ctx: Context,
  guild_id: String,
  title: String,
  index_type: String,
) {
  h.form(
    [
      a.class("flex"),
      a.method("post"),
      action(ctx, "/guilds/" <> guild_id <> "?action=refresh-search-index"),
    ],
    [
      h.input([a.type_("hidden"), a.name("index_type"), a.value(index_type)]),
      h.input([a.type_("hidden"), a.name("guild_id"), a.value(guild_id)]),
      h.button(
        [
          a.type_("submit"),
          a.class(
            "w-full px-4 py-3 rounded-lg border border-neutral-300 bg-white text-neutral-900 text-sm font-medium hover:bg-neutral-100 transition-colors",
          ),
        ],
        [element.text("Refresh " <> title)],
      ),
    ],
  )
}
