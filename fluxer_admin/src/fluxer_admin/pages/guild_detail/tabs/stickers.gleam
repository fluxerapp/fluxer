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
import fluxer_admin/api/guild_assets
import fluxer_admin/components/errors
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session, action, href}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn stickers_tab(
  ctx: Context,
  session: Session,
  guild_id: String,
  admin_acls: List(String),
) {
  case acl.has_permission(admin_acls, constants.acl_asset_purge) {
    True ->
      case guild_assets.list_guild_stickers(ctx, session, guild_id) {
        Ok(response) -> render_stickers(ctx, guild_id, response.stickers)
        Error(err) ->
          errors.api_error_view(
            ctx,
            err,
            option.Some("/guilds/" <> guild_id <> "?tab=stickers"),
            option.Some("Back to Guild"),
          )
      }
    False -> render_permission_notice()
  }
}

fn render_stickers(
  ctx: Context,
  guild_id: String,
  stickers: List(guild_assets.GuildStickerAsset),
) {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin(
      "Stickers (" <> int.to_string(list.length(stickers)) <> ")",
    ),
    case list.is_empty(stickers) {
      True ->
        h.p([a.class("text-sm text-neutral-600")], [
          element.text("No stickers found for this guild."),
        ])
      False ->
        h.div(
          [a.class("mt-4 grid gap-4 md:grid-cols-2 xl:grid-cols-3")],
          list.map(stickers, fn(sticker) {
            render_sticker_card(ctx, guild_id, sticker)
          }),
        )
    },
  ])
}

fn render_sticker_card(
  ctx: Context,
  guild_id: String,
  sticker: guild_assets.GuildStickerAsset,
) {
  h.div(
    [
      a.class(
        "flex flex-col border border-neutral-200 rounded-lg overflow-hidden bg-white shadow-sm",
      ),
    ],
    [
      h.div(
        [a.class("bg-neutral-100 flex items-center justify-center p-6 h-32")],
        [
          h.img([
            a.src(sticker.media_url),
            a.alt(sticker.name),
            a.class("max-h-full max-w-full object-contain"),
            a.loading("lazy"),
          ]),
        ],
      ),
      h.div([a.class("px-4 py-3 flex-1 flex flex-col")], [
        h.div([a.class("flex items-center justify-between gap-2")], [
          h.span([a.class("text-sm font-semibold text-neutral-900")], [
            element.text(sticker.name),
          ]),
          h.span(
            [
              a.class(
                "text-xs font-semibold uppercase tracking-wide text-neutral-500 px-2 py-0.5 border border-neutral-200 rounded",
              ),
            ],
            [
              element.text(sticker_format_label(sticker.format_type)),
            ],
          ),
        ]),
        h.p([a.class("text-xs text-neutral-500 mt-1 break-words")], [
          element.text("ID: " <> sticker.id),
        ]),
        h.a(
          [
            href(ctx, "/users/" <> sticker.creator_id),
            a.class("text-xs text-blue-600 hover:underline mt-1"),
          ],
          [
            element.text("Uploader: " <> sticker.creator_id),
          ],
        ),
        h.form(
          [
            action(
              ctx,
              "/guilds/" <> guild_id <> "?tab=stickers&action=delete-sticker",
            ),
            a.method("post"),
            a.class("mt-4"),
          ],
          [
            h.input([
              a.type_("hidden"),
              a.name("sticker_id"),
              a.value(sticker.id),
            ]),
            ui.button("Delete Sticker", "submit", ui.Danger, ui.Small, ui.Full, [
              a.class("mt-2"),
            ]),
          ],
        ),
      ]),
    ],
  )
}

fn sticker_format_label(format_type: Int) -> String {
  case format_type {
    4 -> "GIF"
    _ -> "PNG"
  }
}

fn render_permission_notice() {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Permission required"),
    h.p([a.class("text-sm text-neutral-600")], [
      element.text("You need the asset:purge ACL to manage guild stickers."),
    ]),
  ])
}
