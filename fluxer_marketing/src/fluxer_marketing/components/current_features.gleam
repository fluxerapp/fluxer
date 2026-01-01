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

import fluxer_marketing/components/feature_card
import fluxer_marketing/i18n
import fluxer_marketing/web.{type Context}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-[#4641D9] to-[#3832B8] px-6 py-20 md:py-32",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("mb-16 md:mb-20 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-6 md:mb-8 text-white text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "What's available today")),
            ],
          ),
          html.p(
            [
              attribute.class("lead mx-auto max-w-3xl text-white/90"),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "All the basics you expect, plus a few things you don't.",
              )),
            ],
          ),
        ]),
        html.div(
          [attribute.class("grid gap-8 md:gap-10 grid-cols-1 lg:grid-cols-2")],
          [
            feature_card.render(
              ctx,
              "chats",
              g_(i18n_ctx, "Messaging"),
              g_(
                i18n_ctx,
                "DM your friends, chat with groups, or build communities with channels.",
              ),
              [
                g_(i18n_ctx, "Full Markdown support in messages"),
                g_(i18n_ctx, "Private DMs and group chats"),
                g_(i18n_ctx, "Organized channels for communities"),
                g_(i18n_ctx, "Share files and preview links"),
              ],
              "dark",
            ),
            feature_card.render(
              ctx,
              "microphone",
              g_(i18n_ctx, "Voice & video"),
              g_(
                i18n_ctx,
                "Hop in a call with friends or share your screen to work together.",
              ),
              [
                g_(i18n_ctx, "Join from multiple devices at once"),
                g_(i18n_ctx, "Screen sharing built in"),
                g_(i18n_ctx, "Noise suppression and echo cancellation"),
                g_(i18n_ctx, "Mute, deafen, and camera controls"),
              ],
              "dark",
            ),
            feature_card.render(
              ctx,
              "gear",
              g_(i18n_ctx, "Moderation tools"),
              g_(
                i18n_ctx,
                "Keep your community running smoothly with roles, permissions, and logs.",
              ),
              [
                g_(i18n_ctx, "Granular roles and permissions"),
                g_(i18n_ctx, "Moderation actions and tools"),
                g_(i18n_ctx, "Audit logs for transparency"),
                g_(i18n_ctx, "Webhooks and bot support"),
              ],
              "dark",
            ),
            feature_card.render(
              ctx,
              "magnifying-glass",
              g_(i18n_ctx, "Search & quick switcher"),
              g_(
                i18n_ctx,
                "Find old messages or jump between communities and channels in seconds.",
              ),
              [
                g_(i18n_ctx, "Search through message history"),
                g_(i18n_ctx, "Filter by users, dates, and more"),
                g_(i18n_ctx, "Quick switcher with keyboard shortcuts"),
                g_(i18n_ctx, "Manage friends and block users"),
              ],
              "dark",
            ),
            feature_card.render(
              ctx,
              "palette",
              g_(i18n_ctx, "Customization"),
              g_(
                i18n_ctx,
                "Add custom emojis, save media for later, and style the app with custom CSS.",
              ),
              [
                g_(i18n_ctx, "Upload custom emojis and stickers"),
                g_(i18n_ctx, "Save images, videos, GIFs, and audio"),
                g_(i18n_ctx, "Custom CSS themes"),
                g_(i18n_ctx, "Compact mode and display options"),
              ],
              "dark",
            ),
            feature_card.render(
              ctx,
              "server",
              g_(i18n_ctx, "Self-hosting"),
              g_(
                i18n_ctx,
                "Run the Fluxer backend on your own hardware and connect with our apps.",
              ),
              [
                g_(i18n_ctx, "Fully open source (AGPLv3)"),
                g_(i18n_ctx, "Host your own instance"),
                g_(i18n_ctx, "Use our desktop client (mobile coming soon)"),
                g_(i18n_ctx, "Switch between multiple instances"),
              ],
              "dark",
            ),
          ],
        ),
      ]),
    ],
  )
}
