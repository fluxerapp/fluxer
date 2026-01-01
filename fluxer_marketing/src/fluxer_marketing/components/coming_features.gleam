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

import fluxer_marketing/i18n
import fluxer_marketing/web.{type Context}
import gleam/list
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let coming_features = [
    g_(i18n_ctx, "Federation"),
    g_(i18n_ctx, "Opt-in E2EE messaging"),

    g_(i18n_ctx, "Slash commands"),
    g_(i18n_ctx, "Message components"),
    g_(i18n_ctx, "DM folders"),

    g_(i18n_ctx, "Threads and forums"),
    g_(i18n_ctx, "Community templates"),
    g_(i18n_ctx, "Publish forums to the web"),
    g_(i18n_ctx, "RSS/Atom feeds for forums"),
    g_(i18n_ctx, "Polls & events"),
    g_(i18n_ctx, "Stage channels"),
    g_(i18n_ctx, "Event tickets"),
    g_(i18n_ctx, "Discovery"),

    g_(i18n_ctx, "Public profile URLs"),
    g_(i18n_ctx, "Profile connections"),
    g_(i18n_ctx, "Activity sharing"),

    g_(i18n_ctx, "Creator monetization"),
    g_(i18n_ctx, "Theme marketplace"),

    g_(i18n_ctx, "Global voice regions"),
    g_(i18n_ctx, "Better noise cancellation"),
    g_(i18n_ctx, "E2EE calls"),
    g_(i18n_ctx, "Call pop-out"),
    g_(i18n_ctx, "Soundboard"),
    g_(i18n_ctx, "Streamer mode"),
  ]

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-gray-50 to-white px-6 py-24 md:py-40",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("mb-20 md:mb-24 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-8 md:mb-10 text-black text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "What's coming next")),
            ],
          ),
          html.p(
            [
              attribute.class(
                "lead mx-auto max-w-3xl text-gray-700 text-xl md:text-2xl",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "The future is being built right now. These features are coming soon.",
              )),
            ],
          ),
        ]),
        html.div(
          [
            attribute.class("max-w-6xl mx-auto"),
          ],
          [
            html.div(
              [
                attribute.id("coming-features-list"),
                attribute.class("coming-features-masonry"),
              ],
              coming_features
                |> list.map(fn(feature) {
                  html.span(
                    [
                      attribute.class(
                        "feature-pill inline-block rounded-xl sm:rounded-2xl bg-white px-4 py-2.5 sm:px-6 sm:py-3.5 md:px-7 md:py-4.5 text-sm sm:text-base md:text-lg font-semibold text-gray-900 shadow-md border border-gray-200 whitespace-nowrap",
                      ),
                    ],
                    [html.text(feature)],
                  )
                }),
            ),
          ],
        ),
      ]),
    ],
  )
}
