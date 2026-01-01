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

import fluxer_marketing/components/community_type
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
        "bg-gradient-to-b from-white to-gray-50 px-6 py-24 md:py-40",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl text-center")], [
        html.h2(
          [
            attribute.class(
              "display mb-16 md:mb-20 text-black text-4xl md:text-5xl lg:text-6xl",
            ),
          ],
          [
            html.text(g_(i18n_ctx, "Built for communities of all kinds.")),
          ],
        ),
        html.div(
          [
            attribute.class(
              "mb-16 md:mb-20 grid grid-cols-5 gap-2 sm:gap-4 md:gap-6 max-w-6xl mx-auto",
            ),
          ],
          [
            community_type.render(
              "game-controller",
              "blue",
              g_(i18n_ctx, "Gaming"),
            ),
            community_type.render(
              "video-camera",
              "purple",
              g_(i18n_ctx, "Creators"),
            ),
            community_type.render(
              "graduation-cap",
              "green",
              g_(i18n_ctx, "Education"),
            ),
            community_type.render(
              "users-three",
              "orange",
              g_(i18n_ctx, "Hobbyists"),
            ),
            community_type.render("code", "red", g_(i18n_ctx, "Developers")),
          ],
        ),
        html.p(
          [
            attribute.class(
              "lead lead-soft mx-auto max-w-4xl text-gray-700 leading-relaxed",
            ),
          ],
          [
            html.text(g_(
              i18n_ctx,
              "A chat platform that answers to you, not investors. It's ad-free, open source, community-funded, and never sells your data or nags you with upgrade pop-ups.",
            )),
          ],
        ),
        html.div(
          [
            attribute.class("my-6 mx-auto flex flex-col items-center gap-3"),
            attribute.attribute("aria-hidden", "true"),
          ],
          [],
        ),
        html.p(
          [
            attribute.class(
              "lead lead-soft mx-auto max-w-4xl text-gray-700 leading-relaxed",
            ),
          ],
          [
            html.text(g_(
              i18n_ctx,
              "Over time, we'd love to explore optional monetization tools that help creators and communities earn, with a small, transparent fee that keeps the app sustainable.",
            )),
          ],
        ),
      ]),
    ],
  )
}
