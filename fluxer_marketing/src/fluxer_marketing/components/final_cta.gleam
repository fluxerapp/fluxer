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
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class("bg-gradient-to-b from-white to-gray-50"),
    ],
    [
      html.div(
        [
          attribute.class(
            "rounded-t-3xl bg-gradient-to-b from-[#4641D9] to-[#3832B8] text-white",
          ),
        ],
        [
          html.div(
            [
              attribute.class(
                "px-4 py-24 md:px-8 md:py-32 lg:py-40 text-center",
              ),
            ],
            [
              html.h2(
                [
                  attribute.class(
                    "display mb-8 md:mb-10 text-white text-5xl md:text-7xl lg:text-8xl font-bold",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "We need your support to make this work.",
                  )),
                ],
              ),
              html.p(
                [
                  attribute.class(
                    "lead mb-12 md:mb-14 text-white/90 text-xl md:text-2xl",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "Create an account and help us build something good.",
                  )),
                ],
              ),
              html.a(
                [
                  attribute.href(ctx.app_endpoint <> "/register"),
                  attribute.class(
                    "label inline-block rounded-xl bg-white px-10 py-5 md:px-12 md:py-6 text-lg md:text-xl text-[#4641D9] transition-colors hover:bg-opacity-90 shadow-lg",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Join Fluxer"))],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}
