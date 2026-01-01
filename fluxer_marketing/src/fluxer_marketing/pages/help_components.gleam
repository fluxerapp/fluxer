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

pub fn hero_section(
  ctx: Context,
  title: String,
  subtitle: String,
  search_action: String,
  search_placeholder: String,
  search_value: String,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class("px-6 pt-48 md:pt-60 pb-16 md:pb-20 lg:pb-24 text-white"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-4xl text-center")], [
        html.div([attribute.class("mb-5 flex justify-center")], [
          html.span(
            [
              attribute.class(
                "inline-flex items-center gap-2 rounded-full bg-white/10 px-4 py-2 text-sm font-semibold uppercase tracking-wide text-white/80 backdrop-blur-sm",
              ),
            ],
            [html.text(g_(i18n_ctx, "Help Center"))],
          ),
        ]),
        html.h1(
          [
            attribute.class("hero mb-4 md:mb-5"),
          ],
          [
            html.text(title),
          ],
        ),
        html.p(
          [
            attribute.class(
              "body-lg mb-8 md:mb-10 text-white/90 max-w-2xl mx-auto",
            ),
          ],
          [
            html.text(subtitle),
          ],
        ),
        html.form(
          [
            attribute.method("GET"),
            attribute.action(search_action),
            attribute.class("mx-auto max-w-2xl"),
          ],
          [
            html.div([attribute.class("relative flex items-center")], [
              html.input([
                attribute.type_("text"),
                attribute.name("q"),
                attribute.value(search_value),
                attribute.placeholder(search_placeholder),
                attribute.class(
                  "w-full rounded-xl border border-white/10 bg-white/95 pl-4 pr-24 sm:pr-28 py-4 text-base sm:text-lg text-black shadow-lg outline-none focus-visible:ring-2 focus-visible:ring-[#4641D9]/70 placeholder:hidden sm:placeholder:inline",
                ),
              ]),
              html.button(
                [
                  attribute.type_("submit"),
                  attribute.class(
                    "label absolute right-2 top-1/2 -translate-y-1/2 rounded-lg bg-[#4641D9] px-5 py-2 text-sm md:text-base text-white shadow-md transition hover:bg-[#3d38c7]",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Search"))],
              ),
            ]),
          ],
        ),
      ]),
    ],
  )
}
