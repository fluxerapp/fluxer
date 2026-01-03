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

  html.div(
    [
      attribute.class(
        "mt-8 inline-flex items-center gap-3 rounded-full bg-white/10 backdrop-blur-sm border border-white/20 px-4 py-2.5 md:px-5",
      ),
    ],
    [
      html.p([attribute.class("text-sm md:text-base text-white/90")], [
        html.text(g_(i18n_ctx, "Try it without an email at")),
        html.text(" "),
        html.a(
          [
            attribute.href("https://fluxer.gg/fluxer-hq"),
            attribute.class(
              "font-semibold text-white underline underline-offset-2 hover:no-underline",
            ),
          ],
          [html.text("fluxer.gg/fluxer-hq")],
        ),
      ]),
    ],
  )
}
