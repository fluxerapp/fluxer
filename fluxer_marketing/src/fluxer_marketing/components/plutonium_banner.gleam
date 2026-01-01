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
import fluxer_marketing/icons
import fluxer_marketing/number_format
import fluxer_marketing/web.{type Context, href}
import gleam/string
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let slots = ctx.visionary_slots
  let remaining_text = number_format.format_number(slots.remaining)
  let total_text = number_format.format_number(slots.total)

  let headline = case slots.total {
    0 ->
      g_(
        i18n_ctx,
        "Lifetime Plutonium + Operator Pass with Visionary — limited slots",
      )
    _ ->
      g_(
        i18n_ctx,
        "Only {0} of {1} Visionary lifetime slots left — lifetime Plutonium + Operator Pass",
      )
      |> string.replace("{0}", remaining_text)
      |> string.replace("{1}", total_text)
  }

  html.div(
    [
      attribute.class(
        "fixed top-0 left-0 right-0 z-30 bg-gradient-to-r from-black to-gray-900 text-white",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl px-4 py-3 md:py-2.5")], [
        html.div(
          [
            attribute.class(
              "flex flex-col sm:flex-row items-center justify-center gap-2 sm:gap-3 text-xs sm:text-sm font-medium md:text-base text-center sm:text-left",
            ),
          ],
          [
            html.div([attribute.class("flex items-center gap-2 sm:gap-3")], [
              icons.infinity([
                attribute.class("hidden sm:block h-5 w-5 flex-shrink-0"),
              ]),
              html.span([attribute.class("leading-tight")], [
                html.text(headline),
              ]),
            ]),
            html.a(
              [
                href(ctx, "/plutonium#visionary"),
                attribute.class(
                  "rounded-lg bg-white px-3 py-1.5 sm:px-4 text-xs sm:text-sm font-semibold text-black hover:bg-gray-100 transition-colors whitespace-nowrap",
                ),
              ],
              [html.text(g_(i18n_ctx, "Get Visionary"))],
            ),
          ],
        ),
      ]),
    ],
  )
}
