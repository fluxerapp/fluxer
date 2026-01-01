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

import fluxer_marketing/components/feature_pill
import fluxer_marketing/i18n
import fluxer_marketing/web.{type Context}
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(
  ctx: Context,
  title: String,
  description: String,
  features: List(#(String, feature_pill.Status)),
  theme: feature_pill.Theme,
) -> Element(a) {
  let _i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let #(bg_class, text_class, desc_class) = case theme {
    feature_pill.Light -> #("bg-white", "text-black", "text-gray-600")
    feature_pill.Dark -> #("bg-[#4641D9]", "text-white", "text-white/80")
  }

  html.section([attribute.class(bg_class <> " px-6 py-20 md:py-32")], [
    html.div([attribute.class("mx-auto max-w-7xl")], [
      html.div([attribute.class("mb-16 text-center")], [
        html.h2(
          [
            attribute.class(
              "mb-6 text-3xl font-bold " <> text_class <> " md:text-4xl",
            ),
          ],
          [html.text(title)],
        ),
        html.p(
          [
            attribute.class(
              "mx-auto max-w-3xl text-lg " <> desc_class <> " md:text-xl",
            ),
          ],
          [html.text(description)],
        ),
      ]),
      html.div(
        [attribute.class("flex flex-wrap justify-center gap-3")],
        features
          |> list.map(fn(feature) {
            let #(text, status) = feature
            feature_pill.render_with_theme(text, status, theme)
          }),
      ),
    ]),
  ])
}
