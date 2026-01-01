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
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/web.{type Context, href}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    html.div([attribute.class("h-28 shrink-0 md:h-36")], []),
    html.main(
      [
        attribute.class(
          "flex flex-1 flex-col items-center justify-center px-6 pt-36 pb-12 text-center md:px-12 md:pt-44 md:pb-16",
        ),
      ],
      [
        html.div([attribute.class("mx-auto max-w-2xl")], [
          html.div([attribute.class("mb-8")], [
            icons.fluxer_logo_wordmark([
              attribute.class("mx-auto h-16 opacity-80"),
            ]),
          ]),
          html.div([attribute.class("mb-6")], [
            html.h1([attribute.class("hero text-white/90")], [html.text("404")]),
          ]),
          html.div([attribute.class("mb-8")], [
            html.h2(
              [
                attribute.class("display mb-4 text-white"),
              ],
              [html.text(g_(i18n_ctx, "Page not found"))],
            ),
            html.p([attribute.class("body-lg text-white/80")], [
              html.text(g_(
                i18n_ctx,
                "This page doesn't exist. But there's plenty more to explore.",
              )),
            ]),
          ]),
          html.div(
            [
              attribute.class(
                "flex flex-col items-center gap-4 sm:flex-row sm:justify-center",
              ),
            ],
            [
              html.a(
                [
                  href(ctx, "/"),
                  attribute.class(
                    "rounded-lg border border-white bg-white px-6 py-3 text-[#4641D9] transition-opacity hover:opacity-90",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Go home"))],
              ),
              html.a(
                [
                  href(ctx, "/help"),
                  attribute.class(
                    "rounded-lg border border-white/30 px-6 py-3 text-white transition-colors hover:border-white/50 hover:bg-white/10",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Get help"))],
              ),
            ],
          ),
        ]),
      ],
    ),
    html.div([attribute.class("h-28 shrink-0 md:h-36")], []),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: "Fluxer | " <> g_(i18n_ctx, "Page Not Found"),
      description: g_(
        i18n_ctx,
        "This page doesn't exist. But there's plenty more to explore.",
      ),
      og_type: "website",
    ),
    content,
  )
  |> element.to_document_string_tree
  |> wisp.html_response(404)
}
