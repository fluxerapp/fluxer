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

import fluxer_marketing/components/hackernews_banner
import fluxer_marketing/components/platform_download_button
import fluxer_marketing/i18n
import fluxer_marketing/locale
import fluxer_marketing/web.{type Context, prepend_base_path}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.main(
    [
      attribute.class(
        "flex flex-col items-center justify-center px-6 pb-16 pt-52 md:pb-20 md:pt-64 lg:pb-24",
      ),
    ],
    [
      html.div([attribute.class("max-w-4xl space-y-6 text-center")], [
        case ctx.locale {
          locale.Ja ->
            html.div([attribute.class("mb-2 flex justify-center")], [
              html.span([attribute.class("text-3xl font-bold text-white")], [
                html.text("Fluxer（フラクサー）"),
              ]),
            ])
          _ -> element.none()
        },
        html.div([attribute.class("mb-4 flex justify-center")], [
          html.span(
            [
              attribute.class(
                "rounded-full bg-white/20 px-4 py-2 backdrop-blur-sm",
              ),
            ],
            [html.text(g_(i18n_ctx, "Public Beta"))],
          ),
        ]),
        html.h1([attribute.class("hero")], [
          html.text(g_(i18n_ctx, "A chat app that puts you first")),
        ]),
        html.p([attribute.class("lead text-white/90")], [
          html.text(g_(
            i18n_ctx,
            "Fluxer is an open-source, independent instant messaging and VoIP platform. Built for friends, groups, and communities.",
          )),
        ]),
        html.div(
          [
            attribute.class(
              "flex flex-col items-center justify-center gap-4 pt-4 sm:flex-row sm:items-stretch",
            ),
          ],
          [
            platform_download_button.render_with_overlay(ctx),
            html.a(
              [
                attribute.href(ctx.app_endpoint <> "/channels/@me"),
                attribute.class(
                  "hidden sm:inline-flex items-center justify-center gap-3 rounded-2xl bg-white/10 backdrop-blur-sm border-2 border-white/30 px-8 md:px-10 text-lg md:text-xl font-semibold text-white transition-colors hover:bg-white/20 shadow-lg",
                ),
              ],
              [html.text(g_(i18n_ctx, "Open in Browser"))],
            ),
          ],
        ),
        hackernews_banner.render(ctx),
        html.div([attribute.class("mt-6 flex justify-center")], [
          html.a(
            [
              attribute.href(
                "https://www.producthunt.com/products/fluxer?embed=true&utm_source=badge-featured&utm_medium=badge&utm_campaign=badge-fluxer",
              ),
              attribute.target("_blank"),
              attribute.attribute("rel", "noopener noreferrer"),
            ],
            [
              html.img([
                attribute.alt(
                  "Fluxer - Open-source Discord-like instant messaging & VoIP platform | Product Hunt",
                ),
                attribute.attribute("width", "250"),
                attribute.attribute("height", "54"),
                attribute.src(prepend_base_path(ctx, "/api/badges/product-hunt")),
              ]),
            ],
          ),
        ]),
      ]),
      html.div(
        [
          attribute.class(
            "mt-16 flex w-full max-w-7xl items-end justify-center gap-4 px-6 md:mt-24 md:gap-8",
          ),
        ],
        [
          html.div(
            [attribute.class("hidden w-full md:block md:w-3/4 lg:w-2/3")],
            [
              element.element("picture", [], [
                element.element(
                  "source",
                  [
                    attribute.type_("image/avif"),
                    attribute.attribute(
                      "srcset",
                      ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-480w.avif?v=4 480w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-768w.avif?v=4 768w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-1024w.avif?v=4 1024w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-1920w.avif?v=4 1920w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-2560w.avif?v=4 2560w",
                    ),
                    attribute.attribute(
                      "sizes",
                      "(max-width: 768px) 100vw, 75vw",
                    ),
                  ],
                  [],
                ),
                element.element(
                  "source",
                  [
                    attribute.type_("image/webp"),
                    attribute.attribute(
                      "srcset",
                      ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-480w.webp?v=4 480w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-768w.webp?v=4 768w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-1024w.webp?v=4 1024w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-1920w.webp?v=4 1920w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/desktop-2560w.webp?v=4 2560w",
                    ),
                    attribute.attribute(
                      "sizes",
                      "(max-width: 768px) 100vw, 75vw",
                    ),
                  ],
                  [],
                ),
                html.img([
                  attribute.src(
                    ctx.cdn_endpoint
                    <> "/marketing/screenshots/desktop-1920w.png?v=4",
                  ),
                  attribute.attribute(
                    "srcset",
                    ctx.cdn_endpoint
                      <> "/marketing/screenshots/desktop-480w.png?v=4 480w, "
                      <> ctx.cdn_endpoint
                      <> "/marketing/screenshots/desktop-768w.png?v=4 768w, "
                      <> ctx.cdn_endpoint
                      <> "/marketing/screenshots/desktop-1024w.png?v=4 1024w, "
                      <> ctx.cdn_endpoint
                      <> "/marketing/screenshots/desktop-1920w.png?v=4 1920w, "
                      <> ctx.cdn_endpoint
                      <> "/marketing/screenshots/desktop-2560w.png?v=4 2560w",
                  ),
                  attribute.attribute("sizes", "(max-width: 768px) 100vw, 75vw"),
                  attribute.alt("Fluxer desktop interface"),
                  attribute.class(
                    "aspect-video w-full rounded-lg border-2 border-white/50",
                  ),
                ]),
              ]),
            ],
          ),
          html.div(
            [
              attribute.class(
                "w-full max-w-[320px] md:w-1/4 md:max-w-none lg:w-1/5",
              ),
            ],
            [
              element.element("picture", [], [
                element.element(
                  "source",
                  [
                    attribute.type_("image/avif"),
                    attribute.attribute(
                      "srcset",
                      ctx.cdn_endpoint
                        <> "/marketing/screenshots/mobile-480w.avif?v=4 480w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/mobile-768w.avif?v=4 768w",
                    ),
                    attribute.attribute(
                      "sizes",
                      "(max-width: 768px) 320px, 25vw",
                    ),
                  ],
                  [],
                ),
                element.element(
                  "source",
                  [
                    attribute.type_("image/webp"),
                    attribute.attribute(
                      "srcset",
                      ctx.cdn_endpoint
                        <> "/marketing/screenshots/mobile-480w.webp?v=4 480w, "
                        <> ctx.cdn_endpoint
                        <> "/marketing/screenshots/mobile-768w.webp?v=4 768w",
                    ),
                    attribute.attribute(
                      "sizes",
                      "(max-width: 768px) 320px, 25vw",
                    ),
                  ],
                  [],
                ),
                html.img([
                  attribute.src(
                    ctx.cdn_endpoint
                    <> "/marketing/screenshots/mobile-768w.png?v=4",
                  ),
                  attribute.attribute(
                    "srcset",
                    ctx.cdn_endpoint
                      <> "/marketing/screenshots/mobile-480w.png?v=4 480w, "
                      <> ctx.cdn_endpoint
                      <> "/marketing/screenshots/mobile-768w.png?v=4 768w",
                  ),
                  attribute.attribute("sizes", "(max-width: 768px) 320px, 25vw"),
                  attribute.alt("Fluxer mobile interface"),
                  attribute.class(
                    "aspect-[9/19] w-full rounded-3xl border-2 border-white/50",
                  ),
                ]),
              ]),
            ],
          ),
        ],
      ),
    ],
  )
}
