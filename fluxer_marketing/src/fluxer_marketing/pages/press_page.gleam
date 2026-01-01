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

import fluxer_marketing/components/hero_base
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/web.{type Context}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    hero_section(ctx),
    logo_section(ctx),
    symbol_section(ctx),
    colors_section(ctx),
    contact_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Press & Brand Assets"),
      description: g_(
        i18n_ctx,
        "Download Fluxer logos, brand assets, and get in touch with our press team",
      ),
      og_type: "website",
    ),
    content,
  )
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}

fn hero_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  hero_base.render(hero_base.HeroConfig(
    icon: icons.newspaper([
      attribute.class("h-14 w-14 md:h-18 md:w-18 text-white"),
    ]),
    title: g_(i18n_ctx, "Press & Brand Assets"),
    description: g_(
      i18n_ctx,
      "Download our logos, learn about our brand colors, and get in touch with our press team.",
    ),
    extra_content: element.none(),
    custom_padding: hero_base.default_padding(),
  ))
}

fn logo_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-white to-gray-50 px-6 py-24 md:py-32",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-6xl")], [
        html.div([attribute.class("mb-16 md:mb-20 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [html.text(g_(i18n_ctx, "Logo"))],
          ),
          html.p(
            [
              attribute.class("body-lg text-gray-600 max-w-3xl mx-auto"),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "Our full logo including the wordmark. Use this as the primary representation of our brand.",
              )),
            ],
          ),
        ]),
        html.div([attribute.class("grid gap-6 md:gap-8 md:grid-cols-3")], [
          asset_card(
            ctx,
            g_(i18n_ctx, "White Logo"),
            g_(i18n_ctx, "For dark backgrounds"),
            ctx.cdn_endpoint <> "/marketing/branding/logo-white.svg",
            "bg-[#1a1a1a]",
          ),
          asset_card(
            ctx,
            g_(i18n_ctx, "Black Logo"),
            g_(i18n_ctx, "For light backgrounds"),
            ctx.cdn_endpoint <> "/marketing/branding/logo-black.svg",
            "bg-gray-50",
          ),
          asset_card(
            ctx,
            g_(i18n_ctx, "Color Logo"),
            g_(i18n_ctx, "Full color version"),
            ctx.cdn_endpoint <> "/marketing/branding/logo-color.svg",
            "bg-gray-50",
          ),
        ]),
      ]),
    ],
  )
}

fn symbol_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-white px-6 py-24 md:py-32")], [
    html.div([attribute.class("mx-auto max-w-6xl")], [
      html.div([attribute.class("mb-16 md:mb-20 text-center")], [
        html.h2(
          [
            attribute.class(
              "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
            ),
          ],
          [html.text(g_(i18n_ctx, "Symbol"))],
        ),
        html.p(
          [
            attribute.class("body-lg text-gray-600 max-w-3xl mx-auto"),
          ],
          [
            html.text(g_(
              i18n_ctx,
              "Our standalone symbol. Use this only when our brand is clearly visible or well-established elsewhere in the context.",
            )),
          ],
        ),
      ]),
      html.div([attribute.class("grid gap-6 md:gap-8 md:grid-cols-3")], [
        asset_card(
          ctx,
          g_(i18n_ctx, "White Symbol"),
          g_(i18n_ctx, "For dark backgrounds"),
          ctx.cdn_endpoint <> "/marketing/branding/symbol-white.svg",
          "bg-[#1a1a1a]",
        ),
        asset_card(
          ctx,
          g_(i18n_ctx, "Black Symbol"),
          g_(i18n_ctx, "For light backgrounds"),
          ctx.cdn_endpoint <> "/marketing/branding/symbol-black.svg",
          "bg-gray-50",
        ),
        asset_card(
          ctx,
          g_(i18n_ctx, "Color Symbol"),
          g_(i18n_ctx, "Full color version"),
          ctx.cdn_endpoint <> "/marketing/branding/symbol-color.svg",
          "bg-gray-50",
        ),
      ]),
    ]),
  ])
}

fn colors_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-gray-50 px-6 py-24 md:py-32")], [
    html.div([attribute.class("mx-auto max-w-6xl")], [
      html.div([attribute.class("mb-16 md:mb-20 text-center")], [
        html.h2(
          [
            attribute.class(
              "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
            ),
          ],
          [html.text(g_(i18n_ctx, "Brand Colors"))],
        ),
        html.p(
          [
            attribute.class("body-lg text-gray-600 max-w-3xl mx-auto"),
          ],
          [
            html.text(g_(
              i18n_ctx,
              "Our carefully selected color palette that represents the Fluxer brand.",
            )),
          ],
        ),
      ]),
      html.div([attribute.class("grid gap-6 md:gap-8 md:grid-cols-3")], [
        color_card(
          ctx,
          g_(i18n_ctx, "Blue (Da Ba Dee)"),
          "#4641D9",
          g_(
            i18n_ctx,
            "Our primary brand color. Use this for key brand elements and accents.",
          ),
        ),
        color_card(
          ctx,
          g_(i18n_ctx, "White"),
          "#FFFFFF",
          g_(
            i18n_ctx,
            "For backgrounds, text on dark surfaces, and creating contrast.",
          ),
        ),
        color_card(
          ctx,
          g_(i18n_ctx, "Black"),
          "#000000",
          g_(i18n_ctx, "For text, icons on light surfaces, and creating depth."),
        ),
      ]),
    ]),
  ])
}

fn contact_section(ctx: Context) -> Element(a) {
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
                "mx-auto max-w-3xl px-6 py-20 md:py-28 text-center",
              ),
            ],
            [
              html.h2(
                [
                  attribute.class(
                    "display mb-6 md:mb-8 text-4xl md:text-5xl lg:text-6xl",
                  ),
                ],
                [
                  html.text(g_(i18n_ctx, "Press Contact")),
                ],
              ),
              html.p(
                [
                  attribute.class(
                    "body-lg mb-8 md:mb-10 text-white/90 max-w-3xl mx-auto",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "Have a story about Fluxer? Need more information or high-resolution assets?",
                  )),
                ],
              ),
              html.a(
                [
                  attribute.href("mailto:press@fluxer.app"),
                  attribute.class(
                    "label inline-block rounded-xl bg-white px-8 py-4 text-[#4641D9] shadow-lg transition hover:bg-gray-100",
                  ),
                ],
                [html.text("press@fluxer.app")],
              ),
              html.p([attribute.class("body-sm mt-6 text-white/80")], [
                html.text(g_(
                  i18n_ctx,
                  "We typically respond within 24 hours during business days.",
                )),
              ]),
            ],
          ),
        ],
      ),
    ],
  )
}

fn asset_card(
  ctx: Context,
  title: String,
  description: String,
  asset_path: String,
  bg_class: String,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div(
    [
      attribute.class(
        "overflow-hidden rounded-2xl border border-gray-200/80 bg-white shadow-lg",
      ),
    ],
    [
      html.div(
        [
          attribute.class(
            "flex aspect-video items-center justify-center p-12 " <> bg_class,
          ),
        ],
        [
          html.img([
            attribute.src(asset_path),
            attribute.alt(title),
            attribute.class("max-h-32 w-auto"),
          ]),
        ],
      ),
      html.div([attribute.class("h-px bg-gray-200")], []),
      html.div(
        [attribute.class("flex items-start justify-between bg-white p-6")],
        [
          html.div([attribute.class("flex-1")], [
            html.h3([attribute.class("subtitle mb-2 text-black")], [
              html.text(title),
            ]),
            html.p([attribute.class("body-sm text-gray-600")], [
              html.text(description),
            ]),
          ]),
          html.a(
            [
              attribute.href(asset_path),
              attribute.target("_blank"),
              attribute.download(asset_path),
              attribute.attribute("aria-label", g_(i18n_ctx, "Download")),
              attribute.class(
                "flex items-center justify-center rounded-lg bg-[#4641D9] p-3 text-white hover:bg-[#3d38c7]",
              ),
            ],
            [icons.download([attribute.class("h-5 w-5")])],
          ),
        ],
      ),
    ],
  )
}

fn color_card(
  _ctx: Context,
  name: String,
  hex: String,
  description: String,
) -> Element(a) {
  html.div(
    [
      attribute.class(
        "overflow-hidden rounded-2xl border border-gray-200/80 bg-white shadow-lg",
      ),
    ],
    [
      html.div(
        [attribute.class("h-32"), attribute.style("background-color", hex)],
        [],
      ),
      html.div([attribute.class("h-px bg-gray-200")], []),
      html.div([attribute.class("bg-white p-6")], [
        html.h3([attribute.class("title-sm mb-1 text-black")], [
          html.text(name),
        ]),
        html.p([attribute.class("caption mb-3 font-mono text-gray-500")], [
          html.text(hex),
        ]),
        html.p([attribute.class("body-sm text-gray-600")], [
          html.text(description),
        ]),
      ]),
    ],
  )
}
