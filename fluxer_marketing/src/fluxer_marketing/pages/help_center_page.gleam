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

import fluxer_marketing/help_center
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/locale
import fluxer_marketing/pages/help_components
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/web.{type Context, href}
import gleam/int
import gleam/list
import gleam/string
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let help_data = help_center.load_help_articles(ctx.locale)

  let content = [hero_section(ctx), categories_section(ctx, help_data)]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Help Center"),
      description: g_(
        i18n_ctx,
        "Find answers to common questions and learn how to use Fluxer.",
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

  help_components.hero_section(
    ctx,
    g_(i18n_ctx, "How can we help?"),
    g_(i18n_ctx, "Search our help articles or browse by category."),
    web.prepend_base_path(ctx, "/help/search"),
    g_(i18n_ctx, "Search for help..."),
    "",
  )
}

fn categories_section(
  ctx: Context,
  help_data: help_center.HelpCenterData,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-white to-gray-50 px-6 py-20 md:py-32",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div(
          [attribute.class("mb-12 md:mb-16 text-center max-w-3xl mx-auto")],
          [
            html.h2(
              [
                attribute.class(
                  "display mb-4 text-black text-4xl md:text-5xl lg:text-6xl",
                ),
              ],
              [html.text(g_(i18n_ctx, "Browse by category"))],
            ),
            html.p(
              [
                attribute.class("body-lg text-gray-700"),
              ],
              [
                html.text(g_(
                  i18n_ctx,
                  "Find step-by-step guides and answers, organized by topic.",
                )),
              ],
            ),
          ],
        ),
        html.div(
          [
            attribute.class("grid gap-6 md:gap-8 md:grid-cols-2 lg:grid-cols-3"),
          ],
          {
            help_data.categories
            |> list.map(fn(category) { category_card(ctx, category) })
          },
        ),
      ]),
    ],
  )
}

fn category_card(ctx: Context, category: help_center.HelpCategory) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let locale_code = locale.get_code_from_locale(ctx.locale) |> string.lowercase

  let icon = get_icon_for_category(category.icon)

  html.a(
    [
      href(ctx, "/help/" <> locale_code <> "/" <> category.name),
      attribute.class(
        "group flex h-full flex-col justify-between rounded-2xl border border-gray-100 bg-white/90 p-7 md:p-8 shadow-sm transition hover:border-[#4641D9] hover:shadow-md",
      ),
    ],
    [
      html.div([attribute.class("mb-5 flex items-start gap-4")], [
        html.div(
          [
            attribute.class(
              "inline-flex h-12 w-12 items-center justify-center rounded-2xl bg-gradient-to-br from-[#4641D9]/12 to-[#4641D9]/5 text-[#4641D9]",
            ),
          ],
          [icon],
        ),
        html.div([], [
          html.h3([attribute.class("title-sm mb-1 text-black")], [
            html.text(category.title),
          ]),
          html.p([attribute.class("body-sm text-gray-600")], [
            html.text(
              int.to_string(category.article_count)
              <> " "
              <> case category.article_count {
                1 -> g_(i18n_ctx, "article")
                _ -> g_(i18n_ctx, "articles")
              },
            ),
          ]),
        ]),
      ]),
      html.div([attribute.class("mt-auto flex items-center justify-between")], [
        html.span(
          [
            attribute.class("label text-sm text-[#4641D9]"),
          ],
          [html.text(g_(i18n_ctx, "Browse articles"))],
        ),
        icons.arrow_right([
          attribute.class(
            "h-5 w-5 text-[#4641D9] transition-transform group-hover:translate-x-0.5",
          ),
        ]),
      ]),
    ],
  )
}

fn get_icon_for_category(icon_name: String) -> Element(a) {
  let class = "h-6 w-6"

  case icon_name {
    "rocket_launch" -> icons.rocket_launch([attribute.class(class)])
    "shield_check" -> icons.shield_check([attribute.class(class)])
    "users_three" -> icons.users_three([attribute.class(class)])
    "sparkle" -> icons.sparkle([attribute.class(class)])
    "chats" -> icons.chats([attribute.class(class)])
    "gear" -> icons.gear([attribute.class(class)])
    "heart" -> icons.heart([attribute.class(class)])
    "brain" -> icons.brain([attribute.class(class)])
    "paperclip" -> icons.paperclip([attribute.class(class)])
    "question" -> icons.question([attribute.class(class)])
    _ -> icons.sparkle([attribute.class(class)])
  }
}
