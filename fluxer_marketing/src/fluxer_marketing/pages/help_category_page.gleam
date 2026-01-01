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
import fluxer_marketing/pages/not_found_page
import fluxer_marketing/web.{type Context, href}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(
  req: wisp.Request,
  ctx: Context,
  category_name: String,
  search_query: option.Option(String),
) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let help_data = help_center.load_help_articles(ctx.locale)

  case help_center.get_category(help_data, category_name) {
    Ok(category) -> {
      let articles = case search_query {
        Some(query) ->
          help_center.search_articles(help_data, query)
          |> help_center.filter_by_category(category_name)
        None -> category.articles
      }

      let content = [
        hero_section(ctx, category, search_query),
        articles_section(ctx, category, articles, search_query),
      ]

      layout.render(
        req,
        ctx,
        PageMeta(
          title: category.title <> " | " <> g_(i18n_ctx, "Help Center"),
          description: g_(i18n_ctx, "Browse help articles about ")
            <> string.lowercase(category.title)
            <> ".",
          og_type: "website",
        ),
        content,
      )
      |> element.to_document_string_tree
      |> wisp.html_response(200)
    }
    Error(_) -> not_found_page.render(req, ctx)
  }
}

fn hero_section(
  ctx: Context,
  category: help_center.HelpCategory,
  search_query: option.Option(String),
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let locale_code = locale.get_code_from_locale(ctx.locale) |> string.lowercase

  help_components.hero_section(
    ctx,
    category.title,
    g_(i18n_ctx, "Search our help articles or browse by category."),
    web.prepend_base_path(ctx, "/help/" <> locale_code <> "/" <> category.name),
    g_(i18n_ctx, "Search in " <> category.title <> "..."),
    case search_query {
      Some(q) -> q
      None -> ""
    },
  )
}

fn articles_section(
  ctx: Context,
  category: help_center.HelpCategory,
  articles: List(help_center.HelpArticle),
  search_query: option.Option(String),
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let locale_code = locale.get_code_from_locale(ctx.locale) |> string.lowercase

  html.section(
    [
      attribute.class("bg-white px-6 py-16 md:py-24"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-4xl")], [
        case search_query {
          Some(query) ->
            html.div(
              [
                attribute.class(
                  "mb-6 flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between",
                ),
              ],
              [
                html.div([attribute.class("body-sm text-gray-600")], [
                  html.text(
                    g_(i18n_ctx, "Search results for") <> " \"" <> query <> "\"",
                  ),
                ]),
                html.a(
                  [
                    href(ctx, "/help/" <> locale_code <> "/" <> category.name),
                    attribute.class(
                      "label text-sm text-[#4641D9] hover:underline",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Clear search"))],
                ),
              ],
            )
          None ->
            html.div([attribute.class("mb-10 text-left")], [
              html.h2([attribute.class("headline text-black mb-2")], [
                html.text(g_(i18n_ctx, "Articles in ") <> category.title),
              ]),
              html.p([attribute.class("body text-gray-600")], [
                html.text(g_(
                  i18n_ctx,
                  "Choose a guide below or search again if you don't see what you need.",
                )),
              ]),
            ])
        },
        case list.is_empty(articles) {
          True ->
            html.div(
              [
                attribute.class(
                  "rounded-2xl bg-gray-50 p-10 text-center border border-dashed border-gray-200",
                ),
              ],
              [
                html.p([attribute.class("body text-gray-600")], [
                  html.text(g_(i18n_ctx, "No articles found.")),
                ]),
                html.a(
                  [
                    href(ctx, "/help/" <> locale_code <> "/" <> category.name),
                    attribute.class(
                      "mt-4 inline-block label text-sm text-[#4641D9] hover:underline",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "View all articles"))],
                ),
              ],
            )
          False ->
            html.div([attribute.class("space-y-3 md:space-y-4")], {
              articles
              |> list.map(fn(article) { article_card(ctx, category, article) })
            })
        },
      ]),
    ],
  )
}

fn article_card(
  ctx: Context,
  _category: help_center.HelpCategory,
  article: help_center.HelpArticle,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let locale_code = locale.get_code_from_locale(ctx.locale)
  let url =
    web.prepend_base_path(
      ctx,
      "/help/"
        <> string.lowercase(locale_code)
        <> "/articles/"
        <> article.snowflake_id
        <> "-"
        <> help_center.create_slug(article.title),
    )

  html.a(
    [
      attribute.href(url),
      attribute.class(
        "group block rounded-2xl border border-gray-100 bg-white/90 p-6 md:p-7 shadow-sm transition hover:border-[#4641D9] hover:shadow-md",
      ),
    ],
    [
      html.h3(
        [attribute.class("title-sm mb-2 text-black group-hover:text-[#4641D9]")],
        [
          html.text(article.title),
        ],
      ),
      html.p([attribute.class("body text-gray-600")], [
        html.text(article.description),
      ]),
      html.div(
        [
          attribute.class("mt-3 flex items-center gap-2 text-[#4641D9] body-sm"),
        ],
        [
          html.span([], [html.text(g_(i18n_ctx, "Read article"))]),
          icons.arrow_right([
            attribute.class(
              "h-4 w-4 transition-transform group-hover:translate-x-0.5",
            ),
          ]),
        ],
      ),
    ],
  )
}
