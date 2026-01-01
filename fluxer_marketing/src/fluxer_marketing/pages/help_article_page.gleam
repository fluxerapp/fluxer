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
import fluxer_marketing/markdown_utils
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/pages/not_found_page
import fluxer_marketing/web.{type Context, href}
import gleam/list
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
  article_slug: String,
) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let help_data = help_center.load_help_articles(ctx.locale)

  case help_center.get_article(help_data, category_name, article_slug) {
    Ok(article) -> {
      let content = [
        hero_section(ctx, article),
        article_section(ctx, article, help_data),
      ]

      layout.render(
        req,
        ctx,
        PageMeta(
          title: article.title <> " | " <> g_(i18n_ctx, "Help Center"),
          description: article.description,
          og_type: "article",
        ),
        content,
      )
      |> element.to_document_string_tree
      |> wisp.html_response(200)
    }
    Error(_) -> not_found_page.render(req, ctx)
  }
}

pub fn render_with_locale(
  req: wisp.Request,
  ctx: Context,
  article: help_center.HelpArticle,
  help_data: help_center.HelpCenterData,
) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    hero_section(ctx, article),
    article_section_with_sidebar(ctx, article, help_data),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: article.title <> " | " <> g_(i18n_ctx, "Help Center"),
      description: article.description,
      og_type: "article",
    ),
    content,
  )
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}

fn hero_section(ctx: Context, article: help_center.HelpArticle) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class("px-6 pt-48 md:pt-60 pb-16 md:pb-20 lg:pb-24 text-white"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-4xl")], [
        html.div(
          [
            attribute.class(
              "mb-4 flex flex-wrap items-center gap-2 text-xs font-semibold uppercase tracking-wide text-white/70",
            ),
          ],
          [
            html.a(
              [
                href(ctx, "/help"),
                attribute.class("hover:text-white hover:underline"),
              ],
              [html.text(g_(i18n_ctx, "Help Center"))],
            ),
            html.span([], [html.text("•")]),
            html.a(
              [
                href(
                  ctx,
                  "/help/"
                    <> string.lowercase(locale.get_code_from_locale(ctx.locale))
                    <> "/"
                    <> article.category,
                ),
                attribute.class("hover:text-white hover:underline"),
              ],
              [html.text(article.category_title)],
            ),
          ],
        ),
        html.h1([attribute.class("hero mb-4 md:mb-5")], [
          html.text(article.title),
        ]),
        html.p(
          [
            attribute.class("body-lg text-white/90 max-w-3xl"),
          ],
          [
            html.text(article.description),
          ],
        ),
      ]),
    ],
  )
}

fn article_section(
  ctx: Context,
  article: help_center.HelpArticle,
  help_data: help_center.HelpCenterData,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-white px-6 py-16 md:py-24")], [
    html.div([attribute.class("mx-auto max-w-4xl")], [
      html.div(
        [
          attribute.class(
            "mb-6 flex items-center justify-between gap-3 text-sm text-gray-500",
          ),
        ],
        [
          html.a(
            [
              href(
                ctx,
                "/help/"
                  <> string.lowercase(locale.get_code_from_locale(ctx.locale))
                  <> "/"
                  <> article.category,
              ),
              attribute.class(
                "label text-sm text-[#4641D9] hover:underline flex items-center gap-2",
              ),
            ],
            [
              icons.arrow_right([attribute.class("h-4 w-4 rotate-180")]),
              html.text(g_(i18n_ctx, "Back to ") <> article.category_title),
            ],
          ),
        ],
      ),
      html.article(
        [
          attribute.class(
            "prose prose-lg max-w-none rounded-2xl bg-white/95 p-8 md:p-10 shadow-sm border border-gray-100",
          ),
        ],
        [
          html.div(
            [
              attribute.class(
                "markdown-content text-gray-800 [&>h1]:mb-4 [&>h1]:mt-8 [&>h1]:text-3xl [&>h1]:font-bold [&>h2]:mb-3 [&>h2]:mt-6 [&>h2]:text-2xl [&>h2]:font-bold [&>h3]:mb-2 [&>h3]:mt-4 [&>h3]:text-xl [&>h3]:font-semibold [&>p]:mb-4 [&>p]:leading-relaxed [&>ul]:mb-4 [&>ul]:ml-6 [&>ul]:list-disc [&>ol]:mb-4 [&>ol]:ml-6 [&>ol]:list-decimal [&>li]:mb-2 [&>a]:text-[#4641D9] [&>a]:hover:underline [&>code]:rounded [&>code]:bg-gray-100 [&>code]:px-1 [&>code]:py-0.5 [&>code]:text-sm [&>strong]:font-semibold",
              ),
            ],
            [
              markdown_utils.render_markdown_to_element(
                article.content,
                ctx,
                help_data,
              ),
            ],
          ),
        ],
      ),
      html.div(
        [
          attribute.class(
            "mt-10 rounded-2xl border border-gray-100 bg-gray-50/80 p-8",
          ),
        ],
        [
          html.h3([attribute.class("title-sm mb-3 text-black")], [
            html.text(g_(i18n_ctx, "Still need help?")),
          ]),
          html.p([attribute.class("body mb-4 text-gray-600")], [
            html.text(g_(
              i18n_ctx,
              "If you couldn't find what you're looking for, our support team is here to help.",
            )),
          ]),
          html.a(
            [
              attribute.href("mailto:support@fluxer.app"),
              attribute.class(
                "label inline-block rounded-xl bg-[#4641D9] px-6 py-3 text-white transition hover:bg-[#3d38c7]",
              ),
            ],
            [html.text(g_(i18n_ctx, "Contact Support"))],
          ),
        ],
      ),
    ]),
  ])
}

fn article_section_with_sidebar(
  ctx: Context,
  article: help_center.HelpArticle,
  help_data: help_center.HelpCenterData,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let locale_code = locale.get_code_from_locale(ctx.locale)

  let category_articles =
    help_data.all_articles
    |> list.filter(fn(a) { a.category == article.category })

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-white to-gray-50 px-6 py-16 md:py-24",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("flex flex-col lg:flex-row gap-8")], [
          html.aside(
            [
              attribute.class(
                "w-full lg:w-64 flex-shrink-0 lg:sticky lg:top-32 lg:self-start",
              ),
            ],
            [
              html.div([attribute.class("mb-6")], [
                html.a(
                  [
                    href(ctx, "/help"),
                    attribute.class(
                      "label text-[#4641D9] hover:underline flex items-center gap-2",
                    ),
                  ],
                  [
                    icons.arrow_right([attribute.class("h-4 w-4 rotate-180")]),
                    html.text(g_(i18n_ctx, "Back to Help Center")),
                  ],
                ),
              ]),
              html.div(
                [
                  attribute.class(
                    "rounded-lg border-2 border-gray-100 bg-gray-50 p-6",
                  ),
                ],
                [
                  html.h3([attribute.class("subtitle mb-4 text-black")], [
                    html.text(g_(i18n_ctx, "Articles in this section")),
                  ]),
                  html.ul([attribute.class("space-y-2")], {
                    category_articles
                    |> list.map(fn(a) {
                      let is_current = a.snowflake_id == article.snowflake_id
                      let url =
                        web.prepend_base_path(
                          ctx,
                          "/help/"
                            <> string.lowercase(locale_code)
                            <> "/articles/"
                            <> a.snowflake_id
                            <> "-"
                            <> help_center.create_slug(a.title),
                        )

                      html.li([], [
                        html.a(
                          [
                            attribute.href(url),
                            attribute.class(case is_current {
                              True ->
                                "block text-[#4641D9] font-semibold hover:underline"
                              False ->
                                "block text-gray-700 hover:text-[#4641D9] hover:underline"
                            }),
                          ],
                          [html.text(a.title)],
                        ),
                      ])
                    })
                  }),
                ],
              ),
            ],
          ),
          html.div([attribute.class("flex-1 min-w-0")], [
            html.article(
              [
                attribute.class(
                  "prose prose-lg max-w-none prose-p:first-of-type:!mt-0",
                ),
              ],
              [
                html.div(
                  [
                    attribute.class(
                      "markdown-content text-gray-800 [&>*:first-child]:!mt-0 [&_p:first-child]:!mt-0 [&>h1]:mb-4 [&>h1]:mt-8 [&>h1]:text-3xl [&>h1]:font-bold [&>h2]:mb-3 [&>h2]:mt-6 [&>h2]:text-2xl [&>h2]:font-bold [&>h3]:mb-2 [&>h3]:mt-4 [&>h3]:text-xl [&>h3]:font-semibold [&>p]:mb-4 [&>p]:leading-relaxed [&>ul]:mb-4 [&>ul]:ml-6 [&>ul]:list-disc [&>ol]:mb-4 [&>ol]:ml-6 [&>ol]:list-decimal [&>li]:mb-2 [&>a]:text-[#4641D9] [&>a]:hover:underline [&>code]:rounded [&>code]:bg-gray-100 [&>code]:px-1 [&>code]:py-0.5 [&>code]:text-sm [&>strong]:font-semibold",
                    ),
                  ],
                  [
                    markdown_utils.render_markdown_to_element(
                      article.content,
                      ctx,
                      help_data,
                    ),
                  ],
                ),
              ],
            ),
            html.div(
              [
                attribute.class(
                  "mt-12 rounded-lg border-2 border-gray-100 bg-gray-50 p-8",
                ),
              ],
              [
                html.h3([attribute.class("title-sm mb-4 text-black")], [
                  html.text(g_(i18n_ctx, "Still need help?")),
                ]),
                html.p([attribute.class("body mb-4 text-gray-600")], [
                  html.text(g_(
                    i18n_ctx,
                    "If you couldn't find what you're looking for, our support team is here to help.",
                  )),
                ]),
                html.a(
                  [
                    attribute.href("mailto:support@fluxer.app"),
                    attribute.class(
                      "label inline-block rounded-lg bg-[#4641D9] px-6 py-3 text-white transition hover:bg-[#3d38c7]",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Contact Support"))],
                ),
              ],
            ),
          ]),
        ]),
      ]),
    ],
  )
}
