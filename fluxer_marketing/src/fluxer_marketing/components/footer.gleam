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
import fluxer_marketing/web.{type Context, href}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let help_data = help_center.load_help_articles(ctx.locale)
  let bug_article_href =
    help_center.article_href(ctx.locale, help_data, "1447264362996695040")

  html.footer(
    [
      attribute.class(
        "bg-gradient-to-b from-[#4641D9] to-[#3832B8] px-4 py-20 md:py-24 text-white md:px-8",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div(
          [
            attribute.class(
              "mb-12 md:mb-16 rounded-2xl bg-white/10 backdrop-blur-sm border border-white/20 px-6 py-6 md:px-10 md:py-10",
            ),
          ],
          [
            html.div(
              [
                attribute.class(
                  "flex flex-col md:flex-row items-center md:items-center justify-between gap-6 md:gap-8 text-center md:text-left",
                ),
              ],
              [
                html.div(
                  [
                    attribute.class(
                      "flex flex-col md:flex-row items-center md:items-center gap-3 md:gap-4",
                    ),
                  ],
                  [
                    icons.heart([attribute.class("h-7 w-7 text-white")]),
                    html.p([attribute.class("body-lg text-white/90")], [
                      html.text(g_(
                        i18n_ctx,
                        "Help support an independent communication platform. Your donation funds the platform's infrastructure and development.",
                      )),
                    ]),
                  ],
                ),
                html.a(
                  [
                    href(ctx, "/donate"),
                    attribute.class(
                      "inline-flex items-center justify-center rounded-xl bg-white px-8 py-4 text-base md:text-lg font-semibold text-[#4641D9] transition-colors hover:bg-white/90 shadow-lg whitespace-nowrap",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Donate to Fluxer"))],
                ),
              ],
            ),
          ],
        ),
        html.div(
          [attribute.class("grid grid-cols-1 gap-12 md:gap-16 md:grid-cols-4")],
          [
            html.div([attribute.class("md:col-span-1")], [
              icons.fluxer_logo_wordmark([attribute.class("h-10 md:h-12")]),
            ]),
            html.div(
              [
                attribute.class(
                  "grid grid-cols-1 gap-8 md:gap-12 sm:grid-cols-3 md:col-span-3",
                ),
              ],
              [
                html.div([], [
                  html.h3([attribute.class("title mb-4 md:mb-6 text-white")], [
                    html.text(g_(i18n_ctx, "Fluxer")),
                  ]),
                  html.ul([attribute.class("space-y-3")], [
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/plutonium"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Pricing"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/partners"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Partners"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/download"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Download"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          attribute.href("https://github.com/fluxerapp/fluxer"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Source Code"))],
                      ),
                    ]),
                    html.li([], [
                      html.div([attribute.class("flex items-center gap-2")], [
                        html.a(
                          [
                            attribute.href(
                              "https://bsky.app/profile/fluxer.app",
                            ),
                            attribute.class(
                              "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                            ),
                          ],
                          [html.text(g_(i18n_ctx, "Bluesky"))],
                        ),
                        html.a(
                          [
                            attribute.href(
                              "https://bsky.app/profile/fluxer.app/rss",
                            ),
                            attribute.title("RSS Feed"),
                            attribute.target("_blank"),
                            attribute.rel("noopener noreferrer"),
                            attribute.class(
                              "text-white/90 hover:text-white transition-colors",
                            ),
                          ],
                          [
                            icons.rss([
                              attribute.class("h-[1em] w-[1em]"),
                            ]),
                          ],
                        ),
                      ]),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/help"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Help Center"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/press"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Press"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          attribute.href("https://docs.fluxer.app"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Developer Docs"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/careers"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Careers"))],
                      ),
                    ]),
                  ]),
                ]),
                html.div([], [
                  html.h3([attribute.class("title mb-4 md:mb-6 text-white")], [
                    html.text(g_(i18n_ctx, "Policies")),
                  ]),
                  html.ul([attribute.class("space-y-3")], [
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/terms"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Terms of Service"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/privacy"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Privacy Policy"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/guidelines"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Community Guidelines"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/security"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Security Bug Bounty"))],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, "/company-information"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Company Information"))],
                      ),
                    ]),
                  ]),
                ]),
                html.div([], [
                  html.h3([attribute.class("title mb-4 md:mb-6 text-white")], [
                    html.text(g_(i18n_ctx, "Connect")),
                  ]),
                  html.ul([attribute.class("space-y-3")], [
                    html.li([], [
                      html.a(
                        [
                          attribute.href("mailto:press@fluxer.app"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text("press@fluxer.app")],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          attribute.href("mailto:support@fluxer.app"),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text("support@fluxer.app")],
                      ),
                    ]),
                    html.li([], [
                      html.a(
                        [
                          href(ctx, bug_article_href),
                          attribute.class(
                            "body-lg text-white/90 hover:text-white hover:underline transition-colors",
                          ),
                        ],
                        [html.text(g_(i18n_ctx, "Report a bug"))],
                      ),
                    ]),
                  ]),
                ]),
              ],
            ),
          ],
        ),
        html.div([attribute.class("mt-12 border-t border-white/20 pt-8")], [
          html.div([attribute.class("flex flex-col gap-2")], [
            html.p([attribute.class("body-sm text-white/80")], [
              html.text(g_(
                i18n_ctx,
                "© Fluxer Platform AB (Swedish limited liability company: 559537-3993)",
              )),
            ]),
            html.p([attribute.class("body-sm text-white/80")], [
              html.text(g_(
                i18n_ctx,
                "This product includes GeoLite2 Data created by MaxMind, available from ",
              )),
              html.a(
                [
                  attribute.href("https://www.maxmind.com"),
                  attribute.target("_blank"),
                  attribute.rel("noopener noreferrer"),
                  attribute.class("hover:underline"),
                ],
                [html.text("MaxMind")],
              ),
              html.text("."),
            ]),
          ]),
        ]),
      ]),
    ],
  )
}
