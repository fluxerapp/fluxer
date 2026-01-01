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

import fluxer_marketing/components/support_card
import fluxer_marketing/help_center
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/web.{type Context}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let help_data = help_center.load_help_articles(ctx.locale)
  let bug_article_href =
    help_center.article_href(ctx.locale, help_data, "1447264362996695040")

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-[#4641D9] to-[#3832B8] px-6 py-24 md:py-40",
      ),
      attribute.id("get-involved"),
      attribute.style("scroll-margin-top", "8rem"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("mb-20 md:mb-24 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-8 md:mb-10 text-white text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [html.text(g_(i18n_ctx, "Get involved"))],
          ),
          html.p(
            [
              attribute.class(
                "lead mx-auto max-w-3xl text-white/90 text-xl md:text-2xl",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "We're building a complete platform with all the features you'd expect. But we can't do it without your help!",
              )),
            ],
          ),
        ]),
        html.div(
          [
            attribute.class("grid gap-10 md:gap-12 grid-cols-1 md:grid-cols-2"),
          ],
          [
            support_card.render(
              ctx,
              "rocket-launch",
              g_(i18n_ctx, "Join and spread the word"),
              g_(
                i18n_ctx,
                "We're limiting registrations during beta. Once you're in, you can give friends a code to skip the queue.",
              ),
              g_(i18n_ctx, "Register now"),
              ctx.app_endpoint <> "/register",
            ),
            support_card.render(
              ctx,
              "chat-centered-text",
              g_(i18n_ctx, "Join Fluxer HQ"),
              g_(
                i18n_ctx,
                "Get updates, see upcoming features, discuss suggestions, and chat with the team.",
              ),
              g_(i18n_ctx, "Join Fluxer HQ"),
              "https://fluxer.gg/fluxer-hq",
            ),
            html.div(
              [
                attribute.class(
                  "flex h-full flex-col rounded-3xl bg-white/95 backdrop-blur-sm p-10 md:p-12 shadow-lg border border-white/50",
                ),
              ],
              [
                html.div([attribute.class("mb-8 text-center")], [
                  html.div(
                    [
                      attribute.class(
                        "inline-flex items-center justify-center w-24 h-24 md:w-28 md:h-28 rounded-3xl bg-[#4641D9] mb-6",
                      ),
                    ],
                    [
                      icons.bluesky([
                        attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                      ]),
                    ],
                  ),
                  html.h3(
                    [
                      attribute.class(
                        "title text-gray-900 mb-4 text-xl md:text-2xl",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Follow us on Bluesky"))],
                  ),
                  html.p(
                    [attribute.class("body-lg text-gray-700 leading-relaxed")],
                    [
                      html.text(g_(
                        i18n_ctx,
                        "Stay updated on news, service status, and what's happening. You can also follow ",
                      )),
                      html.a(
                        [
                          attribute.href(
                            "https://bsky.app/profile/fluxer.app/rss",
                          ),
                          attribute.class(
                            "underline hover:text-gray-900 transition-colors",
                          ),
                          attribute.target("_blank"),
                          attribute.rel("noopener noreferrer"),
                        ],
                        [html.text(g_(i18n_ctx, "our RSS feed"))],
                      ),
                      html.text("."),
                    ],
                  ),
                ]),
                html.div(
                  [attribute.class("mt-auto flex flex-col items-center")],
                  [
                    html.a(
                      [
                        attribute.href("https://bsky.app/profile/fluxer.app"),
                        attribute.class(
                          "label inline-block rounded-xl bg-[#4641D9] px-8 py-4 text-base md:text-lg text-white transition-colors hover:bg-opacity-90 shadow-md w-full text-center",
                        ),
                        attribute.target("_blank"),
                        attribute.rel("noopener noreferrer"),
                      ],
                      [html.text(g_(i18n_ctx, "Follow @fluxer.app"))],
                    ),
                  ],
                ),
              ],
            ),
            support_card.render(
              ctx,
              "bug",
              g_(i18n_ctx, "Report bugs"),
              g_(
                i18n_ctx,
                "Approved reports grant access to Fluxer Testers, where you can earn points for Plutonium codes and the Bug Hunter badge.",
              ),
              g_(i18n_ctx, "Read the Guide"),
              bug_article_href,
            ),
            support_card.render(
              ctx,
              "code",
              g_(i18n_ctx, "Contribute code"),
              g_(
                i18n_ctx,
                "Fluxer is open source (AGPLv3). Contribute directly on GitHub by opening pull requests.",
              ),
              g_(i18n_ctx, "View repository"),
              "https://github.com/fluxerapp/fluxer",
            ),
            support_card.render(
              ctx,
              "shield-check",
              g_(i18n_ctx, "Found a security issue?"),
              g_(
                i18n_ctx,
                "We appreciate responsible disclosure to security@fluxer.app. We offer Plutonium codes and Bug Hunter badges based on severity.",
              ),
              "security@fluxer.app",
              "mailto:security@fluxer.app",
            ),
          ],
        ),
      ]),
    ],
  )
}
