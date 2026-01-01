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
import fluxer_marketing/web.{type Context, href}
import gleam/option.{None}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-gray-50 to-white px-6 pb-24 md:pb-40",
      ),
    ],
    [
      html.div(
        [
          attribute.class(
            "mx-auto max-w-6xl rounded-3xl bg-gradient-to-br from-black to-gray-900 p-10 text-white md:p-16 lg:p-20 shadow-xl",
          ),
        ],
        [
          html.div([attribute.class("mb-10 md:mb-12 text-center")], [
            html.div(
              [
                attribute.class(
                  "inline-flex items-center justify-center w-20 h-20 md:w-24 md:h-24 rounded-2xl bg-white/10 backdrop-blur-sm mb-6 md:mb-8",
                ),
              ],
              [
                icons.fluxer_partner([
                  attribute.class("h-10 w-10 md:h-12 md:w-12"),
                ]),
              ],
            ),
            html.h2(
              [
                attribute.class(
                  "display mb-6 md:mb-8 text-white text-3xl md:text-4xl lg:text-5xl",
                ),
              ],
              [
                html.text(g_(i18n_ctx, "Become a Fluxer Partner")),
              ],
            ),
            html.p([attribute.class("lead mx-auto max-w-3xl text-white/90")], [
              html.text(g_(
                i18n_ctx,
                "Content creators and community owners: unlock exclusive perks including free Plutonium, a Partner badge, custom vanity URLs, and more.",
              )),
            ]),
          ]),
          html.div(
            [
              attribute.class(
                "mb-10 md:mb-12 grid gap-6 md:gap-8 grid-cols-1 sm:grid-cols-3 max-w-3xl mx-auto",
              ),
            ],
            [
              html.div(
                [attribute.class("flex flex-col items-center text-center")],
                [
                  html.div(
                    [
                      attribute.class(
                        "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-xl bg-white/10 backdrop-blur-sm mb-4",
                      ),
                    ],
                    [
                      icons.fluxer_premium(None, [
                        attribute.class("h-8 w-8 md:h-10 md:w-10"),
                      ]),
                    ],
                  ),
                  html.p([attribute.class("body-lg font-semibold text-white")], [
                    html.text(g_(i18n_ctx, "Free Plutonium")),
                  ]),
                ],
              ),
              html.div(
                [attribute.class("flex flex-col items-center text-center")],
                [
                  html.div(
                    [
                      attribute.class(
                        "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-xl bg-white/10 backdrop-blur-sm mb-4",
                      ),
                    ],
                    [
                      icons.seal_check([
                        attribute.class("h-8 w-8 md:h-10 md:w-10"),
                      ]),
                    ],
                  ),
                  html.p([attribute.class("body-lg font-semibold text-white")], [
                    html.text(g_(i18n_ctx, "Verified Community")),
                  ]),
                ],
              ),
              html.div(
                [attribute.class("flex flex-col items-center text-center")],
                [
                  html.div(
                    [
                      attribute.class(
                        "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-xl bg-white/10 backdrop-blur-sm mb-4",
                      ),
                    ],
                    [
                      icons.chats_circle([
                        attribute.class("h-8 w-8 md:h-10 md:w-10"),
                      ]),
                    ],
                  ),
                  html.p([attribute.class("body-lg font-semibold text-white")], [
                    html.text(g_(i18n_ctx, "Direct Team Access")),
                  ]),
                ],
              ),
            ],
          ),
          html.div([attribute.class("text-center")], [
            html.a(
              [
                href(ctx, "/partners"),
                attribute.class(
                  "label inline-block rounded-lg bg-white px-8 py-4 text-black transition-colors hover:bg-opacity-90",
                ),
              ],
              [html.text(g_(i18n_ctx, "Become a Partner"))],
            ),
          ]),
        ],
      ),
    ],
  )
}
