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
import fluxer_marketing/pricing
import fluxer_marketing/web.{type Context, href}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let monthly_price =
    pricing.get_formatted_price(pricing.Monthly, ctx.country_code)
  let yearly_price =
    pricing.get_formatted_price(pricing.Yearly, ctx.country_code)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-white to-gray-50 px-6 py-24 md:py-40",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("mb-16 md:mb-20 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-8 md:mb-10 text-black text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "Get more with Fluxer Plutonium")),
            ],
          ),
          html.div(
            [
              attribute.class(
                "flex flex-col sm:flex-row items-center justify-center gap-3 mb-6",
              ),
            ],
            [
              html.span(
                [attribute.class("text-3xl md:text-4xl font-bold text-black")],
                [
                  html.text(
                    monthly_price
                    <> g_(i18n_ctx, "/mo")
                    <> " "
                    <> g_(i18n_ctx, "or")
                    <> " "
                    <> yearly_price
                    <> g_(i18n_ctx, "/yr"),
                  ),
                ],
              ),
              html.span(
                [
                  attribute.class(
                    "inline-flex items-center rounded-xl bg-[#4641D9] px-4 py-2 text-sm md:text-base font-semibold text-white",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Save 17%"))],
              ),
            ],
          ),
          html.p(
            [
              attribute.class("lead mx-auto max-w-2xl text-gray-700"),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "Higher limits, exclusive features, and early access to new updates",
              )),
            ],
          ),
        ]),
        html.div(
          [
            attribute.class(
              "mb-16 md:mb-20 grid grid-cols-1 md:grid-cols-3 gap-8 md:gap-12 max-w-4xl mx-auto",
            ),
          ],
          [
            html.div(
              [attribute.class("flex flex-col items-center text-center")],
              [
                html.div(
                  [
                    attribute.class(
                      "inline-flex items-center justify-center w-20 h-20 md:w-24 md:h-24 rounded-3xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 mb-4",
                    ),
                  ],
                  [
                    icons.hash([
                      attribute.class(
                        "h-10 w-10 md:h-12 md:w-12 text-[#4641D9]",
                      ),
                    ]),
                  ],
                ),
                html.h3(
                  [
                    attribute.class(
                      "title text-black text-lg md:text-xl whitespace-nowrap",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Custom identity"))],
                ),
              ],
            ),
            html.div(
              [attribute.class("flex flex-col items-center text-center")],
              [
                html.div(
                  [
                    attribute.class(
                      "inline-flex items-center justify-center w-20 h-20 md:w-24 md:h-24 rounded-3xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 mb-4",
                    ),
                  ],
                  [
                    icons.video_camera([
                      attribute.class(
                        "h-10 w-10 md:h-12 md:w-12 text-[#4641D9]",
                      ),
                    ]),
                  ],
                ),
                html.h3(
                  [
                    attribute.class(
                      "title text-black text-lg md:text-xl whitespace-nowrap",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Premium quality"))],
                ),
              ],
            ),
            html.div(
              [attribute.class("flex flex-col items-center text-center")],
              [
                html.div(
                  [
                    attribute.class(
                      "inline-flex items-center justify-center w-20 h-20 md:w-24 md:h-24 rounded-3xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 mb-4",
                    ),
                  ],
                  [
                    icons.sparkle([
                      attribute.class(
                        "h-10 w-10 md:h-12 md:w-12 text-[#4641D9]",
                      ),
                    ]),
                  ],
                ),
                html.h3(
                  [
                    attribute.class(
                      "title text-black text-lg md:text-xl whitespace-nowrap",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Exclusive features"))],
                ),
              ],
            ),
          ],
        ),
        html.div([attribute.class("text-center")], [
          html.a(
            [
              href(ctx, "/plutonium"),
              attribute.class(
                "label inline-block rounded-xl bg-[#4641D9] px-10 py-5 md:px-12 md:py-6 text-lg md:text-xl text-white transition hover:bg-opacity-90 shadow-lg",
              ),
            ],
            [html.text(g_(i18n_ctx, "Learn more"))],
          ),
        ]),
      ]),
    ],
  )
}
