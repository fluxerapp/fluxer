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
import fluxer_marketing/web.{type Context, href}
import gleam/option.{None}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    hero_section(ctx),
    perks_section(ctx),
    cta_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Fluxer Partner"),
      description: g_(
        i18n_ctx,
        "Join the Fluxer Partner Program and unlock exclusive benefits for you and your community",
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
    icon: icons.fluxer_partner([
      attribute.class("h-14 w-14 md:h-18 md:w-18"),
    ]),
    title: g_(i18n_ctx, "Become a Fluxer Partner"),
    description: g_(
      i18n_ctx,
      "Exclusive perks and benefits for content creators and large community owners.",
    ),
    extra_content: element.none(),
    custom_padding: hero_base.default_padding(),
  ))
}

fn perks_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class("bg-white px-6 py-24 md:py-40"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-6xl")], [
        html.div([attribute.class("mb-12 md:mb-16 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [html.text(g_(i18n_ctx, "Partner Perks"))],
          ),
          html.p(
            [
              attribute.class("lead mx-auto max-w-3xl text-gray-700"),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "Rewards that recognize your impact, help your community grow, and give you a direct line to the Fluxer team.",
              )),
            ],
          ),
        ]),
        html.div(
          [
            attribute.class("grid gap-6 md:gap-8 md:grid-cols-2 lg:grid-cols-3"),
          ],
          [
            perk_card(
              ctx,
              "fluxer_premium",
              g_(i18n_ctx, "Free Plutonium"),
              g_(
                i18n_ctx,
                "Get free Plutonium for your account to enjoy all premium features.",
              ),
              False,
              "/plutonium",
            ),
            perk_card(
              ctx,
              "fluxer_partner",
              g_(i18n_ctx, "Partner Badge"),
              g_(
                i18n_ctx,
                "Display an exclusive Partner badge on your profile to stand out.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "seal_check",
              g_(i18n_ctx, "Community Verification"),
              g_(
                i18n_ctx,
                "Your community gets verified status for authenticity and trust.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "link",
              g_(i18n_ctx, "Custom Vanity URL"),
              g_(
                i18n_ctx,
                "Get an exclusive custom vanity URL like fluxer.gg/yourcommunity.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "fluxer_staff",
              g_(i18n_ctx, "Direct Team Access"),
              g_(
                i18n_ctx,
                "Join the exclusive Partners community with direct access to the Fluxer team.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "magnifying_glass",
              g_(i18n_ctx, "Featured in Discovery"),
              g_(
                i18n_ctx,
                "Get heightened visibility by being featured in Community Discovery.",
              ),
              True,
              "",
            ),
            perk_card(
              ctx,
              "gif",
              g_(i18n_ctx, "Animated Avatar & Banner"),
              g_(
                i18n_ctx,
                "Upload animated GIFs as your community's avatar and banner.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "arrow_up",
              g_(i18n_ctx, "Increased Limits"),
              g_(
                i18n_ctx,
                "Your community receives increased limits when you need them.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "rocket",
              g_(i18n_ctx, "Early Feature Access"),
              g_(
                i18n_ctx,
                "Unlock and test new features before they're available to everyone.",
              ),
              False,
              "",
            ),
            perk_card(
              ctx,
              "coins",
              g_(i18n_ctx, "Creator Monetization"),
              g_(
                i18n_ctx,
                "Early access to creator monetization features with lower platform fees than non-partners.",
              ),
              True,
              "",
            ),
            perk_card(
              ctx,
              "microphone",
              g_(i18n_ctx, "VIP Voice Servers"),
              g_(
                i18n_ctx,
                "Access to VIP voice servers reserved exclusively for partnered communities.",
              ),
              True,
              "",
            ),
            perk_card(
              ctx,
              "tshirt",
              g_(i18n_ctx, "Exclusive Merch"),
              g_(
                i18n_ctx,
                "Get exclusive Fluxer Partner-only merchandise and swag.",
              ),
              True,
              "",
            ),
          ],
        ),
      ]),
    ],
  )
}

fn perk_card(
  ctx: Context,
  icon_name: String,
  title: String,
  description: String,
  coming_soon: Bool,
  link: String,
) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let icon = case icon_name {
    "fluxer_premium" ->
      icons.fluxer_premium(None, [attribute.class("h-8 w-8 text-[#4641D9]")])
    "fluxer_partner" ->
      icons.fluxer_partner([attribute.class("h-8 w-8 text-[#4641D9]")])
    "fluxer_staff" ->
      icons.fluxer_staff([attribute.class("h-8 w-8 text-[#4641D9]")])
    "seal_check" ->
      icons.seal_check([attribute.class("h-8 w-8 text-[#4641D9]")])
    "link" -> icons.link([attribute.class("h-8 w-8 text-[#4641D9]")])
    "magnifying_glass" ->
      icons.magnifying_glass([attribute.class("h-8 w-8 text-[#4641D9]")])
    "chart_line" ->
      icons.chart_line([attribute.class("h-8 w-8 text-[#4641D9]")])
    "arrow_up" -> icons.arrow_up([attribute.class("h-8 w-8 text-[#4641D9]")])
    "rocket" -> icons.rocket([attribute.class("h-8 w-8 text-[#4641D9]")])
    "coins" -> icons.coins([attribute.class("h-8 w-8 text-[#4641D9]")])
    "microphone" ->
      icons.microphone([attribute.class("h-8 w-8 text-[#4641D9]")])
    "tshirt" -> icons.tshirt([attribute.class("h-8 w-8 text-[#4641D9]")])
    "gif" -> icons.gif([attribute.class("h-8 w-8 text-[#4641D9]")])
    _ -> icons.sparkle([attribute.class("h-8 w-8 text-[#4641D9]")])
  }

  html.div(
    [
      attribute.class(
        "relative flex h-full flex-col rounded-2xl bg-white border border-gray-200/80 p-6 md:p-7 shadow-lg",
      ),
    ],
    [
      case coming_soon {
        True ->
          html.div(
            [
              attribute.class(
                "caption absolute -top-2 -right-2 rounded-full bg-[#4641D9] px-3 py-1 text-white",
              ),
            ],
            [html.text(g_(i18n_ctx, "Coming Soon"))],
          )
        False -> html.div([], [])
      },
      case link {
        "" -> html.div([], [])
        _ ->
          html.a(
            [
              href(ctx, link),
              attribute.class(
                "caption absolute top-2 right-2 rounded-full bg-[#4641D9] px-3 py-1 text-white hover:bg-[#3d38c7] transition flex items-center gap-1",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "See perks")),
              icons.arrow_right([attribute.class("h-3 w-3")]),
            ],
          )
      },
      html.div(
        [
          attribute.class(
            "mb-4 inline-flex items-center justify-center w-12 h-12 rounded-xl bg-[#4641D9]/10",
          ),
        ],
        [icon],
      ),
      html.h3([attribute.class("title-sm mb-2 text-black")], [
        html.text(title),
      ]),
      html.p([attribute.class("body text-gray-600")], [html.text(description)]),
    ],
  )
}

fn cta_section(ctx: Context) -> Element(a) {
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
                "mx-auto max-w-4xl px-6 py-20 md:py-28 text-center",
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
                  html.text(g_(i18n_ctx, "Ready to become a Partner?")),
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
                    "Send us an email with the following information to get started:",
                  )),
                ],
              ),
              html.div(
                [
                  attribute.class(
                    "mb-8 rounded-2xl border border-white/15 bg-white/5 p-6 md:p-8 text-left",
                  ),
                ],
                [
                  html.ul([attribute.class("body space-y-3 text-white/90")], [
                    html.li([attribute.class("flex items-start gap-3")], [
                      icons.check([
                        attribute.class(
                          "h-5 w-5 mt-0.5 flex-shrink-0 text-white",
                        ),
                      ]),
                      html.span([], [
                        html.text(g_(
                          i18n_ctx,
                          "Your name and platform username",
                        )),
                      ]),
                    ]),
                    html.li([attribute.class("flex items-start gap-3")], [
                      icons.check([
                        attribute.class(
                          "h-5 w-5 mt-0.5 flex-shrink-0 text-white",
                        ),
                      ]),
                      html.span([], [
                        html.text(g_(
                          i18n_ctx,
                          "Links to your content or community (YouTube, Twitch, Discord, etc.)",
                        )),
                      ]),
                    ]),
                    html.li([attribute.class("flex items-start gap-3")], [
                      icons.check([
                        attribute.class(
                          "h-5 w-5 mt-0.5 flex-shrink-0 text-white",
                        ),
                      ]),
                      html.span([], [
                        html.text(g_(
                          i18n_ctx,
                          "Brief description of your audience and what you do",
                        )),
                      ]),
                    ]),
                    html.li([attribute.class("flex items-start gap-3")], [
                      icons.check([
                        attribute.class(
                          "h-5 w-5 mt-0.5 flex-shrink-0 text-white",
                        ),
                      ]),
                      html.span([], [
                        html.text(g_(
                          i18n_ctx,
                          "How you plan to use Fluxer with your community",
                        )),
                      ]),
                    ]),
                  ]),
                ],
              ),
              html.a(
                [
                  attribute.href("mailto:partners@fluxer.app"),
                  attribute.class(
                    "label inline-block rounded-xl bg-white px-8 py-4 text-[#4641D9] shadow-lg transition hover:bg-gray-100",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Apply at partners@fluxer.app"))],
              ),
              html.p([attribute.class("body-sm mt-6 text-white/80")], [
                html.text(g_(
                  i18n_ctx,
                  "We review all applications and will get back to you as soon as possible.",
                )),
              ]),
            ],
          ),
        ],
      ),
    ],
  )
}
