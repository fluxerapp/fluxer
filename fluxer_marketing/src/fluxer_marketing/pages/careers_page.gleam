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
import fluxer_marketing/components/support_card
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
    community_team_section(ctx),
    contribute_section(ctx),
    future_section(ctx),
    cta_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Careers at Fluxer"),
      description: g_(
        i18n_ctx,
        "Join the Fluxer community and help build the future of communication through open source and community contributions.",
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
    icon: icons.fluxer_staff([
      attribute.class("h-14 w-14 md:h-18 md:w-18 text-white"),
    ]),
    title: g_(i18n_ctx, "Join the team behind Fluxer"),
    description: g_(
      i18n_ctx,
      "Help us build a different kind of communication platform — open source, community-funded, and built with care.",
    ),
    extra_content: element.none(),
    custom_padding: hero_base.default_padding(),
  ))
}

fn contribute_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section(
    [
      attribute.class(
        "bg-gradient-to-b from-[#4641D9] to-[#3832B8] px-6 py-24 md:py-40",
      ),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("mb-16 md:mb-20 text-center")], [
          html.h2(
            [
              attribute.class(
                "display mb-6 md:mb-8 text-white text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [html.text(g_(i18n_ctx, "Ways to contribute"))],
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
                "Fluxer is built in the open. Pick the path that fits how you like to help.",
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
              "code",
              g_(i18n_ctx, "Open source"),
              g_(
                i18n_ctx,
                "Fix bugs, ship features, or polish the experience in our main repo.",
              ),
              g_(i18n_ctx, "View repository"),
              "https://github.com/fluxerapp/fluxer",
            ),
            support_card.render(
              ctx,
              "translate",
              g_(i18n_ctx, "Localization"),
              g_(
                i18n_ctx,
                "Help translate Fluxer and its docs so it feels native everywhere.",
              ),
              "i18n@fluxer.app",
              "mailto:i18n@fluxer.app",
            ),
            support_card.render(
              ctx,
              "chat-centered-text",
              g_(i18n_ctx, "Community"),
              g_(
                i18n_ctx,
                "Share feedback, report bugs, and help make Fluxer welcoming for everyone.",
              ),
              g_(i18n_ctx, "Join Fluxer HQ"),
              "https://fluxer.gg/fluxer-hq",
            ),
            support_card.render(
              ctx,
              "bug",
              g_(i18n_ctx, "Security reports"),
              g_(
                i18n_ctx,
                "Report vulnerabilities and help us keep the platform secure.",
              ),
              g_(i18n_ctx, "Security Bug Bounty"),
              "/security",
            ),
            support_card.render(
              ctx,
              "shield-check",
              g_(i18n_ctx, "Trust & safety"),
              g_(
                i18n_ctx,
                "Report people or communities so we can keep Fluxer safe for everyone.",
              ),
              "safety@fluxer.app",
              "mailto:safety@fluxer.app",
            ),
            support_card.render(
              ctx,
              "fluxer-partner",
              g_(i18n_ctx, "Become a Fluxer Partner"),
              g_(
                i18n_ctx,
                "If you're a creator or community owner, explore the Partner program and its perks.",
              ),
              g_(i18n_ctx, "Learn about Partners"),
              "/partners",
            ),
          ],
        ),
      ]),
    ],
  )
}

fn community_team_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

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
                "display mb-6 md:mb-8 text-black text-5xl md:text-6xl lg:text-7xl",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "Fluxer Community Team")),
            ],
          ),
          html.p(
            [
              attribute.class(
                "lead lead-soft mx-auto max-w-3xl text-gray-700 text-xl md:text-2xl",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "A non-paid role for recurring contributors who help shape Fluxer alongside the core team.",
              )),
            ],
          ),
        ]),
        html.div(
          [
            attribute.class(
              "flex flex-wrap justify-center gap-3 sm:gap-4 md:gap-5 lg:gap-6 max-w-5xl mx-auto",
            ),
          ],
          [
            community_pill(
              ctx,
              icons.code_icon([
                attribute.class("h-6 w-6 md:h-7 md:w-7 text-[#4641D9]"),
              ]),
              g_(i18n_ctx, "Software development"),
            ),
            community_pill(
              ctx,
              icons.translate([
                attribute.class("h-6 w-6 md:h-7 md:w-7 text-[#4641D9]"),
              ]),
              g_(i18n_ctx, "Translation & localization"),
            ),
            community_pill(
              ctx,
              icons.palette([
                attribute.class("h-6 w-6 md:h-7 md:w-7 text-[#4641D9]"),
              ]),
              g_(i18n_ctx, "Design & branding"),
            ),
            community_pill(
              ctx,
              icons.shield_check([
                attribute.class("h-6 w-6 md:h-7 md:w-7 text-[#4641D9]"),
              ]),
              g_(i18n_ctx, "Trust & safety"),
            ),
          ],
        ),
        html.div([attribute.class("mt-10 md:mt-12 text-center")], [
          html.a(
            [
              attribute.href("mailto:careers@fluxer.app"),
              attribute.class(
                "label inline-flex items-center justify-center rounded-xl bg-[#4641D9] px-8 py-4 text-base md:text-lg text-white shadow-lg transition hover:bg-opacity-90",
              ),
            ],
            [html.text("careers@fluxer.app")],
          ),
        ]),
      ]),
    ],
  )
}

fn community_pill(ctx: Context, icon: Element(a), label: String) -> Element(a) {
  let _ = ctx

  html.div(
    [
      attribute.class(
        "inline-flex items-center gap-3 sm:gap-4 rounded-full bg-white border border-gray-200/80 px-5 sm:px-6 md:px-7 py-3.5 sm:py-4 shadow-md lg:shadow-lg",
      ),
    ],
    [
      icon,
      html.span(
        [
          attribute.class(
            "body-lg text-gray-800 whitespace-nowrap text-base md:text-lg",
          ),
        ],
        [
          html.text(label),
        ],
      ),
    ],
  )
}

fn future_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-white px-6 py-24 md:py-40")], [
    html.div([attribute.class("mx-auto max-w-7xl text-center")], [
      html.h2(
        [
          attribute.class(
            "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
          ),
        ],
        [html.text(g_(i18n_ctx, "The future of paid roles"))],
      ),
      html.p(
        [
          attribute.class(
            "lead lead-soft mx-auto max-w-3xl text-gray-700 text-xl md:text-2xl mb-10 md:mb-12",
          ),
        ],
        [
          html.text(g_(
            i18n_ctx,
            "We're still building the business around Fluxer and aren't quite ready for paid roles yet.",
          )),
        ],
      ),
      html.div(
        [
          attribute.class(
            "mx-auto max-w-4xl rounded-3xl bg-white/95 backdrop-blur-sm border border-gray-200/80 p-8 md:p-10 shadow-xl text-left",
          ),
        ],
        [
          html.p(
            [
              attribute.class(
                "body-lg text-gray-900 leading-relaxed text-base md:text-lg mb-4 md:mb-5",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "Right now we're focused on shipping a great product and keeping Fluxer independent and bootstrapped.",
              )),
            ],
          ),
          html.p(
            [
              attribute.class(
                "body-lg text-gray-700 leading-relaxed text-base md:text-lg",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "With your support, we hope to make this independent communication platform sustainable and start offering paid roles in the future.",
              )),
            ],
          ),
        ],
      ),
    ]),
  ])
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
                  html.text(g_(i18n_ctx, "Want to help build Fluxer?")),
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
                    "Share a bit about yourself, what you'd like to work on, and links to any relevant work.",
                  )),
                ],
              ),
              html.div(
                [
                  attribute.class(
                    "flex flex-col sm:flex-row items-center justify-center gap-3 sm:gap-4",
                  ),
                ],
                [
                  html.a(
                    [
                      attribute.href("mailto:careers@fluxer.app"),
                      attribute.class(
                        "label inline-flex items-center justify-center rounded-xl bg-white px-8 py-4 text-[#4641D9] shadow-lg transition hover:bg-gray-100",
                      ),
                    ],
                    [html.text("careers@fluxer.app")],
                  ),
                  html.a(
                    [
                      attribute.href("https://fluxer.gg/fluxer-hq"),
                      attribute.target("_blank"),
                      attribute.rel("noopener noreferrer"),
                      attribute.class(
                        "label inline-flex items-center justify-center gap-2 rounded-xl bg-white/10 px-8 py-4 text-white border border-white/25 hover:bg-white/15",
                      ),
                    ],
                    [
                      icons.chats_circle([
                        attribute.class("h-5 w-5 text-white"),
                      ]),
                      html.span([], [
                        html.text(g_(i18n_ctx, "Join Fluxer HQ community")),
                      ]),
                    ],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}
