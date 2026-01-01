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
import fluxer_marketing/components/platform_download_button.{Light}
import fluxer_marketing/components/pwa_install_dialog
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/web.{type Context}
import gleam/list
import gleam/option.{Some}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    hero_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Download Fluxer"),
      description: g_(
        i18n_ctx,
        "Download Fluxer for Windows, macOS, and Linux. Mobile apps are underway.",
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
    icon: icons.download([
      attribute.class("h-14 w-14 md:h-18 md:w-18 text-white"),
    ]),
    title: g_(i18n_ctx, "Download Fluxer"),
    description: g_(i18n_ctx, "Available on your desktop — and on the web"),
    extra_content: download_grid(ctx),
    custom_padding: hero_base.default_padding(),
  ))
}

fn download_grid(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div([attribute.class("mt-12 md:mt-16 w-full max-w-3xl mx-auto")], [
    html.div([attribute.class("mb-10")], [
      html.div(
        [
          attribute.class(
            "flex flex-col sm:flex-row flex-wrap gap-6 justify-center items-stretch sm:items-start",
          ),
        ],
        [
          html.div(
            [
              attribute.class(
                "flex flex-col items-stretch w-full sm:w-auto sm:items-start",
              ),
            ],
            [
              platform_download_button.render_desktop_button(
                ctx,
                platform_download_button.Windows,
                Light,
                Some("dl"),
                True,
                True,
              ),
              html.p(
                [
                  attribute.class(
                    "mt-2 text-xs text-white/50 text-center w-full",
                  ),
                ],
                [
                  html.text(platform_download_button.get_system_requirements(
                    ctx,
                    platform_download_button.Windows,
                  )),
                ],
              ),
            ],
          ),
          html.div(
            [
              attribute.class(
                "flex flex-col items-stretch w-full sm:w-auto sm:items-start",
              ),
            ],
            [
              platform_download_button.render_desktop_button(
                ctx,
                platform_download_button.MacOS,
                Light,
                Some("dl"),
                True,
                True,
              ),
              html.p(
                [
                  attribute.class(
                    "mt-2 text-xs text-white/50 text-center w-full",
                  ),
                ],
                [
                  html.text(platform_download_button.get_system_requirements(
                    ctx,
                    platform_download_button.MacOS,
                  )),
                ],
              ),
            ],
          ),
          html.div(
            [
              attribute.class(
                "flex flex-col items-stretch w-full sm:w-auto sm:items-start",
              ),
            ],
            [
              platform_download_button.render_desktop_button(
                ctx,
                platform_download_button.Linux,
                Light,
                Some("dl"),
                True,
                True,
              ),
              html.p(
                [
                  attribute.class(
                    "mt-2 text-xs text-white/50 text-center invisible w-full",
                  ),
                ],
                [html.text("—")],
              ),
            ],
          ),
        ],
      ),
    ]),

    html.div(
      [
        attribute.class("flex justify-center my-10 sm:my-12"),
        attribute.attribute("aria-hidden", "true"),
      ],
      [
        svg.svg(
          [
            attribute.class("w-40 sm:w-52 h-6 text-white/40"),
            attribute.attribute("viewBox", "0 0 200 20"),
            attribute.attribute("fill", "none"),
            attribute.attribute("stroke", "currentColor"),
            attribute.attribute("stroke-width", "3"),
          ],
          [
            svg.path([
              attribute.attribute(
                "d",
                "M0 10 Q20 0 40 10 T80 10 T120 10 T160 10 T200 10",
              ),
              attribute.attribute("stroke-linecap", "round"),
            ]),
          ],
        ),
      ],
    ),

    html.div([attribute.class("max-w-2xl mx-auto text-center")], [
      html.h3([attribute.class("text-lg md:text-xl font-semibold text-white")], [
        html.text(g_(i18n_ctx, "Mobile apps are on the way")),
      ]),
      html.p(
        [
          attribute.class(
            "mt-3 text-sm md:text-base text-white/70 leading-relaxed",
          ),
        ],
        [
          html.text(g_(
            i18n_ctx,
            "We're building apps for iOS and Android. Until they're ready, Fluxer works in your mobile browser — and we've put a lot of effort into making it feel as app-like as possible.",
          )),
        ],
      ),
      html.ul(
        [
          attribute.class(
            "mt-4 text-sm md:text-base text-white/70 text-left mx-auto max-w-xl list-disc pl-6 space-y-2",
          ),
        ],
        [
          html.li([], [
            html.text(g_(
              i18n_ctx,
              "Add Fluxer to your home screen to hide the browser UI.",
            )),
          ]),
          html.li([], [
            html.text(g_(i18n_ctx, "See badge counts on the app icon.")),
          ]),
          html.li([], [
            html.text(g_(
              i18n_ctx,
              "Get push notifications when you're away from the app.",
            )),
          ]),
        ],
      ),
      html.div([attribute.class("mt-6 flex justify-center")], [
        pwa_install_dialog.render_trigger(ctx),
      ]),
      pwa_install_dialog.render_modal(ctx),
      html.p(
        [
          attribute.class(
            "mt-4 text-sm md:text-base text-white/70 leading-relaxed",
          ),
        ],
        [
          html.text(g_(
            i18n_ctx,
            "This isn't a full replacement for the desktop app yet, and some things you'd expect from a mobile chat app are still missing.",
          )),
        ],
      ),

      html.div(
        [
          attribute.class("flex justify-center my-10 sm:my-12"),
          attribute.attribute("aria-hidden", "true"),
        ],
        [
          svg.svg(
            [
              attribute.class("w-40 sm:w-52 h-6 text-white/40"),
              attribute.attribute("viewBox", "0 0 200 20"),
              attribute.attribute("fill", "none"),
              attribute.attribute("stroke", "currentColor"),
              attribute.attribute("stroke-width", "3"),
            ],
            [
              svg.path([
                attribute.attribute(
                  "d",
                  "M0 10 Q20 0 40 10 T80 10 T120 10 T160 10 T200 10",
                ),
                attribute.attribute("stroke-linecap", "round"),
              ]),
            ],
          ),
        ],
      ),

      html.div([attribute.class("mt-10")], [
        html.h4(
          [attribute.class("text-base md:text-lg font-semibold text-white")],
          [
            html.text(g_(i18n_ctx, "Help make the mobile app a reality")),
          ],
        ),
        html.p(
          [
            attribute.class(
              "mt-2 text-sm md:text-base text-white/70 leading-relaxed",
            ),
          ],
          [
            html.text(g_(
              i18n_ctx,
              "Fluxer is community-funded. If you'd like to help support iOS and Android development:",
            )),
          ],
        ),
        html.div(
          [
            attribute.class("mt-5 grid grid-cols-1 sm:grid-cols-2 gap-4"),
          ],
          [
            support_cta_button(
              web.prepend_base_path(ctx, "/plutonium"),
              icons.coins([attribute.class("h-6 w-6 shrink-0")]),
              g_(i18n_ctx, "Plutonium"),
              g_(i18n_ctx, "Get perks and help fund development"),
              False,
            ),
            support_cta_button(
              web.prepend_base_path(ctx, "/plutonium#visionary"),
              icons.crown([attribute.class("h-6 w-6 shrink-0")]),
              g_(i18n_ctx, "Become a Visionary"),
              g_(i18n_ctx, "Sponsor the roadmap for mobile"),
              False,
            ),
            support_cta_button(
              web.prepend_base_path(ctx, "/donate"),
              icons.heart([attribute.class("h-6 w-6 shrink-0")]),
              g_(i18n_ctx, "Donate to Fluxer"),
              g_(i18n_ctx, "Send a one-time gift"),
              False,
            ),
            support_cta_button(
              "https://github.com/fluxerapp/fluxer",
              icons.github([attribute.class("h-6 w-6 shrink-0")]),
              g_(i18n_ctx, "Contribute on GitHub"),
              g_(i18n_ctx, "Code, issues, docs, and reviews"),
              True,
            ),
          ],
        ),
      ]),
    ]),
  ])
}

fn support_cta_button(
  href: String,
  icon: Element(a),
  title: String,
  helper: String,
  new_tab: Bool,
) -> Element(a) {
  let attrs = [
    attribute.href(href),
    attribute.class(
      "inline-flex flex-col items-center justify-center gap-1 rounded-2xl px-6 py-5 md:px-8 md:py-6 transition-colors shadow-lg bg-white text-[#4641D9] hover:bg-white/90",
    ),
  ]

  let attrs = case new_tab {
    True ->
      attrs
      |> list.append([
        attribute.target("_blank"),
        attribute.rel("noopener noreferrer"),
      ])
    False -> attrs
  }

  html.a(attrs, [
    html.div([attribute.class("flex items-center gap-3")], [
      icon,
      html.span([attribute.class("text-base md:text-lg font-semibold")], [
        html.text(title),
      ]),
    ]),
    html.span([attribute.class("text-xs text-[#4641D9]/70")], [
      html.text(helper),
    ]),
  ])
}
