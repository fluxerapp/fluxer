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
import fluxer_marketing/number_format
import fluxer_marketing/pages/layout
import fluxer_marketing/pages/layout/meta.{PageMeta}
import fluxer_marketing/pricing
import fluxer_marketing/web.{type Context, href}
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp

pub fn render(req: wisp.Request, ctx: Context) -> wisp.Response {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let content = [
    hero_section(ctx),
    comparison_section(ctx),
    features_section(ctx),
    visionary_section(ctx),
    self_hosting_section(ctx),
    cta_section(ctx),
  ]

  layout.render(
    req,
    ctx,
    PageMeta(
      title: g_(i18n_ctx, "Fluxer Plutonium"),
      description: g_(
        i18n_ctx,
        "Upgrade to Plutonium for $4.99/mo: custom username tags, per-community profiles, message scheduling, 4K streaming, 500MB uploads, and more exclusive features.",
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

  let monthly_price =
    pricing.get_formatted_price(pricing.Monthly, ctx.country_code)
  let yearly_price =
    pricing.get_formatted_price(pricing.Yearly, ctx.country_code)

  hero_base.render(hero_base.HeroConfig(
    icon: icons.fluxer_premium(Some("#4641D9"), [
      attribute.class("h-14 w-14 md:h-18 md:w-18 text-white"),
    ]),
    title: g_(i18n_ctx, "Fluxer Plutonium"),
    description: g_(
      i18n_ctx,
      "Unlock higher limits and exclusive features while supporting an independent communication platform.",
    ),
    extra_content: html.div([], [
      html.p(
        [
          attribute.class(
            "body-lg mt-6 mb-10 md:mb-12 text-white/70 max-w-3xl mx-auto",
          ),
        ],
        [
          html.text(g_(
            i18n_ctx,
            "Note: Plutonium and Visionary benefits only apply to the official Fluxer.app instance, not third-party or self-hosted instances.",
          )),
        ],
      ),
      html.div(
        [
          attribute.class(
            "mb-8 flex flex-col sm:flex-row items-center justify-center gap-3 sm:gap-4",
          ),
        ],
        [
          html.span([attribute.class("text-4xl md:text-5xl font-bold")], [
            html.text(monthly_price <> g_(i18n_ctx, "/mo")),
          ]),
          html.span([attribute.class("text-xl text-white/80")], [
            html.text(g_(i18n_ctx, "or")),
          ]),
          html.span([attribute.class("text-4xl md:text-5xl font-bold")], [
            html.text(yearly_price <> g_(i18n_ctx, "/yr")),
          ]),
          html.span(
            [
              attribute.class(
                "inline-flex items-center rounded-xl bg-white/20 px-4 py-2 text-base md:text-lg font-semibold backdrop-blur-sm",
              ),
            ],
            [html.text(g_(i18n_ctx, "Save 17%"))],
          ),
        ],
      ),
    ]),
    custom_padding: hero_base.default_padding(),
  ))
}

fn comparison_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let free_price = case pricing.get_currency(ctx.country_code) {
    pricing.USD -> "$0"
    pricing.EUR -> "€0"
  }
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
      html.div([attribute.class("mx-auto max-w-6xl")], [
        html.h2(
          [
            attribute.class(
              "display mb-16 md:mb-20 text-center text-black text-4xl md:text-5xl lg:text-6xl",
            ),
          ],
          [html.text(g_(i18n_ctx, "Free vs Plutonium"))],
        ),
        html.div(
          [
            attribute.class(
              "mb-16 md:mb-20 grid gap-8 md:gap-10 grid-cols-1 md:grid-cols-2 max-w-4xl mx-auto",
            ),
          ],
          [
            html.div(
              [
                attribute.class(
                  "rounded-3xl bg-white border-2 border-gray-200 p-10 md:p-12 text-center shadow-lg",
                ),
              ],
              [
                html.h3(
                  [
                    attribute.class(
                      "title mb-4 text-black text-2xl md:text-3xl",
                    ),
                  ],
                  [
                    html.text(g_(i18n_ctx, "Free")),
                  ],
                ),
                html.p(
                  [
                    attribute.class(
                      "text-4xl md:text-5xl font-bold text-gray-900 mb-3",
                    ),
                  ],
                  [
                    html.text(free_price),
                  ],
                ),
                html.p([attribute.class("body-lg text-gray-600")], [
                  html.text(g_(i18n_ctx, "Forever")),
                ]),
              ],
            ),
            html.div(
              [
                attribute.class(
                  "relative rounded-3xl border-2 border-[#4641D9] p-10 md:p-12 text-center bg-gradient-to-br from-[#4641D9]/5 to-[#6b5ce7]/5 shadow-xl",
                ),
              ],
              [
                html.div(
                  [
                    attribute.class(
                      "label absolute -top-4 left-1/2 -translate-x-1/2 rounded-xl bg-[#4641D9] px-4 py-2 text-white shadow-md",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Most popular"))],
                ),
                html.h3(
                  [
                    attribute.class(
                      "title mb-4 text-black text-2xl md:text-3xl",
                    ),
                  ],
                  [
                    html.text(g_(i18n_ctx, "Plutonium")),
                  ],
                ),
                html.p(
                  [
                    attribute.class(
                      "text-4xl md:text-5xl font-bold text-[#4641D9] mb-3",
                    ),
                  ],
                  [
                    html.text(monthly_price <> g_(i18n_ctx, "/mo")),
                  ],
                ),
                html.p([attribute.class("body-lg text-gray-700")], [
                  html.text(
                    g_(i18n_ctx, "or")
                    <> " "
                    <> yearly_price
                    <> g_(i18n_ctx, "/year"),
                  ),
                ]),
              ],
            ),
          ],
        ),
        html.div([attribute.class("overflow-x-auto")], [
          html.table(
            [
              attribute.class(
                "w-full border-collapse rounded-lg border border-gray-200",
              ),
              attribute.style("table-layout", "fixed"),
            ],
            [
              html.thead([attribute.class("bg-gray-50")], [
                html.tr([], [
                  html.th(
                    [
                      attribute.class(
                        "label border-b border-gray-200 px-4 py-3 text-left text-black w-1/2",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Feature"))],
                  ),
                  html.th(
                    [
                      attribute.class(
                        "label border-b border-gray-200 px-2 sm:px-3 py-3 text-center text-black w-1/4 text-xs sm:text-sm",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Free"))],
                  ),
                  html.th(
                    [
                      attribute.class(
                        "label border-b border-gray-200 px-2 sm:px-3 py-3 text-center text-[#4641D9] w-1/4 text-xs sm:text-sm",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Plutonium"))],
                  ),
                ]),
              ]),
              html.tbody([], [
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Custom 4-digit username tag"),
                  False,
                  True,
                  None,
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Per-community profiles"),
                  False,
                  True,
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Message scheduling"),
                  g_(i18n_ctx, "Not available"),
                  g_(i18n_ctx, "Coming soon"),
                  Some(g_(i18n_ctx, "Coming soon")),
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Profile badge"),
                  False,
                  True,
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Custom video backgrounds"),
                  "1",
                  g_(i18n_ctx, "Coming soon"),
                  Some(g_(i18n_ctx, "Coming soon")),
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Custom entrance sounds"),
                  False,
                  True,
                  Some(g_(i18n_ctx, "Beta")),
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Custom notification sounds"),
                  False,
                  True,
                  Some(g_(i18n_ctx, "Beta")),
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Communities"),
                  "100",
                  "200",
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Message character limit"),
                  "2,000",
                  "4,000",
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Bookmarked messages"),
                  "50",
                  "300",
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Bio character limit"),
                  "160",
                  "320",
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "File upload size"),
                  "25 MB",
                  "500 MB",
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Emoji & sticker packs"),
                  "0",
                  g_(i18n_ctx, "Coming soon"),
                  Some(g_(i18n_ctx, "Coming soon")),
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Saved media"),
                  "50",
                  "500",
                  Some(g_(i18n_ctx, "Beta")),
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Use animated emojis"),
                  True,
                  True,
                  None,
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Global emoji & sticker access"),
                  False,
                  True,
                  None,
                ),
                comparison_row(
                  ctx,
                  g_(i18n_ctx, "Video quality"),
                  "720p/30fps",
                  "Up to 4K/60fps",
                  None,
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Animated avatars & banners"),
                  False,
                  True,
                  None,
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Early access to new features"),
                  False,
                  True,
                  None,
                ),
                comparison_check_row(
                  ctx,
                  g_(i18n_ctx, "Custom themes"),
                  True,
                  True,
                  None,
                ),
              ]),
            ],
          ),
        ]),
        html.div([attribute.class("mt-12 md:mt-16 text-center")], [
          html.a(
            [
              attribute.href(ctx.app_endpoint <> "/channels/@me"),
              attribute.class(
                "label inline-block rounded-xl bg-[#4641D9] px-10 py-5 md:px-12 md:py-6 text-lg md:text-xl text-white shadow-lg transition hover:bg-[#3d38c7]",
              ),
            ],
            [html.text(g_(i18n_ctx, "Get Plutonium"))],
          ),
        ]),
      ]),
    ],
  )
}

fn comparison_row(
  _ctx: Context,
  feature: String,
  free_value: String,
  plutonium_value: String,
  badge: option.Option(String),
) -> Element(a) {
  let badge_elements = case badge {
    Some(label) -> [
      html.span(
        [
          attribute.class(
            "caption inline-flex items-center rounded-full border border-[#4641D9] px-3 py-1 text-[0.6rem] font-semibold uppercase tracking-[0.2em] text-[#4641D9]",
          ),
        ],
        [html.text(label)],
      ),
    ]
    None -> []
  }

  let label_nodes =
    list.append([html.span([], [html.text(feature)])], badge_elements)

  html.tr([attribute.class("border-b border-gray-100")], [
    html.td([attribute.class("body px-4 py-3 text-gray-900")], [
      html.div(
        [attribute.class("flex flex-wrap items-center gap-2")],
        label_nodes,
      ),
    ]),
    html.td(
      [
        attribute.class(
          "body px-2 sm:px-3 py-3 text-center text-gray-600 text-xs sm:text-sm",
        ),
      ],
      [
        html.text(free_value),
      ],
    ),
    html.td(
      [
        attribute.class(
          "label px-2 sm:px-3 py-3 text-center text-[#4641D9] text-xs sm:text-sm",
        ),
      ],
      [
        html.text(plutonium_value),
      ],
    ),
  ])
}

fn comparison_check_row(
  _ctx: Context,
  feature: String,
  free_has: Bool,
  plutonium_has: Bool,
  badge: option.Option(String),
) -> Element(a) {
  let badge_elements = case badge {
    Some(label) -> [
      html.span(
        [
          attribute.class(
            "caption inline-flex items-center rounded-full border border-[#4641D9] px-3 py-1 text-[0.6rem] font-semibold uppercase tracking-[0.2em] text-[#4641D9]",
          ),
        ],
        [html.text(label)],
      ),
    ]
    None -> []
  }

  let label_nodes =
    list.append([html.span([], [html.text(feature)])], badge_elements)

  html.tr([attribute.class("border-b border-gray-100")], [
    html.td([attribute.class("body px-4 py-3 text-gray-900")], [
      html.div(
        [attribute.class("flex flex-wrap items-center gap-2")],
        label_nodes,
      ),
    ]),
    html.td([attribute.class("px-3 py-3 text-center")], [
      case free_has {
        True -> icons.check([attribute.class("h-5 w-5 mx-auto text-green-600")])
        False -> icons.x([attribute.class("h-5 w-5 mx-auto text-gray-400")])
      },
    ]),
    html.td([attribute.class("px-3 py-3 text-center")], [
      case plutonium_has {
        True -> icons.check([attribute.class("h-5 w-5 mx-auto text-[#4641D9]")])
        False -> icons.x([attribute.class("h-5 w-5 mx-auto text-gray-400")])
      },
    ]),
  ])
}

fn features_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.section([attribute.class("bg-white px-6 py-24 md:py-40")], [
    html.div([attribute.class("mx-auto max-w-7xl")], [
      html.h2(
        [
          attribute.class(
            "display mb-16 md:mb-20 text-center text-black text-4xl md:text-5xl lg:text-6xl",
          ),
        ],
        [html.text(g_(i18n_ctx, "What you get with Plutonium"))],
      ),
      html.div(
        [
          attribute.class(
            "grid gap-8 md:gap-10 grid-cols-1 md:grid-cols-2 lg:grid-cols-3",
          ),
        ],
        [
          feature_card(
            ctx,
            "hash",
            g_(i18n_ctx, "Custom username tag"),
            g_(
              i18n_ctx,
              "Choose your own 4-digit tag like #0001, #1337, or #9999 to make your username truly unique.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "user_circle",
            g_(i18n_ctx, "Per-community profiles"),
            g_(
              i18n_ctx,
              "Customize your profile differently for each community you're part of.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "calendar_check",
            g_(i18n_ctx, "Message scheduling"),
            g_(
              i18n_ctx,
              "Schedule messages to be sent at a specific time in the future. No DeLorean required.",
            ),
            Some(g_(i18n_ctx, "Coming soon")),
          ),
          feature_card(
            ctx,
            "gif",
            g_(i18n_ctx, "Animated avatars & banners"),
            g_(
              i18n_ctx,
              "Stand out with animated profile pictures and banners to express your personality.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "smiley",
            g_(i18n_ctx, "Global emoji & sticker access"),
            g_(
              i18n_ctx,
              "Use your favorite emojis and stickers from any community, anywhere across Fluxer.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "video_camera",
            g_(i18n_ctx, "Up to 4K video quality"),
            g_(
              i18n_ctx,
              "Stream in up to 4K resolution at 60fps so others can see you in stunning clarity.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "video",
            g_(i18n_ctx, "Video backgrounds"),
            g_(
              i18n_ctx,
              "Store up to 15 video backgrounds for calls. No more Downloads folder scavenger hunts!",
            ),
            Some(g_(i18n_ctx, "Beta")),
          ),
          feature_card(
            ctx,
            "user_plus",
            g_(i18n_ctx, "Custom entrance sounds"),
            g_(
              i18n_ctx,
              "Set personalized sounds when you join voice channels to make your presence known.",
            ),
            Some(g_(i18n_ctx, "Beta")),
          ),
          feature_card(
            ctx,
            "speaker_high",
            g_(i18n_ctx, "Custom notification sounds"),
            g_(
              i18n_ctx,
              "Upload and use your own notification sounds to personalize your Fluxer experience.",
            ),
            Some(g_(i18n_ctx, "Beta")),
          ),
          feature_card(
            ctx,
            "arrow_up",
            g_(i18n_ctx, "Higher limits everywhere"),
            g_(
              i18n_ctx,
              "500MB uploads, 4000 character messages, 300 bookmarks, 50 emoji packs, and much more.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "fluxer_premium",
            g_(i18n_ctx, "Profile badge"),
            g_(
              i18n_ctx,
              "Show off your Plutonium status with an exclusive badge on your profile.",
            ),
            None,
          ),
          feature_card(
            ctx,
            "rocket",
            g_(i18n_ctx, "Early access"),
            g_(
              i18n_ctx,
              "Be the first to try new features before they're released to everyone else.",
            ),
            None,
          ),
        ],
      ),
    ]),
  ])
}

fn feature_card(
  _ctx: Context,
  icon_name: String,
  title: String,
  description: String,
  badge: option.Option(String),
) -> Element(a) {
  let icon = case icon_name {
    "sparkle" ->
      icons.sparkle([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "video" ->
      icons.video([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "video_camera" ->
      icons.video_camera([
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "user_circle" ->
      icons.user_circle([
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "user_plus" ->
      icons.user_plus([
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "speaker_high" ->
      icons.speaker_high([
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "fluxer_premium" ->
      icons.fluxer_premium(None, [
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "calendar_check" ->
      icons.calendar_check([
        attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
      ])
    "rocket" ->
      icons.rocket([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "hash" ->
      icons.hash([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "gif" ->
      icons.gif([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "arrow_up" ->
      icons.arrow_up([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    "smiley" ->
      icons.smiley([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
    _ ->
      icons.sparkle([attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]")])
  }

  let badge_nodes = case badge {
    Some(label) -> [
      html.div(
        [
          attribute.class(
            "caption absolute top-4 right-4 rounded-full bg-[#4641D9] px-3 py-1 text-[0.65rem] font-semibold uppercase text-white shadow-lg",
          ),
        ],
        [html.text(label)],
      ),
    ]
    None -> []
  }

  let card_children =
    list.append(badge_nodes, [
      html.div(
        [
          attribute.class(
            "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-2xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 mb-5",
          ),
        ],
        [icon],
      ),
      html.h3([attribute.class("title mb-3 text-black text-xl md:text-2xl")], [
        html.text(title),
      ]),
      html.p([attribute.class("body-lg text-gray-700 leading-relaxed")], [
        html.text(description),
      ]),
    ])

  html.div(
    [
      attribute.class(
        "relative rounded-3xl bg-gray-50 p-8 md:p-10 shadow-md border border-gray-100",
      ),
    ],
    card_children,
  )
}

fn visionary_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let visionary_price =
    pricing.get_formatted_price(pricing.Visionary, ctx.country_code)
  let slots = ctx.visionary_slots
  let total_text = number_format.format_number(slots.total)
  let remaining_text = number_format.format_number(slots.remaining)

  let badge_range_text = case slots.total {
    0 -> g_(i18n_ctx, "Numbered badge showing your Visionary status")
    _ ->
      g_(i18n_ctx, "Numbered badge (1-{0}) showing your Visionary status")
      |> string.replace("{0}", total_text)
  }

  let remaining_elements = case slots.total {
    0 -> []
    _ -> [
      html.p(
        [
          attribute.class("text-lg text-white/80 mt-3"),
        ],
        [
          html.text(
            g_(i18n_ctx, "{0} of {1} slots left")
            |> string.replace("{0}", remaining_text)
            |> string.replace("{1}", total_text),
          ),
        ],
      ),
    ]
  }

  html.section(
    [
      attribute.id("visionary"),
      attribute.class(
        "bg-gradient-to-b from-black to-gray-900 px-6 py-24 md:py-40 text-white",
      ),
      attribute.style("scroll-margin-top", "8rem"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("text-center mb-16 md:mb-20")], [
          html.h2([attribute.class("display mb-8 md:mb-10 font-bold")], [
            html.text(g_(i18n_ctx, "Visionary")),
          ]),
          html.p([attribute.class("text-5xl md:text-6xl font-bold mb-4")], [
            html.text(visionary_price),
          ]),
          html.p([attribute.class("lead text-white/90 text-xl md:text-2xl")], [
            html.text(g_(i18n_ctx, "One-time purchase, lifetime access")),
          ]),
          ..remaining_elements
        ]),
        html.div(
          [
            attribute.class(
              "grid gap-8 md:gap-10 grid-cols-1 md:grid-cols-2 mb-16 md:mb-20",
            ),
          ],
          [
            visionary_benefit(
              ctx,
              "infinity",
              g_(i18n_ctx, "Lifetime Plutonium"),
              g_(i18n_ctx, "All current and future Plutonium benefits, forever"),
            ),
            visionary_benefit(
              ctx,
              "globe",
              g_(i18n_ctx, "Lifetime Operator Pass"),
              g_(
                i18n_ctx,
                "Priority support and Operators community access for self-hosting",
              ),
            ),
            visionary_benefit(
              ctx,
              "medal",
              g_(i18n_ctx, "Unique profile badge"),
              badge_range_text,
            ),
            visionary_benefit(
              ctx,
              "chats_circle",
              g_(i18n_ctx, "Exclusive communities"),
              g_(i18n_ctx, "Visionary & Operators communities with team access"),
            ),
          ],
        ),
        html.div([attribute.class("text-center")], [
          html.a(
            [
              attribute.href(ctx.app_endpoint <> "/channels/@me"),
              attribute.class(
                "label inline-block rounded-xl bg-white px-10 py-5 md:px-12 md:py-6 text-lg md:text-xl text-black shadow-lg transition hover:bg-gray-100",
              ),
            ],
            [html.text(g_(i18n_ctx, "Become a Visionary"))],
          ),
          html.p(
            [attribute.class("body-lg mt-6 text-white/80 max-w-3xl mx-auto")],
            [
              html.text(g_(
                i18n_ctx,
                "Purchase Visionary directly in the app after creating your account. Your purchase date and order number will be displayed on your profile (can be hidden in privacy settings).",
              )),
            ],
          ),
        ]),
      ]),
    ],
  )
}

fn visionary_benefit(
  _ctx: Context,
  icon_name: String,
  title: String,
  description: String,
) -> Element(a) {
  let icon = case icon_name {
    "infinity" -> icons.infinity([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    "hash" -> icons.hash([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    "medal" -> icons.medal([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    "globe" -> icons.globe([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    "chats_circle" ->
      icons.chats_circle([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    "flask" -> icons.flask([attribute.class("h-8 w-8 md:h-10 md:w-10")])
    _ -> icons.infinity([attribute.class("h-8 w-8 md:h-10 md:w-10")])
  }

  html.div(
    [
      attribute.class(
        "border-2 border-white/20 rounded-3xl p-8 md:p-10 text-center bg-white/5 backdrop-blur-sm",
      ),
    ],
    [
      html.div(
        [
          attribute.class(
            "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-2xl bg-white/10 backdrop-blur-sm mb-5",
          ),
        ],
        [icon],
      ),
      html.h3([attribute.class("title mb-4 text-xl md:text-2xl")], [
        html.text(title),
      ]),
      html.p([attribute.class("body-lg text-white/80 leading-relaxed")], [
        html.text(description),
      ]),
    ],
  )
}

fn self_hosting_section(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  let free_price = case pricing.get_currency(ctx.country_code) {
    pricing.USD -> "$0"
    pricing.EUR -> "€0"
  }

  let operator_price = case pricing.get_currency(ctx.country_code) {
    pricing.USD -> "$299"
    pricing.EUR -> "€299"
  }

  let visionary_cap_text = case ctx.visionary_slots.total {
    0 -> g_(i18n_ctx, "all")
    total -> number_format.format_number(total)
  }

  let limited_offer_text =
    g_(
      i18n_ctx,
      "Get lifetime Plutonium on Fluxer.app plus an Operator Pass for self-hosting. Pay {0} one-time. Available until October 1, 2026 or when {1} slots are claimed.",
    )
    |> string.replace("{0}", operator_price)
    |> string.replace("{1}", visionary_cap_text)

  html.section(
    [
      attribute.id("self-hosting"),
      attribute.class(
        "bg-gradient-to-b from-gray-50 to-white px-6 py-24 md:py-40",
      ),
      attribute.style("scroll-margin-top", "8rem"),
    ],
    [
      html.div([attribute.class("mx-auto max-w-7xl")], [
        html.div([attribute.class("text-center mb-16 md:mb-20")], [
          html.h2(
            [
              attribute.class(
                "display mb-6 md:mb-8 text-black text-4xl md:text-5xl lg:text-6xl",
              ),
            ],
            [
              html.text(g_(i18n_ctx, "Self-Hosting")),
            ],
          ),
          html.p(
            [
              attribute.class(
                "lead text-gray-700 max-w-3xl mx-auto mb-3 text-xl md:text-2xl",
              ),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "Fluxer is free and open source. Anyone can self-host for free.",
              )),
            ],
          ),
          html.p([attribute.class("body-lg text-gray-600 max-w-3xl mx-auto")], [
            html.text(g_(
              i18n_ctx,
              "The Operator Pass is optional: pay for support and community access.",
            )),
          ]),
        ]),
        html.div(
          [
            attribute.class(
              "grid gap-10 md:gap-12 grid-cols-1 md:grid-cols-2 max-w-5xl mx-auto mb-16 md:mb-20",
            ),
          ],
          [
            html.div(
              [
                attribute.class(
                  "rounded-3xl bg-white border-2 border-gray-200 p-10 md:p-12 shadow-lg",
                ),
              ],
              [
                html.div([attribute.class("mb-4 flex justify-center")], [
                  icons.globe([attribute.class("h-16 w-16 text-gray-400")]),
                ]),
                html.h3(
                  [
                    attribute.class("title text-black mb-2 text-center"),
                  ],
                  [
                    html.text(g_(i18n_ctx, "Free Self-Hosting")),
                  ],
                ),
                html.div([attribute.class("mb-6 text-center")], [
                  html.span([attribute.class("display text-black")], [
                    html.text(free_price),
                  ]),
                  html.span([attribute.class("body-lg text-gray-600")], [
                    html.text(g_(i18n_ctx, "/forever")),
                  ]),
                ]),
                html.div([attribute.class("space-y-3 mb-6")], [
                  benefit_item(ctx, g_(i18n_ctx, "Unlimited users")),
                  benefit_item(ctx, g_(i18n_ctx, "Full access to all features")),
                  benefit_item(ctx, g_(i18n_ctx, "Connect from any client")),
                  benefit_item(ctx, g_(i18n_ctx, "Open source (AGPL-3.0)")),
                  benefit_item(ctx, g_(i18n_ctx, "Community support")),
                ]),
              ],
            ),
            html.div(
              [
                attribute.class(
                  "rounded-3xl bg-white border-2 border-[#4641D9] p-10 md:p-12 shadow-xl",
                ),
              ],
              [
                html.div([attribute.class("mb-4 flex justify-center")], [
                  icons.globe([attribute.class("h-16 w-16 text-[#4641D9]")]),
                ]),
                html.h3(
                  [
                    attribute.class("title text-black mb-2 text-center"),
                  ],
                  [
                    html.text(g_(i18n_ctx, "Operator Pass")),
                  ],
                ),
                html.div([attribute.class("mb-6 text-center")], [
                  html.span([attribute.class("display text-black")], [
                    html.text(operator_price),
                  ]),
                  html.span([attribute.class("body-lg text-gray-600 block")], [
                    html.text(g_(i18n_ctx, "one-time purchase")),
                  ]),
                ]),
                html.div([attribute.class("space-y-3 mb-6")], [
                  benefit_item(ctx, g_(i18n_ctx, "Everything in Free, plus:")),
                  benefit_item(
                    ctx,
                    g_(i18n_ctx, "Access to Fluxer Operators community"),
                  ),
                  benefit_item(ctx, g_(i18n_ctx, "Direct team support")),
                  benefit_item(ctx, g_(i18n_ctx, "Support future development")),
                ]),
              ],
            ),
          ],
        ),
        html.div(
          [
            attribute.class(
              "bg-gradient-to-br from-[#4641D9] to-[#6b5ce7] rounded-3xl p-10 md:p-12 text-white mb-12 md:mb-16 shadow-xl",
            ),
          ],
          [
            html.div([attribute.class("text-center")], [
              html.div([attribute.class("mb-6 flex justify-center")], [
                icons.sparkle([attribute.class("h-16 w-16 md:h-20 md:w-20")]),
              ]),
              html.h3(
                [
                  attribute.class(
                    "display mb-6 md:mb-8 text-3xl md:text-4xl lg:text-5xl font-bold",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "Limited Offer: Visionary includes Operator Pass",
                  )),
                ],
              ),
              html.p(
                [
                  attribute.class(
                    "body-lg text-white/90 mb-6 max-w-3xl mx-auto",
                  ),
                ],
                [html.text(limited_offer_text)],
              ),
              html.p(
                [
                  attribute.class(
                    "body-sm text-white/80 mb-4 max-w-2xl mx-auto",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "After the offer ends, Operator Pass stays available on its own.",
                  )),
                ],
              ),
              html.a(
                [
                  href(ctx, "#visionary"),
                  attribute.class(
                    "label inline-block rounded-xl bg-white px-8 py-4 md:px-10 md:py-5 text-lg md:text-xl text-[#4641D9] transition hover:bg-gray-100 shadow-lg",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Get Visionary + Operator Pass"))],
              ),
            ]),
          ],
        ),
      ]),
    ],
  )
}

fn benefit_item(_ctx: Context, text: String) -> Element(a) {
  html.div([attribute.class("flex items-start gap-3")], [
    icons.check([attribute.class("h-5 w-5 shrink-0 mt-0.5 text-[#4641D9]")]),
    html.span([attribute.class("body text-gray-700")], [html.text(text)]),
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
                "mx-auto max-w-5xl px-6 py-24 md:py-32 lg:py-40 text-center",
              ),
            ],
            [
              html.h2(
                [
                  attribute.class(
                    "mb-8 md:mb-10 text-white text-6xl md:text-8xl lg:text-9xl xl:text-[10rem] font-bold leading-none tracking-tight",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Ready to upgrade?"))],
              ),
              html.p(
                [
                  attribute.class(
                    "lead mb-12 md:mb-14 text-white/90 text-xl md:text-2xl max-w-4xl mx-auto",
                  ),
                ],
                [
                  html.text(g_(
                    i18n_ctx,
                    "Create your Fluxer account to get that sweet Plutonium while it's hot.",
                  )),
                ],
              ),
              html.a(
                [
                  attribute.href(ctx.app_endpoint <> "/register"),
                  attribute.class(
                    "label inline-block rounded-xl bg-white px-10 py-5 md:px-12 md:py-6 text-lg md:text-xl text-[#4641D9] transition-colors hover:bg-opacity-90 shadow-lg",
                  ),
                ],
                [html.text(g_(i18n_ctx, "Join Fluxer"))],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}
