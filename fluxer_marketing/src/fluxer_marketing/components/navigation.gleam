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

import fluxer_marketing/components/locale_selector
import fluxer_marketing/components/platform_download_button
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/web.{type Context, href}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import wisp.{type Request}

pub fn render(ctx: Context, _req: Request) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let #(drawer_download_url, drawer_download_label, drawer_download_icon) =
    platform_download_button.get_platform_download_info(ctx)

  html.nav(
    [
      attribute.id("navbar"),
      attribute.class("fixed left-0 right-0 z-40"),
      attribute.style("top", "var(--banner-height, 60px)"),
    ],
    [
      html.input([
        attribute.type_("checkbox"),
        attribute.id("nav-toggle"),
        attribute.class("hidden peer"),
      ]),
      html.div([attribute.class("px-4 py-6 md:px-8 md:py-8")], [
        html.div(
          [
            attribute.class(
              "mx-auto max-w-7xl rounded-2xl bg-white/95 backdrop-blur-lg shadow-lg border border-gray-200/60 px-4 py-3 md:px-6 md:py-3",
            ),
          ],
          [
            html.div([attribute.class("flex items-center justify-between")], [
              html.div([attribute.class("flex items-center gap-8 xl:gap-12")], [
                html.a(
                  [
                    href(ctx, "/"),
                    attribute.class(
                      "flex items-center transition-opacity hover:opacity-80 relative z-10",
                    ),
                    attribute.attribute("aria-label", "Fluxer home"),
                  ],
                  [
                    icons.fluxer_logo_wordmark([
                      attribute.class("h-8 md:h-9 text-[#4641D9]"),
                    ]),
                  ],
                ),
                html.div(
                  [
                    attribute.class(
                      "hidden lg:flex items-center gap-6 xl:gap-8",
                    ),
                  ],
                  [
                    html.a(
                      [
                        href(ctx, "/download"),
                        attribute.class(
                          "body-lg text-gray-900/90 font-semibold hover:text-gray-900 transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Download"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/plutonium"),
                        attribute.class(
                          "body-lg text-gray-900/90 font-semibold hover:text-gray-900 transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Pricing"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/help"),
                        attribute.class(
                          "body-lg text-gray-900/90 font-semibold hover:text-gray-900 transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Help"))],
                    ),
                    html.a(
                      [
                        attribute.href("https://docs.fluxer.app"),
                        attribute.class(
                          "body-lg text-gray-900/90 font-semibold hover:text-gray-900 transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Docs"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/donate"),
                        attribute.class(
                          "body-lg text-gray-900/90 font-semibold hover:text-gray-900 transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Donate"))],
                    ),
                  ],
                ),
              ]),
              html.div([attribute.class("flex items-center gap-3")], [
                html.a(
                  [
                    attribute.href("https://bsky.app/profile/fluxer.app"),
                    attribute.class(
                      "hidden lg:flex items-center p-2 rounded-lg text-[#4641D9] hover:text-[#3d38c7] hover:bg-gray-100 transition-colors",
                    ),
                    attribute.target("_blank"),
                    attribute.rel("noopener noreferrer"),
                    attribute.attribute("aria-label", "Bluesky"),
                  ],
                  [icons.bluesky([attribute.class("h-5 w-5")])],
                ),
                html.a(
                  [
                    attribute.href("https://github.com/fluxerapp/fluxer"),
                    attribute.class(
                      "hidden lg:flex items-center p-2 rounded-lg text-[#4641D9] hover:text-[#3d38c7] hover:bg-gray-100 transition-colors",
                    ),
                    attribute.target("_blank"),
                    attribute.rel("noopener noreferrer"),
                    attribute.attribute("aria-label", "GitHub"),
                  ],
                  [icons.github([attribute.class("h-5 w-5")])],
                ),
                html.a(
                  [
                    attribute.href("https://bsky.app/profile/fluxer.app/rss"),
                    attribute.class(
                      "hidden lg:flex items-center p-2 rounded-lg text-[#4641D9] hover:text-[#3d38c7] hover:bg-gray-100 transition-colors",
                    ),
                    attribute.target("_blank"),
                    attribute.rel("noopener noreferrer"),
                    attribute.attribute("aria-label", "RSS Feed"),
                  ],
                  [icons.rss([attribute.class("h-5 w-5")])],
                ),
                html.div([attribute.class("hidden lg:block text-[#4641D9]")], [
                  locale_selector.render_trigger(ctx),
                ]),
                html.a(
                  [
                    attribute.href(ctx.app_endpoint <> "/channels/@me"),
                    attribute.class(
                      "hidden lg:inline-flex whitespace-nowrap rounded-xl bg-[#4641D9] px-5 py-2.5 text-base font-semibold text-white transition-colors hover:bg-opacity-90 shadow-md",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Open Fluxer"))],
                ),
                html.label(
                  [
                    attribute.for("nav-toggle"),
                    attribute.class(
                      "lg:hidden flex items-center justify-center p-2 hover:bg-gray-100 rounded-lg transition-colors cursor-pointer relative z-10",
                    ),
                  ],
                  [
                    icons.menu([
                      attribute.class(
                        "h-6 w-6 text-gray-900 peer-checked:hidden",
                      ),
                    ]),
                  ],
                ),
              ]),
            ]),
          ],
        ),
      ]),
      html.div(
        [
          attribute.class(
            "fixed inset-0 z-50 bg-black/50 backdrop-blur-sm opacity-0 pointer-events-none peer-checked:opacity-100 peer-checked:pointer-events-auto transition-opacity lg:hidden",
          ),
        ],
        [
          html.label(
            [attribute.for("nav-toggle"), attribute.class("absolute inset-0")],
            [],
          ),
        ],
      ),
      html.div(
        [
          attribute.class(
            "fixed top-0 right-0 bottom-0 z-50 w-full sm:w-[420px] sm:max-w-[90vw] bg-white rounded-none sm:rounded-l-3xl shadow-2xl transform translate-x-full peer-checked:translate-x-0 transition-transform lg:hidden overflow-y-auto",
          ),
        ],
        [
          html.div([attribute.class("flex h-full flex-col p-6")], [
            html.div(
              [attribute.class("mb-4 flex items-center justify-between")],
              [
                html.a(
                  [
                    href(ctx, "/"),
                    attribute.class(
                      "flex items-center gap-3 rounded-xl px-2 py-1 hover:bg-gray-50 transition-colors",
                    ),
                    attribute.attribute("aria-label", "Fluxer home"),
                  ],
                  [
                    icons.fluxer_logo_wordmark([
                      attribute.class("h-7 text-[#4641D9]"),
                    ]),
                  ],
                ),
                html.label(
                  [
                    attribute.for("nav-toggle"),
                    attribute.class(
                      "p-2 hover:bg-gray-100 rounded-lg transition-colors cursor-pointer",
                    ),
                  ],
                  [icons.x([attribute.class("h-6 w-6 text-gray-900")])],
                ),
              ],
            ),
            html.div([attribute.class("mb-4 text-gray-900")], [
              html.a(
                [
                  href(ctx, "#locale-modal-backdrop"),
                  attribute.class(
                    "flex items-center gap-2 rounded-lg px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 transition-colors",
                  ),
                  attribute.attribute(
                    "aria-label",
                    g_(i18n_ctx, "Change language"),
                  ),
                ],
                [
                  icons.translate([attribute.class("h-5 w-5")]),
                  html.span([], [html.text(g_(i18n_ctx, "Change language"))]),
                ],
              ),
            ]),
            html.div([attribute.class("flex-1 overflow-y-auto")], [
              html.div([attribute.class("flex flex-col gap-4 py-2")], [
                html.div([], [
                  html.p(
                    [
                      attribute.class(
                        "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Product"))],
                  ),
                  html.div([attribute.class("flex flex-col gap-1")], [
                    html.a(
                      [
                        href(ctx, "/download"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Download"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/plutonium"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Pricing"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/partners"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Partners"))],
                    ),
                  ]),
                ]),
                html.div([], [
                  html.p(
                    [
                      attribute.class(
                        "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Resources"))],
                  ),
                  html.div([attribute.class("flex flex-col gap-1")], [
                    html.a(
                      [
                        attribute.href("https://docs.fluxer.app"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Docs"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/help"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Help Center"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/press"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Press"))],
                    ),
                  ]),
                ]),
                html.div([], [
                  html.p(
                    [
                      attribute.class(
                        "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Company"))],
                  ),
                  html.div([attribute.class("flex flex-col gap-1")], [
                    html.a(
                      [
                        href(ctx, "/careers"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Careers"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/donate"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Donate"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/company-information"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Company Information"))],
                    ),
                  ]),
                ]),
                html.div([], [
                  html.p(
                    [
                      attribute.class(
                        "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Policies"))],
                  ),
                  html.div([attribute.class("flex flex-col gap-1")], [
                    html.a(
                      [
                        href(ctx, "/terms"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Terms of Service"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/privacy"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Privacy Policy"))],
                    ),
                    html.a(
                      [
                        href(ctx, "/guidelines"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text(g_(i18n_ctx, "Community Guidelines"))],
                    ),
                  ]),
                ]),
                html.div([], [
                  html.p(
                    [
                      attribute.class(
                        "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                      ),
                    ],
                    [html.text(g_(i18n_ctx, "Connect"))],
                  ),
                  html.div([attribute.class("flex flex-col gap-1")], [
                    html.a(
                      [
                        attribute.href("https://github.com/fluxerapp/fluxer"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                        attribute.target("_blank"),
                        attribute.rel("noopener noreferrer"),
                      ],
                      [html.text(g_(i18n_ctx, "Source Code"))],
                    ),
                    html.a(
                      [
                        attribute.href("https://bsky.app/profile/fluxer.app"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                        attribute.target("_blank"),
                        attribute.rel("noopener noreferrer"),
                      ],
                      [html.text(g_(i18n_ctx, "Bluesky"))],
                    ),
                    html.a(
                      [
                        attribute.href("mailto:support@fluxer.app"),
                        attribute.class(
                          "px-2 py-2 text-base font-semibold text-gray-900 hover:bg-gray-100 rounded-lg transition-colors",
                        ),
                      ],
                      [html.text("support@fluxer.app")],
                    ),
                  ]),
                ]),
              ]),
            ]),
            html.div([attribute.class("mt-6")], [
              html.div([attribute.class("flex flex-col gap-3")], [
                html.a(
                  [
                    attribute.href(ctx.app_endpoint <> "/channels/@me"),
                    attribute.class(
                      "flex items-center justify-center rounded-xl border border-gray-300 px-5 py-3 text-base font-semibold text-gray-900 transition-colors hover:bg-gray-50",
                    ),
                  ],
                  [html.text(g_(i18n_ctx, "Open in Browser"))],
                ),
                html.a(
                  [
                    attribute.href(drawer_download_url),
                    attribute.class(
                      "flex items-center justify-center gap-2 rounded-xl bg-[#4641D9] px-5 py-3 text-base font-semibold text-white transition-colors hover:bg-opacity-90 shadow-md",
                    ),
                  ],
                  [
                    drawer_download_icon,
                    html.span([], [html.text(drawer_download_label)]),
                  ],
                ),
              ]),
            ]),
          ]),
        ],
      ),
    ],
  )
}
