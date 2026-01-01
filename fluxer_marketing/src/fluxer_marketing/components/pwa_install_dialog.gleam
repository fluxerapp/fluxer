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
import fluxer_marketing/web.{type Context}
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

pub fn render_trigger(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.button(
    [
      attribute.id("pwa-install-button"),
      attribute.class(
        "inline-flex items-center gap-2 rounded-xl px-5 py-3 transition-colors bg-white/10 hover:bg-white/20 text-white font-medium text-sm",
      ),
    ],
    [
      icons.devices([attribute.class("h-5 w-5")]),
      html.text(g_(i18n_ctx, "How to install as an app")),
    ],
  )
}

pub fn render_modal(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div(
    [
      attribute.id("pwa-modal-backdrop"),
      attribute.class("pwa-modal-backdrop"),
    ],
    [
      html.div([attribute.class("pwa-modal")], [
        html.div([attribute.class("flex flex-col h-full")], [
          html.div(
            [attribute.class("flex items-center justify-between p-6 pb-4")],
            [
              html.h2([attribute.class("text-xl font-bold text-gray-900")], [
                html.text(g_(i18n_ctx, "Install Fluxer as an app")),
              ]),
              html.button(
                [
                  attribute.class(
                    "p-2 hover:bg-gray-100 rounded-lg text-gray-600 hover:text-gray-900",
                  ),
                  attribute.id("pwa-close"),
                  attribute.attribute("aria-label", "Close"),
                ],
                [
                  svg.svg(
                    [
                      attribute.class("w-5 h-5"),
                      attribute.attribute("fill", "none"),
                      attribute.attribute("stroke", "currentColor"),
                      attribute.attribute("viewBox", "0 0 24 24"),
                    ],
                    [
                      svg.path([
                        attribute.attribute("stroke-linecap", "round"),
                        attribute.attribute("stroke-linejoin", "round"),
                        attribute.attribute("stroke-width", "2"),
                        attribute.attribute("d", "M6 18L18 6M6 6l12 12"),
                      ]),
                    ],
                  ),
                ],
              ),
            ],
          ),
          html.div([attribute.class("px-6")], [
            html.div(
              [
                attribute.class("flex gap-1 p-1 bg-gray-100 rounded-xl"),
                attribute.id("pwa-tabs"),
              ],
              [
                tab_button("android", g_(i18n_ctx, "Android"), True),
                tab_button("ios", g_(i18n_ctx, "iOS / iPadOS"), False),
                tab_button("desktop", g_(i18n_ctx, "Desktop"), False),
              ],
            ),
          ]),
          html.div([attribute.class("flex-1 overflow-y-auto p-6 pt-4")], [
            html.div(
              [
                attribute.id("pwa-panel-android"),
                attribute.class("pwa-panel"),
              ],
              [render_android_steps(ctx)],
            ),
            html.div(
              [
                attribute.id("pwa-panel-ios"),
                attribute.class("pwa-panel hidden"),
              ],
              [render_ios_steps(ctx)],
            ),
            html.div(
              [
                attribute.id("pwa-panel-desktop"),
                attribute.class("pwa-panel hidden"),
              ],
              [render_desktop_steps(ctx)],
            ),
          ]),
          html.div(
            [
              attribute.class("px-6 py-4 border-t border-gray-100 text-center"),
            ],
            [
              html.p([attribute.class("text-xs text-gray-400")], [
                html.text(g_(i18n_ctx, "Screenshots courtesy of ")),
                html.a(
                  [
                    attribute.href("https://installpwa.com/"),
                    attribute.target("_blank"),
                    attribute.rel("noopener noreferrer"),
                    attribute.class(
                      "text-blue-500 hover:text-blue-600 underline",
                    ),
                  ],
                  [html.text("installpwa.com")],
                ),
              ]),
            ],
          ),
        ]),
      ]),
    ],
  )
}

fn tab_button(id: String, label: String, active: Bool) -> Element(a) {
  html.button(
    [
      attribute.attribute("data-tab", id),
      attribute.class(
        "pwa-tab flex-1 px-4 py-2 text-sm font-medium rounded-lg transition-colors "
        <> case active {
          True -> "bg-white text-gray-900 shadow-sm"
          False -> "text-gray-600 hover:text-gray-900"
        },
      ),
    ],
    [html.text(label)],
  )
}

fn render_android_steps(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div([attribute.class("flex flex-col md:flex-row gap-6")], [
    html.div([attribute.class("md:w-1/3 flex justify-center")], [
      render_image(ctx, "android", "240", "320", "480"),
    ]),
    html.div([attribute.class("md:w-2/3")], [
      html.ol([attribute.class("space-y-4")], [
        step_item(
          "1",
          html.span([], [
            html.a(
              [
                attribute.href("https://web.fluxer.app"),
                attribute.target("_blank"),
                attribute.rel("noopener noreferrer"),
                attribute.class("text-blue-600 hover:text-blue-700 underline"),
              ],
              [html.text(g_(i18n_ctx, "Open the web app"))],
            ),
            html.text(g_(i18n_ctx, " in Chrome")),
          ]),
        ),
        step_item(
          "2",
          html.text(g_(
            i18n_ctx,
            "Press the \"More\" (\u{22EE}) button in the top-right corner",
          )),
        ),
        step_item("3", html.text(g_(i18n_ctx, "Press \"Install app\""))),
        step_item(
          "4",
          html.span([attribute.class("text-green-600 font-medium")], [
            html.text(g_(
              i18n_ctx,
              "Done! You can open Fluxer from your home screen.",
            )),
          ]),
        ),
      ]),
    ]),
  ])
}

fn render_ios_steps(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div([attribute.class("flex flex-col md:flex-row gap-6")], [
    html.div([attribute.class("md:w-1/2 flex justify-center")], [
      render_image(ctx, "ios", "320", "480", "640"),
    ]),
    html.div([attribute.class("md:w-1/2")], [
      html.ol([attribute.class("space-y-4")], [
        step_item(
          "1",
          html.span([], [
            html.a(
              [
                attribute.href("https://web.fluxer.app"),
                attribute.target("_blank"),
                attribute.rel("noopener noreferrer"),
                attribute.class("text-blue-600 hover:text-blue-700 underline"),
              ],
              [html.text(g_(i18n_ctx, "Open the web app"))],
            ),
            html.text(g_(i18n_ctx, " in Safari")),
          ]),
        ),
        step_item(
          "2",
          html.text(g_(
            i18n_ctx,
            "Press the Share button (rectangle with upwards-pointing arrow)",
          )),
        ),
        step_item("3", html.text(g_(i18n_ctx, "Press \"Add to Home Screen\""))),
        step_item(
          "4",
          html.text(g_(i18n_ctx, "Press \"Add\" in the upper-right corner")),
        ),
        step_item(
          "5",
          html.span([attribute.class("text-green-600 font-medium")], [
            html.text(g_(
              i18n_ctx,
              "Done! You can open Fluxer from your home screen.",
            )),
          ]),
        ),
      ]),
    ]),
  ])
}

fn render_desktop_steps(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.div([attribute.class("flex flex-col md:flex-row gap-6")], [
    html.div([attribute.class("md:w-1/2 flex justify-center")], [
      render_image(ctx, "desktop", "320", "480", "640"),
    ]),
    html.div([attribute.class("md:w-1/2")], [
      html.ol([attribute.class("space-y-4")], [
        step_item(
          "1",
          html.span([], [
            html.a(
              [
                attribute.href("https://web.fluxer.app"),
                attribute.target("_blank"),
                attribute.rel("noopener noreferrer"),
                attribute.class("text-blue-600 hover:text-blue-700 underline"),
              ],
              [html.text(g_(i18n_ctx, "Open the web app"))],
            ),
            html.text(g_(
              i18n_ctx,
              " in Chrome or another browser with PWA support",
            )),
          ]),
        ),
        step_item(
          "2",
          html.text(g_(
            i18n_ctx,
            "Press the install button (downwards-pointing arrow on monitor) in the address bar",
          )),
        ),
        step_item(
          "3",
          html.text(g_(i18n_ctx, "Press \"Install\" in the popup that appears")),
        ),
        step_item(
          "4",
          html.span([attribute.class("text-green-600 font-medium")], [
            html.text(g_(
              i18n_ctx,
              "Done! You can now open Fluxer as if it were a regular program.",
            )),
          ]),
        ),
      ]),
    ]),
  ])
}

fn step_item(number: String, content: Element(a)) -> Element(a) {
  html.li([attribute.class("flex gap-4")], [
    html.div(
      [
        attribute.class(
          "flex-shrink-0 w-8 h-8 rounded-full bg-blue-100 text-blue-600 flex items-center justify-center font-bold text-sm",
        ),
      ],
      [html.text(number)],
    ),
    html.div([attribute.class("pt-1 text-gray-700")], [content]),
  ])
}

fn render_image(
  ctx: Context,
  name: String,
  small: String,
  medium: String,
  large: String,
) -> Element(a) {
  let base_path = ctx.cdn_endpoint <> "/marketing/pwa-install/" <> name

  element.element("picture", [], [
    element.element(
      "source",
      [
        attribute.type_("image/avif"),
        attribute.attribute(
          "srcset",
          base_path
            <> "-"
            <> small
            <> "w.avif 1x, "
            <> base_path
            <> "-"
            <> medium
            <> "w.avif 1.5x, "
            <> base_path
            <> "-"
            <> large
            <> "w.avif 2x",
        ),
      ],
      [],
    ),
    element.element(
      "source",
      [
        attribute.type_("image/webp"),
        attribute.attribute(
          "srcset",
          base_path
            <> "-"
            <> small
            <> "w.webp 1x, "
            <> base_path
            <> "-"
            <> medium
            <> "w.webp 1.5x, "
            <> base_path
            <> "-"
            <> large
            <> "w.webp 2x",
        ),
      ],
      [],
    ),
    html.img([
      attribute.src(base_path <> "-" <> medium <> "w.png"),
      attribute.attribute(
        "srcset",
        base_path
          <> "-"
          <> small
          <> "w.png 1x, "
          <> base_path
          <> "-"
          <> medium
          <> "w.png 1.5x, "
          <> base_path
          <> "-"
          <> large
          <> "w.png 2x",
      ),
      attribute.alt("PWA installation guide for " <> name),
      attribute.class(
        "max-w-full h-auto rounded-lg shadow-lg border border-gray-200",
      ),
    ]),
  ])
}
