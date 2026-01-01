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

import fluxer_marketing/flags
import fluxer_marketing/i18n
import fluxer_marketing/icons
import fluxer_marketing/locale
import fluxer_marketing/web.{type Context, href}
import gleam/list
import kielet.{gettext as g_}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

pub fn render_trigger(ctx: Context) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)

  html.a(
    [
      attribute.class(
        "locale-toggle flex items-center justify-center p-2 rounded-lg hover:bg-gray-100 transition-colors",
      ),
      attribute.attribute("aria-label", g_(i18n_ctx, "Change language")),
      attribute.id("locale-button"),
      href(ctx, "#locale-modal-backdrop"),
    ],
    [icons.translate([attribute.class("h-5 w-5")])],
  )
}

pub fn render_modal(ctx: Context, current_path: String) -> Element(a) {
  let i18n_ctx = i18n.get_context(ctx.i18n_db, ctx.locale)
  let current_locale = ctx.locale
  let all_locales = locale.all_locales()

  html.div(
    [
      attribute.id("locale-modal-backdrop"),
      attribute.class("locale-modal-backdrop"),
    ],
    [
      html.div([attribute.class("locale-modal")], [
        html.div([attribute.class("flex flex-col h-full")], [
          html.div(
            [attribute.class("flex items-center justify-between p-6 pb-0")],
            [
              html.h2([attribute.class("text-xl font-bold text-gray-900")], [
                html.text(g_(i18n_ctx, "Choose your language")),
              ]),
              html.a(
                [
                  attribute.class(
                    "p-2 hover:bg-gray-100 rounded-lg text-gray-600 hover:text-gray-900",
                  ),
                  attribute.id("locale-close"),
                  attribute.attribute("aria-label", "Close"),
                  href(ctx, "#"),
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
          html.p(
            [
              attribute.class("px-6 pb-2 text-xs text-gray-500 leading-relaxed"),
            ],
            [
              html.text(g_(
                i18n_ctx,
                "All translations are currently LLM-generated with minimal human revision. We'd love to get real people to help us out localizing Fluxer into your language! To do so, shoot an email to i18n@fluxer.app and we'll be happy to accept your contributions.",
              )),
            ],
          ),
          html.div([attribute.class("flex-1 overflow-y-auto p-6 pt-4")], [
            html.div(
              [
                attribute.class(
                  "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-5 gap-3",
                ),
              ],
              list.map(all_locales, fn(loc) {
                let is_current = loc == current_locale
                let locale_code = locale.get_code_from_locale(loc)
                let native_name = locale.get_locale_name(loc)
                let localized_name = case loc {
                  locale.Ar -> g_(i18n_ctx, "Arabic")
                  locale.Bg -> g_(i18n_ctx, "Bulgarian")
                  locale.Cs -> g_(i18n_ctx, "Czech")
                  locale.Da -> g_(i18n_ctx, "Danish")
                  locale.De -> g_(i18n_ctx, "German")
                  locale.El -> g_(i18n_ctx, "Greek")
                  locale.EnGB -> g_(i18n_ctx, "English")
                  locale.EnUS -> g_(i18n_ctx, "English (US)")
                  locale.EsES -> g_(i18n_ctx, "Spanish (Spain)")
                  locale.Es419 -> g_(i18n_ctx, "Spanish (Latin America)")
                  locale.Fi -> g_(i18n_ctx, "Finnish")
                  locale.Fr -> g_(i18n_ctx, "French")
                  locale.He -> g_(i18n_ctx, "Hebrew")
                  locale.Hi -> g_(i18n_ctx, "Hindi")
                  locale.Hr -> g_(i18n_ctx, "Croatian")
                  locale.Hu -> g_(i18n_ctx, "Hungarian")
                  locale.Id -> g_(i18n_ctx, "Indonesian")
                  locale.It -> g_(i18n_ctx, "Italian")
                  locale.Ja -> g_(i18n_ctx, "Japanese")
                  locale.Ko -> g_(i18n_ctx, "Korean")
                  locale.Lt -> g_(i18n_ctx, "Lithuanian")
                  locale.Nl -> g_(i18n_ctx, "Dutch")
                  locale.No -> g_(i18n_ctx, "Norwegian")
                  locale.Pl -> g_(i18n_ctx, "Polish")
                  locale.PtBR -> g_(i18n_ctx, "Portuguese (Brazil)")
                  locale.Ro -> g_(i18n_ctx, "Romanian")
                  locale.Ru -> g_(i18n_ctx, "Russian")
                  locale.SvSE -> g_(i18n_ctx, "Swedish")
                  locale.Th -> g_(i18n_ctx, "Thai")
                  locale.Tr -> g_(i18n_ctx, "Turkish")
                  locale.Uk -> g_(i18n_ctx, "Ukrainian")
                  locale.Vi -> g_(i18n_ctx, "Vietnamese")
                  locale.ZhCN -> g_(i18n_ctx, "Chinese (Simplified)")
                  locale.ZhTW -> g_(i18n_ctx, "Chinese (Traditional)")
                }

                html.form(
                  [
                    attribute.action(web.prepend_base_path(ctx, "/_locale")),
                    attribute.method("POST"),
                    attribute.class("contents locale-form"),
                  ],
                  [
                    html.input([
                      attribute.type_("hidden"),
                      attribute.name("locale"),
                      attribute.value(locale_code),
                    ]),
                    html.input([
                      attribute.type_("hidden"),
                      attribute.name("redirect"),
                      attribute.value(current_path),
                    ]),
                    html.button(
                      [
                        attribute.type_("submit"),
                        attribute.class(
                          "relative flex flex-col items-center gap-3 p-4 rounded-xl border-2 hover:bg-gray-50 transition-colors text-center min-h-[120px] justify-center "
                          <> case is_current {
                            True -> "border-blue-500 bg-blue-50"
                            False -> "border-gray-200"
                          },
                        ),
                      ],
                      [
                        case is_current {
                          True ->
                            html.div(
                              [
                                attribute.class(
                                  "absolute top-2 right-2 w-6 h-6 bg-blue-600 rounded-full flex items-center justify-center",
                                ),
                              ],
                              [
                                svg.svg(
                                  [
                                    attribute.class("w-4 h-4 text-white"),
                                    attribute.attribute("fill", "none"),
                                    attribute.attribute(
                                      "stroke",
                                      "currentColor",
                                    ),
                                    attribute.attribute("viewBox", "0 0 24 24"),
                                    attribute.attribute("stroke-width", "2"),
                                  ],
                                  [
                                    svg.path([
                                      attribute.attribute(
                                        "stroke-linecap",
                                        "round",
                                      ),
                                      attribute.attribute(
                                        "stroke-linejoin",
                                        "round",
                                      ),
                                      attribute.attribute("d", "M5 13l4 4L19 7"),
                                    ]),
                                  ],
                                ),
                              ],
                            )
                          False -> html.text("")
                        },
                        flags.flag_svg(loc, ctx, [
                          attribute.class("w-8 h-8 rounded"),
                        ]),
                        html.div(
                          [
                            attribute.class(
                              "font-semibold text-gray-900 text-sm",
                            ),
                          ],
                          [html.text(native_name)],
                        ),
                        html.div([attribute.class("text-xs text-gray-500")], [
                          html.text(localized_name),
                        ]),
                      ],
                    ),
                  ],
                )
              }),
            ),
          ]),
        ]),
      ]),
    ],
  )
}

pub fn render(ctx: Context, current_path: String) -> Element(a) {
  html.div([], [
    render_trigger(ctx),
    render_modal(ctx, current_path),
  ])
}
