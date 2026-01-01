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

import fluxer_marketing/icons
import fluxer_marketing/web.{type Context}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(
  _ctx: Context,
  icon: String,
  title: String,
  description: String,
  button_text: String,
  button_href: String,
) -> Element(a) {
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
            case icon {
              "rocket-launch" ->
                icons.rocket_launch([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "fluxer-partner" ->
                icons.fluxer_partner([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "chat-centered-text" ->
                icons.chat_centered_text([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "bluesky" ->
                icons.bluesky([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "bug" ->
                icons.bug([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "code" ->
                icons.code_icon([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "translate" ->
                icons.translate([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              "shield-check" ->
                icons.shield_check([
                  attribute.class("h-12 w-12 md:h-14 md:w-14 text-white"),
                ])
              _ -> html.div([], [])
            },
          ],
        ),
        html.h3(
          [attribute.class("title text-gray-900 mb-4 text-xl md:text-2xl")],
          [
            html.text(title),
          ],
        ),
        html.p([attribute.class("body-lg text-gray-700 leading-relaxed")], [
          html.text(description),
        ]),
      ]),
      html.div([attribute.class("mt-auto flex flex-col items-center")], [
        html.a(
          case button_href {
            "https://" <> _ | "http://" <> _ | "mailto:" <> _ -> [
              attribute.href(button_href),
              attribute.class(
                "label inline-block rounded-xl bg-[#4641D9] px-8 py-4 text-base md:text-lg text-white transition-colors hover:bg-opacity-90 shadow-md w-full text-center",
              ),
              attribute.target("_blank"),
              attribute.rel("noopener noreferrer"),
            ]
            _ -> [
              attribute.href(button_href),
              attribute.class(
                "label inline-block rounded-xl bg-[#4641D9] px-8 py-4 text-base md:text-lg text-white transition-colors hover:bg-opacity-90 shadow-md w-full text-center",
              ),
            ]
          },
          [html.text(button_text)],
        ),
      ]),
    ],
  )
}
