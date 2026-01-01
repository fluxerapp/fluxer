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
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(
  _ctx: Context,
  icon: String,
  title: String,
  description: String,
  features: List(String),
  theme: String,
) -> Element(a) {
  let card_bg = case theme {
    "light" -> "bg-white"
    "dark" -> "bg-white/95 backdrop-blur-sm"
    _ -> "bg-white/95 backdrop-blur-sm"
  }

  let text_color = case theme {
    "light" -> "text-gray-900"
    "dark" -> "text-gray-900"
    _ -> "text-gray-900"
  }

  let description_color = case theme {
    "light" -> "text-gray-600"
    "dark" -> "text-gray-700"
    _ -> "text-gray-700"
  }

  html.div(
    [
      attribute.class(
        "flex h-full flex-col rounded-2xl "
        <> card_bg
        <> " p-8 md:p-10 shadow-md border border-gray-100",
      ),
    ],
    [
      html.div([attribute.class("mb-6")], [
        html.div(
          [
            attribute.class(
              "inline-flex items-center justify-center w-16 h-16 md:w-20 md:h-20 rounded-2xl bg-gradient-to-br from-[#4641D9]/10 to-[#4641D9]/5 mb-5",
            ),
          ],
          [
            case icon {
              "chats" ->
                icons.chats([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "microphone" ->
                icons.microphone([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "palette" ->
                icons.palette([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "magnifying-glass" ->
                icons.magnifying_glass([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "devices" ->
                icons.devices([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "gear" ->
                icons.gear([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "heart" ->
                icons.heart([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "lightning" ->
                icons.lightning([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "globe" ->
                icons.globe([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "server" ->
                icons.globe([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "shopping-cart" ->
                icons.shopping_cart([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "newspaper" ->
                icons.newspaper([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              "brain" ->
                icons.brain([
                  attribute.class("h-8 w-8 md:h-10 md:w-10 text-[#4641D9]"),
                ])
              _ -> html.div([], [])
            },
          ],
        ),
        html.h3([attribute.class("title " <> text_color <> " mb-3")], [
          html.text(title),
        ]),
        html.p([attribute.class("body-lg " <> description_color)], [
          html.text(description),
        ]),
      ]),
      html.div([attribute.class("flex-1 mt-2")], [
        html.ul(
          [attribute.class("space-y-3")],
          features
            |> list.map(fn(feature) {
              html.li([attribute.class("flex items-start gap-3")], [
                html.span(
                  [
                    attribute.class(
                      "mt-[.7em] h-1.5 w-1.5 rounded-full bg-[#4641D9] shrink-0",
                    ),
                  ],
                  [],
                ),
                html.span([attribute.class("body-lg " <> text_color)], [
                  html.text(feature),
                ]),
              ])
            }),
        ),
      ]),
    ],
  )
}
