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

import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type HeroConfig(a) {
  HeroConfig(
    icon: Element(a),
    title: String,
    description: String,
    extra_content: Element(a),
    custom_padding: String,
  )
}

pub fn default_padding() -> String {
  "px-6 pt-48 md:pt-60 pb-16 md:pb-20 lg:pb-24 text-white"
}

pub fn render(config: HeroConfig(a)) -> Element(a) {
  html.section([attribute.class(config.custom_padding)], [
    html.div([attribute.class("mx-auto max-w-5xl text-center")], [
      html.div([attribute.class("mb-8 flex justify-center")], [
        html.div(
          [
            attribute.class(
              "inline-flex items-center justify-center w-28 h-28 md:w-36 md:h-36 rounded-3xl bg-white/10 backdrop-blur-sm",
            ),
          ],
          [config.icon],
        ),
      ]),
      html.h1(
        [
          attribute.class(
            "hero mb-8 md:mb-10 text-5xl md:text-6xl lg:text-7xl font-bold",
          ),
        ],
        [html.text(config.title)],
      ),
      html.p(
        [
          attribute.class(
            "lead text-white/90 text-xl md:text-2xl max-w-4xl mx-auto",
          ),
        ],
        [html.text(config.description)],
      ),
      config.extra_content,
    ]),
  ])
}
