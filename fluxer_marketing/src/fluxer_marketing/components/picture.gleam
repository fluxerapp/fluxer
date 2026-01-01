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

import gleam/int
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(
  src: String,
  alt: String,
  class: String,
  sizes: String,
  width_1x: Int,
  width_2x: Int,
) -> Element(a) {
  render_with_priority(src, alt, class, sizes, width_1x, width_2x, False)
}

pub fn render_priority(
  src: String,
  alt: String,
  class: String,
  sizes: String,
  width_1x: Int,
  width_2x: Int,
) -> Element(a) {
  render_with_priority(src, alt, class, sizes, width_1x, width_2x, True)
}

fn render_with_priority(
  src: String,
  alt: String,
  class: String,
  sizes: String,
  width_1x: Int,
  width_2x: Int,
  high_priority: Bool,
) -> Element(a) {
  let base_path = case string.split(src, ".") {
    [] -> src
    parts -> {
      parts
      |> remove_last
      |> string.join(".")
    }
  }

  let width_1x_str = int.to_string(width_1x)
  let width_2x_str = int.to_string(width_2x)

  html.picture([], [
    html.source([
      attribute.attribute(
        "srcset",
        base_path
          <> "-1x.avif "
          <> width_1x_str
          <> "w, "
          <> base_path
          <> "-2x.avif "
          <> width_2x_str
          <> "w",
      ),
      attribute.attribute("sizes", sizes),
      attribute.attribute("type", "image/avif"),
    ]),
    html.source([
      attribute.attribute(
        "srcset",
        base_path
          <> "-1x.webp "
          <> width_1x_str
          <> "w, "
          <> base_path
          <> "-2x.webp "
          <> width_2x_str
          <> "w",
      ),
      attribute.attribute("sizes", sizes),
      attribute.attribute("type", "image/webp"),
    ]),
    html.img(case high_priority {
      True -> [
        attribute.attribute(
          "srcset",
          base_path
            <> "-1x.png "
            <> width_1x_str
            <> "w, "
            <> base_path
            <> "-2x.png "
            <> width_2x_str
            <> "w",
        ),
        attribute.attribute("sizes", sizes),
        attribute.attribute("fetchpriority", "high"),
        attribute.src(base_path <> "-1x.png"),
        attribute.alt(alt),
        attribute.class(class),
      ]
      False -> [
        attribute.attribute(
          "srcset",
          base_path
            <> "-1x.png "
            <> width_1x_str
            <> "w, "
            <> base_path
            <> "-2x.png "
            <> width_2x_str
            <> "w",
        ),
        attribute.attribute("sizes", sizes),
        attribute.src(base_path <> "-1x.png"),
        attribute.alt(alt),
        attribute.class(class),
      ]
    }),
  ])
}

fn remove_last(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [_] -> []
    [first, ..rest] -> [first, ..remove_last(rest)]
  }
}
