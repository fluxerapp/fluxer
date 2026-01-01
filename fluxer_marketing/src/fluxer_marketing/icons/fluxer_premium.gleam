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

import gleam/option.{type Option}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/svg

pub fn fluxer_premium(
  fill_color: Option(String),
  attrs: List(attribute.Attribute(a)),
) -> Element(a) {
  let fill = option.unwrap(fill_color, "white")

  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("width", "336"),
      attribute.attribute("height", "274"),
      attribute.attribute("viewBox", "0 0 336 274"),
      attribute.attribute("fill", "none"),
      ..attrs
    ],
    [
      svg.path([
        attribute.attribute("fill-rule", "evenodd"),
        attribute.attribute("clip-rule", "evenodd"),
        attribute.attribute(
          "d",
          "M59.2774 198C59.2774 208.471 50.7886 216.96 40.3174 216.96C29.8461 216.96 21.3574 208.471 21.3574 198C21.3574 187.529 29.8461 179.04 40.3174 179.04C50.7886 179.04 59.2774 187.529 59.2774 198ZM314.398 198C314.398 208.471 305.909 216.96 295.438 216.96C284.966 216.96 276.478 208.471 276.478 198C276.478 187.529 284.966 179.04 295.438 179.04C305.909 179.04 314.398 187.529 314.398 198Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute("fill-rule", "evenodd"),
        attribute.attribute("clip-rule", "evenodd"),
        attribute.attribute(
          "d",
          "M98.9976 75.24L137.158 126.48L58.0781 135.6L83.5181 76.92C77.3981 74.52 73.0781 68.52 73.0781 61.56C73.0781 52.44 80.5181 45 89.6381 45C98.7576 45 106.198 52.44 106.198 61.56C106.198 67.32 103.318 72.24 98.9976 75.24ZM252.12 76.92L277.56 135.6L198.48 126.48L236.64 75.24C232.32 72.24 229.44 67.2 229.44 61.56C229.44 52.44 236.88 45 246 45C255.12 45 262.56 52.44 262.56 61.56C262.56 68.52 258.24 74.52 252.12 76.92Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M335.76 73.08C335.76 63.96 328.32 56.52 319.2 56.52C310.08 56.52 302.64 63.96 302.64 73.08C302.64 76.44 303.72 79.68 305.4 82.2C302.76 82.2 300.12 82.8 297.6 84.48L250.44 114.96C244.2 118.92 236.04 117.48 231.6 111.48L179.16 40.32C178.8 39.72 178.32 39.24 177.84 38.76C184.2 35.2801 188.52 28.44 188.52 20.64C188.52 9.24 179.28 0 167.88 0C156.48 0 147.24 9.24 147.24 20.64C147.24 28.44 151.56 35.2801 157.92 38.76C157.44 39.24 156.96 39.72 156.6 40.32L104.16 111.48C99.84 117.48 91.56 118.92 85.32 114.96L38.16 84.48C35.64 82.8 33 82.2 30.36 82.2C32.16 79.56 33.12 76.44 33.12 73.08C33.12 63.96 25.68 56.52 16.56 56.52C7.44 56.52 0 63.96 0 73.08C0 82.2 7.44 89.64 16.56 89.64C17.16 89.64 17.76 89.64 18.24 89.52C16.68 92.28 16.08 95.52 16.8 99L48 254.64C50.28 265.8 60.12 273.84 71.52 273.84H264.24C275.64 273.84 285.48 265.8 287.76 254.64L318.96 99C319.68 95.52 318.96 92.16 317.52 89.52C318.12 89.52 318.72 89.64 319.2 89.64C328.32 89.64 335.76 82.2 335.76 73.08Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M167.88 228C184.448 228 197.88 214.568 197.88 198C197.88 181.432 184.448 168 167.88 168C151.311 168 137.88 181.432 137.88 198C137.88 214.568 151.311 228 167.88 228Z",
        ),
        attribute.attribute("fill", fill),
      ]),
      svg.path([
        attribute.attribute("fill-rule", "evenodd"),
        attribute.attribute("clip-rule", "evenodd"),
        attribute.attribute(
          "d",
          "M108 198C108 207.941 99.9408 216 90 216C80.0588 216 72 207.941 72 198C72 188.059 80.0588 180 90 180C99.9408 180 108 188.059 108 198ZM264 198C264 207.941 255.941 216 246 216C236.059 216 228 207.941 228 198C228 188.059 236.059 180 246 180C255.941 180 264 188.059 264 198Z",
        ),
        attribute.attribute("fill", fill),
      ]),
    ],
  )
}
