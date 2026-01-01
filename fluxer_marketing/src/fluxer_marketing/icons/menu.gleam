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
import lustre/element/svg

pub fn menu(attrs: List(attribute.Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("viewBox", "0 0 256 256"),
      attribute.attribute("fill", "none"),
      attribute.attribute("stroke", "currentColor"),
      attribute.attribute("stroke-linecap", "round"),
      attribute.attribute("stroke-linejoin", "round"),
      attribute.attribute("stroke-width", "24"),
      ..attrs
    ],
    [
      svg.line([
        attribute.attribute("x1", "40"),
        attribute.attribute("y1", "64"),
        attribute.attribute("x2", "216"),
        attribute.attribute("y2", "64"),
      ]),
      svg.line([
        attribute.attribute("x1", "40"),
        attribute.attribute("y1", "128"),
        attribute.attribute("x2", "216"),
        attribute.attribute("y2", "128"),
      ]),
      svg.line([
        attribute.attribute("x1", "40"),
        attribute.attribute("y1", "192"),
        attribute.attribute("x2", "216"),
        attribute.attribute("y2", "192"),
      ]),
    ],
  )
}
