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

pub fn hash(attrs: List(attribute.Attribute(a))) -> Element(a) {
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
        attribute.attribute("x1", "48"),
        attribute.attribute("y1", "96"),
        attribute.attribute("x2", "224"),
        attribute.attribute("y2", "96"),
      ]),
      svg.line([
        attribute.attribute("x1", "176"),
        attribute.attribute("y1", "40"),
        attribute.attribute("x2", "144"),
        attribute.attribute("y2", "216"),
      ]),
      svg.line([
        attribute.attribute("x1", "112"),
        attribute.attribute("y1", "40"),
        attribute.attribute("x2", "80"),
        attribute.attribute("y2", "216"),
      ]),
      svg.line([
        attribute.attribute("x1", "32"),
        attribute.attribute("y1", "160"),
        attribute.attribute("x2", "208"),
        attribute.attribute("y2", "160"),
      ]),
    ],
  )
}
