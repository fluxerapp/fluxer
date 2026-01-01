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

pub fn translate(attrs: List(attribute.Attribute(a))) -> Element(a) {
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
      svg.polyline([attribute.attribute("points", "240,216 184,104 128,216")]),
      svg.line([
        attribute.attribute("x1", "144"),
        attribute.attribute("y1", "184"),
        attribute.attribute("x2", "224"),
        attribute.attribute("y2", "184"),
      ]),
      svg.line([
        attribute.attribute("x1", "96"),
        attribute.attribute("y1", "32"),
        attribute.attribute("x2", "96"),
        attribute.attribute("y2", "56"),
      ]),
      svg.line([
        attribute.attribute("x1", "32"),
        attribute.attribute("y1", "56"),
        attribute.attribute("x2", "160"),
        attribute.attribute("y2", "56"),
      ]),
      svg.path([attribute.attribute("d", "M128,56a96,96,0,0,1-96,96")]),
      svg.path([attribute.attribute("d", "M72.7,96A96,96,0,0,0,160,152")]),
    ],
  )
}
