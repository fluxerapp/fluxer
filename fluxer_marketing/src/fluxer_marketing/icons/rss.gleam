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

pub fn rss(attrs: List(attribute.Attribute(a))) -> Element(a) {
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
      svg.path([attribute.attribute("d", "M56,136a64,64,0,0,1,64,64")]),
      svg.path([attribute.attribute("d", "M56,88A112,112,0,0,1,168,200")]),
      svg.path([attribute.attribute("d", "M56,40A160,160,0,0,1,216,200")]),
      svg.circle([
        attribute.attribute("cx", "60"),
        attribute.attribute("cy", "196"),
        attribute.attribute("r", "16"),
        attribute.attribute("fill", "currentColor"),
      ]),
    ],
  )
}
