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

pub fn chats(attrs: List(attribute.Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("viewBox", "0 0 256 256"),
      attribute.attribute("fill", "currentColor"),
      ..attrs
    ],
    [
      svg.path([
        attribute.attribute(
          "d",
          "M232,96a16,16,0,0,0-16-16H184V48a16,16,0,0,0-16-16H40A16,16,0,0,0,24,48V176a8,8,0,0,0,13,6.22L72,154V184a16,16,0,0,0,16,16h93.59L219,230.22a8,8,0,0,0,5,1.78,8,8,0,0,0,8-8Zm-42.55,89.78a8,8,0,0,0-5-1.78H88V152h80a16,16,0,0,0,16-16V96h32V207.25Z",
        ),
      ]),
    ],
  )
}
