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

import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/element/svg

pub fn google_play(attributes: List(Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("viewBox", "0 0 256 256"),
      attribute.attribute("fill", "currentColor"),
      ..attributes
    ],
    [
      svg.path([
        attribute.attribute(
          "d",
          "M239.82,114.18,72,18.16a16,16,0,0,0-16.12,0A15.68,15.68,0,0,0,48,31.87V224.13a15.68,15.68,0,0,0,7.92,13.67,16,16,0,0,0,16.12,0l167.78-96a15.76,15.76,0,0,0,0-27.64ZM160,139.31l18.92,18.92-88.5,50.66ZM90.4,47.1l88.53,50.67L160,116.69ZM193.31,150l-22-22,22-22,38.43,22Z",
        ),
      ]),
    ],
  )
}
