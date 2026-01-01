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

import lustre/attribute as a
import lustre/element

pub fn paperclip_icon(color: String) {
  element.element(
    "svg",
    [
      a.attribute("xmlns", "http://www.w3.org/2000/svg"),
      a.attribute("viewBox", "0 0 256 256"),
      a.class("w-3 h-3 inline-block " <> color),
    ],
    [
      element.element(
        "rect",
        [
          a.attribute("width", "256"),
          a.attribute("height", "256"),
          a.attribute("fill", "none"),
        ],
        [],
      ),
      element.element(
        "path",
        [
          a.attribute(
            "d",
            "M108.71,197.23l-5.11,5.11a46.63,46.63,0,0,1-66-.05h0a46.63,46.63,0,0,1,.06-65.89L72.4,101.66a46.62,46.62,0,0,1,65.94,0h0A46.34,46.34,0,0,1,150.78,124",
          ),
          a.attribute("fill", "none"),
          a.attribute("stroke", "currentColor"),
          a.attribute("stroke-linecap", "round"),
          a.attribute("stroke-linejoin", "round"),
          a.attribute("stroke-width", "24"),
        ],
        [],
      ),
      element.element(
        "path",
        [
          a.attribute(
            "d",
            "M147.29,58.77l5.11-5.11a46.62,46.62,0,0,1,65.94,0h0a46.62,46.62,0,0,1,0,65.94L193.94,144,183.6,154.34a46.63,46.63,0,0,1-66-.05h0A46.46,46.46,0,0,1,105.22,132",
          ),
          a.attribute("fill", "none"),
          a.attribute("stroke", "currentColor"),
          a.attribute("stroke-linecap", "round"),
          a.attribute("stroke-linejoin", "round"),
          a.attribute("stroke-width", "24"),
        ],
        [],
      ),
    ],
  )
}

pub fn checkmark_icon(color: String) {
  element.element(
    "svg",
    [
      a.attribute("xmlns", "http://www.w3.org/2000/svg"),
      a.attribute("viewBox", "0 0 256 256"),
      a.class("w-4 h-4 inline-block " <> color),
    ],
    [
      element.element(
        "rect",
        [
          a.attribute("width", "256"),
          a.attribute("height", "256"),
          a.attribute("fill", "none"),
        ],
        [],
      ),
      element.element(
        "polyline",
        [
          a.attribute("points", "40 144 96 200 224 72"),
          a.attribute("fill", "none"),
          a.attribute("stroke", "currentColor"),
          a.attribute("stroke-linecap", "round"),
          a.attribute("stroke-linejoin", "round"),
          a.attribute("stroke-width", "24"),
        ],
        [],
      ),
    ],
  )
}

pub fn x_icon(color: String) {
  element.element(
    "svg",
    [
      a.attribute("xmlns", "http://www.w3.org/2000/svg"),
      a.attribute("viewBox", "0 0 256 256"),
      a.class("w-4 h-4 inline-block " <> color),
    ],
    [
      element.element(
        "rect",
        [
          a.attribute("width", "256"),
          a.attribute("height", "256"),
          a.attribute("fill", "none"),
        ],
        [],
      ),
      element.element(
        "line",
        [
          a.attribute("x1", "200"),
          a.attribute("y1", "56"),
          a.attribute("x2", "56"),
          a.attribute("y2", "200"),
          a.attribute("fill", "none"),
          a.attribute("stroke", "currentColor"),
          a.attribute("stroke-linecap", "round"),
          a.attribute("stroke-linejoin", "round"),
          a.attribute("stroke-width", "24"),
        ],
        [],
      ),
      element.element(
        "line",
        [
          a.attribute("x1", "200"),
          a.attribute("y1", "200"),
          a.attribute("x2", "56"),
          a.attribute("y2", "56"),
          a.attribute("fill", "none"),
          a.attribute("stroke", "currentColor"),
          a.attribute("stroke-linecap", "round"),
          a.attribute("stroke-linejoin", "round"),
          a.attribute("stroke-width", "24"),
        ],
        [],
      ),
    ],
  )
}
