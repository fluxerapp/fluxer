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

pub fn fluxer_partner(attrs: List(attribute.Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("width", "512"),
      attribute.attribute("height", "512"),
      attribute.attribute("viewBox", "0 0 512 512"),
      attribute.attribute("fill", "none"),
      ..attrs
    ],
    [
      svg.path([
        attribute.attribute(
          "d",
          "M372.514 144.155C335.074 142.235 301.794 158.235 280.034 184.635C280.034 184.635 255.554 216.315 255.554 216.155C255.554 216.155 224.674 256.635 224.834 256.635L198.274 291.515L196.194 294.235C180.994 314.555 154.113 325.915 125.793 317.595C104.353 311.195 87.5529 293.275 82.7529 271.515C73.3129 230.075 104.833 193.115 144.673 193.115C164.034 193.115 178.434 201.275 188.354 209.915C198.914 219.195 215.074 217.755 223.714 206.555C231.234 196.795 230.274 182.715 221.314 174.395C170.754 127.995 75.0729 138.555 44.5129 203.035C4.83292 286.715 65.313 369.275 144.673 369.275C179.554 369.275 210.754 353.755 231.394 328.795L238.274 319.835C238.274 319.835 255.714 296.955 255.714 297.115C255.714 297.115 286.594 256.635 286.434 256.635L313.474 221.275C314.114 220.475 314.754 219.675 315.394 218.875C329.314 199.995 353.314 189.275 379.714 194.395C403.074 199.035 422.434 217.275 428.194 240.475C438.754 282.555 407.074 320.475 366.594 320.475C347.554 320.475 333.154 312.315 323.234 303.515C312.674 294.235 296.354 295.835 287.874 307.035C280.034 317.275 281.474 331.515 291.074 340.155C305.954 353.435 331.394 369.595 366.594 369.595C433.794 369.595 487.394 310.555 478.274 241.595C471.234 188.475 425.794 146.875 372.514 144.155Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
    ],
  )
}
