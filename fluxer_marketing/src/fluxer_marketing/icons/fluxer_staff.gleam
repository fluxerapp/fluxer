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

pub fn fluxer_staff(attrs: List(attribute.Attribute(a))) -> Element(a) {
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
          "M320.878 273.038L469.518 124.399C472.718 121.199 472.718 116.079 469.518 112.719L446.318 89.5191C443.118 86.3191 437.998 86.3191 434.638 89.5191L285.998 238.158C280.878 243.278 273.838 246.158 266.478 246.158C258.958 246.158 251.598 249.198 246.318 254.478L86.3188 414.638C81.5187 419.438 81.5187 427.278 86.3188 432.078L126.959 472.718C131.759 477.518 139.599 477.518 144.399 472.718L304.558 312.558C309.838 307.278 312.878 299.918 312.878 292.398C312.878 285.198 315.758 278.158 320.878 273.038Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M262.483 243.76L113.842 95.1207C110.642 91.9207 105.522 91.9207 102.162 95.1207L78.9625 118.481C75.7625 121.681 75.7625 126.801 78.9625 130.161L227.603 278.8C232.723 283.92 235.603 290.96 235.603 298.32C235.603 305.84 238.643 313.2 243.923 318.48L398.003 472.56C402.803 477.36 410.643 477.36 415.443 472.56L456.083 431.92C460.883 427.12 460.883 419.28 456.083 414.48L302.003 260.4C296.723 255.12 289.363 252.08 281.843 252.08C274.643 251.92 267.603 248.88 262.483 243.76Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M116.72 266.32L250.16 132.88C254.96 128.08 254.96 120.24 250.32 115.44L186.48 51.6C181.68 46.8 173.84 46.8 169.04 51.6L35.6 185.04C30.8 189.84 30.8 197.68 35.6 202.48L99.28 266.32C104.08 271.12 111.92 271.12 116.72 266.32Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M303.92 132.88L428.4 257.36C438.16 267.12 454 267.12 463.76 257.2C467.28 253.68 469.68 249.04 470.64 243.92L478.64 198.32C483.6 170 474.48 140.88 454 120.4L385.2 51.6C380.4 46.8 372.56 46.8 367.76 51.6L303.92 115.44C299.12 120.24 299.12 128.08 303.92 132.88Z",
        ),
        attribute.attribute("fill", "currentColor"),
      ]),
    ],
  )
}
