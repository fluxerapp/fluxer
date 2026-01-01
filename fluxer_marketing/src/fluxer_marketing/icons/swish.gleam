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

pub fn swish(attrs: List(attribute.Attribute(a))) -> Element(a) {
  svg.svg(
    [
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("xmlns:xlink", "http://www.w3.org/1999/xlink"),
      attribute.attribute("viewBox", "0 0 420 420"),
      attribute.attribute("fill-rule", "evenodd"),
      ..attrs
    ],
    [
      svg.defs([], [
        svg.linear_gradient(
          [
            attribute.id("swish-grad-1"),
            attribute.attribute("x1", "-746"),
            attribute.attribute("y1", "822.6"),
            attribute.attribute("x2", "-746.2"),
            attribute.attribute("y2", "823.1"),
            attribute.attribute(
              "gradientTransform",
              "translate(224261.6 305063) scale(300.3 -370.5)",
            ),
            attribute.attribute("gradientUnits", "userSpaceOnUse"),
          ],
          [
            svg.stop([
              attribute.attribute("offset", "0"),
              attribute.attribute("stop-color", "#ef2131"),
            ]),
            svg.stop([
              attribute.attribute("offset", "1"),
              attribute.attribute("stop-color", "#fecf2c"),
            ]),
          ],
        ),
        svg.linear_gradient(
          [
            attribute.id("swish-grad-2"),
            attribute.attribute("x1", "-745.4"),
            attribute.attribute("y1", "823"),
            attribute.attribute("x2", "-745.9"),
            attribute.attribute("y2", "822.1"),
            attribute.attribute(
              "gradientTransform",
              "translate(204470.4 247194.2) scale(273.8 -300.2)",
            ),
            attribute.attribute("gradientUnits", "userSpaceOnUse"),
          ],
          [
            svg.stop([
              attribute.attribute("offset", "0"),
              attribute.attribute("stop-color", "#fbc52c"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".3"),
              attribute.attribute("stop-color", "#f87130"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".6"),
              attribute.attribute("stop-color", "#ef52e2"),
            ]),
            svg.stop([
              attribute.attribute("offset", "1"),
              attribute.attribute("stop-color", "#661eec"),
            ]),
          ],
        ),
        svg.linear_gradient(
          [
            attribute.id("swish-grad-3"),
            attribute.attribute("x1", "-746"),
            attribute.attribute("y1", "823"),
            attribute.attribute("x2", "-745.8"),
            attribute.attribute("y2", "822.5"),
            attribute.attribute(
              "gradientTransform",
              "translate(224142 305014) scale(300.3 -370.5)",
            ),
            attribute.attribute("gradientUnits", "userSpaceOnUse"),
          ],
          [
            svg.stop([
              attribute.attribute("offset", "0"),
              attribute.attribute("stop-color", "#78f6d8"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".3"),
              attribute.attribute("stop-color", "#77d1f6"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".6"),
              attribute.attribute("stop-color", "#70a4f3"),
            ]),
            svg.stop([
              attribute.attribute("offset", "1"),
              attribute.attribute("stop-color", "#661eec"),
            ]),
          ],
        ),
        svg.linear_gradient(
          [
            attribute.id("swish-grad-4"),
            attribute.attribute("x1", "-746.1"),
            attribute.attribute("y1", "822.3"),
            attribute.attribute("x2", "-745.6"),
            attribute.attribute("y2", "823.2"),
            attribute.attribute(
              "gradientTransform",
              "translate(204377.3 247074.5) scale(273.8 -300.2)",
            ),
            attribute.attribute("gradientUnits", "userSpaceOnUse"),
          ],
          [
            svg.stop([
              attribute.attribute("offset", "0"),
              attribute.attribute("stop-color", "#536eed"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".2"),
              attribute.attribute("stop-color", "#54c3ec"),
            ]),
            svg.stop([
              attribute.attribute("offset", ".6"),
              attribute.attribute("stop-color", "#64d769"),
            ]),
            svg.stop([
              attribute.attribute("offset", "1"),
              attribute.attribute("stop-color", "#fecf2c"),
            ]),
          ],
        ),
      ]),
      svg.g([], [
        svg.path([
          attribute.attribute("fill", "url(#swish-grad-1)"),
          attribute.attribute(
            "d",
            "M119.3,399.2c84.3,40.3,188.3,20.4,251.2-54.5,74.5-88.8,62.9-221.1-25.8-295.5l-59,70.3c69.3,58.2,78.4,161.5,20.2,230.9-46.4,55.3-122.8,73.7-186.5,48.9",
          ),
        ]),
        svg.path([
          attribute.attribute("fill", "url(#swish-grad-2)"),
          attribute.attribute(
            "d",
            "M119.3,399.2c84.3,40.3,188.3,20.4,251.2-54.5,7.7-9.2,14.5-18.8,20.3-28.8,9.9-61.7-11.9-126.9-63.2-169.9-13-10.9-27.2-19.8-41.9-26.5,69.3,58.2,78.4,161.5,20.2,230.9-46.4,55.3-122.8,73.7-186.5,48.9",
          ),
        ]),
        svg.path([
          attribute.attribute("fill", "url(#swish-grad-3)"),
          attribute.attribute(
            "d",
            "M300.3,20.4C216-19.9,111.9,0,49.1,74.9c-74.5,88.8-62.9,221.1,25.8,295.5l59-70.3c-69.3-58.2-78.4-161.5-20.2-230.9C160.2,14,236.6-4.5,300.3,20.4",
          ),
        ]),
        svg.path([
          attribute.attribute("fill", "url(#swish-grad-4)"),
          attribute.attribute(
            "d",
            "M300.3,20.4C216-19.9,111.9,0,49.1,74.9c-7.7,9.2-14.5,18.8-20.3,28.8-9.9,61.7,11.9,126.9,63.2,169.9,13,10.9,27.2,19.8,41.9,26.5-69.3-58.2-78.4-161.5-20.2-230.9C160.2,14,236.6-4.5,300.3,20.4",
          ),
        ]),
      ]),
    ],
  )
}
