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

pub fn fluxer_bug_hunter(attrs: List(attribute.Attribute(a))) -> Element(a) {
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
          "M464 208C464 296.365 392.365 368 304 368C215.635 368 144 296.365 144 208C144 119.634 215.635 48 304 48C392.365 48 464 119.634 464 208Z",
        ),
        attribute.attribute("fill", "#7570FF"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M389.57 123.713C398.407 139.018 396.264 156.798 384.784 163.426C373.306 170.053 356.837 163.018 348 147.713C339.163 132.408 341.306 114.627 352.784 108C364.264 101.373 380.733 108.408 389.57 123.713Z",
        ),
        attribute.attribute("fill", "#837FFF"),
      ]),
      svg.path([
        attribute.attribute(
          "d",
          "M480 208C480 305.202 401.203 384 304 384C267.277 384 233.181 372.754 204.97 353.515C209.366 372.938 204.003 394.123 188.882 409.246L137.969 460.158C114.538 483.589 76.548 483.589 53.1166 460.158C29.6851 436.726 29.6851 398.736 53.1166 375.306L104.028 324.394C119.028 309.394 139.995 303.995 159.289 308.202C139.561 279.763 128 245.232 128 208C128 110.798 206.798 32 304 32C401.203 32 480 110.798 480 208ZM448 208C448 128.471 383.53 64 304 64C224.472 64 160 128.471 160 208C160 287.53 224.472 352 304 352C383.53 352 448 287.53 448 208Z",
        ),
        attribute.attribute("fill", "#4641D9"),
      ]),
    ],
  )
}
