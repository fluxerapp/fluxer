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
import lustre/element.{type Element}
import lustre/element/html as h

pub fn build_icon_links(cdn_endpoint: String) -> List(Element(t)) {
  [
    h.link([
      a.rel("icon"),
      a.attribute("type", "image/x-icon"),
      a.href(cdn_endpoint <> "/web/favicon.ico"),
    ]),
    h.link([
      a.rel("apple-touch-icon"),
      a.href(cdn_endpoint <> "/web/apple-touch-icon.png"),
    ]),
    h.link([
      a.rel("icon"),
      a.attribute("type", "image/png"),
      a.attribute("sizes", "32x32"),
      a.href(cdn_endpoint <> "/web/favicon-32x32.png"),
    ]),
    h.link([
      a.rel("icon"),
      a.attribute("type", "image/png"),
      a.attribute("sizes", "16x16"),
      a.href(cdn_endpoint <> "/web/favicon-16x16.png"),
    ]),
  ]
}
