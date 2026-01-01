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
import lustre/element/html

pub fn build_icon_links(cdn_endpoint: String) -> List(Element(a)) {
  [
    html.link([
      attribute.rel("icon"),
      attribute.type_("image/x-icon"),
      attribute.href(cdn_endpoint <> "/web/favicon.ico"),
    ]),
    html.link([
      attribute.rel("apple-touch-icon"),
      attribute.href(cdn_endpoint <> "/web/apple-touch-icon.png"),
    ]),
    html.link([
      attribute.rel("icon"),
      attribute.type_("image/png"),
      attribute.sizes("32x32"),
      attribute.href(cdn_endpoint <> "/web/favicon-32x32.png"),
    ]),
    html.link([
      attribute.rel("icon"),
      attribute.type_("image/png"),
      attribute.sizes("16x16"),
      attribute.href(cdn_endpoint <> "/web/favicon-16x16.png"),
    ]),
  ]
}
