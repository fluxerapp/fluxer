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

import fluxer_marketing/locale.{type Locale}
import fluxer_marketing/web.{type Context}
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn flag_svg(
  locale: Locale,
  ctx: Context,
  attributes: List(attribute.Attribute(a)),
) -> Element(a) {
  let flag_code = locale.get_flag_code(locale)

  html.img(
    [
      attribute.src(
        ctx.cdn_endpoint <> "/marketing/flags/" <> flag_code <> ".svg",
      ),
      attribute.alt("Flag"),
      attribute.attribute("loading", "lazy"),
    ]
    |> list.append(attributes),
  )
}
