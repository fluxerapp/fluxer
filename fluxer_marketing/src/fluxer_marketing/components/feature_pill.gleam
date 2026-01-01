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

pub type Status {
  Live
  ComingSoon
}

pub type Theme {
  Light
  Dark
}

pub fn render(text: String, _status: Status) -> Element(a) {
  html.span(
    [
      attribute.class(
        "inline-block rounded-lg bg-white px-3 py-2 sm:px-4 sm:py-3 text-sm sm:text-base font-medium text-[#4641D9] shadow-sm border border-white/20",
      ),
    ],
    [html.text(text)],
  )
}

pub fn render_with_theme(
  text: String,
  _status: Status,
  theme: Theme,
) -> Element(a) {
  let pill_class = case theme {
    Light ->
      "inline-block rounded-lg bg-[#4641D9] px-3 py-2 sm:px-4 sm:py-3 text-sm sm:text-base font-medium text-white shadow-sm border border-gray-200"
    Dark ->
      "inline-block rounded-lg bg-white px-3 py-2 sm:px-4 sm:py-3 text-sm sm:text-base font-medium text-[#4641D9] shadow-sm border border-white/20"
  }

  html.span([attribute.class(pill_class)], [html.text(text)])
}
