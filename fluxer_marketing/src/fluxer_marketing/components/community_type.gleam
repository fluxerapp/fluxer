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

import fluxer_marketing/icons
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn render(icon: String, color: String, label: String) -> Element(a) {
  let bg_class = case color {
    "blue" -> "bg-gradient-to-br from-blue-400 to-blue-600"
    "purple" -> "bg-gradient-to-br from-purple-400 to-purple-600"
    "green" -> "bg-gradient-to-br from-emerald-400 to-emerald-600"
    "orange" -> "bg-gradient-to-br from-orange-400 to-orange-600"
    "red" -> "bg-gradient-to-br from-rose-400 to-rose-600"
    _ -> "bg-gradient-to-br from-gray-400 to-gray-600"
  }

  let icon_element = case icon {
    "game-controller" -> icons.game_controller
    "video-camera" -> icons.video_camera
    "graduation-cap" -> icons.graduation_cap
    "users-three" -> icons.users_three
    "code" -> icons.code_icon
    _ -> fn(_) { html.div([], []) }
  }

  html.div([], [
    html.div(
      [
        attribute.class(
          "lg:hidden w-full aspect-square flex items-center justify-center rounded-xl p-3 sm:p-4 "
          <> bg_class,
        ),
      ],
      [
        icon_element([
          attribute.class(
            "w-3/5 h-3/5 sm:w-1/2 sm:h-1/2 text-white flex-shrink-0",
          ),
        ]),
      ],
    ),
    html.div(
      [
        attribute.class(
          "hidden lg:flex w-full aspect-square flex-col items-center justify-center rounded-2xl p-6 xl:p-8 "
          <> bg_class,
        ),
      ],
      [
        icon_element([
          attribute.class("w-2/5 h-2/5 text-white flex-shrink-0"),
        ]),
        html.p(
          [
            attribute.class(
              "subtitle text-white font-bold text-center mt-4 xl:mt-5 whitespace-nowrap overflow-hidden text-ellipsis",
            ),
          ],
          [html.text(label)],
        ),
      ],
    ),
  ])
}
