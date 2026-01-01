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

import gleam/int
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn range_slider_section(
  slider_id: String,
  value_id: String,
  min_value: Int,
  max_value: Int,
  current_value: Int,
) {
  [
    h.input([
      a.id(slider_id),
      a.type_("range"),
      a.name("count"),
      a.min(int.to_string(min_value)),
      a.max(int.to_string(max_value)),
      a.value(int.to_string(current_value)),
      a.class("w-full h-2 bg-neutral-200 rounded-lg accent-neutral-900"),
    ]),
    h.div(
      [a.class("flex items-baseline justify-between text-xs text-neutral-500")],
      [
        h.span([], [element.text("Selected amount")]),
        h.span([a.id(value_id), a.class("font-semibold text-neutral-900")], [
          element.text(int.to_string(current_value)),
        ]),
      ],
    ),
  ]
}

pub fn slider_sync_script(
  slider_id: String,
  value_id: String,
) -> element.Element(a) {
  let script =
    "(function(){const slider=document.getElementById('"
    <> slider_id
    <> "');const value=document.getElementById('"
    <> value_id
    <> "');if(!slider||!value)return;const update=()=>value.textContent=slider.value;update();slider.addEventListener('input',update);})();"

  h.script([a.attribute("defer", "defer")], script)
}
