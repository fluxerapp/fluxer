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

import fluxer_admin/web.{type Context, href}
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub type Tab {
  Tab(label: String, path: String, active: Bool)
}

pub fn render_tabs(ctx: Context, tabs: List(Tab)) -> element.Element(a) {
  h.div([a.class("border-b border-neutral-200 mb-6")], [
    h.nav(
      [a.class("flex gap-6")],
      list.map(tabs, fn(tab) { render_tab(ctx, tab) }),
    ),
  ])
}

fn render_tab(ctx: Context, tab: Tab) -> element.Element(a) {
  let class_active = case tab.active {
    True -> "border-b-2 border-neutral-900 text-neutral-900 text-sm pb-3"
    False ->
      "border-b-2 border-transparent text-neutral-600 hover:text-neutral-900 hover:border-neutral-300 text-sm pb-3 transition-colors"
  }

  h.a([href(ctx, tab.path), a.class(class_active)], [element.text(tab.label)])
}
