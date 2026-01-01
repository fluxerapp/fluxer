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
import gleam/int
import gleam/list
import gleam/option.{type Option}
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub const table_container_class = "bg-white border border-neutral-200 rounded-lg overflow-hidden"

pub const table_header_cell_class = "px-6 py-3 text-left text-xs text-neutral-600 uppercase tracking-wider"

pub const table_cell_class = "px-6 py-4 text-sm text-neutral-900"

pub const table_cell_muted_class = "px-6 py-4 text-sm text-neutral-600"

pub fn table_container(children: List(element.Element(a))) -> element.Element(a) {
  h.div([a.class(table_container_class)], children)
}

pub fn table_header_cell(label: String) -> element.Element(a) {
  h.th([a.class(table_header_cell_class)], [element.text(label)])
}

pub type PillTone {
  PillNeutral
  PillInfo
  PillSuccess
  PillWarning
  PillDanger
  PillPrimary
  PillPurple
  PillOrange
}

pub fn pill(label: String, tone: PillTone) -> element.Element(a) {
  h.span(
    [
      a.class(
        "inline-flex items-center px-2 py-1 rounded text-xs font-medium "
        <> pill_classes(tone),
      ),
    ],
    [element.text(label)],
  )
}

fn pill_classes(tone: PillTone) -> String {
  case tone {
    PillNeutral -> "bg-neutral-100 text-neutral-700"
    PillInfo -> "bg-blue-100 text-blue-700"
    PillSuccess -> "bg-green-100 text-green-700"
    PillWarning -> "bg-yellow-100 text-yellow-700"
    PillDanger -> "bg-red-100 text-red-700"
    PillPrimary -> "bg-neutral-900 text-white"
    PillPurple -> "bg-purple-100 text-purple-700"
    PillOrange -> "bg-orange-100 text-orange-700"
  }
}

pub type ButtonSize {
  Small
  Medium
  Large
}

pub type ButtonVariant {
  Primary
  Secondary
  Danger
  Success
  Info
  Ghost
}

pub type ButtonWidth {
  Auto
  Full
}

pub fn button_primary(
  text: String,
  type_: String,
  attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  button(text, type_, Primary, Medium, Auto, attrs)
}

pub fn button_danger(
  text: String,
  type_: String,
  attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  button(text, type_, Danger, Medium, Auto, attrs)
}

pub fn button_success(
  text: String,
  type_: String,
  attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  button(text, type_, Success, Medium, Auto, attrs)
}

pub fn button_info(
  text: String,
  type_: String,
  attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  button(text, type_, Info, Medium, Auto, attrs)
}

pub fn button_secondary(
  text: String,
  type_: String,
  attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  button(text, type_, Secondary, Medium, Auto, attrs)
}

pub fn button(
  text: String,
  type_: String,
  variant: ButtonVariant,
  size: ButtonSize,
  width: ButtonWidth,
  extra_attrs: List(a.Attribute(msg)),
) -> element.Element(msg) {
  let base_classes = "text-sm font-medium rounded-lg transition-colors"

  let size_classes = case size {
    Small -> "px-3 py-1.5 text-sm"
    Medium -> "px-4 py-2"
    Large -> "px-6 py-3 text-base"
  }

  let width_classes = case width {
    Auto -> ""
    Full -> "w-full"
  }

  let variant_classes = case variant {
    Primary -> "bg-neutral-900 text-white hover:bg-neutral-800"
    Secondary ->
      "text-neutral-700 hover:text-neutral-900 border border-neutral-300 hover:border-neutral-400"
    Danger -> "bg-red-600 text-white hover:bg-red-700"
    Success -> "bg-blue-600 text-white hover:bg-blue-700"
    Info -> "bg-blue-50 text-blue-700 hover:bg-blue-100"
    Ghost -> "text-neutral-600 hover:text-neutral-900 hover:bg-neutral-100"
  }

  let classes =
    [base_classes, size_classes, width_classes, variant_classes]
    |> list.filter(fn(c) { c != "" })
    |> list.fold("", fn(acc, c) { acc <> " " <> c })
    |> string_trim

  let attrs = [a.type_(type_), a.class(classes), ..extra_attrs]

  h.button(attrs, [element.text(text)])
}

@external(erlang, "string", "trim")
fn string_trim(string: String) -> String

pub type InputType {
  Text
  Email
  Password
  Tel
  Number
  Date
  Url
}

fn input_type_to_string(input_type: InputType) -> String {
  case input_type {
    Text -> "text"
    Email -> "email"
    Password -> "password"
    Tel -> "tel"
    Number -> "number"
    Date -> "date"
    Url -> "url"
  }
}

pub fn input(
  label: String,
  name: String,
  input_type: InputType,
  value: Option(String),
  required: Bool,
  placeholder: Option(String),
) -> element.Element(a) {
  h.div([a.class("space-y-2")], [
    h.label([a.class("text-sm text-neutral-700")], [
      element.text(label),
    ]),
    input_field(name, input_type, value, required, placeholder),
  ])
}

pub fn input_field(
  name: String,
  input_type: InputType,
  value: Option(String),
  required: Bool,
  placeholder: Option(String),
) -> element.Element(a) {
  let base_attrs = [
    a.type_(input_type_to_string(input_type)),
    a.name(name),
    a.class(
      "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
    ),
  ]

  let value_attr = case value {
    option.Some(v) -> [a.value(v)]
    option.None -> []
  }

  let required_attr = case required {
    True -> [a.required(True)]
    False -> []
  }

  let placeholder_attr = case placeholder {
    option.Some(p) -> [a.placeholder(p)]
    option.None -> []
  }

  let attrs =
    list.flatten([base_attrs, value_attr, required_attr, placeholder_attr])

  h.input(attrs)
}

pub fn textarea(
  label: String,
  name: String,
  value: Option(String),
  required: Bool,
  placeholder: Option(String),
  rows: Int,
) -> element.Element(a) {
  h.div([a.class("space-y-2")], [
    h.label([a.class("text-sm text-neutral-700")], [
      element.text(label),
    ]),
    textarea_field(name, value, required, placeholder, rows),
  ])
}

pub fn textarea_field(
  name: String,
  value: Option(String),
  required: Bool,
  placeholder: Option(String),
  rows: Int,
) -> element.Element(a) {
  let base_attrs = [
    a.name(name),
    a.attribute("rows", int.to_string(rows)),
    a.class(
      "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
    ),
  ]

  let required_attr = case required {
    True -> [a.required(True)]
    False -> []
  }

  let placeholder_attr = case placeholder {
    option.Some(p) -> [a.placeholder(p)]
    option.None -> []
  }

  let value_text = option.unwrap(value, "")

  let attrs = list.flatten([base_attrs, required_attr, placeholder_attr])

  h.textarea(attrs, value_text)
}

pub type CardPadding {
  PaddingNone
  PaddingSmall
  PaddingMedium
  PaddingLarge
  PaddingExtraLarge
}

pub fn card(
  padding: CardPadding,
  children: List(element.Element(a)),
) -> element.Element(a) {
  let padding_class = case padding {
    PaddingNone -> "p-0"
    PaddingSmall -> "p-4"
    PaddingMedium -> "p-6"
    PaddingLarge -> "p-8"
    PaddingExtraLarge -> "p-12"
  }

  h.div(
    [a.class("bg-white border border-neutral-200 rounded-lg " <> padding_class)],
    children,
  )
}

pub fn card_elevated(
  padding: CardPadding,
  children: List(element.Element(a)),
) -> element.Element(a) {
  let padding_class = case padding {
    PaddingNone -> "p-0"
    PaddingSmall -> "p-4"
    PaddingMedium -> "p-6"
    PaddingLarge -> "p-8"
    PaddingExtraLarge -> "p-12"
  }

  h.div(
    [
      a.class(
        "bg-white border border-neutral-200 rounded-lg shadow-sm "
        <> padding_class,
      ),
    ],
    children,
  )
}

pub fn card_empty(children: List(element.Element(a))) -> element.Element(a) {
  h.div(
    [
      a.class("bg-white border border-neutral-200 rounded-lg p-12 text-center"),
    ],
    children,
  )
}

pub fn heading_page(text: String) -> element.Element(a) {
  h.h1([a.class("text-lg font-semibold text-neutral-900")], [element.text(text)])
}

pub fn heading_section(text: String) -> element.Element(a) {
  h.h2([a.class("text-base font-semibold text-neutral-900")], [
    element.text(text),
  ])
}

pub fn heading_card(text: String) -> element.Element(a) {
  h.h3([a.class("text-base font-medium text-neutral-900")], [element.text(text)])
}

pub fn heading_card_with_margin(text: String) -> element.Element(a) {
  h.div([a.class("mb-4")], [heading_card(text)])
}

pub fn text_muted(text: String) -> element.Element(a) {
  h.p([a.class("text-sm text-neutral-600")], [element.text(text)])
}

pub fn text_small_muted(text: String) -> element.Element(a) {
  h.p([a.class("text-xs text-neutral-500")], [element.text(text)])
}

pub fn detail_header(
  title: String,
  subtitle_items: List(#(String, element.Element(a))),
) -> element.Element(a) {
  h.div([a.class("flex-1")], [
    h.div([a.class("flex items-center gap-3 mb-3")], [
      h.h1([a.class("text-base font-semibold text-neutral-900")], [
        element.text(title),
      ]),
    ]),
    h.div(
      [a.class("flex flex-wrap items-start gap-4")],
      list.map(subtitle_items, fn(item) {
        let #(label, value) = item
        h.div([a.class("flex items-start gap-2")], [
          h.div([a.class("text-sm font-medium text-neutral-600")], [
            element.text(label),
          ]),
          value,
        ])
      }),
    ),
  ])
}

pub fn info_item_text(label: String, value: String) -> element.Element(a) {
  info_item(
    label,
    h.div([a.class("text-sm text-neutral-900")], [element.text(value)]),
  )
}

pub fn info_item(label: String, value: element.Element(a)) -> element.Element(a) {
  h.div([], [
    h.div([a.class("text-sm font-medium text-neutral-600 mb-1")], [
      element.text(label),
    ]),
    value,
  ])
}

pub fn info_grid(items: List(element.Element(a))) -> element.Element(a) {
  h.div([a.class("grid grid-cols-2 md:grid-cols-3 gap-x-6 gap-y-3")], items)
}

pub type BadgeVariant {
  BadgeDefault
  BadgeInfo
  BadgeSuccess
  BadgeWarning
  BadgeDanger
}

pub fn badge(text: String, variant: BadgeVariant) -> element.Element(a) {
  let variant_classes = case variant {
    BadgeDefault -> "bg-neutral-100 text-neutral-700"
    BadgeInfo -> "bg-blue-100 text-blue-700"
    BadgeSuccess -> "bg-green-100 text-green-700"
    BadgeWarning -> "bg-yellow-100 text-yellow-700"
    BadgeDanger -> "bg-red-100 text-red-700"
  }

  h.span(
    [
      a.class(
        "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs "
        <> variant_classes,
      ),
    ],
    [element.text(text)],
  )
}

pub fn flex_row(
  gap: String,
  children: List(element.Element(a)),
) -> element.Element(a) {
  h.div([a.class("flex items-center gap-" <> gap)], children)
}

pub fn flex_row_between(
  children: List(element.Element(a)),
) -> element.Element(a) {
  h.div([a.class("mb-6 flex items-center justify-between")], children)
}

pub fn stack(
  gap: String,
  children: List(element.Element(a)),
) -> element.Element(a) {
  h.div([a.class("space-y-" <> gap)], children)
}

pub fn grid(
  cols: String,
  gap: String,
  children: List(element.Element(a)),
) -> element.Element(a) {
  h.div([a.class("grid grid-cols-" <> cols <> " gap-" <> gap)], children)
}

pub fn definition_list(
  cols: Int,
  items: List(element.Element(a)),
) -> element.Element(a) {
  let cols_class = case cols {
    1 -> "grid-cols-1"
    2 -> "grid-cols-1 sm:grid-cols-2"
    3 -> "grid-cols-1 sm:grid-cols-2 lg:grid-cols-3"
    4 -> "grid-cols-1 sm:grid-cols-2 lg:grid-cols-4"
    _ -> "grid-cols-1 sm:grid-cols-2"
  }

  h.dl([a.class("grid " <> cols_class <> " gap-x-6 gap-y-2")], items)
}

pub fn back_button(
  ctx: Context,
  url: String,
  label: String,
) -> element.Element(a) {
  h.a(
    [
      href(ctx, url),
      a.class(
        "inline-flex items-center gap-2 text-neutral-900 hover:text-neutral-600 underline decoration-neutral-300 hover:decoration-neutral-500 text-sm",
      ),
    ],
    [element.text("← " <> label)],
  )
}

pub fn not_found_view(
  ctx: Context,
  resource_name: String,
  back_url: String,
  back_label: String,
) -> element.Element(a) {
  h.div([a.class("max-w-2xl mx-auto")], [
    card(PaddingLarge, [
      h.div([a.class("text-center space-y-4")], [
        h.div(
          [
            a.class(
              "mx-auto w-16 h-16 bg-neutral-100 rounded-full flex items-center justify-center",
            ),
          ],
          [
            h.span([a.class("text-neutral-400 text-2xl font-semibold")], [
              element.text("?"),
            ]),
          ],
        ),
        h.h2([a.class("text-lg font-semibold text-neutral-900")], [
          element.text(resource_name <> " Not Found"),
        ]),
        h.p([a.class("text-neutral-600")], [
          element.text(
            "The "
            <> resource_name
            <> " you're looking for doesn't exist or you don't have permission to view it.",
          ),
        ]),
        h.div([a.class("pt-4")], [back_button(ctx, back_url, back_label)]),
      ]),
    ]),
  ])
}

pub type TableColumn(row, msg) {
  TableColumn(
    header: String,
    cell_class: String,
    render: fn(row) -> element.Element(msg),
  )
}

pub fn data_table(
  columns: List(TableColumn(row, msg)),
  rows: List(row),
) -> element.Element(msg) {
  h.div(
    [a.class("bg-white border border-neutral-200 rounded-lg overflow-hidden")],
    [
      h.table([a.class("min-w-full divide-y divide-neutral-200")], [
        h.thead([a.class("bg-neutral-50")], [
          h.tr(
            [],
            list.map(columns, fn(col) {
              let TableColumn(header, _, _) = col
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-left text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text(header)],
              )
            }),
          ),
        ]),
        h.tbody(
          [a.class("bg-white divide-y divide-neutral-200")],
          list.map(rows, fn(row) {
            h.tr(
              [a.class("hover:bg-neutral-50 transition-colors")],
              list.map(columns, fn(col) {
                let TableColumn(_, cell_class, render) = col
                h.td([a.class(cell_class)], [render(row)])
              }),
            )
          }),
        ),
      ]),
    ],
  )
}

pub fn custom_checkbox(
  name: String,
  value: String,
  label: String,
  checked: Bool,
  on_change: Option(String),
) -> element.Element(a) {
  let checkbox_attrs = case on_change {
    option.Some(script) -> [
      a.type_("checkbox"),
      a.name(name),
      a.value(value),
      a.checked(checked),
      a.class("peer hidden"),
      a.attribute("onchange", script),
    ]
    option.None -> [
      a.type_("checkbox"),
      a.name(name),
      a.value(value),
      a.checked(checked),
      a.class("peer hidden"),
    ]
  }

  h.label([a.class("flex items-center gap-3 cursor-pointer group")], [
    h.input(checkbox_attrs),
    element.element(
      "svg",
      [
        a.attribute("xmlns", "http://www.w3.org/2000/svg"),
        a.attribute("viewBox", "0 0 256 256"),
        a.class(
          "w-5 h-5 bg-white border-2 border-neutral-300 rounded p-0.5 text-white peer-checked:bg-neutral-900 peer-checked:border-neutral-900 transition-colors",
        ),
      ],
      [
        element.element(
          "polyline",
          [
            a.attribute("points", "40 144 96 200 224 72"),
            a.attribute("fill", "none"),
            a.attribute("stroke", "currentColor"),
            a.attribute("stroke-linecap", "round"),
            a.attribute("stroke-linejoin", "round"),
            a.attribute("stroke-width", "24"),
          ],
          [],
        ),
      ],
    ),
    h.span([a.class("text-sm text-neutral-900 group-hover:text-neutral-700")], [
      element.text(label),
    ]),
  ])
}
