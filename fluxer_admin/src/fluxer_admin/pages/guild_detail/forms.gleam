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

import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, action}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn render_features_form(
  ctx: Context,
  current_features: List(String),
  guild_id: String,
) {
  let all_features = constants.get_guild_features()

  let known_feature_values = list.map(all_features, fn(f) { f.value })
  let custom_features =
    list.filter(current_features, fn(f) {
      !list.contains(known_feature_values, f)
    })

  h.form(
    [
      a.method("POST"),
      action(
        ctx,
        "/guilds/" <> guild_id <> "?action=update-features&tab=features",
      ),
      a.id("features-form"),
    ],
    [
      h.div(
        [a.class("space-y-3")],
        list.map(all_features, fn(feature) {
          render_feature_checkbox(feature, current_features)
        }),
      ),
      h.div([a.class("mt-6 pt-6 border-t border-neutral-200")], [
        h.label([a.class("block")], [
          h.span([a.class("text-sm text-neutral-900 mb-2 block")], [
            element.text("Custom Features"),
          ]),
          h.p([a.class("text-xs text-neutral-600 mb-2")], [
            element.text(
              "Enter custom feature strings separated by commas (e.g., CUSTOM_FEATURE_1, CUSTOM_FEATURE_2)",
            ),
          ]),
          h.input([
            a.type_("text"),
            a.name("custom_features"),
            a.placeholder("CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"),
            a.value(
              list.fold(custom_features, "", fn(acc, f) {
                case acc {
                  "" -> f
                  _ -> acc <> ", " <> f
                }
              }),
            ),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-neutral-900",
            ),
            a.attribute(
              "onchange",
              "document.getElementById('features-save-button').classList.remove('hidden')",
            ),
          ]),
        ]),
      ]),
      h.div(
        [
          a.class("mt-6 pt-6 border-t border-neutral-200"),
          a.id("features-save-button"),
        ],
        [
          ui.button_primary("Save Changes", "submit", []),
        ],
      ),
    ],
  )
}

pub fn render_feature_checkbox(
  feature: constants.GuildFeature,
  current_features: List(String),
) {
  let is_checked = list.contains(current_features, feature.value)

  let onchange_script = case feature.value {
    "UNAVAILABLE_FOR_EVERYONE" ->
      "if(this.checked){const other=document.querySelector('input[value=\"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF\"]');if(other)other.checked=false;}document.getElementById('features-save-button').classList.remove('hidden')"
    "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF" ->
      "if(this.checked){const other=document.querySelector('input[value=\"UNAVAILABLE_FOR_EVERYONE\"]');if(other)other.checked=false;}document.getElementById('features-save-button').classList.remove('hidden')"
    _ ->
      "document.getElementById('features-save-button').classList.remove('hidden')"
  }
  ui.custom_checkbox(
    "features[]",
    feature.value,
    feature.value,
    is_checked,
    option.Some(onchange_script),
  )
}

pub fn render_disabled_operations_form(
  ctx: Context,
  current_disabled_operations: Int,
  guild_id: String,
) {
  let all_operations = constants.get_disabled_operations()

  h.form(
    [
      a.method("POST"),
      action(
        ctx,
        "/guilds/"
          <> guild_id
          <> "?action=update-disabled-operations&tab=settings",
      ),
      a.id("disabled-ops-form"),
    ],
    [
      h.div(
        [a.class("space-y-3")],
        list.map(all_operations, fn(operation) {
          render_disabled_operation_checkbox(
            operation,
            current_disabled_operations,
          )
        }),
      ),
      h.div(
        [
          a.class("mt-6 pt-6 border-t border-neutral-200 hidden"),
          a.id("disabled-ops-save-button"),
        ],
        [
          ui.button_primary("Save Changes", "submit", []),
        ],
      ),
    ],
  )
}

pub fn render_disabled_operation_checkbox(
  operation: constants.Flag,
  current_disabled_operations: Int,
) {
  let is_checked =
    int.bitwise_and(current_disabled_operations, operation.value)
    == operation.value

  ui.custom_checkbox(
    "disabled_operations[]",
    int.to_string(operation.value),
    operation.name,
    is_checked,
    option.Some(
      "document.getElementById('disabled-ops-save-button').classList.remove('hidden')",
    ),
  )
}
