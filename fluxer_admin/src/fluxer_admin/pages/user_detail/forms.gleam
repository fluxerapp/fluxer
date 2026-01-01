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

import fluxer_admin/api/common
import fluxer_admin/components/ui
import fluxer_admin/constants
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn render_flags_form(current_flags: String) {
  let patchable_flags = constants.get_patchable_flags()

  h.form(
    [a.method("POST"), a.action("?action=update-flags"), a.id("flags-form")],
    [
      h.div(
        [a.class("space-y-3")],
        list.map(patchable_flags, fn(flag) {
          render_flag_checkbox(flag, current_flags)
        }),
      ),
      h.div(
        [
          a.class("mt-6 pt-6 border-t border-neutral-200 hidden"),
          a.id("flags-save-button"),
        ],
        [
          ui.button_primary("Save Changes", "submit", []),
        ],
      ),
    ],
  )
}

pub fn render_flag_checkbox(flag: constants.Flag, current_flags: String) {
  let is_checked = case int.parse(current_flags) {
    Ok(flags_int) -> int.bitwise_and(flags_int, flag.value) == flag.value
    Error(_) -> False
  }
  ui.custom_checkbox(
    "flags[]",
    int.to_string(flag.value),
    flag.name,
    is_checked,
    option.Some(
      "document.getElementById('flags-save-button').classList.remove('hidden')",
    ),
  )
}

pub fn render_suspicious_flags_form(current_flags: Int) {
  let suspicious_flags = constants.get_suspicious_activity_flags()

  h.form(
    [
      a.method("POST"),
      a.action("?action=update-suspicious-flags"),
      a.id("suspicious-flags-form"),
    ],
    [
      h.div(
        [a.class("space-y-3")],
        list.map(suspicious_flags, fn(flag) {
          render_suspicious_flag_checkbox(flag, current_flags)
        }),
      ),
      h.div(
        [
          a.class("mt-6 pt-6 border-t border-neutral-200 hidden"),
          a.id("suspicious-flags-save-button"),
        ],
        [
          ui.button_primary("Save Changes", "submit", []),
        ],
      ),
    ],
  )
}

pub fn render_suspicious_flag_checkbox(flag: constants.Flag, current_flags: Int) {
  let is_checked = int.bitwise_and(current_flags, flag.value) == flag.value
  ui.custom_checkbox(
    "suspicious_flags[]",
    int.to_string(flag.value),
    flag.name,
    is_checked,
    option.Some(
      "document.getElementById('suspicious-flags-save-button').classList.remove('hidden')",
    ),
  )
}

pub fn render_acls_form(user: common.UserLookupResult, admin_acls: List(String)) {
  let can_edit_acls =
    list.contains(admin_acls, constants.acl_acl_set_user)
    || list.contains(admin_acls, constants.acl_wildcard)

  let is_disabled = !can_edit_acls

  case is_disabled {
    True -> {
      case list.is_empty(user.acls) {
        True ->
          h.p([a.class("text-sm text-neutral-500 italic")], [
            element.text("No ACLs assigned"),
          ])
        False ->
          h.div(
            [a.class("space-y-1")],
            list.map(user.acls, fn(acl) {
              h.div(
                [
                  a.class(
                    "text-sm text-neutral-700 bg-neutral-50 px-2 py-1 rounded",
                  ),
                ],
                [element.text(acl)],
              )
            }),
          )
      }
    }
    False -> {
      let all_acls = constants.get_all_acls()

      h.form(
        [a.method("POST"), a.action("?action=update-acls"), a.id("acls-form")],
        [
          h.div(
            [a.class("space-y-2 max-h-96 overflow-y-auto overscroll-contain")],
            list.map(all_acls, fn(acl) { render_acl_checkbox(acl, user.acls) }),
          ),
          h.div(
            [
              a.class("mt-6 pt-6 border-t border-neutral-200 hidden"),
              a.id("acls-save-button"),
            ],
            [
              h.button(
                [
                  a.type_("submit"),
                  a.class(
                    "px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors",
                  ),
                ],
                [element.text("Save Changes")],
              ),
            ],
          ),
        ],
      )
    }
  }
}

fn render_acl_checkbox(acl: String, current_acls: List(String)) {
  let is_checked = list.contains(current_acls, acl)

  h.label([a.class("flex items-center gap-3 cursor-pointer group")], [
    h.input([
      a.type_("checkbox"),
      a.name("acls[]"),
      a.value(acl),
      a.checked(is_checked),
      a.class("peer hidden"),
      a.attribute(
        "onchange",
        "document.getElementById('acls-save-button').classList.remove('hidden')",
      ),
    ]),
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
    h.span(
      [
        a.class("text-xs text-neutral-900 group-hover:text-neutral-700"),
      ],
      [element.text(acl)],
    ),
  ])
}
