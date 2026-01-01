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

import fluxer_admin/acl
import fluxer_admin/api/common
import fluxer_admin/api/users
import fluxer_admin/components/helpers
import fluxer_admin/components/icons
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/pages/user_detail/forms
import fluxer_admin/user
import fluxer_admin/web.{type Context}
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn overview_tab(
  _ctx: Context,
  user: common.UserLookupResult,
  admin_acls: List(String),
  change_log_result: Result(users.ListUserChangeLogResponse, common.ApiError),
) {
  h.div([a.class("space-y-6")], [
    case user.temp_banned_until, user.pending_deletion_at {
      option.Some(until), _ ->
        h.div([a.class("p-4 bg-red-50 border border-red-200 rounded-lg")], [
          h.div(
            [
              a.class(
                "flex items-center gap-2 text-red-900 text-sm font-medium",
              ),
            ],
            [
              element.text("Temporarily Banned Until: " <> until),
            ],
          ),
        ])
      _, option.Some(deletion_date) ->
        h.div(
          [a.class("p-4 bg-orange-50 border border-orange-200 rounded-lg")],
          [
            h.div([a.class("text-orange-900 text-sm font-medium")], [
              element.text("Scheduled for Deletion: " <> deletion_date),
            ]),
            case user.deletion_reason_code, user.deletion_public_reason {
              option.Some(code), option.Some(reason) ->
                h.div([a.class("text-sm text-orange-700 mt-1")], [
                  element.text(
                    "Reason: "
                    <> reason
                    <> " (code: "
                    <> int.to_string(code)
                    <> ")",
                  ),
                ])
              option.Some(code), option.None ->
                h.div([a.class("text-sm text-orange-700 mt-1")], [
                  element.text("Reason code: " <> int.to_string(code)),
                ])
              option.None, option.Some(reason) ->
                h.div([a.class("text-sm text-orange-700 mt-1")], [
                  element.text("Reason: " <> reason),
                ])
              _, _ -> element.none()
            },
          ],
        )
      _, _ -> element.none()
    },
    case user.pending_bulk_message_deletion_at {
      option.Some(deletion_date) ->
        h.div(
          [a.class("p-4 bg-yellow-50 border border-yellow-200 rounded-lg")],
          [
            h.div([a.class("text-yellow-900 text-sm font-medium")], [
              element.text(
                "Bulk message deletion scheduled for: " <> deletion_date,
              ),
            ]),
            case
              acl.has_permission(
                admin_acls,
                constants.acl_user_cancel_bulk_message_deletion,
              )
            {
              True ->
                h.form(
                  [
                    a.method("POST"),
                    a.action(
                      "?action=cancel-bulk-message-deletion&tab=overview",
                    ),
                    a.attribute(
                      "onsubmit",
                      "return confirm('Are you sure you want to cancel the scheduled bulk message deletion for this user?')",
                    ),
                  ],
                  [
                    h.button(
                      [
                        a.type_("submit"),
                        a.class(
                          "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors mt-3",
                        ),
                      ],
                      [element.text("Cancel Bulk Message Deletion")],
                    ),
                  ],
                )
              False -> element.none()
            },
          ],
        )
      option.None -> element.none()
    },
    h.div([a.class("grid grid-cols-1 md:grid-cols-3 gap-6 items-start")], [
      h.div([a.class("md:col-span-2 space-y-6")], [
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Account Information"),
          h.div(
            [a.class("grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-2 text-sm")],
            [
              helpers.compact_info_mono("User ID", user.id),
              case user.extract_timestamp(user.id) {
                Ok(created_at) -> helpers.compact_info("Created", created_at)
                Error(_) -> element.none()
              },
              helpers.compact_info(
                "Username",
                user.username
                  <> "#"
                  <> user.format_discriminator(user.discriminator),
              ),
              helpers.compact_info_with_element("Email", case user.email {
                option.Some(email) ->
                  h.span([], [
                    h.span([a.class("")], [element.text(email)]),
                    element.text(" "),
                    case user.email_verified {
                      True -> icons.checkmark_icon("text-green-600")
                      False -> icons.x_icon("text-red-600")
                    },
                    case user.email_bounced {
                      True ->
                        h.span([a.class("text-orange-600 ml-1")], [
                          element.text("(bounced)"),
                        ])
                      False -> element.none()
                    },
                  ])
                option.None ->
                  h.span([a.class("text-neutral-500")], [
                    element.text("Not set"),
                  ])
              }),
              helpers.compact_info("Phone", case user.phone {
                option.Some(phone) -> phone
                option.None -> "Not set"
              }),
              helpers.compact_info("Date of Birth", case user.date_of_birth {
                option.Some(dob) -> dob
                option.None -> "Not set"
              }),
              helpers.compact_info("Locale", case user.locale {
                option.Some(locale) -> locale
                option.None -> "Not set"
              }),
              case user.bio {
                option.Some(bio) ->
                  h.div([a.class("md:col-span-2")], [
                    helpers.compact_info("Bio", bio),
                  ])
                option.None -> element.none()
              },
              case user.pronouns {
                option.Some(pronouns) ->
                  helpers.compact_info("Pronouns", pronouns)
                option.None -> element.none()
              },
              helpers.compact_info("Bot", case user.bot {
                True -> "Yes"
                False -> "No"
              }),
              helpers.compact_info("System", case user.system {
                True -> "Yes"
                False -> "No"
              }),
              helpers.compact_info("Last Active", case user.last_active_at {
                option.Some(at) -> at
                option.None -> "Never"
              }),
              helpers.compact_info_with_element(
                "Last Active IP",
                case user.last_active_ip {
                  option.Some(ip) ->
                    h.span([], [
                      h.span([a.class("font-mono")], [element.text(ip)]),
                      case user.last_active_ip_reverse {
                        option.Some(reverse) ->
                          h.span([a.class("text-neutral-500 ml-2")], [
                            element.text("(" <> reverse <> ")"),
                          ])
                        option.None -> element.none()
                      },
                    ])
                  option.None ->
                    h.span([a.class("text-neutral-500")], [
                      element.text("Not recorded"),
                    ])
                },
              ),
              helpers.compact_info("Location", case user.last_active_location {
                option.Some(location) -> location
                option.None -> "Unknown Location"
              }),
            ],
          ),
        ]),
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Security & Premium"),
          h.div(
            [a.class("grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-2 text-sm")],
            [
              helpers.compact_info(
                "Authenticators",
                case list.is_empty(user.authenticator_types) {
                  True -> "None"
                  False -> {
                    let types =
                      list.map(user.authenticator_types, fn(t) {
                        case t {
                          0 -> "TOTP"
                          1 -> "SMS"
                          2 -> "WebAuthn"
                          _ -> "Unknown"
                        }
                      })
                    string.join(types, ", ")
                  }
                },
              ),
              helpers.compact_info("Premium Type", case user.premium_type {
                option.Some(0) | option.None -> "None"
                option.Some(1) -> "Subscription"
                option.Some(2) -> "Lifetime"
                option.Some(_) -> "Unknown"
              }),
              case user.premium_since {
                option.Some(since) ->
                  helpers.compact_info("Premium Since", since)
                option.None -> element.none()
              },
              case user.premium_until {
                option.Some(until) ->
                  helpers.compact_info("Premium Until", until)
                option.None -> element.none()
              },
            ],
          ),
        ]),
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("User Flags"),
          forms.render_flags_form(user.flags),
        ]),
        ui.card(ui.PaddingMedium, [
          ui.heading_card_with_margin("Suspicious Activity Flags"),
          forms.render_suspicious_flags_form(user.suspicious_activity_flags),
        ]),
        render_change_log(change_log_result),
      ]),
      ui.card(ui.PaddingMedium, [
        ui.heading_card_with_margin("Admin ACLs"),
        forms.render_acls_form(user, admin_acls),
      ]),
    ]),
  ])
}

fn render_change_log(
  change_log_result: Result(users.ListUserChangeLogResponse, common.ApiError),
) {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Contact Change Log"),
    case change_log_result {
      Ok(resp) -> render_change_log_entries(resp.entries)
      Error(err) ->
        h.div([a.class("text-sm text-red-700")], [
          element.text("Failed to load change log: " <> format_error(err)),
        ])
    },
  ])
}

fn render_change_log_entries(entries: List(users.ContactChangeLogEntry)) {
  case entries {
    [] ->
      h.div([a.class("text-sm text-neutral-600")], [
        element.text("No contact changes recorded."),
      ])
    _ ->
      h.ul(
        [a.class("divide-y divide-neutral-200")],
        list.map(entries, render_entry),
      )
  }
}

fn render_entry(entry: users.ContactChangeLogEntry) {
  h.li([a.class("py-3 flex flex-col gap-1")], [
    h.div([a.class("flex items-center gap-2 text-sm")], [
      h.span([a.class("font-medium text-neutral-900")], [
        element.text(label_for_field(entry.field)),
      ]),
      h.span([a.class("text-neutral-500")], [element.text(entry.event_at)]),
    ]),
    h.div([a.class("text-sm text-neutral-800")], [
      element.text(old_new_text(entry.old_value, entry.new_value)),
    ]),
    h.div([a.class("text-xs text-neutral-600")], [
      element.text("Reason: " <> entry.reason),
      case entry.actor_user_id {
        option.Some(actor) -> element.text(" • Actor: " <> actor)
        option.None -> element.none()
      },
    ]),
  ])
}

fn label_for_field(field: String) {
  case field {
    "email" -> "Email"
    "phone" -> "Phone"
    "fluxer_tag" -> "FluxerTag"
    _ -> field
  }
}

fn old_new_text(
  old_value: option.Option(String),
  new_value: option.Option(String),
) {
  let old_display = case old_value {
    option.Some(v) -> v
    option.None -> "null"
  }
  let new_display = case new_value {
    option.Some(v) -> v
    option.None -> "null"
  }
  old_display <> " → " <> new_display
}

fn format_error(err: common.ApiError) {
  case err {
    common.Unauthorized -> "Unauthorized"
    common.Forbidden(message) -> message
    common.NotFound -> "Not found"
    common.ServerError -> "Server error"
    common.NetworkError -> "Network error"
  }
}
