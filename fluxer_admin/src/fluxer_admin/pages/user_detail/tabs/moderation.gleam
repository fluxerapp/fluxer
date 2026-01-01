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
import fluxer_admin/api/messages
import fluxer_admin/components/date_time
import fluxer_admin/components/deletion_days_script
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session, href}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn moderation_tab(
  ctx: Context,
  _session: Session,
  user: common.UserLookupResult,
  user_id: String,
  admin_acls: List(String),
  message_shred_job_id: option.Option(String),
  message_shred_status_result: option.Option(
    Result(messages.MessageShredStatus, common.ApiError),
  ),
  delete_all_messages_dry_run: option.Option(#(Int, Int)),
) {
  let temp_ban_durations = constants.get_temp_ban_durations()
  let deletion_reasons = constants.get_deletion_reasons()
  let can_shred_messages =
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_message_shred || acl == constants.acl_wildcard
    })
  let can_delete_all_messages =
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_message_delete_all || acl == constants.acl_wildcard
    })

  h.div([a.class("space-y-6")], [
    h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-6")], [
      ui.card(ui.PaddingMedium, [
        h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
          element.text("Ban Actions"),
        ]),
        h.div([a.class("space-y-4")], [
          case user.temp_banned_until {
            option.Some(_) ->
              h.form(
                [
                  a.method("POST"),
                  a.action("?action=unban&tab=moderation"),
                  a.attribute(
                    "onsubmit",
                    "return confirm('Are you sure you want to unban this user?')",
                  ),
                ],
                [
                  h.button(
                    [
                      a.type_("submit"),
                      a.class(
                        "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                      ),
                    ],
                    [element.text("Unban User")],
                  ),
                ],
              )
            option.None ->
              h.form(
                [
                  a.method("POST"),
                  a.action("?action=temp-ban&tab=moderation"),
                  a.class("space-y-3"),
                  a.attribute(
                    "onsubmit",
                    "return confirm('Are you sure you want to temporarily ban this user?')",
                  ),
                ],
                [
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Duration")],
                    ),
                    h.select(
                      [
                        a.name("duration"),
                        a.class(
                          "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                        ),
                      ],
                      list.map(temp_ban_durations, fn(dur) {
                        h.option([a.value(int.to_string(dur.0))], dur.1)
                      }),
                    ),
                  ]),
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Public Reason (optional)")],
                    ),
                    h.input([
                      a.type_("text"),
                      a.name("reason"),
                      a.placeholder("Enter public ban reason..."),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                      ),
                    ]),
                  ]),
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Private Reason (optional)")],
                    ),
                    h.input([
                      a.type_("text"),
                      a.name("private_reason"),
                      a.placeholder("Enter private ban reason (audit log)..."),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                      ),
                    ]),
                  ]),
                  h.button(
                    [
                      a.type_("submit"),
                      a.class(
                        "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                      ),
                    ],
                    [element.text("Temporary Ban")],
                  ),
                ],
              )
          },
        ]),
      ]),
      ui.card(ui.PaddingMedium, [
        h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
          element.text("Account Deletion"),
        ]),
        h.div([a.class("space-y-4")], [
          case user.pending_deletion_at {
            option.Some(_) ->
              h.form(
                [
                  a.method("POST"),
                  a.action("?action=cancel-deletion&tab=moderation"),
                  a.attribute(
                    "onsubmit",
                    "return confirm('Are you sure you want to cancel the scheduled deletion for this user?')",
                  ),
                ],
                [
                  h.button(
                    [
                      a.type_("submit"),
                      a.class(
                        "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                      ),
                    ],
                    [element.text("Cancel Deletion")],
                  ),
                ],
              )
            option.None ->
              h.form(
                [
                  a.method("POST"),
                  a.action("?action=schedule-deletion&tab=moderation"),
                  a.class("space-y-3"),
                  a.attribute(
                    "onsubmit",
                    "return confirm('Are you sure you want to schedule this user account for deletion? This action will permanently delete the account after the specified number of days.')",
                  ),
                ],
                [
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Days until deletion")],
                    ),
                    h.input([
                      a.type_("number"),
                      a.id("user-deletion-days"),
                      a.name("days"),
                      a.value("14"),
                      a.min("14"),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                      ),
                    ]),
                  ]),
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Reason")],
                    ),
                    h.select(
                      [
                        a.id("user-deletion-reason"),
                        a.name("reason_code"),
                        a.class(
                          "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                        ),
                      ],
                      list.map(deletion_reasons, fn(reason) {
                        h.option([a.value(int.to_string(reason.0))], reason.1)
                      }),
                    ),
                  ]),
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Public Reason (optional)")],
                    ),
                    h.input([
                      a.type_("text"),
                      a.name("public_reason"),
                      a.placeholder("Enter public reason..."),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                      ),
                    ]),
                  ]),
                  h.div([], [
                    h.label(
                      [
                        a.class(
                          "block text-sm font-medium text-neutral-700 mb-1",
                        ),
                      ],
                      [element.text("Private Reason (optional)")],
                    ),
                    h.input([
                      a.type_("text"),
                      a.name("private_reason"),
                      a.placeholder("Enter private reason (audit log)..."),
                      a.class(
                        "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900",
                      ),
                    ]),
                  ]),
                  h.button(
                    [
                      a.type_("submit"),
                      a.class(
                        "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
                      ),
                    ],
                    [element.text("Schedule Deletion")],
                  ),
                ],
              )
          },
        ]),
        deletion_days_script.render(),
      ]),
    ]),
    case can_delete_all_messages {
      True ->
        render_delete_all_messages_section(
          ctx,
          user_id,
          delete_all_messages_dry_run,
        )
      False -> element.none()
    },
    case can_shred_messages {
      True ->
        render_message_shred_section(
          ctx,
          user_id,
          message_shred_job_id,
          message_shred_status_result,
        )
      False -> element.none()
    },
  ])
}

fn render_delete_all_messages_section(
  _ctx: Context,
  _user_id: String,
  dry_run_data: option.Option(#(Int, Int)),
) {
  ui.card(ui.PaddingMedium, [
    h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
      element.text("Delete All Messages"),
    ]),
    h.p([a.class("body-sm text-neutral-600 mb-3")], [
      element.text(
        "Locate every message this user has ever sent and permanently remove them. First run a dry run to see how many channels and messages will be affected.",
      ),
    ]),
    h.form(
      [
        a.method("POST"),
        a.action("?action=delete-all-messages&tab=moderation"),
        a.class("space-y-3"),
      ],
      [
        h.input([
          a.type_("hidden"),
          a.name("dry_run"),
          a.value("true"),
        ]),
        h.button(
          [
            a.type_("submit"),
            a.class(
              "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Preview Deletion")],
        ),
      ],
    ),
    case dry_run_data {
      option.Some(#(channel_count, message_count)) ->
        h.div(
          [
            a.class(
              "mt-4 space-y-3 bg-neutral-50 border border-neutral-200 rounded-lg p-4",
            ),
          ],
          [
            h.div([], [
              h.p([a.class("body-sm text-neutral-700")], [
                element.text(
                  "Channels:"
                  <> " "
                  <> int.to_string(channel_count)
                  <> " · Messages: "
                  <> int.to_string(message_count),
                ),
              ]),
            ]),
            h.form(
              [
                a.method("POST"),
                a.action("?action=delete-all-messages&tab=moderation"),
                a.attribute(
                  "onsubmit",
                  "return confirm('This will permanently delete every message this user has ever sent. Continue?')",
                ),
              ],
              [
                h.input([
                  a.type_("hidden"),
                  a.name("dry_run"),
                  a.value("false"),
                ]),
                h.button(
                  [
                    a.type_("submit"),
                    a.class(
                      "w-full px-4 py-2 bg-red-600 text-white rounded text-sm font-medium hover:bg-red-500 transition-colors",
                    ),
                  ],
                  [element.text("Delete All Messages")],
                ),
              ],
            ),
          ],
        )
      option.None -> element.none()
    },
  ])
}

fn render_message_shred_section(
  ctx: Context,
  user_id: String,
  job_id: option.Option(String),
  status_result: option.Option(
    Result(messages.MessageShredStatus, common.ApiError),
  ),
) {
  let entry_hint =
    "Upload a CSV file where each row includes the channel_id and message_id separated by a comma. Large files are chunked server-side automatically."

  ui.card(ui.PaddingMedium, [
    h.h2([a.class("text-base font-medium text-neutral-900 mb-4")], [
      element.text("Message Shredder"),
    ]),
    h.p([a.class("body-sm text-neutral-600 mb-3")], [
      element.text(entry_hint),
    ]),
    h.form(
      [
        a.method("POST"),
        a.action("?action=message-shred&tab=moderation"),
        a.id("message-shred-form"),
        a.class("space-y-3"),
      ],
      [
        h.input([
          a.type_("hidden"),
          a.name("csv_data"),
          a.id("message-shred-csv-data"),
        ]),
        h.label([a.class("block text-sm font-medium text-neutral-700")], [
          element.text("CSV File"),
        ]),
        h.input([
          a.id("message-shred-file"),
          a.type_("file"),
          a.accept([".csv"]),
          a.class(
            "w-full px-3 py-2 border border-neutral-300 rounded focus:outline-none focus:ring-2 focus:ring-neutral-900 text-sm",
          ),
        ]),
        h.button(
          [
            a.type_("submit"),
            a.id("message-shred-submit"),
            a.class(
              "w-full px-4 py-2 bg-neutral-900 text-white rounded text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [element.text("Shred Messages")],
        ),
      ],
    ),
    render_message_shred_status(ctx, user_id, job_id, status_result),
    message_shred_form_script(),
  ])
}

fn render_message_shred_status(
  ctx: Context,
  user_id: String,
  job_id: option.Option(String),
  status_result: option.Option(
    Result(messages.MessageShredStatus, common.ApiError),
  ),
) {
  case job_id {
    option.Some(_) ->
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg p-4 space-y-3")],
        [
          h.div([a.class("flex items-center justify-between")], [
            h.h2([a.class("subtitle text-neutral-900")], [
              element.text("Message Shred Status"),
            ]),
            h.a(
              [
                href(ctx, "/users/" <> user_id <> "?tab=moderation"),
                a.class("body-sm text-neutral-600"),
              ],
              [
                element.text("Clear"),
              ],
            ),
          ]),
          case status_result {
            option.Some(result) ->
              case result {
                Ok(status) -> render_message_shred_status_content(status)
                Error(common.NotFound) ->
                  h.p([a.class("body-sm text-neutral-700")], [
                    element.text("Preparing job… check back in a moment."),
                  ])
                Error(err) -> render_status_error(err)
              }
            option.None ->
              h.p([a.class("body-sm text-neutral-700")], [
                element.text("Preparing job… check back in a moment."),
              ])
          },
        ],
      )
    option.None -> element.none()
  }
}

fn render_message_shred_status_content(status: messages.MessageShredStatus) {
  let percentage = case status.total, status.processed {
    option.Some(total), option.Some(processed) ->
      case total {
        0 -> 0
        _ -> processed * 100 / total
      }
    _, _ -> 0
  }

  h.div([a.class("space-y-3")], [
    h.p([a.class("body-sm text-neutral-700")], [
      element.text(
        "Status: " <> format_message_shred_status_label(status.status),
      ),
    ]),
    case status.requested, status.skipped {
      option.Some(requested), option.Some(skipped) ->
        h.p([a.class("body-sm text-neutral-700")], [
          element.text(
            "Requested "
            <> int.to_string(requested)
            <> " entries, skipped "
            <> int.to_string(skipped)
            <> " entries",
          ),
        ])
      option.Some(requested), option.None ->
        h.p([a.class("body-sm text-neutral-700")], [
          element.text("Requested " <> int.to_string(requested) <> " entries"),
        ])
      _, _ -> element.none()
    },
    case status.status, status.total, status.processed {
      "in_progress", option.Some(total), option.Some(processed) ->
        h.div([a.class("space-y-2")], [
          h.div([a.class("flex justify-between body-sm text-neutral-700")], [
            h.span([], [
              element.text(
                int.to_string(processed)
                <> " / "
                <> int.to_string(total)
                <> " ("
                <> int.to_string(percentage)
                <> "%)",
              ),
            ]),
          ]),
          h.div(
            [a.class("w-full bg-neutral-200 rounded-full h-2 overflow-hidden")],
            [
              h.div(
                [
                  a.class("bg-neutral-900 h-2 transition-[width] duration-300"),
                  a.attribute(
                    "style",
                    "width: " <> int.to_string(percentage) <> "%",
                  ),
                ],
                [],
              ),
            ],
          ),
        ])
      "completed", option.Some(total), option.Some(processed) ->
        h.p([a.class("body-sm text-neutral-700")], [
          element.text(
            "Deleted "
            <> int.to_string(processed)
            <> " / "
            <> int.to_string(total)
            <> " entries",
          ),
        ])
      _, _, _ -> element.none()
    },
    case status.started_at {
      option.Some(timestamp) ->
        h.p([a.class("caption text-neutral-500")], [
          element.text("Started " <> date_time.format_timestamp(timestamp)),
        ])
      option.None -> element.none()
    },
    case status.completed_at {
      option.Some(timestamp) ->
        h.p([a.class("caption text-neutral-500")], [
          element.text("Completed " <> date_time.format_timestamp(timestamp)),
        ])
      option.None -> element.none()
    },
    case status.failed_at {
      option.Some(timestamp) ->
        h.p([a.class("caption text-red-600")], [
          element.text("Failed " <> date_time.format_timestamp(timestamp)),
        ])
      option.None -> element.none()
    },
    case status.error {
      option.Some(message) ->
        h.p([a.class("body-sm text-red-600")], [element.text(message)])
      option.None -> element.none()
    },
  ])
}

fn format_message_shred_status_label(status: String) -> String {
  case status {
    "in_progress" -> "In progress"
    "completed" -> "Completed"
    "failed" -> "Failed"
    "not_found" -> "Preparing"
    _ -> "Unknown"
  }
}

fn render_status_error(err: common.ApiError) {
  let #(title, message) = case err {
    common.Unauthorized -> #(
      "Authentication Required",
      "Your session has expired. Please log in again.",
    )
    common.Forbidden(msg) -> #("Permission Denied", msg)
    common.NotFound -> #("Not Found", "Status information not found.")
    common.ServerError -> #(
      "Server Error",
      "An internal server error occurred. Please try again later.",
    )
    common.NetworkError -> #(
      "Network Error",
      "Could not connect to the API. Please try again later.",
    )
  }

  h.div([a.class("space-y-1 body-sm text-red-600")], [
    h.p([], [element.text(title)]),
    h.p([], [element.text(message)]),
  ])
}

fn message_shred_form_script() {
  let script =
    "(function(){const form=document.getElementById('message-shred-form');if(!form)return;const file=document.getElementById('message-shred-file');const csvInput=document.getElementById('message-shred-csv-data');const submitButton=document.getElementById('message-shred-submit');if(!file||!csvInput||!submitButton)return;let processing=false;const handle=(event)=>{if(processing){event.preventDefault();return;}const selected=file.files&&file.files[0];if(!selected){event.preventDefault();alert('Please select a CSV file to continue.');return;}event.preventDefault();processing=true;submitButton.disabled=true;submitButton.textContent='Processing…';const reader=new FileReader();reader.onload=()=>{csvInput.value=reader.result||'';form.submit();};reader.onerror=()=>{processing=false;submitButton.disabled=false;submitButton.textContent='Shred Messages';alert('Failed to read the CSV file. Please try again.');};reader.readAsText(selected);};form.addEventListener('submit',handle);})();"

  h.script([a.attribute("defer", "defer")], script)
}
