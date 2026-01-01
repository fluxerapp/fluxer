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

import fluxer_admin/api/archives
import fluxer_admin/api/common
import fluxer_admin/api/messages
import fluxer_admin/api/users
import fluxer_admin/avatar
import fluxer_admin/badge
import fluxer_admin/components/date_time
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/tabs
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/pages/user_detail/handlers
import fluxer_admin/pages/user_detail/tabs/account
import fluxer_admin/pages/user_detail/tabs/guilds
import fluxer_admin/pages/user_detail/tabs/moderation
import fluxer_admin/pages/user_detail/tabs/overview
import fluxer_admin/user
import fluxer_admin/web.{type Context, type Session, action, href, redirect}
import gleam/int
import gleam/list
import gleam/option
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  user_id: String,
  referrer: option.Option(String),
  tab: option.Option(String),
  message_shred_job_id: option.Option(String),
  delete_all_messages_dry_run: option.Option(#(Int, Int)),
) -> Response {
  let result = users.lookup_user(ctx, session, user_id)
  let change_log_result = users.list_user_change_log(ctx, session, user_id)

  let admin_acls = case current_admin {
    option.Some(admin) -> admin.acls
    _ -> []
  }

  let message_shred_status_result = case message_shred_job_id {
    option.Some(job_id) ->
      option.Some(messages.get_message_shred_status(ctx, session, job_id))
    option.None -> option.None
  }

  let can_view_archives =
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_archive_view_all
      || acl == constants.acl_archive_trigger_user
      || acl == constants.acl_wildcard
    })

  let active_tab = case tab {
    option.Some("account") -> "account"
    option.Some("moderation") -> "moderation"
    option.Some("guilds") -> "guilds"
    option.Some("archives") -> "archives"
    _ -> "overview"
  }

  let active_tab = case active_tab == "archives" && can_view_archives == False {
    True -> "overview"
    False -> active_tab
  }

  let should_auto_refresh = case message_shred_status_result {
    option.Some(result) ->
      case result {
        Ok(status) ->
          status.status == "in_progress" || status.status == "not_found"
        Error(common.NotFound) -> True
        Error(_) -> False
      }
    option.None -> False
  }

  let content = case result {
    Ok(option.Some(user_data)) -> {
      let badges = badge.get_user_badges(ctx.cdn_endpoint, user_data.flags)

      h.div([a.class("max-w-7xl mx-auto")], [
        h.div([a.class("mb-6")], [
          h.a(
            [
              href(ctx, option.unwrap(referrer, "/users")),
              a.class(
                "inline-flex items-center gap-2 text-neutral-600 hover:text-neutral-900 transition-colors",
              ),
            ],
            [
              h.span([a.class("text-lg")], [element.text("←")]),
              element.text("Back to Users"),
            ],
          ),
        ]),
        h.div(
          [a.class("bg-white border border-neutral-200 rounded-lg p-6 mb-6")],
          [
            h.div([a.class("flex items-start gap-6")], [
              h.div([a.class("flex-shrink-0")], [
                h.img([
                  a.src(avatar.get_user_avatar_url(
                    ctx.media_endpoint,
                    ctx.cdn_endpoint,
                    user_data.id,
                    user_data.avatar,
                    True,
                    ctx.asset_version,
                  )),
                  a.alt(user_data.username),
                  a.class("w-24 h-24 rounded-full"),
                ]),
              ]),
              h.div([a.class("flex-1")], [
                h.div([a.class("flex items-center gap-3 mb-3")], [
                  ui.heading_section(
                    user_data.username
                    <> "#"
                    <> user.format_discriminator(user_data.discriminator),
                  ),
                  case user_data.bot {
                    True ->
                      h.span(
                        [
                          a.class(
                            "px-2 py-1 bg-blue-100 text-blue-700 text-sm font-medium rounded uppercase",
                          ),
                        ],
                        [element.text("Bot")],
                      )
                    False -> element.none()
                  },
                ]),
                case list.is_empty(badges) {
                  False ->
                    h.div(
                      [a.class("flex items-center gap-2 mb-3")],
                      list.map(badges, fn(b) {
                        h.img([
                          a.src(b.icon),
                          a.alt(b.name),
                          a.title(b.name),
                          a.class("w-6 h-6"),
                        ])
                      }),
                    )
                  True -> element.none()
                },
                h.div([a.class("flex flex-wrap items-start gap-4")], [
                  h.div([a.class("flex items-start gap-2")], [
                    h.div([a.class("text-sm font-medium text-neutral-600")], [
                      element.text("User ID:"),
                    ]),
                    h.div([a.class("text-sm text-neutral-900")], [
                      element.text(user_data.id),
                    ]),
                  ]),
                  case user.extract_timestamp(user_data.id) {
                    Ok(created_at) ->
                      h.div([a.class("flex items-start gap-2")], [
                        h.div(
                          [
                            a.class("text-sm font-medium text-neutral-600"),
                          ],
                          [
                            element.text("Created:"),
                          ],
                        ),
                        h.div([a.class("text-sm text-neutral-900")], [
                          element.text(created_at),
                        ]),
                      ])
                    Error(_) -> element.none()
                  },
                ]),
              ]),
            ]),
          ],
        ),
        render_tabs(
          ctx,
          session,
          user_data,
          admin_acls,
          user_id,
          active_tab,
          change_log_result,
          message_shred_job_id,
          message_shred_status_result,
          delete_all_messages_dry_run,
        ),
      ])
    }
    Ok(option.None) -> not_found_view(ctx)
    Error(err) ->
      errors.api_error_view(
        ctx,
        err,
        option.Some("/users"),
        option.Some("Back to Users"),
      )
  }

  let html =
    layout.page_with_refresh(
      "User Details",
      "users",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
      should_auto_refresh,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_tabs(
  ctx: Context,
  session: Session,
  user: common.UserLookupResult,
  admin_acls: List(String),
  user_id: String,
  active_tab: String,
  change_log_result: Result(users.ListUserChangeLogResponse, common.ApiError),
  message_shred_job_id: option.Option(String),
  message_shred_status_result: option.Option(
    Result(messages.MessageShredStatus, common.ApiError),
  ),
  delete_all_messages_dry_run: option.Option(#(Int, Int)),
) {
  let can_view_archives =
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_archive_view_all
      || acl == constants.acl_archive_trigger_user
      || acl == constants.acl_wildcard
    })

  let tab_list = [
    tabs.Tab(
      label: "Overview",
      path: "/users/" <> user_id <> "?tab=overview",
      active: active_tab == "overview",
    ),
    tabs.Tab(
      label: "Account",
      path: "/users/" <> user_id <> "?tab=account",
      active: active_tab == "account",
    ),
    tabs.Tab(
      label: "Moderation",
      path: "/users/" <> user_id <> "?tab=moderation",
      active: active_tab == "moderation",
    ),
    tabs.Tab(
      label: "Guilds",
      path: "/users/" <> user_id <> "?tab=guilds",
      active: active_tab == "guilds",
    ),
  ]

  let tab_list = case can_view_archives {
    True ->
      tab_list
      |> list.append([
        tabs.Tab(
          label: "Archives",
          path: "/users/" <> user_id <> "?tab=archives",
          active: active_tab == "archives",
        ),
      ])
    False -> tab_list
  }

  h.div([], [
    tabs.render_tabs(ctx, tab_list),
    case active_tab {
      "account" -> account.account_tab(ctx, session, user, user_id)
      "moderation" ->
        moderation.moderation_tab(
          ctx,
          session,
          user,
          user_id,
          admin_acls,
          message_shred_job_id,
          message_shred_status_result,
          delete_all_messages_dry_run,
        )
      "guilds" -> guilds.guilds_tab(ctx, session, user, user_id)
      "archives" -> archives_tab(ctx, session, user_id)
      _ -> overview.overview_tab(ctx, user, admin_acls, change_log_result)
    },
  ])
}

fn not_found_view(ctx: Context) {
  h.div([a.class("max-w-4xl mx-auto")], [
    h.div(
      [
        a.class(
          "bg-white border border-neutral-200 rounded-lg p-12 text-center",
        ),
      ],
      [
        h.h2([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("User Not Found"),
        ]),
        h.p([a.class("text-neutral-600 mb-6")], [
          element.text("The requested user could not be found."),
        ]),
        h.a(
          [
            href(ctx, "/users"),
            a.class(
              "inline-flex items-center gap-2 px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [
            h.span([a.class("text-lg")], [element.text("←")]),
            element.text("Back to Users"),
          ],
        ),
      ],
    ),
  ])
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  action: option.Option(String),
  tab: option.Option(String),
) -> Response {
  let redirect_url = case tab {
    option.Some(t) -> "/users/" <> user_id <> "?tab=" <> t
    option.None -> "/users/" <> user_id
  }

  case action {
    option.Some("update-flags") ->
      handlers.handle_update_flags(req, ctx, session, user_id, redirect_url)
    option.Some("update-suspicious-flags") ->
      handlers.handle_update_suspicious_flags(
        req,
        ctx,
        session,
        user_id,
        redirect_url,
      )
    option.Some("update-acls") ->
      handlers.handle_update_acls(req, ctx, session, user_id, redirect_url)
    option.Some("disable-mfa") ->
      handlers.handle_disable_mfa(ctx, session, user_id, redirect_url)
    option.Some("change-username") ->
      handlers.handle_change_username(req, ctx, session, user_id, redirect_url)
    option.Some("change-email") ->
      handlers.handle_change_email(req, ctx, session, user_id, redirect_url)
    option.Some("verify-email") ->
      handlers.handle_verify_email(ctx, session, user_id, redirect_url)
    option.Some("unlink-phone") ->
      handlers.handle_unlink_phone(ctx, session, user_id, redirect_url)
    option.Some("terminate-sessions") ->
      handlers.handle_terminate_sessions(ctx, session, user_id, redirect_url)
    option.Some("clear-fields") ->
      handlers.handle_clear_fields(req, ctx, session, user_id, redirect_url)
    option.Some("set-bot-status") ->
      handlers.handle_set_bot_status(req, ctx, session, user_id, redirect_url)
    option.Some("set-system-status") ->
      handlers.handle_set_system_status(
        req,
        ctx,
        session,
        user_id,
        redirect_url,
      )
    option.Some("temp-ban") ->
      handlers.handle_temp_ban(req, ctx, session, user_id, redirect_url)
    option.Some("unban") ->
      handlers.handle_unban(ctx, session, user_id, redirect_url)
    option.Some("schedule-deletion") ->
      handlers.handle_schedule_deletion(
        req,
        ctx,
        session,
        user_id,
        redirect_url,
      )
    option.Some("cancel-deletion") ->
      handlers.handle_cancel_deletion(ctx, session, user_id, redirect_url)
    option.Some("cancel-bulk-message-deletion") ->
      handlers.handle_cancel_bulk_message_deletion(
        ctx,
        session,
        user_id,
        redirect_url,
      )
    option.Some("send-password-reset") ->
      handlers.handle_send_password_reset(ctx, session, user_id, redirect_url)
    option.Some("change-dob") ->
      handlers.handle_change_dob(req, ctx, session, user_id, redirect_url)
    option.Some("trigger-archive") ->
      handle_trigger_archive(ctx, session, user_id, redirect_url)
    option.Some("delete-all-messages") ->
      handlers.handle_delete_all_messages(
        req,
        ctx,
        session,
        user_id,
        redirect_url,
      )
    option.Some("message-shred") ->
      handlers.handle_message_shred(req, ctx, session, user_id, redirect_url)
    _ -> redirect(ctx, redirect_url)
  }
}

fn archives_tab(ctx: Context, session: Session, user_id: String) {
  let result =
    archives.list_archives(ctx, session, "user", option.Some(user_id), False)

  h.div([], [
    ui.flex_row_between([
      ui.heading_section("User Archives"),
      h.form(
        [
          a.method("post"),
          action(
            ctx,
            "/users/" <> user_id <> "?tab=archives&action=trigger-archive",
          ),
        ],
        [
          ui.button_primary("Trigger Archive", "submit", []),
        ],
      ),
    ]),
    case result {
      Ok(response) -> render_archive_table(ctx, response.archives)
      Error(err) -> errors.api_error_view(ctx, err, option.None, option.None)
    },
  ])
}

fn render_archive_table(ctx: Context, archives: List(archives.Archive)) {
  case list.is_empty(archives) {
    True ->
      h.div(
        [
          a.class(
            "mt-4 p-4 border border-dashed border-neutral-300 rounded-lg text-neutral-600",
          ),
        ],
        [
          element.text("No archives yet for this user."),
        ],
      )
    False ->
      h.div(
        [
          a.class(
            "mt-4 bg-white border border-neutral-200 rounded-lg overflow-hidden",
          ),
        ],
        [
          h.table([a.class("min-w-full divide-y divide-neutral-200")], [
            h.thead([a.class("bg-neutral-50")], [
              h.tr([], [
                h.th(
                  [
                    a.class(
                      "px-4 py-2 text-left text-xs font-medium text-neutral-700 uppercase tracking-wider",
                    ),
                  ],
                  [
                    element.text("Requested At"),
                  ],
                ),
                h.th(
                  [
                    a.class(
                      "px-4 py-2 text-left text-xs font-medium text-neutral-700 uppercase tracking-wider",
                    ),
                  ],
                  [
                    element.text("Status"),
                  ],
                ),
                h.th(
                  [
                    a.class(
                      "px-4 py-2 text-left text-xs font-medium text-neutral-700 uppercase tracking-wider",
                    ),
                  ],
                  [
                    element.text("Actions"),
                  ],
                ),
              ]),
            ]),
            h.tbody(
              [a.class("divide-y divide-neutral-200")],
              list.map(archives, fn(archive) {
                h.tr([], [
                  h.td([a.class("px-4 py-3 text-sm text-neutral-900")], [
                    element.text(date_time.format_timestamp(
                      archive.requested_at,
                    )),
                  ]),
                  h.td([a.class("px-4 py-3 text-sm text-neutral-900")], [
                    element.text(
                      status_text(archive)
                      <> " ("
                      <> int.to_string(archive.progress_percent)
                      <> "%)",
                    ),
                  ]),
                  h.td([a.class("px-4 py-3 text-sm")], [
                    case archive.completed_at {
                      option.Some(_) ->
                        h.a(
                          [
                            href(
                              ctx,
                              "/archives/download?subject_type=user&subject_id="
                                <> archive.subject_id
                                <> "&archive_id="
                                <> archive.archive_id,
                            ),
                            a.class(
                              "text-sm text-white bg-neutral-900 hover:bg-neutral-800 px-3 py-1.5 rounded transition-colors",
                            ),
                          ],
                          [element.text("Download")],
                        )
                      option.None ->
                        h.span([a.class("text-neutral-500")], [
                          element.text("Pending"),
                        ])
                    },
                  ]),
                ])
              }),
            ),
          ]),
        ],
      )
  }
}

fn status_text(archive: archives.Archive) -> String {
  case archive.failed_at {
    option.Some(_) -> "Failed"
    option.None -> {
      case archive.completed_at {
        option.Some(_) -> "Completed"
        option.None -> option.unwrap(archive.progress_step, "In Progress")
      }
    }
  }
}

fn handle_trigger_archive(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case archives.trigger_user_archive(ctx, session, user_id, option.None) {
    Ok(_) -> redirect(ctx, redirect_url)
    Error(err) ->
      errors.api_error_view(
        ctx,
        err,
        option.Some(redirect_url),
        option.Some("Back"),
      )
      |> element.to_document_string
      |> wisp.html_response(400)
  }
}
