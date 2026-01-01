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
import fluxer_admin/api/archives
import fluxer_admin/api/common
import fluxer_admin/api/guilds
import fluxer_admin/avatar
import fluxer_admin/components/date_time
import fluxer_admin/components/errors
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/tabs
import fluxer_admin/components/ui
import fluxer_admin/constants
import fluxer_admin/pages/guild_detail/handlers
import fluxer_admin/pages/guild_detail/tabs/emojis
import fluxer_admin/pages/guild_detail/tabs/features
import fluxer_admin/pages/guild_detail/tabs/members
import fluxer_admin/pages/guild_detail/tabs/moderation
import fluxer_admin/pages/guild_detail/tabs/overview
import fluxer_admin/pages/guild_detail/tabs/settings
import fluxer_admin/pages/guild_detail/tabs/stickers
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
  guild_id: String,
  referrer: option.Option(String),
  tab: option.Option(String),
  page: option.Option(String),
) -> Response {
  let result = guilds.lookup_guild(ctx, session, guild_id)

  let admin_acls = case current_admin {
    option.Some(admin) -> admin.acls
    _ -> []
  }

  let can_view_archives =
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_archive_view_all
      || acl == constants.acl_archive_trigger_guild
      || acl == constants.acl_wildcard
    })

  let can_manage_assets =
    acl.has_permission(admin_acls, constants.acl_asset_purge)

  let active_tab = case tab {
    option.Some("settings") -> "settings"
    option.Some("features") -> "features"
    option.Some("moderation") -> "moderation"
    option.Some("members") -> "members"
    option.Some("archives") -> "archives"
    option.Some("emojis") -> "emojis"
    option.Some("stickers") -> "stickers"
    _ -> "overview"
  }

  let active_tab = case active_tab {
    "archives" if can_view_archives == False -> "overview"
    "emojis" if can_manage_assets == False -> "overview"
    "stickers" if can_manage_assets == False -> "overview"
    _ -> active_tab
  }

  let current_page = case page {
    option.Some(p) ->
      case int.parse(p) {
        Ok(page_num) -> page_num
        Error(_) -> 0
      }
    option.None -> 0
  }

  let content = case result {
    Ok(option.Some(guild_data)) -> {
      h.div([a.class("max-w-7xl mx-auto")], [
        h.div([a.class("mb-6")], [
          h.a(
            [
              href(ctx, option.unwrap(referrer, "/guilds")),
              a.class(
                "inline-flex items-center gap-2 text-neutral-600 hover:text-neutral-900 transition-colors",
              ),
            ],
            [
              h.span([a.class("text-lg")], [element.text("←")]),
              element.text("Back to Guilds"),
            ],
          ),
        ]),
        h.div(
          [a.class("bg-white border border-neutral-200 rounded-lg p-6 mb-6")],
          [
            h.div([a.class("flex items-start gap-6")], [
              case
                avatar.get_guild_icon_url(
                  ctx.media_endpoint,
                  guild_data.id,
                  guild_data.icon,
                  True,
                )
              {
                option.Some(icon_url) ->
                  h.div([a.class("flex-shrink-0")], [
                    h.img([
                      a.src(icon_url),
                      a.alt(guild_data.name),
                      a.class("w-24 h-24 rounded-full"),
                    ]),
                  ])
                option.None ->
                  h.div([a.class("flex-shrink-0")], [
                    h.div(
                      [
                        a.class(
                          "w-24 h-24 rounded-full bg-neutral-200 flex items-center justify-center text-base font-semibold text-neutral-600",
                        ),
                      ],
                      [
                        element.text(avatar.get_initials_from_name(
                          guild_data.name,
                        )),
                      ],
                    ),
                  ])
              },
              ui.detail_header(guild_data.name, [
                #(
                  "Guild ID:",
                  h.div([a.class("text-sm text-neutral-900")], [
                    element.text(guild_data.id),
                  ]),
                ),
                #(
                  "Owner ID:",
                  h.a(
                    [
                      href(ctx, "/users/" <> guild_data.owner_id),
                      a.class(
                        "text-sm text-neutral-900 hover:text-blue-600 hover:underline",
                      ),
                    ],
                    [element.text(guild_data.owner_id)],
                  ),
                ),
              ]),
            ]),
          ],
        ),
        render_tabs(
          ctx,
          session,
          guild_data,
          admin_acls,
          guild_id,
          active_tab,
          current_page,
        ),
      ])
    }
    Ok(option.None) -> not_found_view(ctx)
    Error(err) ->
      errors.api_error_view(
        ctx,
        err,
        option.Some("/guilds"),
        option.Some("Back to Guilds"),
      )
  }

  let html =
    layout.page(
      "Guild Details",
      "guilds",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_tabs(
  ctx: Context,
  session: Session,
  guild: guilds.GuildLookupResult,
  admin_acls: List(String),
  guild_id: String,
  active_tab: String,
  current_page: Int,
) {
  let tab_list = [
    tabs.Tab(
      label: "Overview",
      path: "/guilds/" <> guild_id <> "?tab=overview",
      active: active_tab == "overview",
    ),
    tabs.Tab(
      label: "Members",
      path: "/guilds/" <> guild_id <> "?tab=members",
      active: active_tab == "members",
    ),
    tabs.Tab(
      label: "Settings",
      path: "/guilds/" <> guild_id <> "?tab=settings",
      active: active_tab == "settings",
    ),
    tabs.Tab(
      label: "Features",
      path: "/guilds/" <> guild_id <> "?tab=features",
      active: active_tab == "features",
    ),
    tabs.Tab(
      label: "Moderation",
      path: "/guilds/" <> guild_id <> "?tab=moderation",
      active: active_tab == "moderation",
    ),
  ]

  let can_manage_assets =
    acl.has_permission(admin_acls, constants.acl_asset_purge)

  let tab_list = case
    list.any(admin_acls, fn(acl) {
      acl == constants.acl_archive_view_all
      || acl == constants.acl_archive_trigger_guild
      || acl == constants.acl_wildcard
    })
  {
    True ->
      tab_list
      |> list.append([
        tabs.Tab(
          label: "Archives",
          path: "/guilds/" <> guild_id <> "?tab=archives",
          active: active_tab == "archives",
        ),
      ])
    False -> tab_list
  }

  let tab_list = case can_manage_assets {
    True ->
      tab_list
      |> list.append([
        tabs.Tab(
          label: "Emojis",
          path: "/guilds/" <> guild_id <> "?tab=emojis",
          active: active_tab == "emojis",
        ),
        tabs.Tab(
          label: "Stickers",
          path: "/guilds/" <> guild_id <> "?tab=stickers",
          active: active_tab == "stickers",
        ),
      ])
    False -> tab_list
  }

  h.div([], [
    tabs.render_tabs(ctx, tab_list),
    case active_tab {
      "members" ->
        members.members_tab(ctx, session, guild_id, admin_acls, current_page)
      "settings" -> settings.settings_tab(ctx, guild, guild_id, admin_acls)
      "features" -> features.features_tab(ctx, guild, guild_id, admin_acls)
      "moderation" ->
        moderation.moderation_tab(ctx, guild, guild_id, admin_acls)
      "archives" -> archives_tab(ctx, session, guild_id)
      "emojis" -> emojis.emojis_tab(ctx, session, guild_id, admin_acls)
      "stickers" -> stickers.stickers_tab(ctx, session, guild_id, admin_acls)
      _ -> overview.overview_tab(ctx, guild)
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
          element.text("Guild Not Found"),
        ]),
        h.p([a.class("text-neutral-600 mb-6")], [
          element.text("The requested guild could not be found."),
        ]),
        h.a(
          [
            href(ctx, "/guilds"),
            a.class(
              "inline-flex items-center gap-2 px-4 py-2 bg-neutral-900 text-white rounded-lg text-sm font-medium hover:bg-neutral-800 transition-colors",
            ),
          ],
          [
            h.span([a.class("text-lg")], [element.text("←")]),
            element.text("Back to Guilds"),
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
  guild_id: String,
  action: option.Option(String),
  tab: option.Option(String),
) -> Response {
  let redirect_url = case tab {
    option.Some(t) -> "/guilds/" <> guild_id <> "?tab=" <> t
    option.None -> "/guilds/" <> guild_id
  }

  case action {
    option.Some("clear-fields") ->
      handlers.handle_clear_fields(req, ctx, session, guild_id, redirect_url)
    option.Some("update-features") ->
      handlers.handle_update_features(req, ctx, session, guild_id, redirect_url)
    option.Some("update-disabled-operations") ->
      handlers.handle_update_disabled_operations(
        req,
        ctx,
        session,
        guild_id,
        redirect_url,
      )
    option.Some("update-name") ->
      handlers.handle_update_name(req, ctx, session, guild_id, redirect_url)
    option.Some("update-vanity") ->
      handlers.handle_update_vanity(req, ctx, session, guild_id, redirect_url)
    option.Some("transfer-ownership") ->
      handlers.handle_transfer_ownership(
        req,
        ctx,
        session,
        guild_id,
        redirect_url,
      )
    option.Some("reload") ->
      handlers.handle_reload(ctx, session, guild_id, redirect_url)
    option.Some("shutdown") ->
      handlers.handle_shutdown(ctx, session, guild_id, redirect_url)
    option.Some("delete-guild") ->
      handlers.handle_delete_guild(ctx, session, guild_id, "/guilds")
    option.Some("update-settings") ->
      handlers.handle_update_settings(req, ctx, session, guild_id, redirect_url)
    option.Some("force-add-user") ->
      handlers.handle_force_add_user(req, ctx, session, guild_id, redirect_url)
    option.Some("refresh-search-index") ->
      handlers.handle_refresh_search_index(
        req,
        ctx,
        session,
        guild_id,
        redirect_url,
      )
    option.Some("trigger-archive") ->
      handle_trigger_archive(ctx, session, guild_id, redirect_url)
    option.Some("delete-emoji") ->
      handlers.handle_delete_emoji(req, ctx, session, guild_id, redirect_url)
    option.Some("delete-sticker") ->
      handlers.handle_delete_sticker(req, ctx, session, guild_id, redirect_url)
    _ -> redirect(ctx, redirect_url)
  }
}

fn archives_tab(ctx: Context, session: Session, guild_id: String) {
  let result =
    archives.list_archives(ctx, session, "guild", option.Some(guild_id), False)

  h.div([], [
    ui.flex_row_between([
      ui.heading_section("Guild Archives"),
      h.form(
        [
          a.method("post"),
          action(
            ctx,
            "/guilds/" <> guild_id <> "?tab=archives&action=trigger-archive",
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
          element.text("No archives yet for this guild."),
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
                              "/archives/download?subject_type=guild&subject_id="
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
  guild_id: String,
  redirect_url: String,
) -> Response {
  case archives.trigger_guild_archive(ctx, session, guild_id, option.None) {
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
