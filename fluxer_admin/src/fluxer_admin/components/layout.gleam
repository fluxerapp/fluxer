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

import fluxer_admin/api/common.{type UserLookupResult}
import fluxer_admin/avatar
import fluxer_admin/components/flash
import fluxer_admin/components/icons_meta
import fluxer_admin/user
import fluxer_admin/web.{type Context, type Session, cache_busted_asset, href}
import gleam/list
import gleam/option.{type Option}
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn build_head(title: String, ctx: Context) -> element.Element(a) {
  build_head_with_refresh(title, ctx, False)
}

pub fn build_head_with_refresh(
  title: String,
  ctx: Context,
  auto_refresh: Bool,
) -> element.Element(a) {
  let refresh_meta = case auto_refresh {
    True -> [
      h.meta([a.attribute("http-equiv", "refresh"), a.attribute("content", "3")]),
    ]
    False -> []
  }

  h.head([], [
    h.meta([a.attribute("charset", "UTF-8")]),
    h.meta([
      a.attribute("name", "viewport"),
      a.attribute("content", "width=device-width, initial-scale=1.0"),
    ]),
    ..list.append(
      refresh_meta,
      list.append(
        [
          h.title([], title <> " ~ Fluxer Admin"),
          h.link([
            a.rel("stylesheet"),
            a.href(cache_busted_asset(ctx, "/static/app.css")),
          ]),
        ],
        icons_meta.build_icon_links(ctx.cdn_endpoint),
      ),
    )
  ])
}

pub fn page(
  title: String,
  active_page: String,
  ctx: Context,
  session: Session,
  current_admin: Option(UserLookupResult),
  flash_data: Option(flash.Flash),
  content: element.Element(a),
) {
  page_with_refresh(
    title,
    active_page,
    ctx,
    session,
    current_admin,
    flash_data,
    content,
    False,
  )
}

pub fn page_with_refresh(
  title: String,
  active_page: String,
  ctx: Context,
  session: Session,
  current_admin: Option(UserLookupResult),
  flash_data: Option(flash.Flash),
  content: element.Element(a),
  auto_refresh: Bool,
) {
  h.html(
    [a.attribute("lang", "en"), a.attribute("data-base-path", ctx.base_path)],
    [
      build_head_with_refresh(title, ctx, auto_refresh),
      h.body([a.class("min-h-screen bg-neutral-50 flex")], [
        sidebar(ctx, active_page),
        h.div([a.class("ml-64 flex-1 flex flex-col")], [
          header(ctx, session, current_admin),
          h.main([a.class("flex-1 p-8")], [
            h.div([a.class("max-w-7xl mx-auto")], [
              case flash_data {
                option.Some(_) ->
                  h.div([a.class("mb-6")], [flash.view(flash_data)])
                option.None -> element.none()
              },
              content,
            ]),
          ]),
        ]),
      ]),
    ],
  )
}

fn sidebar(ctx: Context, active_page: String) {
  h.div(
    [
      a.class(
        "w-64 bg-neutral-900 text-white flex flex-col h-screen fixed left-0 top-0",
      ),
    ],
    [
      h.div([a.class("p-6 border-b border-neutral-800")], [
        h.a([href(ctx, "/users")], [
          h.h1([a.class("text-base font-semibold")], [
            element.text("Fluxer Admin"),
          ]),
        ]),
      ]),
      h.nav(
        [
          a.class("flex-1 overflow-y-auto p-4 space-y-1 sidebar-scrollbar"),
        ],
        admin_sidebar(ctx, active_page),
      ),
      h.script(
        [a.attribute("defer", "defer")],
        "(function(){var el=document.querySelector('[data-active]');if(el)el.scrollIntoView({block:'nearest'});})();",
      ),
    ],
  )
}

fn admin_sidebar(ctx: Context, active_page: String) -> List(element.Element(a)) {
  [
    sidebar_section("Lookup", [
      sidebar_item(ctx, "Users", "/users", active_page == "users"),
      sidebar_item(ctx, "Guilds", "/guilds", active_page == "guilds"),
    ]),
    sidebar_section("Moderation", [
      sidebar_item(ctx, "Reports", "/reports", active_page == "reports"),
      sidebar_item(
        ctx,
        "Pending Verifications",
        "/pending-verifications",
        active_page == "pending-verifications",
      ),
      sidebar_item(
        ctx,
        "Bulk Actions",
        "/bulk-actions",
        active_page == "bulk-actions",
      ),
    ]),
    sidebar_section("Bans", [
      sidebar_item(ctx, "IP Bans", "/ip-bans", active_page == "ip-bans"),
      sidebar_item(
        ctx,
        "Email Bans",
        "/email-bans",
        active_page == "email-bans",
      ),
      sidebar_item(
        ctx,
        "Phone Bans",
        "/phone-bans",
        active_page == "phone-bans",
      ),
    ]),
    sidebar_section("Content", [
      sidebar_item(
        ctx,
        "Message Tools",
        "/messages",
        active_page == "message-tools",
      ),
      sidebar_item(ctx, "Archives", "/archives", active_page == "archives"),
      sidebar_item(
        ctx,
        "Asset Purge",
        "/asset-purge",
        active_page == "asset-purge",
      ),
    ]),
    sidebar_section("Metrics", [
      sidebar_item(ctx, "Overview", "/metrics", active_page == "metrics"),
      sidebar_item(
        ctx,
        "Messaging & API",
        "/messages-metrics",
        active_page == "messages-metrics",
      ),
    ]),
    sidebar_section("Observability", [
      sidebar_item(ctx, "Gateway", "/gateway", active_page == "gateway"),
      sidebar_item(ctx, "Jobs", "/jobs", active_page == "jobs"),
      sidebar_item(ctx, "Storage", "/storage", active_page == "storage"),
      sidebar_item(
        ctx,
        "Audit Logs",
        "/audit-logs",
        active_page == "audit-logs",
      ),
    ]),
    sidebar_section("Platform", [
      sidebar_item(
        ctx,
        "Search Index",
        "/search-index",
        active_page == "search-index",
      ),
      sidebar_item(
        ctx,
        "Voice Regions",
        "/voice-regions",
        active_page == "voice-regions",
      ),
      sidebar_item(
        ctx,
        "Voice Servers",
        "/voice-servers",
        active_page == "voice-servers",
      ),
    ]),
    sidebar_section("Configuration", [
      sidebar_item(
        ctx,
        "Instance Config",
        "/instance-config",
        active_page == "instance-config",
      ),
      sidebar_item(
        ctx,
        "Feature Flags",
        "/feature-flags",
        active_page == "feature-flags",
      ),
    ]),
    sidebar_section("Codes", [
      sidebar_item(
        ctx,
        "Beta Codes",
        "/beta-codes",
        active_page == "beta-codes",
      ),
      sidebar_item(
        ctx,
        "Gift Codes",
        "/gift-codes",
        active_page == "gift-codes",
      ),
    ]),
  ]
}

fn sidebar_section(title: String, items: List(element.Element(a))) {
  h.div([a.class("mb-4")], [
    h.div([a.class("text-neutral-400 text-xs uppercase mb-2")], [
      element.text(title),
    ]),
    h.div([a.class("space-y-1")], items),
  ])
}

fn sidebar_item(ctx: Context, title: String, path: String, active: Bool) {
  let classes = case active {
    True ->
      "block px-3 py-2 rounded bg-neutral-800 text-white text-sm transition-colors"
    False ->
      "block px-3 py-2 rounded text-neutral-300 hover:bg-neutral-800 hover:text-white text-sm transition-colors"
  }

  let attrs = case active {
    True -> [href(ctx, path), a.class(classes), a.attribute("data-active", "")]
    False -> [href(ctx, path), a.class(classes)]
  }

  h.a(attrs, [element.text(title)])
}

fn header(
  ctx: Context,
  session: Session,
  current_admin: Option(UserLookupResult),
) {
  h.header(
    [
      a.class(
        "bg-white border-b border-neutral-200 px-8 py-4 flex items-center justify-between",
      ),
    ],
    [
      render_user_info(ctx, session, current_admin),
      h.a(
        [
          href(ctx, "/logout"),
          a.class(
            "px-4 py-2 text-sm font-medium text-neutral-700 hover:text-neutral-900 border border-neutral-300 rounded hover:border-neutral-400 transition-colors",
          ),
        ],
        [element.text("Logout")],
      ),
    ],
  )
}

fn render_user_info(
  ctx: Context,
  session: Session,
  current_admin: Option(UserLookupResult),
) {
  case current_admin {
    option.Some(admin_user) -> {
      h.a(
        [
          href(ctx, "/users/" <> session.user_id),
          a.class("flex items-center gap-3 hover:opacity-80 transition-opacity"),
        ],
        [
          render_avatar(
            ctx,
            admin_user.id,
            admin_user.avatar,
            admin_user.username,
          ),
          h.div([a.class("flex flex-col")], [
            h.div([a.class("text-sm text-neutral-900")], [
              element.text(
                admin_user.username
                <> "#"
                <> user.format_discriminator(admin_user.discriminator),
              ),
            ]),
            h.div([a.class("text-xs text-neutral-500")], [
              element.text("Admin"),
            ]),
          ]),
        ],
      )
    }
    option.None -> {
      h.div([a.class("text-sm text-neutral-600")], [
        element.text("Logged in as: "),
        h.a(
          [
            href(ctx, "/users/" <> session.user_id),
            a.class("text-blue-600 hover:text-blue-800 hover:underline"),
          ],
          [element.text(session.user_id)],
        ),
      ])
    }
  }
}

fn render_avatar(
  ctx: Context,
  user_id: String,
  avatar: Option(String),
  username: String,
) {
  h.img([
    a.src(avatar.get_user_avatar_url(
      ctx.media_endpoint,
      ctx.cdn_endpoint,
      user_id,
      avatar,
      True,
      ctx.asset_version,
    )),
    a.alt(username <> "'s avatar"),
    a.class("w-10 h-10 rounded-full"),
  ])
}
