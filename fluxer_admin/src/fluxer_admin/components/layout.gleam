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
import fluxer_admin/navigation
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
  let admin_acls = admin_acls_from(current_admin)

  h.html(
    [a.attribute("lang", "en"), a.attribute("data-base-path", ctx.base_path)],
    [
      build_head_with_refresh(title, ctx, auto_refresh),
      h.body([a.class("min-h-screen bg-neutral-50 overflow-hidden")], [
        h.div([a.class("flex h-screen")], [
          sidebar(ctx, active_page, admin_acls),
          h.div(
            [
              a.attribute("data-sidebar-overlay", ""),
              a.class("fixed inset-0 bg-black/50 z-30 hidden lg:hidden"),
            ],
            [],
          ),
          h.div(
            [
              a.class("flex-1 flex flex-col w-full h-screen overflow-y-auto"),
            ],
            [
              header(ctx, session, current_admin),
              h.main([a.class("flex-1 p-4 sm:p-6 lg:p-8")], [
                h.div([a.class("w-full max-w-7xl mx-auto")], [
                  case flash_data {
                    option.Some(_) ->
                      h.div([a.class("mb-6")], [flash.view(flash_data)])
                    option.None -> element.none()
                  },
                  content,
                ]),
              ]),
            ],
          ),
        ]),
        sidebar_interaction_script(),
      ]),
    ],
  )
}

fn sidebar(ctx: Context, active_page: String, admin_acls: List(String)) {
  h.div(
    [
      a.attribute("data-sidebar", ""),
      a.class(
        "fixed inset-y-0 left-0 z-40 w-64 h-screen bg-neutral-900 text-white flex flex-col transform -translate-x-full transition-transform duration-200 ease-in-out lg:translate-x-0 lg:static lg:inset-auto shadow-xl lg:shadow-none",
      ),
    ],
    [
      h.div(
        [
          a.class(
            "p-6 border-b border-neutral-800 flex items-center justify-between gap-3",
          ),
        ],
        [
          h.a([href(ctx, "/users")], [
            h.h1([a.class("text-base font-semibold")], [
              element.text("Fluxer Admin"),
            ]),
          ]),
          h.button(
            [
              a.type_("button"),
              a.attribute("data-sidebar-close", ""),
              a.class(
                "lg:hidden inline-flex items-center justify-center p-2 rounded-md text-neutral-200 hover:bg-neutral-800 focus:outline-none focus:ring-2 focus:ring-white/40",
              ),
              a.attribute("aria-label", "Close sidebar"),
            ],
            [element.text("Close")],
          ),
        ],
      ),
      h.nav(
        [
          a.class("flex-1 overflow-y-auto p-4 space-y-1 sidebar-scrollbar"),
        ],
        admin_sidebar(ctx, active_page, admin_acls),
      ),
      h.script(
        [a.attribute("defer", "defer")],
        "(function(){var el=document.querySelector('[data-active]');if(el)el.scrollIntoView({block:'nearest'});})();",
      ),
    ],
  )
}

fn admin_sidebar(
  ctx: Context,
  active_page: String,
  admin_acls: List(String),
) -> List(element.Element(a)) {
  navigation.accessible_sections(admin_acls)
  |> list.map(fn(section) {
    let items =
      list.map(section.items, fn(item) {
        sidebar_item(ctx, item.title, item.path, active_page == item.active_key)
      })

    sidebar_section(section.title, items)
  })
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
        "bg-white border-b border-neutral-200 px-4 sm:px-6 lg:px-8 py-4 flex items-center justify-between gap-4 sticky top-0 z-10",
      ),
    ],
    [
      h.div([a.class("flex items-center gap-3 min-w-0")], [
        h.button(
          [
            a.type_("button"),
            a.attribute("data-sidebar-toggle", ""),
            a.class(
              "lg:hidden inline-flex items-center justify-center p-2 rounded-md border border-neutral-300 text-neutral-700 hover:bg-neutral-100 focus:outline-none focus:ring-2 focus:ring-neutral-400",
            ),
            a.attribute("aria-label", "Toggle sidebar"),
          ],
          [
            element.element(
              "svg",
              [
                a.attribute("xmlns", "http://www.w3.org/2000/svg"),
                a.attribute("viewBox", "0 0 24 24"),
                a.class("w-5 h-5"),
                a.attribute("fill", "none"),
                a.attribute("stroke", "currentColor"),
                a.attribute("stroke-width", "2"),
              ],
              [
                element.element(
                  "line",
                  [
                    a.attribute("x1", "3"),
                    a.attribute("y1", "6"),
                    a.attribute("x2", "21"),
                    a.attribute("y2", "6"),
                  ],
                  [],
                ),
                element.element(
                  "line",
                  [
                    a.attribute("x1", "3"),
                    a.attribute("y1", "12"),
                    a.attribute("x2", "21"),
                    a.attribute("y2", "12"),
                  ],
                  [],
                ),
                element.element(
                  "line",
                  [
                    a.attribute("x1", "3"),
                    a.attribute("y1", "18"),
                    a.attribute("x2", "21"),
                    a.attribute("y2", "18"),
                  ],
                  [],
                ),
              ],
            ),
          ],
        ),
        render_user_info(ctx, session, current_admin),
      ]),
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

fn sidebar_interaction_script() {
  h.script(
    [a.attribute("defer", "defer")],
    "
(function() {
  const sidebar = document.querySelector('[data-sidebar]');
  const overlay = document.querySelector('[data-sidebar-overlay]');
  const toggles = document.querySelectorAll('[data-sidebar-toggle]');
  const closes = document.querySelectorAll('[data-sidebar-close]');
  if (!sidebar || !overlay) return;

  const open = () => {
    sidebar.classList.remove('-translate-x-full');
    overlay.classList.remove('hidden');
    document.body.classList.add('overflow-hidden');
  };

  const close = () => {
    sidebar.classList.add('-translate-x-full');
    overlay.classList.add('hidden');
    document.body.classList.remove('overflow-hidden');
  };

  toggles.forEach((btn) => btn.addEventListener('click', () => {
    if (sidebar.classList.contains('-translate-x-full')) {
      open();
    } else {
      close();
    }
  }));

  closes.forEach((btn) => btn.addEventListener('click', close));
  overlay.addEventListener('click', close);

  window.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') close();
  });

  const syncForDesktop = () => {
    if (window.innerWidth >= 1024) {
      overlay.classList.add('hidden');
      document.body.classList.remove('overflow-hidden');
      sidebar.classList.remove('-translate-x-full');
    } else {
      sidebar.classList.add('-translate-x-full');
    }
  };

  window.addEventListener('resize', syncForDesktop);
  syncForDesktop();
})();
    ",
  )
}

fn admin_acls_from(current_admin: Option(UserLookupResult)) -> List(String) {
  case current_admin {
    option.Some(admin) -> admin.acls
    option.None -> []
  }
}
