// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::AdminUser,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            detail_tabs::{build_detail_tabs, detail_tabs},
            empty_state::not_found_state,
            media::{user_avatar_url, user_banner_url},
            user_profile_badges::user_profile_badges,
        },
        layout::admin_layout,
        pages::user_detail_tabs,
    },
    utils::bigint::format_discriminator,
};
use maud::{Markup, html};

pub const USER_TABS: &[(&str, &str)] = &[
    ("overview", "Overview"),
    ("account", "Account"),
    ("guilds", "Guilds"),
    ("dm_history", "DM History"),
    ("group_dms", "Group DMs"),
    ("relationships", "Relationships"),
    ("reports", "Reports"),
    ("moderation", "Moderation"),
    ("applications", "Applications"),
    ("archives", "Archives"),
    ("audit_logs", "Audit Logs"),
];

pub fn user_detail_page(
    config: &AdminConfig,
    auth: &AuthContext,
    user: Option<&AdminUser>,
    user_id: &str,
    is_htmx: bool,
) -> Markup {
    user_detail_with_tab(config, auth, user, user_id, "overview", None, is_htmx)
}

pub fn user_detail_with_tab(
    config: &AdminConfig,
    auth: &AuthContext,
    user: Option<&AdminUser>,
    user_id: &str,
    active_tab: &str,
    tab_body: Option<Markup>,
    is_htmx: bool,
) -> Markup {
    let content = match user {
        Some(user) => render_user_detail(config, auth, user, active_tab, tab_body),
        None => not_found_state("User", user_id, None, None),
    };
    let title = user
        .map(|u| format!("{} - User Detail", u.username))
        .unwrap_or_else(|| format!("{user_id} - User Detail"));
    if is_htmx {
        content
    } else {
        admin_layout(config, auth, &title, "users", None, content)
    }
}

pub fn simple_tab_content(config: &AdminConfig, user: &AdminUser, tab: &str) -> Markup {
    match tab {
        "overview" => user_detail_tabs::overview::overview_tab(config, user, &[], "", None),
        _ => user_detail_tabs::overview::overview_tab(config, user, &[], "", None),
    }
}

fn render_user_detail(
    config: &AdminConfig,
    auth: &AuthContext,
    user: &AdminUser,
    active_tab: &str,
    tab_body: Option<Markup>,
) -> Markup {
    let display_name = user
        .global_name
        .as_deref()
        .filter(|n| !n.trim().is_empty())
        .unwrap_or(&user.username);
    let base = &config.base_path;
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let effective_tab = if user_tab_visible(config, active_tab, admin_acls) {
        active_tab
    } else {
        "overview"
    };
    let tabs = build_detail_tabs(
        USER_TABS,
        &format!("{base}/users"),
        &user.id,
        effective_tab,
        |tab_id| user_tab_visible(config, tab_id, admin_acls),
    );
    let body = tab_body.unwrap_or_else(|| simple_tab_content(config, user, effective_tab));
    let avatar_url = user_avatar_url(config, &user.id, user.avatar.as_deref(), 160, false);
    let banner_url = user_banner_url(config, &user.id, user.banner.as_deref(), 1024, false);

    html! {
        div class="mb-4" {
            a href={(base) "/users"}
                class="text-sm text-blue-600 hover:text-blue-800 hover:underline" {
                "Back to Users"
            }
        }
        @if let Some(banner) = banner_url {
            div class="mb-4 space-y-2" {
                a href=(banner) target="_blank" rel="noreferrer noopener"
                    class="block rounded-lg focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary focus-visible:ring-offset-2" {
                    img src=(banner) alt={(user.username) "'s banner"}
                        class="h-32 w-full rounded-lg border border-neutral-200 bg-neutral-50 object-cover sm:h-48"
                        loading="lazy";
                }
                @if let Some(hash) = &user.banner {
                    p class="break-all text-xs text-neutral-500" {
                        "Banner hash: " (hash)
                    }
                }
            }
        }
        div class="mb-6 flex flex-col items-center gap-4 text-center sm:flex-row sm:items-start sm:gap-6 sm:text-left" {
            img src=(avatar_url) alt={(user.username) "'s avatar"}
                class="h-20 w-20 flex-shrink-0 rounded-full";
            div class="min-w-0 flex-1 space-y-2" {
                div class="flex flex-wrap items-center justify-center gap-3 sm:justify-start" {
                    h1 class="break-words text-2xl font-semibold tracking-tight text-neutral-900" {
                        (display_name)
                    }
                    (user_profile_badges(
                        &config.static_cdn_endpoint,
                        user.flags,
                        user.premium_type,
                        user.premium_since.as_deref(),
                        config.self_hosted,
                        false,
                    ))
                }
                p class="break-words text-sm text-neutral-500" {
                    (user.username) "#" (format_discriminator(&user.discriminator))
                }
                p class="break-all text-sm text-neutral-500" {
                    (user.id)
                }
            }
        }
        div class="flex flex-col gap-6 lg:flex-row" {
            (detail_tabs(&tabs, "User detail tabs", "user-tab-content"))
            div id="user-tab-content" class="min-w-0 flex-1" {
                (body)
            }
        }
    }
}

fn user_tab_visible(_config: &AdminConfig, tab_id: &str, admin_acls: &[String]) -> bool {
    match tab_id {
        "overview" | "account" | "guilds" | "dm_history" | "group_dms" | "reports"
        | "moderation" => true,
        "relationships" => acl::has_permission(admin_acls, acl::USER_LIST_RELATIONSHIPS),
        "applications" => acl::has_permission(admin_acls, acl::APPLICATION_LIST_BY_OWNER),
        "archives" => acl::has_any_permission(
            admin_acls,
            &[acl::ARCHIVE_VIEW_ALL, acl::ARCHIVE_TRIGGER_USER],
        ),
        "audit_logs" => acl::has_permission(admin_acls, acl::AUDIT_LOG_VIEW),
        _ => false,
    }
}
