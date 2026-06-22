// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{GuildDetailInfo, GuildInfo},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            detail_tabs::{build_detail_tabs, detail_tabs},
            empty_state::not_found_state,
            media::{guild_icon_url, initials},
        },
        layout::admin_layout,
        pages::guild_detail_tabs,
    },
};
use maud::{Markup, PreEscaped, html};

pub const GUILD_TABS: &[(&str, &str)] = &[
    ("overview", "Overview"),
    ("members", "Members"),
    ("settings", "Settings"),
    ("features", "Features"),
    ("moderation", "Moderation"),
    ("archives", "Archives"),
    ("emojis", "Emojis"),
    ("stickers", "Stickers"),
    ("audit_logs", "Admin Audit Logs"),
    ("audit_log", "Guild Audit Log"),
    ("events", "Events"),
    ("reports", "Reports"),
];

pub fn guild_detail_with_tab(
    config: &AdminConfig,
    auth: &AuthContext,
    guild: Option<&GuildDetailInfo>,
    guild_id: &str,
    active_tab: &str,
    tab_body: Option<Markup>,
    is_htmx: bool,
) -> Markup {
    let content = match guild {
        Some(guild) => render_guild_detail(config, auth, guild, active_tab, tab_body),
        None => not_found_state("Guild", guild_id, None, None),
    };
    let title = if guild.is_some() {
        "Guild Details"
    } else {
        "Guild Not Found"
    };
    if is_htmx {
        content
    } else {
        admin_layout(config, auth, title, "guilds", None, content)
    }
}

pub fn simple_tab_content(
    config: &AdminConfig,
    guild: &GuildDetailInfo,
    tab: &str,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let guild_info = GuildInfo::from(guild.clone());
    match tab {
        "overview" => guild_detail_tabs::overview::overview_tab(config, guild, csrf_token),
        "features" => {
            guild_detail_tabs::features::features_tab(config, &guild_info, csrf_token, admin_acls)
        }
        "settings" => {
            guild_detail_tabs::settings::settings_tab(config, guild, csrf_token, admin_acls)
        }
        "moderation" => guild_detail_tabs::moderation::moderation_tab(
            config,
            &guild_info,
            csrf_token,
            admin_acls,
        ),
        "emojis" => guild_detail_tabs::emojis::emojis_tab(config, &guild_info, &[], csrf_token),
        "stickers" => {
            guild_detail_tabs::stickers::stickers_tab(config, &guild_info, &[], csrf_token)
        }
        "events" => guild_detail_tabs::events::events_tab(&[]),
        _ => guild_detail_tabs::overview::overview_tab(config, guild, csrf_token),
    }
}

fn render_guild_detail(
    config: &AdminConfig,
    auth: &AuthContext,
    guild: &GuildDetailInfo,
    active_tab: &str,
    tab_body: Option<Markup>,
) -> Markup {
    let base = &config.base_path;
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let effective_tab = if guild_tab_visible(config, active_tab, admin_acls) {
        active_tab
    } else {
        "overview"
    };
    let tabs = build_detail_tabs(
        GUILD_TABS,
        &format!("{base}/guilds"),
        &guild.id,
        effective_tab,
        |tab_id| guild_tab_visible(config, tab_id, admin_acls),
    );
    let body = tab_body
        .unwrap_or_else(|| simple_tab_content(config, guild, effective_tab, "", admin_acls));
    html! {
        div class="space-y-6" {
            a href={(base) "/guilds"}
                class="inline-flex w-fit items-center gap-2 rounded text-neutral-600 \
                       transition-colors hover:text-neutral-900 focus:outline-none \
                       focus-visible:ring-2 focus-visible:ring-brand-primary \
                       focus-visible:ring-offset-2" {
                span class="text-lg" aria-hidden="true" { (PreEscaped("&larr;")) }
                "Back to Guilds"
            }
            (render_guild_header(config, guild))
            div class="flex flex-col gap-4 lg:flex-row lg:items-start lg:gap-6" {
                (detail_tabs(&tabs, "Guild sections", "guild-tab-content"))
                div id="guild-tab-content" class="flex flex-1 flex-col gap-6 items-stretch min-w-0" {
                    (body)
                }
            }
        }
    }
}

fn render_guild_header(config: &AdminConfig, guild: &GuildDetailInfo) -> Markup {
    let icon_markup = match guild_icon_url(config, &guild.id, guild.icon.as_deref(), 160, true) {
        Some(url) => {
            html! {
                img src=(url) alt="" class="h-20 w-20 flex-shrink-0 rounded-full sm:h-24 sm:w-24";
            }
        }
        None => {
            html! {
                div class="flex h-20 w-20 flex-shrink-0 items-center justify-center \
                           rounded-full bg-neutral-200 text-center font-semibold \
                           text-base text-neutral-600 sm:h-24 sm:w-24"
                    aria-hidden="true" {
                    (initials(&guild.name))
                }
            }
        }
    };
    let owner_display = guild_detail_tabs::owner_display(guild);
    let base = &config.base_path;
    html! {
        div class="mb-6 rounded-lg border border-neutral-200 bg-white p-4 sm:p-6" {
            div class="flex flex-col items-center gap-4 text-center sm:flex-row \
                        sm:items-start sm:gap-6 sm:text-left" {
                (icon_markup)
                div class="flex flex-1 flex-col gap-3 items-stretch min-w-0" {
                    h1 class="text-gray-900 tracking-tight text-xl" {
                        (guild.name)
                    }
                    div class="grid w-full grid-cols-1 gap-4 sm:grid-cols-2 sm:gap-x-6" {
                        div class="flex flex-col gap-1 items-stretch" {
                            p class="text-sm font-medium text-neutral-500" { "Guild ID:" }
                            p class="break-all text-sm text-gray-900" { (guild.id) }
                        }
                        div class="flex flex-col gap-1 items-stretch" {
                            p class="text-sm font-medium text-neutral-500" { "Owner:" }
                            a href={(base) "/users/" (guild.owner_id)}
                                class="block break-words rounded text-sm text-neutral-900 \
                                       hover:text-blue-600 hover:underline focus:outline-none \
                                       focus-visible:ring-2 focus-visible:ring-brand-primary \
                                       focus-visible:ring-offset-2" {
                                (owner_display)
                            }
                        }
                    }
                }
            }
        }
    }
}

fn guild_tab_visible(_config: &AdminConfig, tab_id: &str, admin_acls: &[String]) -> bool {
    match tab_id {
        "overview" | "members" | "settings" | "features" | "moderation" => true,
        "reports" => acl::has_permission(admin_acls, acl::REPORT_VIEW),
        "emojis" | "stickers" => acl::has_permission(admin_acls, acl::ASSET_PURGE),
        "audit_logs" => acl::has_permission(admin_acls, acl::AUDIT_LOG_VIEW),
        "audit_log" => acl::has_permission(admin_acls, acl::GUILD_AUDIT_LOG_VIEW),
        "archives" => acl::has_any_permission(
            admin_acls,
            &[acl::ARCHIVE_VIEW_ALL, acl::ARCHIVE_TRIGGER_GUILD],
        ),
        _ => false,
    }
}
