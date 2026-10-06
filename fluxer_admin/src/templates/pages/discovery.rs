// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{DiscoveryListedGuild, DiscoveryPendingApplication},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            error_display::error_alert,
            form::csrf_input,
            media::{guild_icon_url, initials},
            page_container::page_header,
            table::empty_state,
        },
        layout::admin_layout,
    },
    utils::bigint::format_discriminator,
    utils::timestamps::format_admin_timestamp,
    utils::user_tag::user_tag,
};
use maud::{Markup, html};

fn tabs_bar(base: &str, tab: &str, pending_count: usize, listed_count: usize) -> Markup {
    let base_class = "px-4 py-2 rounded-md text-sm font-medium transition-colors";
    let active = "bg-neutral-800 text-white";
    let inactive = "bg-neutral-100 text-neutral-600 hover:bg-neutral-200";
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm mb-6" {
            div class="flex gap-2" {
                a href={(base) "/discovery?tab=pending"}
                    class={(base_class) " " (if tab == "pending" { active } else { inactive })} {
                    "Pending Applications"
                    @if tab == "pending" {
                        " (" (pending_count) ")"
                    }
                }
                a href={(base) "/discovery?tab=listed"}
                    class={(base_class) " " (if tab == "listed" { active } else { inactive })} {
                    "Listed Guilds"
                    @if tab == "listed" {
                        " (" (listed_count) ")"
                    }
                }
            }
        }
    }
}

fn category_label(category_type: Option<i32>, fallback: Option<&str>) -> String {
    match category_type {
        Some(0) => "Gaming".to_owned(),
        Some(1) => "Music".to_owned(),
        Some(2) => "Entertainment".to_owned(),
        Some(3) => "Education".to_owned(),
        Some(4) => "Science & Technology".to_owned(),
        Some(5) => "Content Creator".to_owned(),
        Some(6) => "Anime & Manga".to_owned(),
        Some(7) => "Movies & TV".to_owned(),
        Some(8) => "Other".to_owned(),
        _ => fallback.unwrap_or("Unknown").to_owned(),
    }
}

fn owner_display(
    owner_id: &str,
    username: Option<&str>,
    global_name: Option<&str>,
    discriminator: Option<&str>,
) -> String {
    let Some(username) = username else {
        return owner_id.to_owned();
    };
    let Some(discriminator) = discriminator else {
        return owner_id.to_owned();
    };
    let tag = user_tag(username, &format_discriminator(discriminator), false);
    match global_name.filter(|value| !value.trim().is_empty()) {
        Some(global_name) => format!("{global_name} ({tag})"),
        None => tag,
    }
}

fn nsfw_badge(level: Option<i32>) -> Markup {
    match level {
        Some(value) if value >= 3 => badge("Adult", BadgeVariant::Danger),
        Some(value) if value > 0 => badge("Age-restricted", BadgeVariant::Warning),
        _ => html! {},
    }
}

struct GuildHeaderData<'a> {
    base: &'a str,
    config: &'a AdminConfig,
    guild_id: &'a str,
    name: &'a str,
    icon: Option<&'a str>,
    member_count: u64,
    nsfw_level: Option<i32>,
    owner_id: &'a str,
    owner_username: Option<&'a str>,
    owner_global_name: Option<&'a str>,
    owner_discriminator: Option<&'a str>,
}

fn guild_header(data: GuildHeaderData<'_>) -> Markup {
    let icon_url = guild_icon_url(data.config, data.guild_id, data.icon, 160, true);
    let owner = owner_display(
        data.owner_id,
        data.owner_username,
        data.owner_global_name,
        data.owner_discriminator,
    );
    html! {
        div class="flex items-start gap-4" {
            @if let Some(url) = icon_url {
                img src=(url) alt=(data.name) class="h-16 w-16 flex-shrink-0 rounded-full bg-neutral-200";
            } @else {
                div class="flex h-16 w-16 flex-shrink-0 items-center justify-center \
                           rounded-full bg-neutral-200 font-medium text-base text-neutral-600" {
                    (initials(data.name))
                }
            }
            div class="min-w-0 flex-1" {
                div class="flex flex-wrap items-center gap-2" {
                    h2 class="text-lg font-semibold text-neutral-900" { (data.name) }
                    (nsfw_badge(data.nsfw_level))
                }
                p class="text-sm text-neutral-500 break-all" {
                    "ID: "
                    a href={(data.base) "/guilds/" (data.guild_id)}
                        class="hover:text-blue-600 hover:underline" {
                        (data.guild_id)
                    }
                }
                p class="text-sm text-neutral-500" {
                    "Members: " (data.member_count)
                }
                p class="text-sm text-neutral-500" {
                    "Owner: "
                    a href={(data.base) "/users/" (data.owner_id)}
                        class="hover:text-blue-600 hover:underline" {
                        (owner)
                    }
                }
            }
        }
    }
}

fn tag_list(tags: &[String]) -> Markup {
    html! {
        @if tags.is_empty() {
            p class="text-sm text-neutral-500" { "No tags" }
        } @else {
            div class="flex flex-wrap gap-1.5" {
                @for tag in tags {
                    span class="rounded bg-neutral-100 px-2 py-0.5 text-neutral-700 text-xs" {
                        (tag)
                    }
                }
            }
        }
    }
}

fn discovery_details(
    description: Option<&str>,
    category: &str,
    language: Option<&str>,
    tags: &[String],
    applied_at: Option<&str>,
    approved_at: Option<&str>,
) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
            div class="sm:col-span-2" {
                p class="text-xs text-neutral-500 mb-1 uppercase" { "Description" }
                p class="whitespace-pre-wrap break-words text-neutral-900 text-sm" {
                    (description.unwrap_or("No description"))
                }
            }
            div {
                p class="text-xs text-neutral-500 mb-1 uppercase" { "Category" }
                p class="text-sm" { (category) }
            }
            div {
                p class="text-xs text-neutral-500 mb-1 uppercase" { "Primary Language" }
                p class="text-sm" { (language.unwrap_or("Unspecified")) }
            }
            div class="sm:col-span-2" {
                p class="text-xs text-neutral-500 mb-1 uppercase" { "Tags" }
                (tag_list(tags))
            }
            div {
                p class="text-xs text-neutral-500 mb-1 uppercase" { "Applied" }
                p class="text-sm" {
                    @if let Some(applied_at) = applied_at {
                        (format_admin_timestamp(applied_at))
                    } @else {
                        "\u{2014}"
                    }
                }
            }
            @if let Some(approved_at) = approved_at {
                div {
                    p class="text-xs text-neutral-500 mb-1 uppercase" { "Approved" }
                    p class="text-sm" { (format_admin_timestamp(approved_at)) }
                }
            }
        }
    }
}

fn pending_card(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    app: &DiscoveryPendingApplication,
    can_review: bool,
) -> Markup {
    let name = app.guild_name.as_deref().unwrap_or("Unknown Guild");
    let member_count = app.guild_member_count.or(app.member_count).unwrap_or(0);
    let category = category_label(app.category_type, app.category.as_deref());
    let owner_id = app.guild_owner_id.as_deref().unwrap_or("0");

    html! {
        div class="rounded-lg bg-white border border-neutral-200 p-6" {
            div class="flex flex-col gap-4" {
                (guild_header(GuildHeaderData {
                    base,
                    config,
                    guild_id: &app.guild_id,
                    name,
                    icon: app.guild_icon.as_deref(),
                    member_count,
                    nsfw_level: app.guild_nsfw_level,
                    owner_id,
                    owner_username: app.guild_owner_username.as_deref(),
                    owner_global_name: app.guild_owner_global_name.as_deref(),
                    owner_discriminator: app.guild_owner_discriminator.as_deref(),
                }))
                (discovery_details(
                    app.description.as_deref(),
                    &category,
                    app.primary_language.as_deref(),
                    &app.custom_tags,
                    app.applied_at.as_deref(),
                    None,
                ))
                @if can_review {
                    div class="flex flex-col gap-3 border-neutral-200 border-t pt-4 \
                               sm:flex-row sm:items-start" {
                        form method="post" action={(base) "/discovery/approve"}
                            class="flex-shrink-0" {
                            (csrf_input(csrf_token))
                            input type="hidden" name="guild_id" value=(&app.guild_id);
                            button type="submit"
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg bg-neutral-900 text-white px-3 py-1.5 text-sm" {
                                "Approve"
                            }
                        }
                        form method="post" action={(base) "/discovery/reject"}
                            class="flex flex-1 flex-col gap-2 sm:flex-row" {
                            (csrf_input(csrf_token))
                            input type="hidden" name="guild_id" value=(&app.guild_id);
                            input type="text" name="reason" required minlength="1" maxlength="500"
                                placeholder="Rejection reason (required)"
                                class="flex-1 rounded-md border border-neutral-300 px-3 py-1.5 \
                                       text-sm focus:border-neutral-500 focus:outline-none";
                            button type="submit"
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg bg-red-600 text-white px-3 py-1.5 text-sm" {
                                "Reject"
                            }
                        }
                    }
                }
            }
        }
    }
}

fn listed_card(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    guild: &DiscoveryListedGuild,
    can_remove: bool,
) -> Markup {
    let name = guild.guild_name.as_deref().unwrap_or("Unknown Guild");
    let member_count = guild.guild_member_count.or(guild.member_count).unwrap_or(0);
    let category = category_label(guild.category_type, guild.category.as_deref());
    let owner_id = guild.guild_owner_id.as_deref().unwrap_or("0");

    html! {
        div class="rounded-lg bg-white border border-neutral-200 p-6" {
            div class="flex flex-col gap-4" {
                (guild_header(GuildHeaderData {
                    base,
                    config,
                    guild_id: &guild.guild_id,
                    name,
                    icon: guild.guild_icon.as_deref(),
                    member_count,
                    nsfw_level: guild.guild_nsfw_level,
                    owner_id,
                    owner_username: guild.guild_owner_username.as_deref(),
                    owner_global_name: guild.guild_owner_global_name.as_deref(),
                    owner_discriminator: guild.guild_owner_discriminator.as_deref(),
                }))
                (discovery_details(
                    guild.description.as_deref(),
                    &category,
                    guild.primary_language.as_deref(),
                    &guild.custom_tags,
                    guild.applied_at.as_deref(),
                    guild.approved_at.as_deref().or(guild.listed_at.as_deref()),
                ))
                @if can_remove {
                    form method="post" action={(base) "/discovery/remove"}
                        class="flex flex-col gap-2 border-neutral-200 border-t pt-4 sm:flex-row" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="guild_id" value=(&guild.guild_id);
                        input type="text" name="reason" required minlength="1" maxlength="500"
                            placeholder="Removal reason (required)"
                            class="flex-1 rounded-md border border-neutral-300 px-3 py-1.5 \
                                   text-sm focus:border-neutral-500 focus:outline-none";
                        button type="submit"
                            class="inline-flex items-center justify-center gap-2 font-medium \
                                   rounded-lg bg-red-600 text-white px-3 py-1.5 text-sm" {
                            "Remove from discovery"
                        }
                    }
                }
            }
        }
    }
}

pub fn discovery_page(
    config: &AdminConfig,
    auth: &AuthContext,
    tab: &str,
    csrf_token: &str,
    pending: Option<&[DiscoveryPendingApplication]>,
    listed: Option<&[DiscoveryListedGuild]>,
    load_error: Option<&str>,
) -> Markup {
    let base = &config.base_path;
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let can_review = acl::has_permission(admin_acls, acl::DISCOVERY_REVIEW);
    let can_remove = acl::has_permission(admin_acls, acl::DISCOVERY_REMOVE);
    let pending_count = pending.map(|p| p.len()).unwrap_or(0);
    let listed_count = listed.map(|l| l.len()).unwrap_or(0);
    let content = html! {
        (page_header(
            "Discovery",
            Some("Review pending discovery applications and manage currently-listed guilds."),
        ))

        (tabs_bar(base, tab, pending_count, listed_count))

        @if let Some(error) = load_error {
            (error_alert(error))
        }

        @if tab == "pending" && !can_review {
            div class="rounded-lg border border-neutral-200 bg-white p-6 text-sm text-neutral-500" {
                "You do not have permission to review discovery applications."
            }
        } @else if load_error.is_some() {
        } @else if tab == "listed" && !can_remove {
            div class="rounded-lg border border-neutral-200 bg-white p-6 text-sm text-neutral-500" {
                "You do not have permission to manage listed discovery guilds."
            }
        } @else if tab == "pending" {
            @if let Some(apps) = pending {
                @if apps.is_empty() {
                    (empty_state("No pending applications. All caught up."))
                } @else {
                    div class="flex flex-col gap-4" {
                        @for app in apps {
                            (pending_card(config, base, csrf_token, app, can_review))
                        }
                    }
                }
            } @else {
                (empty_state("No pending applications. All caught up."))
            }
        } @else {
            @if let Some(guilds) = listed {
                @if guilds.is_empty() {
                    (empty_state("No guilds currently listed in discovery."))
                } @else {
                    div class="flex flex-col gap-4" {
                        @for guild in guilds {
                            (listed_card(config, base, csrf_token, guild, can_remove))
                        }
                    }
                }
            } @else {
                (empty_state("No guilds currently listed in discovery."))
            }
        }
    };
    admin_layout(config, auth, "Discovery", "discovery", None, content)
}
