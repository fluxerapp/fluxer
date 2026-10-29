// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::DiscoveryGuild,
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
    utils::user_tag::display_with_tag,
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

fn category_label(category_type: i32) -> &'static str {
    match category_type {
        0 => "Gaming",
        1 => "Music",
        2 => "Entertainment",
        3 => "Education",
        4 => "Science & Technology",
        5 => "Content Creator",
        6 => "Anime & Manga",
        7 => "Movies & TV",
        8 => "Other",
        _ => "Unknown",
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
    display_with_tag(
        global_name,
        username,
        &format_discriminator(discriminator),
        false,
    )
}

fn nsfw_badge(level: Option<i32>) -> Markup {
    match level {
        Some(value) if value >= 3 => badge("Adult", BadgeVariant::Danger),
        Some(value) if value > 0 => badge("Age-restricted", BadgeVariant::Warning),
        _ => html! {},
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

fn guild_card(config: &AdminConfig, base: &str, guild: &DiscoveryGuild, actions: Markup) -> Markup {
    let name = guild.guild_name.as_str();
    let icon_url = guild_icon_url(
        config,
        &guild.guild_id,
        guild.guild_icon.as_deref(),
        160,
        true,
    );
    let owner = owner_display(
        &guild.guild_owner_id,
        guild.guild_owner_username.as_deref(),
        guild.guild_owner_global_name.as_deref(),
        guild.guild_owner_discriminator.as_deref(),
    );
    html! {
        div class="rounded-lg bg-white border border-neutral-200 p-6" {
            div class="flex flex-col gap-4" {
                div class="flex items-start gap-4" {
                    @if let Some(url) = icon_url {
                        img src=(url) alt=(name) class="h-16 w-16 flex-shrink-0 rounded-full bg-neutral-200";
                    } @else {
                        div class="flex h-16 w-16 flex-shrink-0 items-center justify-center \
                                   rounded-full bg-neutral-200 font-medium text-base text-neutral-600" {
                            (initials(name))
                        }
                    }
                    div class="min-w-0 flex-1" {
                        div class="flex flex-wrap items-center gap-2" {
                            h2 class="text-lg font-semibold text-neutral-900" { (name) }
                            (nsfw_badge(guild.guild_nsfw_level))
                        }
                        p class="text-sm text-neutral-500 break-all" {
                            "ID: "
                            a href={(base) "/guilds/" (guild.guild_id)}
                                class="hover:text-blue-600 hover:underline" {
                                (guild.guild_id)
                            }
                        }
                        p class="text-sm text-neutral-500" {
                            "Members: " (guild.guild_member_count)
                        }
                        p class="text-sm text-neutral-500" {
                            "Owner: "
                            a href={(base) "/users/" (guild.guild_owner_id)}
                                class="hover:text-blue-600 hover:underline" {
                                (owner)
                            }
                        }
                    }
                }
                div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                    div class="sm:col-span-2" {
                        p class="text-xs text-neutral-500 mb-1 uppercase" { "Description" }
                        p class="whitespace-pre-wrap break-words text-neutral-900 text-sm" {
                            (guild.description)
                        }
                    }
                    div {
                        p class="text-xs text-neutral-500 mb-1 uppercase" { "Category" }
                        p class="text-sm" { (category_label(guild.category_type)) }
                    }
                    div {
                        p class="text-xs text-neutral-500 mb-1 uppercase" { "Primary Language" }
                        p class="text-sm" { (guild.primary_language.as_deref().unwrap_or("Unspecified")) }
                    }
                    div class="sm:col-span-2" {
                        p class="text-xs text-neutral-500 mb-1 uppercase" { "Tags" }
                        (tag_list(&guild.custom_tags))
                    }
                    div {
                        p class="text-xs text-neutral-500 mb-1 uppercase" { "Applied" }
                        p class="text-sm" { (format_admin_timestamp(&guild.applied_at)) }
                    }
                    @if let Some(approved_at) = &guild.approved_at {
                        div {
                            p class="text-xs text-neutral-500 mb-1 uppercase" { "Approved" }
                            p class="text-sm" { (format_admin_timestamp(approved_at)) }
                        }
                    }
                }
                (actions)
            }
        }
    }
}

fn pending_card(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    app: &DiscoveryGuild,
    can_review: bool,
) -> Markup {
    guild_card(
        config,
        base,
        app,
        html! {
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
        },
    )
}

fn listed_card(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    guild: &DiscoveryGuild,
    can_remove: bool,
) -> Markup {
    guild_card(
        config,
        base,
        guild,
        html! {
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
                        "Remove from Explore"
                    }
                }
            }
        },
    )
}

fn discovery_disabled_page(config: &AdminConfig, auth: &AuthContext) -> Markup {
    let content = html! {
        (page_header("Explore", None))
        div class="rounded-lg border border-neutral-200 bg-white p-6 text-sm text-neutral-500" {
            "Explore is turned off on this instance. Set FLUXER_DISCOVERY_ENABLED to true to \
             review applications."
        }
    };
    admin_layout(config, auth, "Explore", "discovery", None, content)
}

pub fn discovery_page(
    config: &AdminConfig,
    auth: &AuthContext,
    tab: &str,
    csrf_token: &str,
    pending: Option<&[DiscoveryGuild]>,
    listed: Option<&[DiscoveryGuild]>,
    load_error: Option<&str>,
) -> Markup {
    if !auth.discovery_enabled() {
        return discovery_disabled_page(config, auth);
    }
    let base = &config.base_path;
    let admin_acls = auth.acls();
    let can_review = acl::has_permission(admin_acls, acl::DISCOVERY_REVIEW);
    let can_remove = acl::has_permission(admin_acls, acl::DISCOVERY_REMOVE);
    let pending_count = pending.map(|p| p.len()).unwrap_or(0);
    let listed_count = listed.map(|l| l.len()).unwrap_or(0);
    let content = html! {
        (page_header(
            "Explore",
            Some("Review pending applications to Explore and manage guilds listed in Explore."),
        ))

        (tabs_bar(base, tab, pending_count, listed_count))

        @if let Some(error) = load_error {
            (error_alert(error))
        }

        @if tab == "pending" && !can_review {
            div class="rounded-lg border border-neutral-200 bg-white p-6 text-sm text-neutral-500" {
                "You do not have permission to review applications to Explore."
            }
        } @else if load_error.is_some() {
        } @else if tab == "listed" && !can_remove {
            div class="rounded-lg border border-neutral-200 bg-white p-6 text-sm text-neutral-500" {
                "You do not have permission to manage guilds listed in Explore."
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
                    (empty_state("No guilds are listed in Explore."))
                } @else {
                    div class="flex flex-col gap-4" {
                        @for guild in guilds {
                            (listed_card(config, base, csrf_token, guild, can_remove))
                        }
                    }
                }
            } @else {
                (empty_state("No guilds are listed in Explore."))
            }
        }
    };
    admin_layout(config, auth, "Explore", "discovery", None, content)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        api::types::{InstancePremiumDiscovery, PremiumBranding},
        config::ProxyConfig,
        session::Session,
    };

    fn self_hosted_config() -> AdminConfig {
        AdminConfig {
            host: String::new(),
            port: 0,
            secret_key_base: String::new(),
            base_path: String::new(),
            api_endpoint: String::new(),
            api_public_endpoint: String::new(),
            media_endpoint: String::new(),
            static_cdn_endpoint: String::new(),
            reports_bucket_origin: String::new(),
            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: String::new(),
            self_hosted: true,
            proxy: ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: String::new(),
            },
        }
    }

    fn reviewer(well_known: &str) -> AuthContext {
        let discovery: InstancePremiumDiscovery = serde_json::from_str(well_known).unwrap();
        AuthContext {
            session: Session {
                user_id: "1".to_owned(),
                access_token: String::new(),
                created_at: 0,
            },
            admin_user: Some(
                serde_json::from_value(serde_json::json!({
                    "id": "1",
                    "username": "admin",
                    "discriminator": "0001",
                    "acls": [acl::DISCOVERY_REVIEW, acl::DISCOVERY_REMOVE],
                }))
                .unwrap(),
            ),
            pending_registration_count: None,
            branding: Some(PremiumBranding::from_discovery(&discovery)),
        }
    }

    fn render(well_known: &str) -> String {
        discovery_page(
            &self_hosted_config(),
            &reviewer(well_known),
            "pending",
            "csrf",
            Some(&[]),
            None,
            None,
        )
        .into_string()
    }

    #[test]
    fn explore_review_is_hidden_when_discovery_is_turned_off() {
        let html = render(r#"{"features":{"discovery_enabled":false}}"#);
        assert!(!html.contains("Explore review"));
        assert!(!html.contains(r#"href="/discovery?tab=pending""#));
        assert!(!html.contains("No pending applications"));
        assert!(html.contains("Explore is turned off on this instance."));
    }

    #[test]
    fn explore_review_shows_when_discovery_is_on() {
        for well_known in [r#"{"features":{"discovery_enabled":true}}"#, "{}"] {
            let html = render(well_known);
            assert!(html.contains("Explore review"));
            assert!(html.contains(r#"href="/discovery?tab=pending""#));
            assert!(html.contains("No pending applications"));
            assert!(!html.contains("Explore is turned off on this instance."));
        }
    }
}
