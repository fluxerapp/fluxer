// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::AdminUser,
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        data_field::{data_field_mono, data_field_muted, data_field_text, data_grid},
        media::user_avatar_url,
        user_profile_badges::user_profile_badges,
    },
    utils::{bigint::format_discriminator, timestamps::snowflake_creation_date},
};
use maud::{Markup, html};

fn premium_type_label(pt: i32) -> &'static str {
    match pt {
        0 => "None",
        1 => "Subscription",
        2 => "Lifetime",
        _ => "Unknown",
    }
}

fn status_badge(user: &AdminUser) -> Markup {
    if user.bot {
        badge("Bot", BadgeVariant::Info)
    } else if user.system {
        badge("System", BadgeVariant::Warning)
    } else {
        badge("User", BadgeVariant::Default)
    }
}

pub fn user_peek_fragment(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    premium_badge_name: Option<&str>,
) -> Markup {
    let base = &config.base_path;
    let can_view_email = acl::has_permission(admin_acls, acl::USER_VIEW_EMAIL);
    let display = user
        .global_name
        .as_deref()
        .filter(|n| !n.trim().is_empty())
        .unwrap_or(&user.username);
    let user_href = format!("{}/users/{}", base, user.id);
    let avatar_url = user_avatar_url(config, &user.id, user.avatar.as_deref(), 160, true);

    html! {
        div class="space-y-5" {
            div class="flex flex-col items-center gap-3 text-center \
                        sm:flex-row sm:items-start sm:gap-4 sm:text-left" {
                img src=(avatar_url) alt={(user.username) "'s avatar"}
                    class="h-16 w-16 flex-shrink-0 rounded-full sm:h-20 sm:w-20";
                div class="min-w-0 flex-1 space-y-1" {
                    div class="flex flex-wrap items-center justify-center gap-2 \
                                sm:justify-start" {
                        h3 class="break-words font-semibold text-base text-neutral-900" {
                            (display)
                        }
                        (user_profile_badges(
                            &config.static_cdn_endpoint,
                            user.flags,
                            user.premium_type,
                            user.premium_since.as_deref(),
                            config.self_hosted,
                            premium_badge_name,
                            true,
                        ))
                    }
                    p class="break-words text-sm text-neutral-500" {
                        (user.username) "#" (format_discriminator(&user.discriminator))
                    }
                    div class="flex flex-wrap items-center justify-center gap-2 \
                                sm:justify-start" {
                        (status_badge(user))
                    }
                }
            }

            (data_grid(2, html! {
                (data_field_mono("User ID", &user.id))
                (data_field_muted("Created", &snowflake_creation_date(&user.id)))
                @if can_view_email {
                    @if let Some(ref email) = user.email {
                        (data_field_text("Email", email))
                        (data_field_text("Email status", email_status_label(user)))
                    }
                }
                @if let Some(pt) = user.premium_type {
                    (data_field_text("Premium type", premium_type_label(pt)))
                }
                @if user.flags != 0 {
                    (data_field_mono("Flags", &user.flags.to_string()))
                }
            }))

            div class="border-t border-neutral-200 pt-4" {
                p class="mb-2 text-xs font-semibold uppercase tracking-wide text-neutral-500" {
                    "Jump to"
                }
                nav aria-label="User sections" class="grid grid-cols-2 gap-2" {
                    (peek_link(&user_href, "Overview"))
                    (peek_link(&format!("{user_href}?tab=account"), "Account"))
                    (peek_link(&format!("{user_href}?tab=guilds"), "Guilds"))
                    (peek_link(&format!("{user_href}?tab=dm_history"), "DMs"))
                    (peek_link(&format!("{user_href}?tab=reports"), "Reports"))
                    (peek_link(&format!("{user_href}?tab=moderation"), "Moderation"))
                }
            }

            a href=(user_href)
                class="inline-flex min-h-[44px] w-full items-center justify-center \
                       rounded-lg bg-neutral-900 px-4 py-2 font-medium text-sm \
                       text-white transition-colors hover:bg-neutral-800 \
                       focus:outline-none focus-visible:ring-2 \
                       focus-visible:ring-brand-primary focus-visible:ring-offset-2" {
                "Open full user page"
            }
        }
    }
}

fn peek_link(href: &str, label: &str) -> Markup {
    html! {
        a href=(href)
            class="inline-flex min-h-[44px] items-center justify-center rounded-md \
                   border border-neutral-300 px-3 py-2 font-medium text-neutral-700 \
                   text-sm transition-colors hover:border-neutral-400 \
                   hover:bg-neutral-50 focus:outline-none focus-visible:ring-2 \
                   focus-visible:ring-brand-primary focus-visible:ring-offset-2" {
            (label)
        }
    }
}

fn email_status_label(user: &AdminUser) -> &'static str {
    if user.email.is_none() {
        return "\u{2014}";
    }
    if user.email_bounced {
        "Bounced"
    } else if user.email_verified {
        "Verified"
    } else {
        "Unverified"
    }
}
