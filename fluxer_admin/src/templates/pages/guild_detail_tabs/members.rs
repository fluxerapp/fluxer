// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildInfo, GuildMember, ListGuildMembersResponse},
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{csrf_input, danger_button},
        media::user_avatar_url,
        page_container::card_with_header,
    },
    utils::bigint::format_discriminator,
};
use maud::{Markup, html};

pub fn members_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    response: &ListGuildMembersResponse,
    page: u32,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let limit = response.limit;
    let total = response.total;
    let total_pages = if limit > 0 { total.div_ceil(limit) } else { 1 };
    let has_previous = page > 0;
    let next_page = page
        .checked_add(1)
        .filter(|page| u64::from(*page) < total_pages);

    html! {
        div class="space-y-4" {
            div class="flex items-center justify-between" {
                h3 class="text-base font-medium text-neutral-900" {
                    "Guild Members (" (total) ")"
                }
                p class="text-sm text-neutral-500" {
                    @if response.members.is_empty() {
                        "Showing 0 of " (total)
                    } @else {
                        @let start = u128::from(response.offset) + 1;
                        @let end = std::cmp::min(
                            u128::from(response.offset) + response.members.len() as u128,
                            u128::from(total),
                        );
                        "Showing " (start) "-" (end) " of " (total)
                    }
                }
            }

            @if response.members.is_empty() {
                (card_with_header("Members", html! {
                    p class="text-sm text-neutral-500" { "No members found." }
                }))
            } @else {
                div class="space-y-2" {
                    @for member in &response.members {
                        (member_card(config, base, &guild.id, member, csrf_token))
                    }
                }
            }

            @if total_pages > 1 {
                div class="mt-4 flex items-center justify-between border-t pt-4" {
                    @if has_previous {
                        a href={(base) "/guilds/" (guild.id) "?tab=members&page=" (page - 1)}
                            class="inline-flex items-center rounded-md bg-brand-primary px-3 \
                                   py-2 text-sm font-medium text-white hover:bg-brand-primary-dark" {
                            "\u{2190} Previous"
                        }
                    } @else {
                        span class="inline-flex items-center rounded-md border border-neutral-200 \
                                    bg-neutral-50 px-3 py-2 text-sm font-medium text-neutral-400" {
                            "\u{2190} Previous"
                        }
                    }
                    p class="text-sm text-neutral-500" {
                        "Page " (u64::from(page) + 1) " of " (total_pages)
                    }
                    @if let Some(next_page) = next_page {
                        a href={(base) "/guilds/" (guild.id) "?tab=members&page=" (next_page)}
                            class="inline-flex items-center rounded-md bg-brand-primary px-3 \
                                   py-2 text-sm font-medium text-white hover:bg-brand-primary-dark" {
                            "Next \u{2192}"
                        }
                    } @else {
                        span class="inline-flex items-center rounded-md border border-neutral-200 \
                                    bg-neutral-50 px-3 py-2 text-sm font-medium text-neutral-400" {
                            "Next \u{2192}"
                        }
                    }
                }
            }
        }
    }
}

fn member_card(
    config: &AdminConfig,
    base: &str,
    guild_id: &str,
    member: &GuildMember,
    csrf_token: &str,
) -> Markup {
    let disc = format_discriminator(&member.user.discriminator);
    let display = match &member.user.global_name {
        Some(gn) if !gn.trim().is_empty() => {
            format!("{} ({}#{})", gn, member.user.username, disc)
        }
        _ => format!("{}#{}", member.user.username, disc),
    };
    let user_url = format!("{base}/users/{}", member.user.id);
    let avatar_url = user_avatar_url(
        config,
        &member.user.id,
        member.user.avatar.as_deref(),
        160,
        true,
    );

    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 transition-colors hover:border-neutral-300" {
            div class="flex items-center gap-4" {
                a href=(user_url) class="flex-shrink-0" {
                    img src=(avatar_url) alt=(member.user.username)
                        class="h-16 w-16 rounded-full bg-neutral-200";
                }
                div class="min-w-0 flex-1" {
                    div class="mb-1 flex items-center gap-2" {
                        a href=(user_url) class="hover:underline" {
                            h2 class="text-base font-medium text-neutral-900" {
                                (display)
                            }
                        }
                        @if member.user.bot {
                            (badge("Bot", BadgeVariant::Info))
                        }
                        @if let Some(ref nick) = member.nick {
                            span class="ml-2 text-sm text-neutral-500" {
                                "(" (nick) ")"
                            }
                        }
                    }
                    div class="space-y-0.5" {
                        p class="text-sm text-neutral-500" {
                            "ID: " (member.user.id)
                        }
                        p class="text-sm text-neutral-500" {
                            "Joined: " (member.joined_at)
                        }
                        @if !member.roles.is_empty() {
                            p class="text-sm text-neutral-500" {
                                (member.roles.len()) " roles"
                            }
                        }
                    }
                }
                div class="flex flex-wrap gap-2 justify-end" {
                    form method="post"
                        action={(base) "/guilds/" (guild_id) "?tab=members&action=ban_member"} {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(member.user.id);
                        (danger_button("Ban"))
                    }
                    form method="post"
                        action={(base) "/guilds/" (guild_id) "?tab=members&action=kick_member"} {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(member.user.id);
                        button type="submit"
                            class="inline-flex items-center rounded-md border border-neutral-300 \
                                   bg-white px-3 py-1.5 text-sm font-medium text-neutral-700 \
                                   hover:bg-neutral-50" {
                            "Kick"
                        }
                    }
                }
            }
        }
    }
}
