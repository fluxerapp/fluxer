// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::plural::count_noun;
use crate::{
    api::types::GuildInfo,
    config::AdminConfig,
    templates::components::{
        media::{guild_icon_url, initials},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

const MAX_GUILDS: usize = 200;

pub fn guilds_tab(config: &AdminConfig, user_id: &str, guilds: &[GuildInfo]) -> Markup {
    let count = guilds.len();
    let title = if count >= MAX_GUILDS {
        format!("Guilds ({count}+)")
    } else {
        format!("Guilds ({count})")
    };
    html! {
        (card_with_header(&title, html! {
            @if guilds.is_empty() {
                p class="py-8 text-center text-sm text-neutral-500" {
                    "This user is not a member of any guilds."
                }
            } @else {
                div class="divide-y divide-neutral-100" {
                    @for guild in guilds {
                        (guild_row(config, user_id, guild))
                    }
                }
            }
        }))
    }
}

fn guild_row(config: &AdminConfig, user_id: &str, guild: &GuildInfo) -> Markup {
    let base = &config.base_path;
    let is_owner = guild.owner_id == user_id;
    html! {
        a href={(base) "/guilds/" (guild.id)}
            class="flex items-center gap-3 py-3 transition-colors hover:bg-neutral-50 -mx-6 px-6" {
            @if let Some(icon_url) = guild_icon_url(
                config, &guild.id, guild.icon.as_deref(), 80, true,
            ) {
                img src=(icon_url) alt="" class="h-10 w-10 flex-shrink-0 rounded-full";
            } @else {
                div class="flex h-10 w-10 flex-shrink-0 items-center justify-center \
                           rounded-full bg-neutral-200 text-xs font-medium text-neutral-600" {
                    (initials(&guild.name))
                }
            }
            div class="min-w-0 flex-1" {
                div class="flex items-center gap-2" {
                    span class="truncate text-sm font-medium text-neutral-900" { (guild.name) }
                    @if is_owner {
                        span class="flex-shrink-0 rounded bg-yellow-100 px-1.5 py-0.5 \
                                    text-xs font-medium text-yellow-700" {
                            "Owner"
                        }
                    }
                }
                span class="text-xs text-neutral-500" {
                    (count_noun(guild.member_count, "member", "members"))
                }
            }
        }
    }
}
