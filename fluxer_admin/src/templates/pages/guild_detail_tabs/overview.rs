// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    api::types::GuildDetailInfo,
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{csrf_input, form_actions, submit_button},
        media::{guild_asset_url, guild_icon_url},
        nsfw_indicators::{adult_content_badge, channel_nsfw_state_badge, content_warning_badge},
        page_container::{card_with_header, detail_row},
    },
};
use maud::{Markup, html};

const FLUXER_EPOCH: u64 = 1_420_070_400_000;

fn current_snowflake() -> String {
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before Unix epoch")
        .as_millis() as u64;
    let offset = now_ms.saturating_sub(FLUXER_EPOCH);
    let snowflake = (offset as u128) * 4_194_304;
    snowflake.to_string()
}

fn channel_type_label(channel_type: i32) -> &'static str {
    match channel_type {
        0 => "Text",
        2 => "Voice",
        4 => "Category",
        13 => "Link",
        _ => "Unknown",
    }
}

pub fn overview_tab(config: &AdminConfig, guild: &GuildDetailInfo, csrf_token: &str) -> Markup {
    let base = &config.base_path;

    let mut sorted_channels: Vec<_> = guild.channels.iter().collect();
    sorted_channels.sort_by_key(|c| c.position);

    let channels_by_id: std::collections::HashMap<&str, &crate::api::types::GuildChannelSummary> =
        guild.channels.iter().map(|c| (c.id.as_str(), c)).collect();

    let mut sorted_roles: Vec<_> = guild.roles.iter().collect();
    sorted_roles.sort_by_key(|role| std::cmp::Reverse(role.position));

    let icon_url = guild_icon_url(config, &guild.id, guild.icon.as_deref(), 256, true);
    let banner_url = guild_asset_url(
        config,
        "banners",
        &guild.id,
        guild.banner.as_deref(),
        600,
        true,
    );
    let splash_url = guild_asset_url(
        config,
        "splashes",
        &guild.id,
        guild.splash.as_deref(),
        480,
        true,
    );
    let embed_splash_url = guild_asset_url(
        config,
        "embed-splashes",
        &guild.id,
        guild.embed_splash.as_deref(),
        480,
        true,
    );

    let snowflake = current_snowflake();

    html! {
        div class="flex flex-col gap-6 items-stretch" {
            (card_with_header("Content Rating", html! {
                div class="flex flex-col gap-4" {
                    div class="flex flex-wrap gap-2" {
                        @if guild.nsfw == Some(true) {
                            (adult_content_badge(true, Some("Adult content (18+)")))
                        } @else {
                            (badge("Not flagged adult", BadgeVariant::Default))
                        }
                        @if guild.content_warning_level == Some(1) {
                            (content_warning_badge(
                                guild.content_warning_level,
                                guild.content_warning_text.as_deref(),
                                true,
                            ))
                        } @else {
                            (badge("No content warning", BadgeVariant::Default))
                        }
                    }
                    @if guild.content_warning_level == Some(1) {
                        div class="flex flex-col gap-1" {
                            p class="text-sm font-semibold text-neutral-500" {
                                "Custom warning text"
                            }
                            @if let Some(text) = guild.content_warning_text.as_deref().filter(|text| !text.trim().is_empty()) {
                                blockquote class="border-amber-300 border-l-2 bg-amber-50 \
                                                  px-3 py-2 text-neutral-800 text-sm italic" {
                                    (text)
                                }
                            } @else {
                                p class="text-sm text-neutral-500" {
                                    "\u{2014} (default fallback shown to users)"
                                }
                            }
                        }
                    }
                }
            }))

            (card_with_header("Assets", html! {
                div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                    (asset_preview("Icon", icon_url.as_deref(), guild.icon.as_deref(), "square"))
                    (asset_preview("Banner", banner_url.as_deref(), guild.banner.as_deref(), "wide"))
                    (asset_preview("Splash", splash_url.as_deref(), guild.splash.as_deref(), "wide"))
                    (asset_preview("Embed Splash", embed_splash_url.as_deref(), guild.embed_splash.as_deref(), "wide"))
                }
            }))

            (card_with_header("Guild Information", html! {
                dl class="divide-y divide-neutral-100" {
                    (detail_row("Guild ID", html! {
                        span class="text-xs" { (guild.id) }
                    }))
                    (detail_row("Name", html! { (guild.name) }))
                    (detail_row("Member Count", html! { (guild.member_count) }))
                    (detail_row("Vanity URL", html! {
                        @if let Some(ref code) = guild.vanity_url_code {
                            (code)
                        } @else {
                            span class="text-neutral-400" { "None" }
                        }
                    }))
                    div class="flex flex-col gap-1 py-3 sm:flex-row sm:gap-4" {
                        dt class="w-48 flex-shrink-0 text-sm font-medium text-neutral-500" {
                            "Owner"
                        }
                        dd class="text-sm text-neutral-900" {
                            div class="flex flex-col gap-0.5" {
                                a href={(base) "/users/" (guild.owner_id)}
                                    class="text-neutral-900 text-sm hover:text-blue-600 \
                                           hover:underline" {
                                    (super::owner_display(guild))
                                }
                                span class="text-xs text-neutral-500" {
                                    (guild.owner_id)
                                }
                            }
                        }
                    }
                }
            }))

            (card_with_header("Features", html! {
                @if guild.features.is_empty() {
                    p class="text-sm text-neutral-500" { "No features enabled" }
                } @else {
                    div class="flex flex-wrap gap-2" {
                        @for feature in &guild.features {
                            (badge(feature, BadgeVariant::Info))
                        }
                    }
                }
            }))

            (card_with_header(&format!("Channels ({})", guild.channels.len()), html! {
                @if guild.channels.is_empty() {
                    p class="text-sm text-neutral-500" { "No channels" }
                } @else {
                    div class="flex flex-col gap-2" {
                        @for channel in &sorted_channels {
                            @let is_link = channel.channel_type == 13;
                            @let parent = channel.parent_id.as_deref()
                                .and_then(|pid| channels_by_id.get(pid));
                            @let parent_nsfw_override = parent
                                .and_then(|p| p.nsfw_override);
                            @if is_link {
                                div class="flex items-center gap-3 rounded border \
                                           border-neutral-200 bg-neutral-50 p-3 \
                                           transition-colors hover:bg-neutral-100" {
                                    div class="flex min-w-0 flex-1 flex-col gap-0" {
                                        span class="text-sm font-semibold" {
                                            (channel.name.as_deref().unwrap_or(""))
                                        }
                                        span class="text-sm text-neutral-500" {
                                            (channel.id)
                                        }
                                        @if let Some(ref url) = channel.url {
                                            a href=(url) target="_blank"
                                                rel="noopener noreferrer"
                                                class="truncate text-blue-600 text-xs \
                                                       hover:underline" {
                                                (url)
                                            }
                                        }
                                    }
                                    div class="flex flex-col items-end gap-1" {
                                        span class="text-sm text-neutral-500 text-right" {
                                            (channel_type_label(channel.channel_type))
                                        }
                                        (channel_nsfw_state_badge(
                                            channel.nsfw.unwrap_or(false),
                                            channel.nsfw_override,
                                            parent_nsfw_override,
                                            guild.nsfw,
                                            channel.content_warning_level,
                                            channel.content_warning_text.as_deref(),
                                            false,
                                        ))
                                    }
                                }
                            } @else {
                                a href={
                                    (base) "/messages?channel_id=" (channel.id)
                                    "&message_id=" (snowflake)
                                    "&context_limit=50"
                                }
                                class="flex items-center gap-3 rounded border \
                                       border-neutral-200 bg-neutral-50 p-3 \
                                       transition-colors hover:bg-neutral-100" {
                                    div class="flex flex-1 flex-col gap-0" {
                                        span class="text-sm font-semibold" {
                                            (channel.name.as_deref().unwrap_or(""))
                                        }
                                        span class="text-sm text-neutral-500" {
                                            (channel.id)
                                        }
                                    }
                                    div class="flex flex-col items-end gap-1" {
                                        span class="text-sm text-neutral-500 text-right" {
                                            (channel_type_label(channel.channel_type))
                                        }
                                        (channel_nsfw_state_badge(
                                            channel.nsfw.unwrap_or(false),
                                            channel.nsfw_override,
                                            parent_nsfw_override,
                                            guild.nsfw,
                                            channel.content_warning_level,
                                            channel.content_warning_text.as_deref(),
                                            false,
                                        ))
                                    }
                                }
                            }
                        }
                    }
                }
            }))

            (card_with_header(&format!("Roles ({})", guild.roles.len()), html! {
                @if guild.roles.is_empty() {
                    p class="text-sm text-neutral-500" { "No roles" }
                } @else {
                    div class="flex flex-col gap-2" {
                        @for role in &sorted_roles {
                            @let color_hex = format!("{:06X}", role.color);
                            div class="flex items-center gap-3 rounded border \
                                       border-neutral-200 bg-neutral-50 p-3" {
                                div style=(format!("background-color: #{color_hex}"))
                                    class="h-4 w-4 rounded" {}
                                div class="flex flex-1 flex-col gap-0" {
                                    span class="text-sm font-semibold" {
                                        (role.name)
                                    }
                                    span class="text-sm text-neutral-500" {
                                        (role.id)
                                    }
                                }
                                div class="flex gap-2" {
                                    @if role.hoist {
                                        (badge("Hoisted", BadgeVariant::Info))
                                    }
                                    @if role.mentionable {
                                        (badge("Mentionable", BadgeVariant::Success))
                                    }
                                }
                            }
                        }
                    }
                }
            }))

            (card_with_header("Search Index Management", html! {
                div class="flex flex-col gap-4" {
                    p class="text-sm text-neutral-500" {
                        "Refresh search indexes for this guild."
                    }
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=refresh_search_index"}
                        class="w-full" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="index_type" value="channel_messages";
                        input type="hidden" name="guild_id" value=(guild.id);
                        (form_actions(html! {
                            (submit_button("Refresh Channel Messages"))
                        }))
                    }
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=refresh_search_index"}
                        class="w-full" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="index_type" value="guild_members";
                        input type="hidden" name="guild_id" value=(guild.id);
                        (form_actions(html! {
                            (submit_button("Refresh Guild Members"))
                        }))
                    }
                }
            }))
        }
    }
}

fn asset_preview(label: &str, url: Option<&str>, hash: Option<&str>, variant: &str) -> Markup {
    let image_class = if variant == "square" {
        "h-24 w-24 rounded bg-neutral-100 object-cover"
    } else {
        "h-36 w-full rounded bg-neutral-100 object-cover"
    };
    let placeholder_class = if variant == "square" {
        "flex items-center justify-center rounded bg-neutral-100 text-neutral-500 text-sm h-24 w-24"
    } else {
        "flex items-center justify-center rounded bg-neutral-100 text-neutral-500 text-sm h-36 w-full"
    };
    let hash_display = hash.unwrap_or("null");
    html! {
        div class="flex flex-col gap-2 rounded-lg border border-neutral-200 bg-white p-3" {
            span class="text-sm font-semibold" { (label) }
            @if let Some(url) = url {
                a href=(url) target="_blank" rel="noreferrer noopener" class="block" {
                    img src=(url) alt={(label) " preview"} class=(image_class) loading="lazy";
                }
            } @else {
                div class=(placeholder_class) { "Not set" }
            }
            span class="break-all text-xs text-neutral-500" {
                "Hash: " (hash_display)
            }
        }
    }
}
