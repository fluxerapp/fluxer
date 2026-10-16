// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildAssetItem, GuildInfo},
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{csrf_input, danger_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum GuildAssetKind {
    Emoji,
    Sticker,
}

pub fn guild_assets_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    items: &[GuildAssetItem],
    csrf_token: &str,
    kind: GuildAssetKind,
) -> Markup {
    let base = &config.base_path;
    let (title, empty) = match kind {
        GuildAssetKind::Emoji => ("Emojis", "No custom emojis found for this guild."),
        GuildAssetKind::Sticker => ("Stickers", "No stickers found for this guild."),
    };
    html! {
        (card_with_header(
            &format!("{title} ({})", items.len()),
            html! {
                @if items.is_empty() {
                    p class="text-sm text-neutral-500" {
                        (empty)
                    }
                } @else {
                    div class="grid gap-4 md:grid-cols-2 xl:grid-cols-3" {
                        @for item in items {
                            (asset_card(base, &guild.id, item, csrf_token, kind))
                        }
                    }
                }
            },
        ))
    }
}

fn asset_card(
    base: &str,
    guild_id: &str,
    item: &GuildAssetItem,
    csrf_token: &str,
    kind: GuildAssetKind,
) -> Markup {
    let sticker = matches!(kind, GuildAssetKind::Sticker);
    let (query, input_name, button_label) = match kind {
        GuildAssetKind::Emoji => (
            "?tab=emojis&action=delete_emoji",
            "emoji_id",
            "Delete Emoji",
        ),
        GuildAssetKind::Sticker => (
            "?tab=stickers&action=delete_sticker",
            "sticker_id",
            "Delete Sticker",
        ),
    };
    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200 bg-white shadow-sm" {
            div class="flex flex-col" {
                div class="flex h-32 items-center justify-center bg-neutral-100 p-6" {
                    img src=(item.media_url) alt=(item.name)
                        class="max-h-full max-w-full object-contain"
                        loading="lazy";
                }
                div class=(if sticker {"flex-1 space-y-1 px-4 py-3"} else {"flex-1 space-y-3 px-4 py-3"}) {
                    div class=(if sticker {"flex items-center justify-between gap-2"} else {"flex items-center justify-between"}) {
                        span class="text-sm font-semibold text-neutral-900" {
                            (item.name)
                        }
                        @if sticker || item.animated {
                            (badge(if item.animated {"Animated"} else {"Static"}, BadgeVariant::Default))
                        }
                    }
                    p class="break-words text-xs text-neutral-500" {
                        "ID: " (item.id)
                    }
                    a href={(base) "/users/" (item.creator_id)}
                        class="text-xs text-blue-600 hover:underline" {
                        "Uploader: " (item.creator_id)
                    }
                    form method="post"
                        action={(base) "/guilds/" (guild_id) (query)} class=[sticker.then_some("mt-4")] {
                        (csrf_input(csrf_token))
                        input type="hidden" name=(input_name) value=(item.id);
                        (danger_button(button_label))
                    }
                }
            }
        }
    }
}
