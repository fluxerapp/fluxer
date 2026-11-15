// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::plural::count_noun;
use crate::{
    api::types::DmChannel, config::AdminConfig,
    templates::components::page_container::card_with_header,
};
use maud::{Markup, html};

pub fn group_dm_tab(config: &AdminConfig, user_id: &str, channels: &[DmChannel]) -> Markup {
    let base = &config.base_path;
    html! {
        (card_with_header(
            &format!("Group DMs ({})", channels.len()),
            html! {
                @if channels.is_empty() {
                    p class="text-sm text-neutral-500" {
                        "No group DM channels found."
                    }
                } @else {
                    div class="space-y-4" {
                        @for channel in channels {
                            (group_dm_card(base, user_id, channel))
                        }
                    }
                }
            },
        ))
    }
}

fn format_group_dm_name(channel: &DmChannel, user_id: &str) -> String {
    if let Some(ref name) = channel.name
        && !name.is_empty()
    {
        return name.clone();
    }
    let others: Vec<_> = channel
        .recipients
        .iter()
        .filter(|r| r.id != user_id)
        .collect();
    if others.is_empty() {
        return "Empty Group".to_string();
    }
    others
        .iter()
        .map(|r| {
            r.global_name
                .as_deref()
                .filter(|gn| !gn.trim().is_empty())
                .unwrap_or(&r.username)
                .to_string()
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn group_dm_card(base: &str, user_id: &str, channel: &DmChannel) -> Markup {
    let display_name = format_group_dm_name(channel, user_id);
    let owner = channel
        .owner_id
        .as_ref()
        .and_then(|oid| channel.recipients.iter().find(|r| r.id == *oid));

    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200 bg-white \
                    transition-colors hover:border-neutral-300" {
            div class="p-5" {
                div class="flex items-center gap-4" {
                    div class="flex-shrink-0" {
                        div class="flex h-12 w-12 items-center justify-center rounded-full \
                                   bg-neutral-200 text-sm font-medium text-neutral-600" {
                            (channel.recipients.len())
                        }
                    }
                    div class="min-w-0 flex-1" {
                        div class="mb-2 flex items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" {
                                (display_name)
                            }
                            span class="rounded bg-purple-100 px-2 py-0.5 text-xs \
                                        uppercase text-purple-700" {
                                "Group DM"
                            }
                        }
                        div class="space-y-1" {
                            p class="text-sm text-neutral-500" {
                                (count_noun(channel.recipients.len() as u64, "recipient", "recipients")) " \u{00b7} Status: "
                                @if channel.is_open { "Open" } @else { "Closed" }
                            }
                            @if let Some(o) = owner {
                                p class="text-sm text-neutral-500" {
                                    "Owner: "
                                    a href={(base) "/users/" (o.id)}
                                        class="transition-colors hover:text-blue-600 \
                                               hover:underline" {
                                        (super::resolved_user_display(o))
                                    }
                                }
                            }
                            @if !channel.recipients.is_empty() {
                                p class="text-sm text-neutral-600" {
                                    "Recipients: "
                                    @for (i, r) in channel.recipients.iter().enumerate() {
                                        a href={(base) "/users/" (r.id)}
                                            class="transition-colors hover:text-blue-600 \
                                                   hover:underline" {
                                            (super::resolved_user_display(r))
                                        }
                                        @if i < channel.recipients.len() - 1 { ", " }
                                    }
                                }
                            }
                        }
                    }
                    a href={(base) "/messages?channel_id=" (channel.channel_id) "&context_limit=50"}
                        class="inline-flex items-center rounded-md bg-brand-primary px-4 \
                               py-2 text-sm font-medium text-white shadow-sm \
                               hover:bg-brand-primary-dark" {
                        "View Messages"
                    }
                }
            }
        }
    }
}
