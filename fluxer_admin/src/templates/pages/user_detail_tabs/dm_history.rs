// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::pagination::pager_with_missing;

use crate::{
    api::types::DmChannel, config::AdminConfig,
    templates::components::page_container::card_with_header,
};
use maud::{Markup, html};

pub fn dm_history_tab(
    config: &AdminConfig,
    user_id: &str,
    channels: &[DmChannel],
    before: Option<&str>,
    after: Option<&str>,
    limit: usize,
) -> Markup {
    let base = &config.base_path;
    html! {
        div class="space-y-6" {
            (card_with_header(
                &format!("DM History ({})", channels.len()),
                html! {
                    div class="space-y-4" {
                        p class="text-sm text-neutral-500" {
                            "Historical one-to-one DMs for this user. Group DMs are not \
                             included in this dataset."
                        }
                        @if channels.is_empty() {
                            p class="text-sm text-neutral-500" {
                                "No historical DM channels found."
                            }
                        } @else {
                            div class="space-y-4" {
                                @for channel in channels {
                                    (dm_channel_card(base, user_id, channel))
                                }
                            }
                        }
                    }
                },
            ))

            (dm_pagination(base, user_id, channels, before, after, limit))
        }
    }
}

fn format_channel_type(ct: Option<i32>) -> &'static str {
    match ct {
        Some(1) => "DM",
        Some(3) => "Group DM",
        _ => "Unknown",
    }
}

fn counterparty_label(channel: &DmChannel, user_id: &str) -> String {
    let others: Vec<_> = channel
        .recipients
        .iter()
        .filter(|r| r.id != user_id)
        .collect();
    if others.is_empty() {
        return format!("DM {}", channel.channel_id);
    }
    others
        .iter()
        .map(|r| super::resolved_user_display(r))
        .collect::<Vec<_>>()
        .join(", ")
}

fn dm_channel_card(base: &str, user_id: &str, channel: &DmChannel) -> Markup {
    let label = counterparty_label(channel, user_id);
    let others: Vec<_> = channel
        .recipients
        .iter()
        .filter(|r| r.id != user_id)
        .collect();

    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200 bg-white \
                    transition-colors hover:border-neutral-300" {
            div class="p-5" {
                div class="flex flex-col gap-4 lg:flex-row lg:items-center lg:justify-between" {
                    div class="min-w-0 flex-1" {
                        div class="mb-2 flex items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" {
                                (label)
                            }
                            span class="rounded bg-neutral-100 px-2 py-0.5 text-xs \
                                        uppercase text-neutral-700" {
                                (format_channel_type(channel.channel_type))
                            }
                        }
                        div class="space-y-1" {
                            p class="text-sm text-neutral-500" {
                                "Status: " @if channel.is_open { "Open" } @else { "Closed" }
                            }
                            p class="text-sm text-neutral-500" {
                                "Last Message ID: "
                                (channel.last_message_id.as_deref().unwrap_or("None"))
                            }
                            @if !others.is_empty() {
                                p class="text-sm text-neutral-600" {
                                    "Counterparty: "
                                    @for (i, r) in others.iter().enumerate() {
                                        a href={(base) "/users/" (r.id)}
                                            class="transition-colors hover:text-blue-600 \
                                                   hover:underline" {
                                            (super::resolved_user_display(r))
                                        }
                                        @if i < others.len() - 1 { ", " }
                                    }
                                }
                            }
                        }
                    }
                    a href={(base) "/messages?channel_id=" (channel.channel_id) "&context_limit=50"}
                        class="inline-flex items-center rounded-md bg-brand-primary px-4 py-2 \
                               text-sm font-medium text-white shadow-sm \
                               hover:bg-brand-primary-dark" {
                        "View Channel"
                    }
                }
            }
        }
    }
}

fn dm_pagination(
    base: &str,
    user_id: &str,
    channels: &[DmChannel],
    before: Option<&str>,
    after: Option<&str>,
    limit: usize,
) -> Markup {
    let has_next = channels.len() == limit;
    let has_previous = before.is_some() || after.is_some();
    let first_id = channels.first().map(|c| c.channel_id.as_str());
    let last_id = channels.last().map(|c| c.channel_id.as_str());

    if !has_previous && !has_next {
        return html! {};
    }

    pager_with_missing(
        first_id.filter(|_| has_previous).map(|id| {
            format!("{base}/users/{user_id}?tab=dm_history&dm_limit={limit}&dm_after={id}")
        }),
        None,
        last_id.filter(|_| has_next).map(|id| {
            format!("{base}/users/{user_id}?tab=dm_history&dm_limit={limit}&dm_before={id}")
        }),
        true,
    )
}
