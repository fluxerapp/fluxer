// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::GuildScheduledEventItem,
    templates::components::{
        badge::{BadgeVariant, badge},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

pub fn events_tab(events: &[GuildScheduledEventItem]) -> Markup {
    html! {
        (card_with_header(
            &format!("Scheduled Events ({})", events.len()),
            html! {
                @if events.is_empty() {
                    p class="text-sm text-neutral-500" {
                        "No scheduled events found for this guild."
                    }
                } @else {
                    div class="overflow-x-auto" {
                        table class="min-w-full divide-y divide-neutral-200 text-sm" {
                            thead class="bg-neutral-50" {
                                tr {
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Name" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Status" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Type" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Start Time" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "RSVPs" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Creator" }
                                    th class="px-4 py-3 text-left font-medium text-neutral-600" { "Image" }
                                }
                            }
                            tbody class="divide-y divide-neutral-100 bg-white" {
                                @for event in events {
                                    (event_row(event))
                                }
                            }
                        }
                    }
                }
            },
        ))
    }
}

fn event_row(event: &GuildScheduledEventItem) -> Markup {
    let status_variant = match event.status.as_str() {
        "ACTIVE" => BadgeVariant::Success,
        "COMPLETED" => BadgeVariant::Default,
        "CANCELLED" => BadgeVariant::Danger,
        _ => BadgeVariant::Default,
    };
    html! {
        tr class="hover:bg-neutral-50" {
            td class="px-4 py-3 font-medium text-neutral-900" {
                p { (event.name) }
                @if let Some(description) = &event.description {
                    p class="text-xs text-neutral-500 mt-0.5 max-w-xs truncate" { (description) }
                }
            }
            td class="px-4 py-3" { (badge(&event.status, status_variant)) }
            td class="px-4 py-3 text-neutral-600" { (event.entity_type) }
            td class="px-4 py-3 text-neutral-600 whitespace-nowrap" { (event.scheduled_start_time) }
            td class="px-4 py-3 text-neutral-600" { (event.user_count) }
            td class="px-4 py-3 text-neutral-600 font-mono text-xs" { (event.creator_id) }
            td class="px-4 py-3" {
                @if let Some(image_url) = &event.image_url {
                    img src=(image_url) alt="Event cover" class="h-10 w-16 rounded object-cover" loading="lazy";
                } @else {
                    span class="text-xs text-neutral-400" { "ג€”" }
                }
            }
        }
    }
}
