// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

pub fn empty_state_full(icon: Option<Markup>, title: &str, description: Option<&str>) -> Markup {
    html! {
        div class="flex flex-col items-center justify-center py-12 text-center" {
            @if let Some(icon_markup) = icon {
                div class="mb-3 text-neutral-400" {
                    (icon_markup)
                }
            }
            p class="text-sm font-medium text-neutral-900" {
                (title)
            }
            @if let Some(desc) = description {
                p class="mt-1 text-sm text-neutral-500" {
                    (desc)
                }
            }
        }
    }
}

pub fn not_found_state(
    entity_type: &str,
    entity_id: &str,
    back_url: Option<&str>,
    back_label: Option<&str>,
) -> Markup {
    html! {
        div class="flex flex-col items-center justify-center py-16 text-center" {
            h2 class="text-xl font-semibold text-neutral-900 mb-2" {
                (entity_type) " Not Found"
            }
            p class="text-neutral-500 text-sm mb-4" {
                "No " (entity_type.to_lowercase()) " found with ID " (entity_id)
            }
            a href=(back_url.unwrap_or("javascript:history.back()"))
                class="text-sm text-blue-600 hover:text-blue-800 hover:underline" {
                (back_label.unwrap_or("Go back"))
            }
        }
    }
}
