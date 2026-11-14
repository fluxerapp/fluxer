// SPDX-License-Identifier: AGPL-3.0-or-later

use super::form::Checkbox;
use maud::{Markup, html};

pub fn voice_status_badges(vip_only: bool, has_features: bool, has_guild_ids: bool) -> Markup {
    html! {
        @if vip_only {
            span class="inline-flex items-center rounded-md bg-purple-50 px-2 py-1 text-xs font-medium text-purple-700 ring-1 ring-inset ring-purple-700/10" {
                "VIP ONLY"
            }
        }
        @if has_features {
            span class="inline-flex items-center rounded-md bg-orange-50 px-2 py-1 text-xs font-medium text-orange-700 ring-1 ring-inset ring-orange-700/10" {
                "FEATURES"
            }
        }
        @if has_guild_ids {
            span class="inline-flex items-center rounded-md bg-yellow-50 px-2 py-1 text-xs font-medium text-yellow-700 ring-1 ring-inset ring-yellow-700/10" {
                "GUILD IDS"
            }
        }
    }
}

pub fn voice_features_list(features: &[String]) -> Markup {
    if features.is_empty() {
        return html! {};
    }
    html! {
        div {
            span class="font-medium text-neutral-600 text-xs" { "Required Features: " }
            span class="text-neutral-700 text-xs" { (features.join(", ")) }
        }
    }
}

pub fn voice_guild_ids_list(guild_ids: &[String]) -> Markup {
    if guild_ids.is_empty() {
        return html! {};
    }
    html! {
        div class="mt-2" {
            span class="font-medium text-neutral-600 text-xs" { "Allowed Guilds: " }
            div class="mt-1 flex flex-wrap gap-2" {
                @for guild_id in guild_ids {
                    div class="inline-flex items-center rounded-md border border-neutral-200 px-2 py-1" {
                        span class="text-neutral-700 text-xs" { (guild_id) }
                    }
                }
            }
        }
    }
}

pub fn voice_restriction_fields(
    id_prefix: &str,
    vip_only: bool,
    required_features: &str,
    allowed_guild_ids: &str,
) -> Markup {
    let features_id = if id_prefix.is_empty() {
        String::new()
    } else {
        format!("{id_prefix}-required-guild-features")
    };
    let guilds_id = if id_prefix.is_empty() {
        String::new()
    } else {
        format!("{id_prefix}-allowed-guild-ids")
    };
    html! {
        div class="space-y-3 border-neutral-200 border-t pt-3" {
            h4 class="font-medium text-neutral-700 text-sm" { "Access Restrictions" }
            (Checkbox {
                name: "vip_only",
                value: "true",
                label: "VIP Only",
                checked: vip_only,
                enabled: true,
            }.render())
            div class="space-y-1" {
                label class="block text-sm font-medium text-neutral-700" { "Required Guild Features" }
                input type="text" name="required_guild_features" value=(required_features)
                    placeholder="e.g. FEATURE_1, FEATURE_2"
                    id=[(!features_id.is_empty()).then_some(features_id.as_str())]
                    class="block w-full rounded-lg border border-neutral-300 bg-white px-3 py-2 text-sm text-neutral-900 placeholder:text-neutral-400 focus:border-brand-primary focus:outline-none focus:ring-1 focus:ring-brand-primary";
                p class="text-xs text-neutral-500" { "Separate features with commas." }
            }
            div class="space-y-1" {
                label class="block text-sm font-medium text-neutral-700" { "Allowed Guild IDs" }
                input type="text" name="allowed_guild_ids" value=(allowed_guild_ids)
                    placeholder="e.g. 123456789, 987654321"
                    id=[(!guilds_id.is_empty()).then_some(guilds_id.as_str())]
                    class="block w-full rounded-lg border border-neutral-300 bg-white px-3 py-2 text-sm text-neutral-900 placeholder:text-neutral-400 focus:border-brand-primary focus:outline-none focus:ring-1 focus:ring-brand-primary";
                p class="text-xs text-neutral-500" { "Separate guild IDs with commas." }
            }
        }
    }
}
