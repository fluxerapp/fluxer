// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::GuildInfo,
    config::AdminConfig,
    templates::components::{
        form::{checkbox, csrf_input, form_actions, submit_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

const GUILD_FEATURES: &[&str] = &[
    "ANIMATED_ICON",
    "ANIMATED_BANNER",
    "AUDIO_BITRATE_128_KBPS",
    "AUDIO_BITRATE_256_KBPS",
    "AUDIO_BITRATE_384_KBPS",
    "BANNER",
    "CLONE_EMOJI_DISABLED",
    "CLONE_STICKER_DISABLED",
    "DETACHED_BANNER",
    "INVITE_SPLASH",
    "INVITES_DISABLED",
    "RAID_DETECTED",
    "TEXT_CHANNEL_FLEXIBLE_NAMES",
    "HIDE_OWNER_CROWN",
    "MORE_EMOJI",
    "MORE_STICKERS",
    "UNLIMITED_EMOJI",
    "UNLIMITED_STICKERS",
    "EXPRESSION_PURGE_ALLOWED",
    "VANITY_URL",
    "DISCOVERABLE",
    "PARTNERED",
    "VERIFIED",
    "VIP_VOICE",
    "VOICE_E2EE",
    "UNAVAILABLE_FOR_EVERYONE",
    "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF",
    "UNAVAILABLE_HIDDEN",
    "VISIONARY",
    "LARGE_GUILD_OVERRIDE",
    "VERY_LARGE_GUILD",
];

const HOSTED_ONLY: &[&str] = &["VISIONARY", "VIP_VOICE"];

pub fn features_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::GUILD_UPDATE_FEATURES);

    let filtered;
    let features_list: &[&str] = if config.self_hosted {
        filtered = filtered_features();
        filtered.as_slice()
    } else {
        GUILD_FEATURES
    };

    if !can_edit {
        return features_tab_readonly(guild, features_list);
    }

    let base = &config.base_path;
    let custom: Vec<&str> = guild
        .features
        .iter()
        .filter(|f| !features_list.contains(&f.as_str()))
        .map(String::as_str)
        .collect();
    html! {
        div class="space-y-6" {
            (card_with_header("Guild Features", html! {
                p class="mb-4 text-sm text-neutral-500" {
                    "Select which features are enabled for this guild."
                }
                form method="post" id="features-form"
                    action={(base) "/guilds/" (guild.id) "?action=update_features&tab=features"} {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        @for &feature in features_list {
                            (checkbox(
                                "features[]",
                                feature,
                                feature,
                                guild.features.iter().any(|f| f == feature),
                                true,
                            ))
                        }
                    }
                    div class="mt-6 border-t border-neutral-200 pt-6 space-y-2" {
                        label class="block text-sm font-medium text-neutral-700" {
                            "Custom Features"
                        }
                        p class="text-xs text-neutral-500 mb-2" {
                            "Enter custom feature strings separated by commas"
                        }
                        input type="text" name="custom_features"
                            placeholder="CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"
                            value=(custom.join(", "))
                            class="block w-full rounded-md border border-neutral-300 \
                                   px-3 py-2 text-sm shadow-sm \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-1 focus:ring-brand-primary";
                    }
                    div class="mt-6 border-t border-neutral-200 pt-6" {
                        (form_actions(html! {
                        (submit_button("Save Changes"))
                        }))
                    }
                }
            }))
        }
    }
}

fn features_tab_readonly(guild: &GuildInfo, features_list: &[&str]) -> Markup {
    let enabled: Vec<&str> = features_list
        .iter()
        .filter(|f| guild.features.iter().any(|g| g == **f))
        .copied()
        .collect();
    let custom: Vec<&str> = guild
        .features
        .iter()
        .filter(|f| !features_list.contains(&f.as_str()))
        .map(String::as_str)
        .collect();
    html! {
        div class="space-y-6" {
            (card_with_header("Guild Features", html! {
                p class="mb-4 text-sm text-neutral-500" {
                    "You do not have permission to edit guild features."
                }
                div class="flex flex-wrap gap-2" {
                    @for feature in &enabled {
                        span class="inline-flex items-center rounded-full bg-green-100 \
                                    px-2.5 py-0.5 text-xs font-medium text-green-800" {
                            (feature)
                        }
                    }
                    @for feature in &custom {
                        span class="inline-flex items-center rounded-full bg-blue-100 \
                                    px-2.5 py-0.5 text-xs font-medium text-blue-800" {
                            (feature)
                        }
                    }
                    @if enabled.is_empty() && custom.is_empty() {
                        p class="text-sm text-neutral-500" {
                            "No features enabled."
                        }
                    }
                }
            }))
        }
    }
}

fn filtered_features() -> Vec<&'static str> {
    GUILD_FEATURES
        .iter()
        .filter(|f| !HOSTED_ONLY.contains(f))
        .copied()
        .collect()
}
