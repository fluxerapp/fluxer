// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::form::form_field_with_id as form_field;

use crate::{
    api::types::{VoiceRegion, VoiceServer},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            data_field::data_field_text,
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{checkbox, csrf_input},
            page_container::{card, page_header_with_actions},
            voice::{
                voice_features_list, voice_guild_ids_list, voice_restriction_fields,
                voice_status_badges,
            },
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub fn voice_regions_page(
    config: &AdminConfig,
    auth: &AuthContext,
    regions: Option<&[VoiceRegion]>,
    error: Option<&str>,
    csrf_token: &str,
) -> Markup {
    let content = html! {
        @if let Some(err) = error {
            (page_header_with_actions("Voice Regions", None, html! {}))
            (error_alert(err))
        } @else if let Some(regions) = regions {
            (page_header_with_actions("Voice Regions", None, html! {
                a href="#create" {
                    button type="button"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                               hover:bg-neutral-800 w-fit px-4 py-2 text-base \
                               focus:ring-offset-white" {
                        span { "Create Region" }
                    }
                }
            }))
            (regions_list(config, regions, csrf_token))
            div id="create" {
                (create_region_form(config, csrf_token))
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Voice Regions",
        "voice-regions",
        None,
        content,
    )
}

fn regions_list(config: &AdminConfig, regions: &[VoiceRegion], csrf_token: &str) -> Markup {
    if regions.is_empty() {
        return empty_state_full(
            None,
            "No voice regions configured yet.",
            Some("Create your first region to get started."),
        );
    }
    html! {
        div class="space-y-6" {
            @for region in regions {
                (region_card(config, region, csrf_token))
            }
        }
    }
}

fn region_card(config: &AdminConfig, region: &VoiceRegion, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    let is_default = region.is_default.unwrap_or(false);
    let vip_only = region.vip_only.unwrap_or(false);
    let has_features = !region.required_guild_features.is_empty();
    let has_guild_ids = !region.allowed_guild_ids.is_empty();
    let servers = region.servers.as_deref().unwrap_or(&[]);
    let lat = region
        .latitude
        .map_or_else(|| "0".to_string(), |v| v.to_string());
    let lng = region
        .longitude
        .map_or_else(|| "0".to_string(), |v| v.to_string());
    let emoji = region.emoji.as_deref().unwrap_or("");
    let name = region.name.as_deref().unwrap_or(&region.id);

    card(html! {
        div class="flex flex-col gap-4" {
            div class="flex gap-3" {
                span class="text-3xl" { (emoji) }
                div class="flex flex-col gap-1" {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="font-semibold text-base text-neutral-900" { (name) }
                        @if is_default {
                            (badge("DEFAULT", BadgeVariant::Info))
                        }
                        (voice_status_badges(vip_only, has_features, has_guild_ids))
                    }
                    p class="text-sm text-neutral-500" { "Region ID: " (region.id) }
                }
            }
            div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4" {
                (data_field_text("Latitude", &lat))
                (data_field_text("Longitude", &lng))
                (data_field_text("Servers", &servers.len().to_string()))
            }
            (voice_features_list(&region.required_guild_features))
            (voice_guild_ids_list(&region.allowed_guild_ids))
            @if !servers.is_empty() {
                div class="border-neutral-200 border-t pt-4" {
                    h4 class="mb-2 font-semibold text-sm text-neutral-900" { "Servers" }
                    div class="flex flex-col gap-2" {
                        @for server in servers {
                            (server_row(server))
                        }
                    }
                }
            }
            div class="flex flex-wrap gap-2" {
                form method="post" action={(base) "/voice-regions?action=delete"} {
                    (csrf_input(csrf_token))
                    input type="hidden" name="id" value=(region.id);
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-red-600 text-white \
                               hover:bg-red-700 px-3 py-1.5 text-sm focus:ring-offset-white" {
                        "Delete Region"
                    }
                }
                a href={(base) "/voice-servers?region_id=" (region.id)} {
                    button type="button"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-neutral-50 text-neutral-700 \
                               hover:text-neutral-900 border border-neutral-300 \
                               hover:border-neutral-400 px-3 py-1.5 text-sm" {
                        "Manage Servers"
                    }
                }
            }
            details {
                summary class="cursor-pointer rounded bg-blue-50 px-4 py-2 font-medium text-blue-700 text-sm transition-colors hover:bg-blue-100" {
                    "Edit Region"
                }
                div class="border-neutral-200 border-t pt-3 mt-3" {
                    (edit_region_form(config, region, csrf_token))
                }
            }
        }
    })
}

fn server_row(server: &VoiceServer) -> Markup {
    let is_active = server.is_active.unwrap_or(false);
    let endpoint = server.endpoint.as_deref().unwrap_or("");
    let coords = match (server.latitude, server.longitude) {
        (Some(lat), Some(lng)) => format!("Coordinates: {lat}, {lng}"),
        _ => "Coordinates: Region default".to_string(),
    };
    html! {
        div class="flex flex-col gap-1 rounded bg-neutral-50 p-3" {
            div class="flex flex-wrap items-center gap-2" {
                span class="text-sm font-semibold text-neutral-900" { (server.server_id) }
                @if is_active {
                    (badge("ACTIVE", BadgeVariant::Success))
                } @else {
                    (badge("INACTIVE", BadgeVariant::Default))
                }
            }
            p class="text-xs text-neutral-500" { (endpoint) }
            p class="text-xs text-neutral-500" { (coords) }
        }
    }
}

fn edit_region_form(config: &AdminConfig, region: &VoiceRegion, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    let name = region.name.as_deref().unwrap_or("");
    let emoji = region.emoji.as_deref().unwrap_or("");
    let lat = region
        .latitude
        .map_or_else(|| "0".to_string(), |v| v.to_string());
    let lng = region
        .longitude
        .map_or_else(|| "0".to_string(), |v| v.to_string());
    let is_default = region.is_default.unwrap_or(false);
    let vip_only = region.vip_only.unwrap_or(false);
    let features_csv = region.required_guild_features.join(", ");
    let guild_ids_csv = region.allowed_guild_ids.join(", ");
    html! {
        div class="rounded-lg bg-neutral-50 p-4" {
            form method="post" action={(base) "/voice-regions?action=update"} {
                (csrf_input(csrf_token))
                input type="hidden" name="id" value=(region.id);
                div class="flex flex-col gap-4" {
                    div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                        (form_field("Region Name", "region-name", "name", "text", name, "Display name for the region", false))
                        (form_field("Emoji", "region-emoji", "emoji", "text", emoji, "Flag or emoji for the region", false))
                        (form_field("Latitude", "region-latitude", "latitude", "number", &lat, "Geographic latitude", false))
                        (form_field("Longitude", "region-longitude", "longitude", "number", &lng, "Geographic longitude", false))
                    }
                    (checkbox("is_default", "true", "Set as default region", is_default, true))
                    (voice_restriction_fields("edit", vip_only, &features_csv, &guild_ids_csv))
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-green-600 text-white \
                               hover:bg-green-700 w-fit px-4 py-2 text-base \
                               focus:ring-offset-white" {
                        span { "Update Region" }
                    }
                }
            }
        }
    }
}

fn create_region_form(config: &AdminConfig, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    card(html! {
        div class="flex flex-col gap-4" {
            h2 class="font-semibold text-base text-neutral-900" { "Create Voice Region" }
            form method="post" action={(base) "/voice-regions?action=create"} {
                (csrf_input(csrf_token))
                div class="flex flex-col gap-4" {
                    div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                        (form_field("Region ID", "new-region-id", "id", "text", "", "us-east", true))
                        (form_field("Region Name", "new-region-name", "name", "text", "", "US East", true))
                        (form_field("Emoji", "new-region-emoji", "emoji", "text", "", "Flag emoji", true))
                        (form_field("Latitude", "new-region-latitude", "latitude", "number", "", "40.7128", true))
                        (form_field("Longitude", "new-region-longitude", "longitude", "number", "", "-74.0060", true))
                    }
                    (checkbox("is_default", "true", "Set as default region", false, true))
                    (voice_restriction_fields("create", false, "", ""))
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                               hover:bg-neutral-800 w-fit px-4 py-2 text-base \
                               focus:ring-offset-white" {
                        span { "Create Region" }
                    }
                }
            }
        }
    })
}
