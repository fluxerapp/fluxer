// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::VoiceServer,
    config::AdminConfig,
    templates::components::{
        form::{checkbox, csrf_input},
        page_container::card,
        voice::voice_restriction_fields,
    },
};
use maud::{Markup, html};

pub fn edit_server_form(
    config: &AdminConfig,
    region_id: &str,
    server: &VoiceServer,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let id_prefix = format!("voice-server-{}-{}", region_id, server.server_id).replace(
        |c: char| !c.is_ascii_alphanumeric() && c != '-' && c != '_',
        "_",
    );
    let endpoint = server.endpoint.as_deref().unwrap_or("");
    let lat_val = server.latitude.map_or_else(String::new, |v| v.to_string());
    let lng_val = server.longitude.map_or_else(String::new, |v| v.to_string());
    let is_active = server.is_active.unwrap_or(false);
    let soft_limit_val = server
        .soft_connection_limit
        .map_or_else(String::new, |v| v.to_string());
    let vip_only = server.vip_only.unwrap_or(false);
    let features_csv = server.required_guild_features.join(", ");
    let guild_ids_csv = server.allowed_guild_ids.join(", ");

    html! {
        div class="rounded-lg bg-neutral-50 p-4" {
            form method="post" action={(base) "/voice-servers?action=update"} class="space-y-3" {
                (csrf_input(csrf_token))
                input type="hidden" name="region_id" value=(region_id);
                input type="hidden" name="server_id" value=(server.server_id);
                (form_field_with_id("Endpoint", "server-endpoint", "endpoint", "url", endpoint, "wss://livekit.example.com", false))
                div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                    (form_field_with_helper(
                        "Latitude",
                        &format!("{id_prefix}-latitude"),
                        "latitude",
                        "number",
                        &lat_val,
                        "Leave empty to use region coordinate",
                        "Optional per-server coordinate override",
                    ))
                    (form_field_with_helper(
                        "Longitude",
                        &format!("{id_prefix}-longitude"),
                        "longitude",
                        "number",
                        &lng_val,
                        "Leave empty to use region coordinate",
                        "Optional per-server coordinate override",
                    ))
                }
                (form_field_with_helper(
                    "Soft Connection Limit",
                    &format!("{id_prefix}-soft-connection-limit"),
                    "soft_connection_limit",
                    "number",
                    &soft_limit_val,
                    "Leave empty for no limit",
                    "Placement prefers another server once this server holds this many connections",
                ))
                (form_field_with_helper(
                    "API Key",
                    &format!("{id_prefix}-api-key"),
                    "api_key",
                    "text",
                    "",
                    "Leave blank to keep current",
                    "LiveKit API key (leave blank to keep unchanged)",
                ))
                (form_field_with_helper(
                    "API Secret",
                    &format!("{id_prefix}-api-secret"),
                    "api_secret",
                    "password",
                    "",
                    "Leave blank to keep current",
                    "LiveKit API secret (leave blank to keep unchanged)",
                ))
                div class="space-y-2" {
                    (checkbox("is_active", "true", "Server is active", is_active, true))
                }
                (voice_restriction_fields(&id_prefix, vip_only, &features_csv, &guild_ids_csv))
                button type="submit"
                    class="w-full inline-flex items-center justify-center gap-2 font-medium \
                           rounded-lg transition-all duration-150 focus:outline-none \
                           focus:ring-2 focus:ring-offset-2 bg-green-600 text-white \
                           hover:bg-green-700 px-4 py-2 text-base focus:ring-offset-white" {
                    span { "Update Server" }
                }
            }
        }
    }
}

pub fn create_server_form(config: &AdminConfig, region_id: &str, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    card(html! {
        h2 class="mb-4 font-semibold text-base text-neutral-900" { "Add Voice Server" }
        form method="post" action={(base) "/voice-servers?action=create"} class="space-y-4" {
            (csrf_input(csrf_token))
            input type="hidden" name="region_id" value=(region_id);
            div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                (form_field_with_id("Server ID", "new-server-id", "server_id", "text", "", "livekit-us-east-1", true))
                (form_field_with_id("Endpoint", "new-server-endpoint", "endpoint", "url", "", "wss://livekit.example.com", true))
                (form_field_with_id("API Key", "new-server-api-key", "api_key", "text", "", "LiveKit API key", true))
                (form_field_with_id("API Secret", "new-server-api-secret", "api_secret", "password", "", "LiveKit API secret", true))
                (form_field_with_id("Latitude (optional)", "new-server-latitude", "latitude", "number", "", "40.7128", false))
                (form_field_with_id("Longitude (optional)", "new-server-longitude", "longitude", "number", "", "-74.0060", false))
                (form_field_with_helper(
                    "Soft Connection Limit (optional)",
                    "new-server-soft-connection-limit",
                    "soft_connection_limit",
                    "number",
                    "",
                    "Leave empty for no limit",
                    "Placement prefers another server once this server holds this many connections",
                ))
            }
            div class="space-y-3" {
                (checkbox("is_active", "true", "Server is active", true, true))
            }
            (voice_restriction_fields("create", false, "", ""))
            button type="submit"
                class="w-full inline-flex items-center justify-center gap-2 font-medium \
                       rounded-lg transition-all duration-150 focus:outline-none \
                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                       hover:bg-neutral-800 px-4 py-2 text-base focus:ring-offset-white" {
                span { "Add Server" }
            }
        }
    })
}

fn form_field_with_id(
    label: &str,
    id: &str,
    name: &str,
    input_type: &str,
    value: &str,
    placeholder: &str,
    required: bool,
) -> Markup {
    html! {
        div class="space-y-1" {
            label for=(id) class="block text-sm font-medium text-neutral-700" {
                (label)
            }
            input type=(input_type) id=(id) name=(name) value=(value)
                placeholder=(placeholder) required[required]
                step=[if input_type == "number" { Some("any") } else { None }]
                class="block w-full rounded-md border border-neutral-300 \
                       px-3 py-2 text-sm shadow-sm \
                       focus:border-brand-primary focus:outline-none \
                       focus:ring-1 focus:ring-brand-primary";
        }
    }
}

fn form_field_with_helper(
    label: &str,
    id: &str,
    name: &str,
    input_type: &str,
    value: &str,
    placeholder: &str,
    helper: &str,
) -> Markup {
    html! {
        div class="space-y-1" {
            label for=(id) class="block text-sm font-medium text-neutral-700" {
                (label)
            }
            input type=(input_type) id=(id) name=(name) value=(value)
                placeholder=(placeholder)
                step=[if input_type == "number" { Some("any") } else { None }]
                class="block w-full rounded-md border border-neutral-300 \
                       px-3 py-2 text-sm shadow-sm \
                       focus:border-brand-primary focus:outline-none \
                       focus:ring-1 focus:ring-brand-primary";
            p class="text-xs text-neutral-500" { (helper) }
        }
    }
}
