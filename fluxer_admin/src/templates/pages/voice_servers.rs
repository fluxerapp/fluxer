// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::VoiceServer,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            data_field::data_field_text,
            empty_state::empty_state_full,
            error_display::error_alert,
            form::csrf_input,
            page_container::{card, page_header_full},
            voice::{voice_features_list, voice_guild_ids_list, voice_status_badges},
        },
        layout::{LayoutOptions, admin_layout_ext},
    },
};
use maud::{Markup, html};
use std::collections::HashMap;

use super::voice_servers_forms::{create_server_form, edit_server_form};

pub struct VoiceServersPageParams<'a> {
    pub region_id: Option<&'a str>,
    pub region_name: Option<&'a str>,
    pub servers: Option<&'a [VoiceServer]>,
    pub connection_counts: &'a HashMap<String, i64>,
    pub error: Option<&'a str>,
    pub csrf_token: &'a str,
}

pub fn voice_servers_page(
    config: &AdminConfig,
    auth: &AuthContext,
    p: &VoiceServersPageParams<'_>,
) -> Markup {
    let base = &config.base_path;
    let VoiceServersPageParams {
        region_id,
        region_name,
        servers,
        connection_counts,
        error,
        csrf_token,
    } = *p;
    let options = LayoutOptions {
        csrf_token,
        inspected_voice_region_id: region_id,
        ..LayoutOptions::default()
    };

    let content = match region_id {
        None => no_region_view(config),
        Some(rid) => {
            let display_name = region_name.unwrap_or(rid);
            let title = format!("Servers: {display_name}");
            let back_href = format!("{base}/voice-regions");
            html! {
                @if let Some(err) = error {
                    (error_alert(err))
                }
                (page_header_full(
                    &title,
                    None,
                    Some(html! {
                        a href="#create" {
                            button type="button"
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg transition-all duration-150 focus:outline-none \
                                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                                       hover:bg-neutral-800 w-fit px-4 py-2 text-base \
                                       focus:ring-offset-white" {
                                span { "Add Server" }
                            }
                        }
                    }),
                    Some(&back_href),
                    Some("Back to Regions"),
                    html! {},
                ))
                @if let Some(servers) = servers {
                    (servers_list(config, rid, servers, connection_counts, csrf_token))
                }
                div id="create" class="mt-8" {
                    (create_server_form(config, rid, csrf_token))
                }
            }
        }
    };

    admin_layout_ext(
        config,
        auth,
        "Voice Servers",
        "voice-servers",
        None,
        content,
        options,
    )
}

fn no_region_view(config: &AdminConfig) -> Markup {
    let base = &config.base_path;
    html! {
        (page_header_full("Voice Servers", None, None, None, None, html! {}))
        (error_alert("Please select a region first."))
        a href={(base) "/voice-regions"} {
            button type="button"
                class="inline-flex items-center justify-center gap-2 font-medium \
                       rounded-lg transition-all duration-150 focus:outline-none \
                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                       hover:bg-neutral-800 w-fit px-4 py-2 text-base \
                       focus:ring-offset-white" {
                span { "Go to Voice Regions" }
            }
        }
    }
}

fn servers_list(
    config: &AdminConfig,
    region_id: &str,
    servers: &[VoiceServer],
    connection_counts: &HashMap<String, i64>,
    csrf_token: &str,
) -> Markup {
    if servers.is_empty() {
        return empty_state_full(
            None,
            "No servers configured for this region yet.",
            Some("Add your first server to get started."),
        );
    }
    html! {
        div class="space-y-4" {
            @for server in servers {
                (server_card(config, region_id, server, connection_counts.get(&server.server_id).copied(), csrf_token))
            }
        }
    }
}

fn server_card(
    config: &AdminConfig,
    region_id: &str,
    server: &VoiceServer,
    connection_count: Option<i64>,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let is_active = server.is_active.unwrap_or(false);
    let vip_only = server.vip_only.unwrap_or(false);
    let has_features = !server.required_guild_features.is_empty();
    let has_guild_ids = !server.allowed_guild_ids.is_empty();
    let endpoint = server.endpoint.as_deref().unwrap_or("");
    let lat_str = server
        .latitude
        .map_or_else(|| "Region default".to_string(), |v| v.to_string());
    let lng_str = server
        .longitude
        .map_or_else(|| "Region default".to_string(), |v| v.to_string());
    let soft_limit_str = server
        .soft_connection_limit
        .map_or_else(|| "No limit".to_string(), |v| v.to_string());
    let connections_str =
        connection_count.map_or_else(|| "Unavailable".to_string(), |v| v.to_string());
    let at_soft_limit = matches!(
        (server.soft_connection_limit, connection_count),
        (Some(limit), Some(count)) if limit > 0 && count >= limit
    );

    card(html! {
        div class="mb-4 flex flex-col gap-1" {
            div class="flex flex-wrap items-center gap-2" {
                h3 class="font-semibold text-base text-neutral-900" { (server.server_id) }
                @if is_active {
                    (badge("ACTIVE", BadgeVariant::Success))
                } @else {
                    (badge("INACTIVE", BadgeVariant::Default))
                }
                @if at_soft_limit {
                    (badge("AT SOFT LIMIT", BadgeVariant::Warning))
                }
                (voice_status_badges(vip_only, has_features, has_guild_ids))
            }
            p class="text-sm text-neutral-500" { (endpoint) }
        }
        div class="mb-4 grid grid-cols-1 sm:grid-cols-2 gap-4" {
            (data_field_text("Region", &server.region_id))
            (data_field_text("Status", if is_active { "Active" } else { "Inactive" }))
            (data_field_text("Latitude", &lat_str))
            (data_field_text("Longitude", &lng_str))
            (data_field_text("Soft connection limit", &soft_limit_str))
            (data_field_text("Live connections", &connections_str))
        }
        (voice_features_list(&server.required_guild_features))
        (voice_guild_ids_list(&server.allowed_guild_ids))
        div class="flex flex-wrap gap-2 mt-4" {
            form method="post" action={(base) "/voice-servers?action=update"} {
                (csrf_input(csrf_token))
                input type="hidden" name="region_id" value=(region_id);
                input type="hidden" name="server_id" value=(server.server_id);
                input type="hidden" name="endpoint" value=(endpoint);
                input type="hidden" name="is_active" value=(if is_active { "false" } else { "true" });
                input type="hidden" name="vip_only" value=(if vip_only { "true" } else { "false" });
                @if is_active {
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-red-600 text-white \
                               hover:bg-red-700 px-3 py-1.5 text-sm focus:ring-offset-white" {
                        "Deactivate"
                    }
                } @else {
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg transition-all duration-150 focus:outline-none \
                               focus:ring-2 focus:ring-offset-2 bg-green-600 text-white \
                               hover:bg-green-700 px-3 py-1.5 text-sm focus:ring-offset-white" {
                        "Activate"
                    }
                }
            }
            form method="post" action={(base) "/voice-servers?action=delete"} {
                (csrf_input(csrf_token))
                input type="hidden" name="region_id" value=(region_id);
                input type="hidden" name="server_id" value=(server.server_id);
                button type="submit"
                    class="inline-flex items-center justify-center gap-2 font-medium \
                           rounded-lg transition-all duration-150 focus:outline-none \
                           focus:ring-2 focus:ring-offset-2 bg-red-600 text-white \
                           hover:bg-red-700 px-3 py-1.5 text-sm focus:ring-offset-white" {
                    "Delete"
                }
            }
        }
        details class="mt-6" {
            summary class="cursor-pointer rounded bg-blue-50 px-4 py-2 font-medium text-blue-700 text-sm transition-colors hover:bg-blue-100" {
                "Edit Server"
            }
            div class="mt-3 border-neutral-200 border-t pt-3" {
                (edit_server_form(config, region_id, server, csrf_token))
            }
        }
    })
}
