// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{CreateVoiceJoinTokenResponse, VoiceJoinToken, VoiceRegion, VoiceServer},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            data_field::data_field_text,
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{csrf_input, form_field_with_id},
            page_container::{card, page_header_full},
            table::data_table,
            voice::{
                voice_features_list, voice_guild_ids_list, voice_restriction_fields,
                voice_status_badges,
            },
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
    pub regions: &'a [VoiceRegion],
    pub join_tokens: Option<&'a [VoiceJoinToken]>,
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
        regions,
        join_tokens,
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
                        a href="#join" class="mr-2" {
                            button type="button"
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg transition-all duration-150 focus:outline-none \
                                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                                       hover:bg-neutral-800 w-fit px-4 py-2 text-base \
                                       focus:ring-offset-white" {
                                span { "Add a voice server" }
                            }
                        }
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
                div id="join" class="mt-8" {
                    (join_token_form(config, rid, regions, csrf_token))
                }
                @if let Some(tokens) = join_tokens {
                    div class="mt-8" {
                        (join_tokens_table(config, rid, tokens, csrf_token))
                    }
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
    let health = server
        .health
        .as_ref()
        .map_or("unknown", |h| h.status.as_str());
    let health_str = match (
        health,
        server.health.as_ref().and_then(|h| h.error.as_deref()),
    ) {
        ("up", _) => "Up".to_string(),
        ("down", Some(err)) => format!("Down ({err})"),
        ("down", None) => "Down".to_string(),
        (_, Some(err)) => format!("Not checked ({err})"),
        _ => "Not checked yet".to_string(),
    };
    let internal_str = server
        .internal_endpoint
        .clone()
        .unwrap_or_else(|| "Same as the endpoint".to_string());
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
                @if server.draining {
                    (badge("DRAINING", BadgeVariant::Warning))
                }
                @if health == "down" {
                    (badge("DOWN", BadgeVariant::Danger))
                } @else if health == "up" {
                    (badge("UP", BadgeVariant::Success))
                }
                @if server.duplicate_api_key {
                    (badge("SHARED API KEY", BadgeVariant::Danger))
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
            (data_field_text("Health", &health_str))
            (data_field_text("Draining", if server.draining { "Yes" } else { "No" }))
            (data_field_text("Internal URL", &internal_str))
        }
        @if server.duplicate_api_key {
            p class="mb-4 text-sm text-red-700" {
                "Another server uses the same API key. Fluxer checks webhooks from only one of them. Give each server its own key."
            }
        }
        @if let Some(notes) = server.notes.as_deref() {
            div class="mb-4" {
                p class="mb-1 font-medium text-neutral-500 text-xs uppercase tracking-wide" { "Notes" }
                p class="whitespace-pre-wrap break-words text-neutral-900 text-sm" { (notes) }
            }
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

const TRUST_NOTE: &str = "A voice server handles the audio and video of every call placed on it, and whoever runs it can record those calls. Only give this token to a machine you trust.";

fn join_token_form(
    config: &AdminConfig,
    region_id: &str,
    regions: &[VoiceRegion],
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    card(html! {
        h2 class="mb-1 font-semibold text-base text-neutral-900" { "Add a voice server" }
        p class="mb-4 text-sm text-neutral-600" {
            "Create a join token, then run the command it shows on the voice machine. rexctl installs LiveKit there, gets a certificate and registers the server here."
        }
        form method="post" action={(base) "/voice-servers?action=join_token"} class="space-y-4" {
            (csrf_input(csrf_token))
            input type="hidden" name="region_id" value=(region_id);
            div class="space-y-1" {
                label for="join-region" class="block text-sm font-medium text-neutral-700" { "Region" }
                select id="join-region" name="region_choice"
                    class="block w-full rounded-md border border-neutral-300 px-3 py-2 text-sm shadow-sm" {
                    option value="auto" { "Automatic, the nearest existing region" }
                    @for region in regions {
                        option value=(region.id) selected[region.id == region_id] {
                            (region.name.as_deref().unwrap_or(&region.id))
                        }
                    }
                    option value="new" { "A new region" }
                }
            }
            div class="grid grid-cols-1 gap-4 md:grid-cols-3" {
                (form_field_with_id("New region ID", "join-new-region-id", "new_region_id", "text", "", "sgp", false))
                (form_field_with_id("New region name", "join-new-region-name", "new_region_name", "text", "", "Singapore", false))
                (form_field_with_id("New region emoji", "join-new-region-emoji", "new_region_emoji", "text", "", "🇸🇬", false))
            }
            div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                (form_field_with_id("Server ID (optional)", "join-server-id", "server_id", "text", "", "fra-server-2", false))
                (form_field_with_id("Hostname (optional)", "join-hostname", "hostname", "text", "", "voice-fra.chat.example.com", false))
                (form_field_with_id("Soft connection limit (optional)", "join-soft-limit", "soft_connection_limit", "number", "", "300", false))
                (form_field_with_id("Hours until the token expires", "join-expires", "expires_hours", "number", "1", "1", false))
            }
            (voice_restriction_fields("join", false, "", ""))
            p class="text-sm text-neutral-600" { (TRUST_NOTE) }
            button type="submit"
                class="w-full inline-flex items-center justify-center gap-2 font-medium \
                       rounded-lg transition-all duration-150 focus:outline-none \
                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                       hover:bg-neutral-800 px-4 py-2 text-base focus:ring-offset-white" {
                span { "Create join token" }
            }
        }
    })
}

fn join_tokens_table(
    config: &AdminConfig,
    region_id: &str,
    tokens: &[VoiceJoinToken],
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    card(html! {
        h2 class="mb-4 font-semibold text-base text-neutral-900" { "Join tokens" }
        @if tokens.is_empty() {
            p class="text-sm text-neutral-600" { "No join tokens from the last 7 days." }
        } @else {
            (data_table(&["Token", "Status", "Region", "Server", "Expires", ""], html! {
                @for token in tokens {
                    tr {
                        td class="px-4 py-2 text-sm" { (token.token_id) }
                        td class="px-4 py-2 text-sm" {
                            @match token.status.as_str() {
                                "pending" => (badge("PENDING", BadgeVariant::Info)),
                                "used" => (badge("USED", BadgeVariant::Success)),
                                "revoked" => (badge("REVOKED", BadgeVariant::Default)),
                                _ => (badge("EXPIRED", BadgeVariant::Default)),
                            }
                        }
                        td class="px-4 py-2 text-sm" {
                            (token.region_name.as_deref().or(token.region_id.as_deref()).unwrap_or("Automatic"))
                        }
                        td class="px-4 py-2 text-sm" {
                            (token.used_server_id.as_deref().or(token.server_id.as_deref()).unwrap_or(""))
                        }
                        td class="px-4 py-2 text-sm" { (token.expires_at) }
                        td class="px-4 py-2 text-sm" {
                            @if token.status == "pending" {
                                form method="post" action={(base) "/voice-servers?action=revoke_token"} {
                                    (csrf_input(csrf_token))
                                    input type="hidden" name="region_id" value=(region_id);
                                    input type="hidden" name="token_id" value=(token.token_id);
                                    button type="submit" class="text-red-600 hover:underline" { "Revoke" }
                                }
                            }
                        }
                    }
                }
            }))
        }
    })
}

pub fn join_token_created_page(
    config: &AdminConfig,
    auth: &AuthContext,
    region_id: Option<&str>,
    created: &CreateVoiceJoinTokenResponse,
) -> Markup {
    let base = &config.base_path;
    let back = match region_id {
        Some(rid) => format!(
            "{base}/voice-servers?region_id={}",
            urlencoding::encode(rid)
        ),
        None => format!("{base}/voice-servers"),
    };
    let content = html! {
        (page_header_full("Join token created", None, None, Some(&back), Some("Back to servers"), html! {}))
        (card(html! {
            p class="mb-3 text-sm text-neutral-700" {
                "Run this on the voice machine. This page shows the token once."
            }
            pre class="mb-3 overflow-x-auto whitespace-pre-wrap break-all rounded bg-neutral-900 p-3 text-sm text-white" {
                code { (created.command) }
            }
            button type="button"
                class="mb-4 cursor-pointer text-blue-600 hover:underline"
                data-copy-value=(created.command)
                onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
                "Copy command"
            }
            p class="text-sm text-neutral-700" {
                "The token works once and expires at " (created.expires_at) "."
            }
            p class="mt-2 text-sm text-neutral-600" { (TRUST_NOTE) }
        }))
    };
    admin_layout_ext(
        config,
        auth,
        "Voice Servers",
        "voice-servers",
        None,
        content,
        LayoutOptions::default(),
    )
}
