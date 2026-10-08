// SPDX-License-Identifier: AGPL-3.0-or-later

use super::gateway_leaderboard;
use super::gateway_nodes;

use crate::utils::plural::count_noun;
use crate::{
    api::types::{GatewayVoiceStateCountsResponse, GuildMemoryStatsResponse, NodeStatsResponse},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{form::csrf_input, page_container::page_header_with_actions},
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub struct GatewayPageParams<'a> {
    pub csrf_token: &'a str,
    pub node_stats: Option<&'a NodeStatsResponse>,
    pub voice_counts: Option<&'a GatewayVoiceStateCountsResponse>,
    pub guild_stats: Option<&'a GuildMemoryStatsResponse>,
    pub reload_result: Option<u64>,
    pub node_stats_expanded: bool,
    pub leaderboard_limit: u32,
}

pub fn gateway_page(config: &AdminConfig, auth: &AuthContext, p: &GatewayPageParams<'_>) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header_with_actions(
            "Gateway",
            Some("Gateway cluster status and operations"),
            html! {
                form method="post" action={(base) "/gateway?action=reload_all"} {
                    (csrf_input(p.csrf_token))
                    button type="submit"
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg bg-neutral-900 text-white px-4 py-2 text-sm" {
                        "Reload All Guilds"
                    }
                }
            },
        ))
        @if let Some(count) = p.reload_result {
            div class="rounded-lg border p-4 bg-green-50 border-green-200 text-green-700" {
                "Successfully reloaded " (count_noun(count, "guild", "guilds")) "!"
            }
        }
        div class="space-y-6" {
            @if let Some(ns) = p.node_stats {
                (gateway_nodes::node_stats_section(&ns.data, p.node_stats_expanded, base))
            }
            @if let Some(vc) = p.voice_counts {
                (gateway_nodes::voice_state_counts_section(&vc.data))
            }
            @if let Some(gs) = p.guild_stats {
                @let guilds = gs.data.get("guilds")
                    .and_then(|v| v.as_array())
                    .cloned()
                    .unwrap_or_default();
                (gateway_leaderboard::guild_leaderboard(
                    config, base, &guilds, p.leaderboard_limit
                ))
            }
        }
    };
    admin_layout(config, auth, "Gateway", "gateway", None, content)
}
