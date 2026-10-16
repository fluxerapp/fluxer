// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

pub(crate) fn format_memory_from_bytes(bytes_str: &str) -> String {
    let bytes: u64 = bytes_str.parse().unwrap_or(0);
    let mb = (bytes as f64) / 1_048_576.0;
    if mb < 1.0 {
        format!("{:.2} KB", mb * 1024.0)
    } else if mb < 1024.0 {
        format!("{:.2} MB", mb)
    } else {
        format!("{:.2} GB", mb / 1024.0)
    }
}

pub(crate) fn stat_card(label: &str, value: &str) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-neutral-50 p-4" {
            div class="mb-1 text-neutral-600 text-xs uppercase tracking-wider" { (label) }
            div class="font-semibold text-base text-neutral-900" { (value) }
        }
    }
}

pub(crate) fn node_stats_section(data: &serde_json::Value, expanded: bool, base: &str) -> Markup {
    let sessions = data.get("sessions").and_then(|v| v.as_u64()).unwrap_or(0);
    let reconnects: u64 = data
        .get("session_resumes_total")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let guilds = data.get("guilds").and_then(|v| v.as_u64()).unwrap_or(0);
    let presences = data.get("presences").and_then(|v| v.as_u64()).unwrap_or(0);
    let calls = data.get("calls").and_then(|v| v.as_u64()).unwrap_or(0);
    let total_mem = data
        .pointer("/memory/total")
        .and_then(|v| v.as_str())
        .unwrap_or("0");
    let node_count = data.get("node_count").and_then(|v| v.as_u64()).unwrap_or(1);
    let nodes = data
        .get("nodes")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    let toggle = if expanded { "collapsed" } else { "expanded" };
    let toggle_label = if expanded {
        "Show aggregate stats"
    } else {
        "Show per-node stats"
    };
    let toggle_href = format!("{base}/gateway?node_stats={toggle}");

    html! {
        div class="rounded-lg border border-neutral-200 bg-white shadow-sm p-6" {
            div class="flex flex-col gap-4" {
                div class="flex flex-wrap items-center justify-between gap-3" {
                    h2 class="text-gray-900 tracking-tight text-base" { "Gateway Statistics" }
                    a href=(toggle_href)
                        class="inline-flex items-center justify-center gap-2 font-medium \
                               rounded-lg bg-neutral-50 text-neutral-700 border \
                               border-neutral-300 px-3 py-1.5 text-sm" {
                        (toggle_label)
                    }
                }
                @if expanded && !nodes.is_empty() {
                    (node_stats_table(&nodes))
                } @else {
                    div class="grid grid-cols-2 gap-3 sm:gap-4 md:grid-cols-3 lg:grid-cols-6" {
                        (stat_card("Nodes", &node_count.to_string()))
                        (stat_card("Sessions", &sessions.to_string()))
                        (stat_card("Reconnects", &reconnects.to_string()))
                        (stat_card("Guilds", &guilds.to_string()))
                        (stat_card("Presences", &presences.to_string()))
                        (stat_card("Calls", &calls.to_string()))
                        (stat_card("Total RAM", &format_memory_from_bytes(total_mem)))
                    }
                }
            }
        }
    }
}

pub(crate) fn node_stats_table(nodes: &[serde_json::Value]) -> Markup {
    html! {
        div class="table-scroll overflow-x-auto" {
            table class="w-full" {
                thead class="border-neutral-200 border-b bg-neutral-50" {
                    tr {
                        th class="px-6 py-3 text-left text-neutral-600 text-xs uppercase" { "Node" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Sessions" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Session Resumes" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Guilds" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Presences" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Calls" }
                        th class="px-6 py-3 text-right text-neutral-600 text-xs uppercase" { "Total RAM" }
                    }
                }
                tbody class="divide-y divide-neutral-200" {
                    @for (i, node) in nodes.iter().enumerate() {
                        @let node_id = node.get("node_id").and_then(|v| v.as_str()).unwrap_or("");
                        @let label = format_node_id(node_id, i);
                        @let status = node.get("status").and_then(|v| v.as_str()).unwrap_or("-");
                        @let ns = node.get("sessions").and_then(|v| v.as_u64()).unwrap_or(0);
                        @let nsr = node.get("session_resumes_total").and_then(|v| v.as_u64()).unwrap_or(0);
                        @let ng = node.get("guilds").and_then(|v| v.as_u64()).unwrap_or(0);
                        @let np = node.get("presences").and_then(|v| v.as_u64()).unwrap_or(0);
                        @let nc = node.get("calls").and_then(|v| v.as_u64()).unwrap_or(0);
                        @let nm = node.pointer("/memory/total").and_then(|v| v.as_str()).unwrap_or("0");
                        tr class="transition-colors hover:bg-neutral-50" {
                            td class="whitespace-nowrap px-6 py-4 text-sm" {
                                div class="font-medium text-neutral-900 text-sm" { (label) }
                                div class="text-neutral-500 text-xs" { (status) }
                            }
                            td class="whitespace-nowrap px-6 py-4 text-right text-sm" { (ns) }
                            td class="whitespace-nowrap px-6 py-4 text-right text-sm" { (nsr) }
                            td class="whitespace-nowrap px-6 py-4 text-right text-sm" { (ng) }
                            td class="whitespace-nowrap px-6 py-4 text-right text-sm" { (np) }
                            td class="whitespace-nowrap px-6 py-4 text-right text-sm" { (nc) }
                            td class="whitespace-nowrap px-6 py-4 text-right font-medium text-sm" {
                                (format_memory_from_bytes(nm))
                            }
                        }
                    }
                }
            }
        }
    }
}

pub(crate) fn format_node_id(node_id: &str, index: usize) -> String {
    let trimmed = node_id.trim();
    if trimmed.is_empty() {
        return format!("Node {}", index + 1);
    }
    if let Some(pos) = trimmed.find('@') {
        let host = &trimmed[pos + 1..];
        if host == "localhost" || host == "127.0.0.1" || host == "::1" {
            return trimmed[..pos].to_owned();
        }
        let host_label = host.split('.').next().unwrap_or(host);
        if !host_label.is_empty() {
            return host_label.to_owned();
        }
    }
    trimmed.to_owned()
}

pub(crate) fn voice_state_counts_section(data: &serde_json::Value) -> Markup {
    let total = data
        .get("total_voice_states")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let regions = data
        .get("regions")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    let servers = data
        .get("servers")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    html! {
        div class="rounded-lg border border-neutral-200 bg-white shadow-sm p-6" {
            div class="flex flex-col gap-4" {
                h2 class="text-gray-900 tracking-tight text-base" { "Voice state distribution" }
                div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                    (stat_card("Active Voice States", &total.to_string()))
                    (stat_card("Tracked Regions", &regions.len().to_string()))
                    (stat_card("Tracked Servers", &servers.len().to_string()))
                }
                div class="grid grid-cols-1 gap-4 lg:grid-cols-2" {
                    (voice_count_table("By region", "Region", &regions,
                        "region_id", "voice_state_count", "No active voice regions"))
                    (voice_count_table("By server", "Server", &servers,
                        "server_id", "voice_state_count", "No active voice servers"))
                }
            }
        }
    }
}

pub(crate) fn voice_count_table(
    title: &str,
    entity_label: &str,
    rows: &[serde_json::Value],
    id_key: &str,
    count_key: &str,
    empty_label: &str,
) -> Markup {
    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200" {
            div class="border-neutral-200 border-b bg-neutral-50 px-4 py-3" {
                span class="text-sm font-semibold" { (title) }
            }
            @if rows.is_empty() {
                div class="p-4 text-neutral-600 text-sm" { (empty_label) }
            } @else {
                div class="table-scroll overflow-x-auto" {
                    table class="w-full" {
                        thead class="border-neutral-200 border-b bg-white" {
                            tr {
                                th class="px-4 py-2 text-left text-neutral-600 text-xs uppercase" {
                                    (entity_label)
                                }
                                th class="px-4 py-2 text-right text-neutral-600 text-xs uppercase" {
                                    "Voice states"
                                }
                            }
                        }
                        tbody class="divide-y divide-neutral-200" {
                            @for row in rows {
                                @let id = row.get(id_key).and_then(|v| v.as_str()).unwrap_or("-");
                                @let count = row.get(count_key).and_then(|v| v.as_u64()).unwrap_or(0);
                                tr class="hover:bg-neutral-50" {
                                    td class="whitespace-nowrap px-4 py-2 font-medium text-neutral-900 text-sm" {
                                        (id)
                                    }
                                    td class="whitespace-nowrap px-4 py-2 text-right text-sm" {
                                        (count)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
