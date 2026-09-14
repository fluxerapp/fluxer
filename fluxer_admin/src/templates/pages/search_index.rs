// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            auto_refresh::auto_refresh, form::csrf_input, page_container::page_header,
            section_card::section_card_simple,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

fn reindex_button(base: &str, csrf_token: &str, title: &str, index_type: &str) -> Markup {
    html! {
        form method="post" action={(base) "/search-index?action=reindex"} class="w-fit" {
            (csrf_input(csrf_token))
            input type="hidden" name="index_type" value=(index_type);
            button type="submit"
                class="inline-flex w-fit items-center justify-center gap-2 \
                       font-medium rounded-lg transition-all duration-150 \
                       bg-neutral-50 text-neutral-700 hover:text-neutral-900 \
                       border border-neutral-300 hover:border-neutral-400 \
                       px-4 py-2 text-sm" {
                "Reindex " (title)
            }
        }
    }
}

fn disabled_reindex_button(title: &str) -> Markup {
    html! {
        button type="button" disabled
            class="inline-flex w-fit items-center justify-center gap-2 \
                   font-medium rounded-lg bg-neutral-50 text-neutral-400 \
                   border border-neutral-200 px-4 py-2 text-sm \
                   cursor-not-allowed opacity-50" {
            "Reindex " (title)
        }
    }
}

fn reindex_controls(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Global Search Indexes",
        html! {
            div class="flex flex-col items-start gap-3" {
                (reindex_button(base, csrf_token, "Users", "users"))
                (reindex_button(base, csrf_token, "Guilds", "guilds"))
                (reindex_button(base, csrf_token, "Reports", "reports"))
                (reindex_button(base, csrf_token, "Audit Logs", "audit_logs"))

                h3 class="text-sm font-semibold text-neutral-900 mt-6" {
                    "Discovery Index"
                }
                p class="text-sm text-neutral-500 mb-3" {
                    "Rebuilds the discovery search index for all approved discoverable \
                     communities. This syncs guild metadata, descriptions, categories, \
                     and online counts."
                }
                (reindex_button(base, csrf_token, "Discovery Index", "discovery"))

                h3 class="text-sm font-semibold text-neutral-900 mt-6" {
                    "Guild-specific Search Indexes"
                }
                p class="text-sm text-neutral-500 mb-3" {
                    "These indexes require a guild ID and can only be triggered from \
                     the guild detail page."
                }
                (disabled_reindex_button("Channel Messages"))
            }
        },
    )
}

fn format_status_label(status: &str) -> &str {
    match status {
        "in_progress" => "In progress",
        "completed" => "Completed",
        "failed" => "Failed",
        "not_found" => "Preparing",
        _ => "Unknown",
    }
}

pub struct RefreshStatus<'a> {
    pub job_id: &'a str,
    pub status: Option<&'a str>,
    pub total: Option<u64>,
    pub indexed: Option<u64>,
    pub started_at: Option<&'a str>,
    pub completed_at: Option<&'a str>,
    pub error: Option<&'a str>,
}

fn status_section(base: &str, rs: &RefreshStatus) -> Markup {
    let status_str = rs.status.unwrap_or("not_found");
    let is_in_progress = status_str == "in_progress";

    section_card_simple(
        "Reindex progress",
        html! {
            div class="space-y-3" {
                div class="flex items-center justify-between" {
                    p class="text-sm text-neutral-700" {
                        "Status: " (format_status_label(status_str))
                    }
                    a href={(base) "/search-index"}
                        class="text-brand-primary text-sm hover:underline" {
                        "Clear"
                    }
                }

                @if status_str == "not_found" {
                    p class="text-sm text-neutral-700" {
                        "Preparing job... check back in a moment."
                    }
                }

                @if is_in_progress {
                    @if let (Some(idx), Some(tot)) = (rs.indexed, rs.total) {
                        @let pct = (u128::from(idx) * 100).checked_div(u128::from(tot)).unwrap_or(0);
                        div class="space-y-2" {
                            div class="flex items-center justify-between" {
                                p class="text-sm text-neutral-700" {
                                    (idx) " / " (tot) " (" (pct) "%)"
                                }
                            }
                            div class="h-2 w-full overflow-hidden rounded-full bg-neutral-200" {
                                div class="h-2 bg-neutral-900 transition-[width] duration-300"
                                    style={"width: " (pct) "%"} {}
                            }
                        }
                    }
                }

                @if status_str == "completed" {
                    @if let (Some(idx), Some(tot)) = (rs.indexed, rs.total) {
                        p class="text-sm text-neutral-700" {
                            "Indexed " (idx) " / " (tot) " items"
                        }
                    }
                }

                @if let Some(sa) = rs.started_at {
                    p class="text-xs text-neutral-500" {
                        "Started " (sa)
                    }
                }
                @if let Some(ca) = rs.completed_at {
                    p class="text-xs text-neutral-500" {
                        "Completed " (ca)
                    }
                }
                @if let Some(err) = rs.error {
                    p class="text-sm text-red-600" { (err) }
                }

                a href={(base) "/search-index"}
                    class="text-sm text-blue-600 hover:underline" {
                    "View job " (rs.job_id)
                }
            }
        },
    )
}

pub fn search_index_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    refresh: Option<&RefreshStatus>,
) -> Markup {
    let base = &config.base_path;
    let should_auto_refresh = refresh
        .map(|rs| matches!(rs.status, Some("in_progress" | "not_found")) || rs.status.is_none())
        .unwrap_or(false);

    let content = html! {
        (page_header("Search Index Management", None))
        div class="space-y-6" {
            (reindex_controls(base, csrf_token))
            @if let Some(rs) = refresh {
                (status_section(base, rs))
            }
        }
        (auto_refresh(should_auto_refresh, 3000))
    };
    admin_layout(config, auth, "Search Index", "search-index", None, content)
}
