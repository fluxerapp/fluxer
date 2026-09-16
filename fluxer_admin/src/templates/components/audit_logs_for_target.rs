// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::AuditLogEntry,
    templates::pages::audit_logs_table::{
        action_badge_variant, admin_user_cell, details_row, format_action,
    },
};
use maud::{Markup, html};

use super::{
    badge::badge,
    table::{
        empty_state, table, table_body, table_cell, table_container, table_head, table_header_cell,
        table_row,
    },
};

const PAGE_SIZE: u32 = 50;

fn log_row(base_path: &str, log: &AuditLogEntry, index: usize) -> Markup {
    let action = &log.action;
    let expanded_id = format!("audit-expanded-{index}");
    let has_details = log.audit_log_reason.is_some() || !log.metadata.is_empty();

    html! {
        (table_row(html! {
            (table_cell(true, html! {
                span class="text-sm whitespace-nowrap" {
                    (log.created_at)
                }
            }))
            (table_cell(false, badge(&format_action(action), action_badge_variant(action))))
            (table_cell(false, admin_user_cell(base_path, log)))
            (table_cell(true, html! {
                @if has_details {
                    button type="button"
                        class="text-sm text-neutral-600 hover:text-neutral-900 underline"
                        onclick={
                            "document.getElementById('" (expanded_id) "').classList.toggle('hidden')"
                        } {
                        "Show details"
                    }
                } @else {
                    span class="text-sm text-neutral-500 italic" { "-" }
                }
            }))
        }))
        @if has_details {
            tr id=(expanded_id) class="hidden bg-neutral-50" {
                td colspan="4" class="px-6 py-4" {
                    (details_row(base_path, log))
                }
            }
        }
    }
}

pub fn audit_logs_for_target(
    base_path: &str,
    entries: &[AuditLogEntry],
    target_id: &str,
    current_page: u32,
    total: u64,
    tab_href_base: &str,
) -> Markup {
    let has_previous = current_page > 0;
    let offset = (current_page as u64) * (PAGE_SIZE as u64);
    let next_page = current_page
        .checked_add(1)
        .filter(|_| offset + (entries.len() as u64) < total);
    let total_pages = total.div_ceil(u64::from(PAGE_SIZE)).max(1);
    let page_number = u64::from(current_page) + 1;
    let all_logs_href = format!(
        "{base_path}/audit-logs?target_id={}&access=write",
        urlencoding::encode(target_id)
    );

    html! {
        div class="rounded-lg bg-white transition-all border border-neutral-200 p-6" {
            div class="flex flex-col gap-4" {
                div class="flex flex-wrap items-center justify-between gap-3" {
                    h2 class="text-gray-900 tracking-tight text-base" { "Audit Logs" }
                    div class="flex items-center gap-3" {
                        span class="text-sm text-neutral-500" { (total) " total" }
                        a href=(all_logs_href)
                            class="inline-flex items-center justify-center gap-2 font-medium \
                                   rounded-lg transition-all duration-150 focus:outline-none \
                                   focus:ring-2 focus:ring-offset-2 bg-neutral-50 text-neutral-700 \
                                   hover:text-neutral-900 border border-neutral-300 \
                                   hover:border-neutral-400 px-3 py-1.5 text-sm" {
                            "View in Audit Logs"
                        }
                    }
                }
                @if entries.is_empty() {
                    (empty_state("No admin write actions have been recorded against this entity."))
                } @else {
                    (table_container(html! {
                        (table(html! {
                            (table_head(html! {
                                tr {
                                    (table_header_cell("Timestamp"))
                                    (table_header_cell("Action"))
                                    (table_header_cell("Admin"))
                                    (table_header_cell("Details"))
                                }
                            }))
                            (table_body(html! {
                                @for (i, entry) in entries.iter().enumerate() {
                                    (log_row(base_path, entry, i))
                                }
                            }))
                        }))
                    }))
                }
                @if has_previous || next_page.is_some() {
                    div class="flex items-center justify-center gap-2" {
                        @if has_previous {
                            a href={(tab_href_base) "&audit_logs_page=" (current_page - 1)}
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg bg-neutral-50 text-neutral-700 border \
                                       border-neutral-300 px-3 py-1.5 text-sm" {
                                "Previous"
                            }
                        }
                        span class="text-sm text-neutral-500" {
                            "Page " (page_number) " of " (total_pages)
                        }
                        @if let Some(page) = next_page {
                            a href={(tab_href_base) "&audit_logs_page=" (page)}
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg bg-neutral-50 text-neutral-700 border \
                                       border-neutral-300 px-3 py-1.5 text-sm" {
                                "Next"
                            }
                        }
                    }
                }
            }
        }
    }
}
