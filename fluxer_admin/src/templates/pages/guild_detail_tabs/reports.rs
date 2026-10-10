// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildInfo, ReportEntry},
    config::AdminConfig,
    templates::{
        components::page_container::card_with_header,
        pages::user_detail_tabs::reports::{
            format_status, report_tab_table, reported_at_cell, reporter_label,
            type_and_category_cell,
        },
    },
};
use maud::{Markup, html};

pub fn reports_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    reports: &[ReportEntry],
    total: u64,
    page: u32,
) -> Markup {
    let base = &config.base_path;
    let page_size: u64 = 25;
    let has_previous = page > 0;
    let next_page = page.checked_add(1).filter(|next_page| {
        (page > 0 || !reports.is_empty()) && u64::from(*next_page) * page_size < total
    });
    html! {
        div class="space-y-4" {
            div class="flex items-center justify-between" {
                h3 class="text-base font-medium text-neutral-900" {
                    "Reports Against Guild"
                }
                p class="text-sm text-neutral-500" {
                    (total) " total"
                }
            }

            @if reports.is_empty() {
                (card_with_header("Reports", html! {
                    p class="text-sm text-neutral-500" {
                        "No reports filed against this guild."
                    }
                }))
            } @else {
                (report_tab_table(
                    &["Reported At", "Type / Category", "Reporter", "Status", "Actions"],
                    html! {
                        @for report in reports {
                            (report_row(base, report))
                        }
                    },
                ))
            }

            @if has_previous || next_page.is_some() {
                div class="flex justify-center gap-2" {
                    @if has_previous {
                        a href={(base) "/guilds/" (guild.id) "?tab=reports&reports_page=" (page - 1)}
                            class="inline-flex items-center rounded-md border border-neutral-300 \
                                   bg-white px-3 py-2 text-sm font-medium text-neutral-700 \
                                   hover:bg-neutral-50" {
                            "\u{2190} Newer"
                        }
                    }
                    @if let Some(next_page) = next_page {
                        a href={(base) "/guilds/" (guild.id) "?tab=reports&reports_page=" (next_page)}
                            class="inline-flex items-center rounded-md border border-neutral-300 \
                                   bg-white px-3 py-2 text-sm font-medium text-neutral-700 \
                                   hover:bg-neutral-50" {
                            "Older \u{2192}"
                        }
                    }
                }
            }
        }
    }
}

fn report_row(base: &str, report: &ReportEntry) -> Markup {
    html! {
        tr class="hover:bg-neutral-50 transition-colors" {
            (reported_at_cell(base, report))
            (type_and_category_cell(report))
            td class="px-2 py-3 text-sm [overflow-wrap:anywhere] [&_.whitespace-nowrap]:whitespace-normal xl:px-4 xl:[&_.whitespace-nowrap]:whitespace-nowrap" {
                @if let Some(ref rid) = report.reporter_id {
                    a href={(base) "/users/" (rid)}
                        class="hover:underline" {
                        (reporter_label(report))
                    }
                } @else {
                    span { (reporter_label(report)) }
                }
            }
            td class="hidden whitespace-nowrap px-4 py-3 text-sm text-neutral-900 xl:table-cell" {
                (format_status(report.status))
            }
            td class="whitespace-nowrap px-2 py-3 text-sm xl:px-4" {
                div class="mb-1 text-neutral-900 xl:hidden" data-status-compact=(report.report_id) {
                    (format_status(report.status))
                }
                a href={(base) "/reports/" (report.report_id)}
                    class="inline-flex items-center rounded-md border border-neutral-300 \
                           bg-white px-3 py-1.5 text-sm font-medium text-neutral-700 \
                           hover:bg-neutral-50" {
                    "View"
                }
            }
        }
    }
}
