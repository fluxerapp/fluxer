// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildInfo, ReportEntry},
    config::AdminConfig,
    templates::{
        components::{page_container::card_with_header, table::data_table},
        pages::user_detail_tabs::reports::{
            format_status, reported_at_cell, reporter_label, type_and_category_cell,
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
                (data_table(
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
            td class="px-4 py-3 text-sm [overflow-wrap:anywhere]" {
                @if let Some(ref rid) = report.reporter_id {
                    a href={(base) "/users/" (rid)}
                        class="hover:underline" {
                        (reporter_label(report))
                    }
                } @else {
                    span { (reporter_label(report)) }
                }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (format_status(report.status))
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn guild_report_rows_format_the_date_and_wrap_long_cells() {
        let report: ReportEntry = serde_json::from_value(json!({
            "report_id": "1800000000000000006",
            "reporter_id": "1500000000000000000",
            "reporter_username": "reporter_0423a7212d56",
            "reporter_discriminator": "8650",
            "reporter_global_name": "Avery Reporter",
            "reported_at": "2026-10-05T23:23:28.067Z",
            "status": 0,
            "report_type": 0,
            "category": "spam",
            "reason": "spam",
            "reason_label": "Spam",
            "reported_guild_id": "1700000000000000800"
        }))
        .expect("valid report");
        let markup = report_row("/admin", &report).into_string();
        assert!(markup.contains("Oct 5, 2026, 11:23 PM UTC"), "{markup}");
        assert!(!markup.contains("2026-10-05T23:23:28.067Z"), "{markup}");
        assert!(
            markup
                .contains(r#"<span class="whitespace-nowrap text-neutral-500 text-xs">(reporter_0423a7212d56#8650)</span>"#),
            "{markup}"
        );
        assert!(markup.contains("Avery Reporter "), "{markup}");
        assert_eq!(
            markup.matches("whitespace-nowrap px-4").count(),
            2,
            "{markup}"
        );
        assert!(markup.contains(">Pending<"), "{markup}");
        assert!(markup.contains(">View<"), "{markup}");
        assert_eq!(markup.matches(">Spam<").count(), 1, "{markup}");
        assert!(markup.contains(r#"data-report-reason="spam""#), "{markup}");
    }
}
