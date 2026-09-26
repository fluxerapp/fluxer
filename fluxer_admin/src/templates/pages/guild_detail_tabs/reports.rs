// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildInfo, ReportEntry},
    config::AdminConfig,
    templates::components::{page_container::card_with_header, table::data_table},
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
                    &["Reported At", "Report ID", "Type", "Category",
                      "Reporter", "Status", "Actions"],
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

fn format_report_type(report_type: i32) -> &'static str {
    match report_type {
        0 => "Message",
        1 => "User",
        2 => "Guild",
        _ => "Unknown",
    }
}

fn format_status(status: i32) -> &'static str {
    match status {
        0 => "Pending",
        1 => "Resolved",
        _ => "Unknown",
    }
}

fn format_reporter(report: &ReportEntry) -> String {
    if let Some(ref username) = report.reporter_username {
        let disc = report.reporter_discriminator.as_deref().unwrap_or("0000");
        let tag = format!("{username}#{disc}");
        if let Some(ref gn) = report.reporter_global_name {
            let trimmed = gn.trim();
            if !trimmed.is_empty() {
                return format!("{trimmed} ({tag})");
            }
        }
        return tag;
    }
    if let Some(ref tag) = report.reporter_tag {
        return tag.clone();
    }
    if let Some(ref email) = report.reporter_email {
        return email.clone();
    }
    report
        .reporter_id
        .as_deref()
        .unwrap_or("Unknown reporter")
        .to_string()
}

fn report_row(base: &str, report: &ReportEntry) -> Markup {
    let reporter_display = format_reporter(report);
    html! {
        tr class="hover:bg-neutral-50 transition-colors" {
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (report.reported_at)
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
                a href={(base) "/reports/" (report.report_id)}
                    class="hover:underline" {
                    (report.report_id)
                }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (format_report_type(report.report_type))
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (report.category.as_deref().unwrap_or(""))
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
                @if let Some(ref rid) = report.reporter_id {
                    a href={(base) "/users/" (rid)}
                        class="hover:underline" {
                        (reporter_display)
                    }
                } @else {
                    span { (reporter_display) }
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
