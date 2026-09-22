// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{ReportEntry, SearchReportsResponse},
    config::AdminConfig,
    templates::components::{
        media::{guild_icon_url, initials, user_avatar_url},
        page_container::card_with_header,
        table::data_table,
    },
};
use maud::{Markup, html};

pub fn reports_tab(
    config: &AdminConfig,
    user_id: &str,
    sent: Option<&SearchReportsResponse>,
    received: Option<&SearchReportsResponse>,
    sent_page: u32,
    received_page: u32,
    limit: u32,
) -> Markup {
    let base = &config.base_path;
    html! {
        div class="space-y-6" {
            (report_section(
                config,
                base,
                user_id,
                "sent",
                "Reports Sent by User",
                "No reports sent by this user.",
                sent,
                sent_page,
                received_page,
                limit,
            ))
            (report_section(
                config,
                base,
                user_id,
                "received",
                "Reports Against User",
                "No reports against this user.",
                received,
                received_page,
                sent_page,
                limit,
            ))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn report_section(
    config: &AdminConfig,
    base: &str,
    user_id: &str,
    kind: &str,
    title: &str,
    empty_msg: &str,
    data: Option<&SearchReportsResponse>,
    current_page: u32,
    other_page: u32,
    limit: u32,
) -> Markup {
    let (reports, total) = match data {
        Some(d) => (&d.reports, d.total),
        None => {
            return card_with_header(
                title,
                html! {
                    p class="text-sm text-neutral-500" {
                        "Failed to load reports."
                    }
                },
            );
        }
    };

    let offset = u64::from(current_page) * u64::from(limit);
    let has_previous = current_page > 0;
    let next_page = current_page
        .checked_add(1)
        .filter(|_| (reports.len() as u64) < total.saturating_sub(offset));

    let (sent_page, received_page) = if kind == "sent" {
        (current_page, other_page)
    } else {
        (other_page, current_page)
    };

    html! {
        (card_with_header(title, html! {
            div class="space-y-4" {
                p class="text-sm text-neutral-500" {
                    (total) " total"
                }

                @if reports.is_empty() {
                    p class="text-sm text-neutral-500" { (empty_msg) }
                } @else {
                    @let headers = if kind == "sent" {
                        vec!["Reported At", "Report ID", "Type", "Category",
                             "Reported Entity", "Status", "Actions"]
                    } else {
                        vec!["Reported At", "Report ID", "Type", "Category",
                             "Reporter", "Status", "Actions"]
                    };
                    (data_table(
                        &headers,
                        html! {
                            @for report in reports {
                                (report_row(config, base, kind, report))
                            }
                        },
                    ))
                }

                @if has_previous || next_page.is_some() {
                    div class="flex justify-center gap-2" {
                        @if has_previous {
                            a href={(base) "/users/" (user_id) "?tab=reports&reports_limit=" (limit) "&reports_sent_page=" (if kind == "sent" { current_page - 1 } else { sent_page }) "&reports_received_page=" (if kind == "received" { current_page - 1 } else { received_page })}
                                class="inline-flex items-center rounded-md border \
                                       border-neutral-300 bg-white px-3 py-2 text-sm \
                                       font-medium text-neutral-700 hover:bg-neutral-50" {
                                "Previous"
                            }
                        }
                        @if let Some(next_page) = next_page {
                            a href={(base) "/users/" (user_id) "?tab=reports&reports_limit=" (limit) "&reports_sent_page=" (if kind == "sent" { next_page } else { sent_page }) "&reports_received_page=" (if kind == "received" { next_page } else { received_page })}
                                class="inline-flex items-center rounded-md border \
                                       border-neutral-300 bg-white px-3 py-2 text-sm \
                                       font-medium text-neutral-700 hover:bg-neutral-50" {
                                "Next"
                            }
                        }
                    }
                }
            }
        }))
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

fn report_row(config: &AdminConfig, base: &str, kind: &str, report: &ReportEntry) -> Markup {
    let entity_display = if kind == "sent" {
        format_reported_entity(report)
    } else {
        format_reporter(report)
    };

    let entity_href = if kind == "sent" {
        report
            .reported_user_id
            .as_ref()
            .map(|id| format!("{base}/users/{id}"))
            .or_else(|| {
                report
                    .reported_guild_id
                    .as_ref()
                    .map(|id| format!("{base}/guilds/{id}"))
            })
    } else {
        report
            .reporter_id
            .as_ref()
            .map(|id| format!("{base}/users/{id}"))
    };

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
                @if let Some(href) = entity_href {
                    a href=(href)
                        class="inline-flex items-center gap-2 hover:underline" {
                        @if kind == "sent" {
                            (reported_entity_icon(config, report, &entity_display))
                        }
                        span { (entity_display) }
                    }
                } @else {
                    span { (entity_display) }
                }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (format_status(report.status))
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
                a href={(base) "/reports/" (report.report_id)}
                    class="inline-flex items-center rounded-md border \
                           border-neutral-300 bg-white px-3 py-1.5 text-sm \
                           font-medium text-neutral-700 hover:bg-neutral-50" {
                    "View"
                }
            }
        }
    }
}

fn format_reported_entity(report: &ReportEntry) -> String {
    if let Some(ref username) = report.reported_user_username {
        let disc = report
            .reported_user_discriminator
            .as_deref()
            .unwrap_or("0000");
        let tag = format!("{username}#{disc}");
        if let Some(ref gn) = report.reported_user_global_name {
            let trimmed = gn.trim();
            if !trimmed.is_empty() {
                return format!("{trimmed} ({tag})");
            }
        }
        return tag;
    }
    if let Some(ref tag) = report.reported_user_tag {
        return tag.clone();
    }
    if let Some(ref id) = report.reported_user_id {
        return id.clone();
    }
    report
        .reported_guild_name
        .as_ref()
        .or(report.reported_guild_id.as_ref())
        .cloned()
        .unwrap_or_else(|| "Unknown".to_owned())
}

fn reported_entity_icon(config: &AdminConfig, report: &ReportEntry, label: &str) -> Markup {
    if let Some(ref user_id) = report.reported_user_id {
        return html! {
            img
                src=(user_avatar_url(config, user_id, report.reported_user_avatar_hash.as_deref(), 80, true))
                alt=""
                class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
        };
    }
    if let Some(ref guild_id) = report.reported_guild_id {
        if let Some(url) = guild_icon_url(
            config,
            guild_id,
            report.reported_guild_icon_hash.as_deref(),
            80,
            true,
        ) {
            return html! {
                img src=(url) alt="" class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
            };
        }
        return html! {
            span class="flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-neutral-200 text-neutral-600 text-xs" {
                (initials(label))
            }
        };
    }
    html! {}
}
