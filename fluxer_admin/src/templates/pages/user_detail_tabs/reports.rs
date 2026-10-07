// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{ReportEntry, SearchReportsResponse},
    config::AdminConfig,
    templates::components::{
        media::{guild_icon_url, initials, user_avatar_url},
        page_container::card_with_header,
        report_category::{reason_repeats_category, report_category},
        report_webhook::{reported_webhook, webhook_identity},
        table::data_table,
    },
    utils::{timestamps::format_admin_timestamp, user_tag::user_tag},
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
                        vec!["Reported At", "Type / Category", "Reported Entity", "Status"]
                    } else {
                        vec!["Reported At", "Type / Category", "Reporter", "Status"]
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

pub(crate) fn format_status(status: i32) -> &'static str {
    match status {
        0 => "Pending",
        1 => "Resolved",
        _ => "Unknown",
    }
}

pub(crate) fn tagged_user(
    global_name: Option<&str>,
    username: &str,
    discriminator: Option<&str>,
) -> Markup {
    let tag = user_tag(username, discriminator.unwrap_or("0000"), false);
    html! {
        @if let Some(name) = global_name.map(str::trim).filter(|name| !name.is_empty()) {
            (name) " "
            span class="whitespace-nowrap text-neutral-500 text-xs" { "(" (tag) ")" }
        } @else {
            span class="whitespace-nowrap" { (tag) }
        }
    }
}

pub(crate) fn reporter_label(report: &ReportEntry) -> Markup {
    if let Some(ref username) = report.reporter_username {
        return tagged_user(
            report.reporter_global_name.as_deref(),
            username,
            report.reporter_discriminator.as_deref(),
        );
    }
    if let Some(ref tag) = report.reporter_tag {
        return html! { span class="whitespace-nowrap" { (tag) } };
    }
    html! {
        (report
            .reporter_email
            .as_deref()
            .or(report.reporter_id.as_deref())
            .unwrap_or("Unknown reporter"))
    }
}

pub(crate) fn reported_at_cell(base: &str, report: &ReportEntry) -> Markup {
    html! {
        td class="px-3 py-3 text-sm text-neutral-900" {
            div { (format_admin_timestamp(&report.reported_at)) }
            a href={(base) "/reports/" (report.report_id)}
                class="whitespace-nowrap font-mono text-blue-600 text-xs tracking-tight hover:underline" {
                (report.report_id)
            }
        }
    }
}

pub(crate) fn type_and_category_cell(report: &ReportEntry) -> Markup {
    let reason_label = report
        .reason
        .as_deref()
        .map(|reason| report.reason_label.as_deref().unwrap_or(reason));
    let repeats = report
        .category
        .as_deref()
        .zip(reason_label)
        .is_some_and(|(category, label)| reason_repeats_category(category, label));
    html! {
        td class="px-4 py-3 text-sm text-neutral-900" {
            div class="text-neutral-500 text-xs" { (format_report_type(report.report_type)) }
            @if let Some(category) = &report.category {
                div class="break-words" data-report-reason=[report.reason.as_deref().filter(|_| repeats)] {
                    (report_category(category))
                }
            }
            @if !repeats && let (Some(reason), Some(label)) = (&report.reason, reason_label) {
                div class="break-words text-neutral-500 text-xs" data-report-reason=(reason) {
                    (label)
                }
            }
        }
    }
}

fn report_row(config: &AdminConfig, base: &str, kind: &str, report: &ReportEntry) -> Markup {
    let webhook = if kind == "sent" {
        reported_webhook(config, report)
    } else {
        None
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

    let entity_display = if kind == "sent" {
        format_reported_entity(report)
    } else {
        reporter_label(report)
    };

    html! {
        tr class="hover:bg-neutral-50 transition-colors" {
            (reported_at_cell(base, report))
            (type_and_category_cell(report))
            td class="px-4 py-3 text-sm [overflow-wrap:anywhere]" {
                @if let Some(webhook) = &webhook {
                    a href=(webhook.reports_href) class="hover:underline" {
                        (webhook_identity(webhook))
                    }
                } @else if let Some(href) = entity_href {
                    a href=(href)
                        class="inline-flex items-center gap-2 hover:underline" {
                        @if kind == "sent" {
                            (reported_entity_icon(config, report))
                        }
                        span class="min-w-0" { (entity_display) }
                    }
                } @else {
                    span { (entity_display) }
                }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (format_status(report.status))
            }
        }
    }
}

fn format_reported_entity(report: &ReportEntry) -> Markup {
    if let Some(ref username) = report.reported_user_username {
        return tagged_user(
            report.reported_user_global_name.as_deref(),
            username,
            report.reported_user_discriminator.as_deref(),
        );
    }
    if let Some(ref tag) = report.reported_user_tag {
        return html! { span class="whitespace-nowrap" { (tag) } };
    }
    html! {
        (report
            .reported_user_id
            .as_ref()
            .or(report.reported_guild_name.as_ref())
            .or(report.reported_guild_id.as_ref())
            .map_or("Unknown", String::as_str))
    }
}

fn reported_entity_icon(config: &AdminConfig, report: &ReportEntry) -> Markup {
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
                (initials(report.reported_guild_name.as_deref().unwrap_or(guild_id)))
            }
        };
    }
    html! {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{ProxyConfig, RuntimeEnv};
    use serde_json::json;

    fn test_config() -> AdminConfig {
        AdminConfig {
            env: RuntimeEnv::Test,
            host: String::new(),
            port: 0,
            secret_key_base: "test-secret".to_owned(),
            base_path: "/admin".to_owned(),
            api_endpoint: String::new(),
            media_endpoint: "https://media.example.test".to_owned(),
            static_cdn_endpoint: String::new(),
            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: "test".to_owned(),
            self_hosted: false,
            proxy: ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: String::new(),
            },
        }
    }

    #[test]
    fn sent_webhook_reports_name_the_webhook_instead_of_the_guild() {
        let report: ReportEntry = serde_json::from_value(json!({
            "report_id": "1800000000000000004",
            "reporter_id": "1500000000000000000",
            "reported_at": "2026-10-04T10:00:00.000Z",
            "status": 0,
            "report_type": 0,
            "category": "spam",
            "reported_user_id": null,
            "reported_webhook_id": "1700000000000000500",
            "reported_webhook_name": "Harbor Bulletin",
            "reported_guild_id": "1700000000000000800",
            "reported_guild_name": "Harbor"
        }))
        .expect("valid report");
        let config = test_config();
        let sent = report_row(&config, "/admin", "sent", &report).into_string();
        assert!(
            sent.contains(r#"href="/admin/reports?reported_webhook_id=1700000000000000500""#),
            "{sent}"
        );
        assert!(
            sent.contains(r#"data-report-webhook="1700000000000000500""#),
            "{sent}"
        );
        assert!(sent.contains("Harbor Bulletin"), "{sent}");
        assert!(!sent.contains("/admin/guilds/"), "{sent}");
        let received = report_row(&config, "/admin", "received", &report).into_string();
        assert!(!received.contains("data-report-webhook"), "{received}");
        assert!(
            received.contains(r#"href="/admin/users/1500000000000000000""#),
            "{received}"
        );
    }

    #[test]
    fn report_rows_label_the_category_and_keep_the_key() {
        let config = test_config();
        let report: ReportEntry = serde_json::from_value(json!({
            "report_id": "1800000000000000005",
            "reporter_id": "1500000000000000000",
            "reported_at": "2026-10-04T10:00:00.000Z",
            "status": 0,
            "report_type": 1,
            "category": "inappropriate_profile",
            "reported_user_id": "1500000000000000001",
            "reason": "harassment",
            "reason_label": "Harassment or bullying"
        }))
        .expect("valid report");
        for kind in ["sent", "received"] {
            let markup = report_row(&config, "/admin", kind, &report).into_string();
            assert!(
                markup.contains(r#"data-report-category="inappropriate_profile""#),
                "{markup}"
            );
            assert!(markup.contains(">Inappropriate profile<"), "{markup}");
            assert!(!markup.contains(">inappropriate_profile<"), "{markup}");
            assert!(
                markup.contains(r#"data-report-reason="harassment""#),
                "{markup}"
            );
            assert!(markup.contains("Oct 4, 2026, 10:00 AM UTC"), "{markup}");
            assert!(!markup.contains("2026-10-04T10:00:00.000Z"), "{markup}");
            assert_eq!(
                markup
                    .matches(r#"href="/admin/reports/1800000000000000005""#)
                    .count(),
                1,
                "{markup}"
            );
            assert!(!markup.contains(">View<"), "{markup}");
        }
        let mut unknown = report.clone();
        unknown.category = Some("future_value".to_owned());
        let markup = report_row(&config, "/admin", "sent", &unknown).into_string();
        assert!(markup.contains(">future_value<"), "{markup}");
    }

    #[test]
    fn report_rows_skip_a_reason_that_repeats_the_category() {
        let config = test_config();
        let report: ReportEntry = serde_json::from_value(json!({
            "report_id": "1800000000000000007",
            "reporter_id": "1500000000000000000",
            "reporter_username": "reporter_0423a7212d56",
            "reporter_discriminator": "8650",
            "reporter_global_name": "Avery Reporter",
            "reported_at": "2026-10-04T10:00:00.000Z",
            "status": 0,
            "report_type": 0,
            "category": "harassment",
            "reported_user_id": "1500000000000000001",
            "reason": "harassment",
            "reason_label": "Harassment or bullying"
        }))
        .expect("valid report");
        let markup = report_row(&config, "/admin", "received", &report).into_string();
        assert_eq!(
            markup.matches("Harassment or bullying").count(),
            1,
            "{markup}"
        );
        assert_eq!(
            markup.matches(r#"data-report-reason="harassment""#).count(),
            1,
            "{markup}"
        );
        assert!(
            markup.contains(r#"data-report-category="harassment""#),
            "{markup}"
        );
        assert!(markup.contains(">Message<"), "{markup}");
        assert!(
            markup.contains(r#"Avery Reporter <span class="whitespace-nowrap text-neutral-500 text-xs">(reporter_0423a7212d56#8650)</span>"#),
            "{markup}"
        );
        assert!(markup.contains(">Pending<"), "{markup}");
    }
}
