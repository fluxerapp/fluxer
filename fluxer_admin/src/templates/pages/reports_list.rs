// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{ReportEntry, ReportReasonEntry, SearchReportsResponse},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            drawer::{DrawerSide, DrawerWidth, drawer},
            form::{secondary_button_link, select_input, submit_button, text_input},
            media::{guild_icon_url, initials, user_avatar_url},
            page_container::page_header_with_actions,
            report_category::{reason_repeats_category, report_category, report_category_options},
            report_webhook::{
                ReportedWebhook, reported_webhook, webhook_creator_badges, webhook_identity,
            },
            table::{empty_state, table, table_body, table_container, table_head},
        },
        layout::admin_layout,
        pages::user_detail_tabs::reports::tagged_user,
    },
    utils::{plural::count_noun, timestamps::format_admin_timestamp, user_tag::user_tag},
};
use maud::{Markup, PreEscaped, html};

pub struct ReportFilters<'a> {
    pub query: Option<&'a str>,
    pub status: Option<&'a str>,
    pub report_type: Option<&'a str>,
    pub category: Option<&'a str>,
    pub reason: Option<&'a str>,
    pub reporter_id: Option<&'a str>,
    pub reported_user_id: Option<&'a str>,
    pub reported_webhook_id: Option<&'a str>,
    pub reported_guild_id: Option<&'a str>,
    pub reported_channel_id: Option<&'a str>,
    pub guild_context_id: Option<&'a str>,
    pub resolved_by_admin_id: Option<&'a str>,
    pub sort: &'a str,
}

pub fn reports_list_page(
    config: &AdminConfig,
    auth: &AuthContext,
    result: Option<&SearchReportsResponse>,
    filters: &ReportFilters<'_>,
    reasons: Option<&[ReportReasonEntry]>,
    page: u32,
    limit: u32,
) -> Markup {
    let content = html! {
        div class="space-y-6" {
            (page_header_with_actions(
                "Reports",
                None,
                report_count_summary(result),
            ))
            (filters_card(config, filters, reasons, limit))
            @if let Some(result) = result {
                @if result.reports.is_empty() {
                    (empty_state("No reports found."))
                } @else {
                    (render_reports_table(config, &result.reports))
                    (reports_pagination(config, filters, page, limit, result.total))
                }
            } @else {
                (empty_state("Failed to load reports."))
            }
        }
        (drawer(
            "report-peek", "Report", None,
            DrawerSide::Right, DrawerWidth::Xl, None, None,
        ))
    };
    admin_layout(config, auth, "Reports", "reports", None, content)
}

fn report_count_summary(result: Option<&SearchReportsResponse>) -> Markup {
    html! {
        @if let Some(result) = result {
            p class="text-neutral-500 text-sm" {
                "Found " (count_noun(result.total, "result", "results"))
                " (showing " (result.reports.len()) ")"
            }
        }
    }
}

fn filters_card(
    config: &AdminConfig,
    filters: &ReportFilters<'_>,
    reasons: Option<&[ReportReasonEntry]>,
    limit: u32,
) -> Markup {
    let limit_value = limit.to_string();
    let select_grid_class = if reasons.is_some() {
        "grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3"
    } else {
        "grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-5"
    };
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-6 transition-all" {
            form method="get" {
              div class="space-y-4" {
                (text_input(
                    "q",
                    "Search",
                    filters.query.unwrap_or(""),
                    "Search by ID, reporter, category, or description...",
                ))
                div class=(select_grid_class) {
                    (select_input("status", "Status", &[
                        ("", "All"),
                        ("0", "Pending"),
                        ("1", "Resolved"),
                    ], filters.status.unwrap_or("")))
                    (select_input("type", "Type", &[
                        ("", "All"),
                        ("0", "Message"),
                        ("1", "User"),
                        ("2", "Guild"),
                    ], filters.report_type.unwrap_or("")))
                    (select_input("category", "Category", &report_category_options(filters.category.unwrap_or("")), filters.category.unwrap_or("")))
                    @if let Some(reasons) = reasons {
                        div class="sm:col-span-2 lg:col-span-2" {
                            (reason_select(reasons, filters.reason.unwrap_or("")))
                        }
                    }
                    (select_input("sort", "Sort", &[
                        ("reportedAt_desc", "Reported (newest first)"),
                        ("reportedAt_asc", "Reported (oldest first)"),
                        ("createdAt_desc", "Created (newest first)"),
                        ("createdAt_asc", "Created (oldest first)"),
                        ("resolvedAt_desc", "Resolved (newest first)"),
                        ("resolvedAt_asc", "Resolved (oldest first)"),
                    ], filters.sort))
                    (select_input("limit", "Page size", &[
                        ("25", "25"),
                        ("50", "50"),
                        ("100", "100"),
                        ("150", "150"),
                    ], &limit_value))
                }
                div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3" {
                    (text_input("reporter_id", "Reporter user ID", filters.reporter_id.unwrap_or(""), "Snowflake"))
                    (text_input("reported_user_id", "Reported user ID", filters.reported_user_id.unwrap_or(""), "Snowflake"))
                    (text_input("reported_webhook_id", "Reported webhook ID", filters.reported_webhook_id.unwrap_or(""), "Snowflake"))
                    (text_input("reported_guild_id", "Reported guild ID", filters.reported_guild_id.unwrap_or(""), "Snowflake"))
                    (text_input("reported_channel_id", "Reported channel ID", filters.reported_channel_id.unwrap_or(""), "Snowflake"))
                    (text_input("guild_context_id", "Guild context ID", filters.guild_context_id.unwrap_or(""), "Snowflake"))
                    (text_input("resolved_by_admin_id", "Resolved by admin ID", filters.resolved_by_admin_id.unwrap_or(""), "Snowflake"))
                }
                div class="flex flex-wrap gap-2" {
                    (submit_button("Search & Filter"))
                    (secondary_button_link("Clear", &format!("{}/reports", config.base_path)))
                }
              }
            }
        }
    }
}

fn reason_select(reasons: &[ReportReasonEntry], selected: &str) -> Markup {
    let labels = reasons
        .iter()
        .map(|reason| {
            if reason.highest_priority {
                format!("Priority: {}", reason.label)
            } else {
                reason.label.clone()
            }
        })
        .collect::<Vec<_>>();
    let mut options = std::iter::once(("", "All"))
        .chain(
            reasons
                .iter()
                .zip(&labels)
                .map(|(reason, label)| (reason.key.as_str(), label.as_str())),
        )
        .collect::<Vec<_>>();
    if !options.iter().any(|(key, _)| *key == selected) {
        options.push((selected, selected));
    }
    select_input("reason", "Reason", &options, selected)
}

fn format_status(status: i32) -> (&'static str, BadgeVariant) {
    match status {
        0 => ("Pending", BadgeVariant::Warning),
        1 => ("Resolved", BadgeVariant::Success),
        _ => ("Unknown", BadgeVariant::Default),
    }
}

fn format_report_type(report_type: i32) -> (&'static str, BadgeVariant) {
    match report_type {
        0 => ("Message", BadgeVariant::Info),
        1 => ("User", BadgeVariant::Default),
        2 => ("Guild", BadgeVariant::Warning),
        _ => ("Unknown", BadgeVariant::Default),
    }
}

fn reporter_label(report: &ReportEntry) -> Markup {
    if let Some(username) = &report.reporter_username {
        return tagged_user(
            report.reporter_global_name.as_deref(),
            username,
            report.reporter_discriminator.as_deref(),
        );
    }
    html! {
        (report
            .reporter_tag
            .as_deref()
            .or(report.reporter_email.as_deref())
            .unwrap_or("Anonymous"))
    }
}

fn reported_user_label(report: &ReportEntry) -> String {
    if let Some(username) = &report.reported_user_username {
        let discriminator = report
            .reported_user_discriminator
            .as_deref()
            .unwrap_or("0000");
        let tag = user_tag(username, discriminator, false);
        if let Some(display) = report
            .reported_user_global_name
            .as_ref()
            .filter(|v| !v.trim().is_empty())
        {
            return format!("{display} ({tag})");
        }
        return tag;
    }
    if let Some(tag) = &report.reported_user_tag {
        return tag.to_owned();
    }
    format!(
        "User {}",
        report.reported_user_id.as_deref().unwrap_or("unknown")
    )
}

fn message_lookup_href(base: &str, channel_id: &str, message_id: Option<&str>) -> String {
    let mut href = format!(
        "{base}/messages?channel_id={}&context_limit=50",
        urlencoding::encode(channel_id)
    );
    if let Some(message_id) = message_id.filter(|id| !id.is_empty()) {
        href.push_str("&message_id=");
        href.push_str(&urlencoding::encode(message_id));
    }
    href
}

fn reporter_cell(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let primary = reporter_label(report);
    html! {
        div class="flex flex-col gap-1" {
            @if let Some(id) = &report.reporter_id {
                a href={(base) "/users/" (id)} class="font-medium text-blue-600 text-sm hover:underline" {
                    (primary)
                }
            } @else {
                span class="text-neutral-900 text-sm" { (primary) }
            }
            @if let Some(value) = &report.reporter_full_legal_name {
                div class="text-neutral-500 text-xs" { (value) }
            }
            @if let Some(value) = &report.reporter_country_of_residence {
                div class="text-neutral-500 text-xs" { (value) }
            }
        }
    }
}

fn reported_cell(config: &AdminConfig, report: &ReportEntry) -> Markup {
    match report.report_type {
        0 => reported_message_cell(config, report),
        1 => reported_user_cell(config, report),
        2 => reported_guild_cell(config, report),
        _ => html! { span class="text-neutral-400 text-sm italic" { "Unknown" } },
    }
}

fn reported_user_name(report: &ReportEntry) -> Markup {
    match &report.reported_user_username {
        Some(username) => tagged_user(
            report.reported_user_global_name.as_deref(),
            username,
            report.reported_user_discriminator.as_deref(),
        ),
        None => html! { (reported_user_label(report)) },
    }
}

fn reported_user_cell(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let primary = reported_user_label(report);
    html! {
        @if let Some(id) = &report.reported_user_id {
            div class="flex flex-wrap items-center gap-2" {
                a href={(base) "/users/" (id)} class="flex items-center gap-2 text-blue-600 hover:underline" {
                    (reported_user_avatar(config, report, id, &primary))
                    span class="font-medium text-sm" { (reported_user_name(report)) }
                }
                @if report.reported_user_bot == Some(true) {
                    span class="inline-flex" data-report-user-bot=(id) { (badge("Bot", BadgeVariant::Info)) }
                }
            }
        } @else {
            span class="text-neutral-900 text-sm" { (primary) }
        }
    }
}

fn reported_guild_cell(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let name = report.reported_guild_name.as_deref();
    html! {
        @if let Some(id) = &report.reported_guild_id {
            div class="flex items-start gap-2" {
                (reported_guild_icon(config, report, id, name.unwrap_or(id)))
                div class="min-w-0 flex flex-col gap-1" {
                    a href={(base) "/guilds/" (id)} class="font-medium text-blue-600 text-sm hover:underline" {
                        (name.unwrap_or(id))
                    }
                    @if let Some(invite) = &report.reported_guild_invite_code {
                        div class="text-neutral-500 text-xs" { "Invite: " (invite) }
                    }
                }
            }
        } @else {
            span class="text-neutral-400 text-sm italic" { "\u{2014}" }
        }
    }
}

fn reported_guild_icon(
    config: &AdminConfig,
    report: &ReportEntry,
    guild_id: &str,
    label: &str,
) -> Markup {
    match guild_icon_url(
        config,
        guild_id,
        report.reported_guild_icon_hash.as_deref(),
        80,
        true,
    ) {
        Some(url) => html! {
            img src=(url) alt="" class="h-8 w-8 max-w-none flex-shrink-0 rounded-full object-cover";
        },
        None => html! {
            span class="flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-neutral-200 text-neutral-600 text-xs" {
                (initials(label))
            }
        },
    }
}

fn reported_message_cell(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let channel_label = report
        .reported_channel_name
        .as_deref()
        .or(report.reported_channel_id.as_deref())
        .unwrap_or("Unknown channel");
    let channel_prefix =
        if report.reported_guild_id.is_none() && report.reported_channel_id.is_some() {
            "DM channel: "
        } else {
            "Channel: "
        };
    html! {
        div class="flex flex-col gap-1" {
            @if let Some(webhook) = reported_webhook(config, report) {
                (reported_webhook_author(&webhook))
            } @else {
                (reported_user_cell(config, report))
            }
            @if let Some(channel_id) = &report.reported_channel_id {
                a href=(message_lookup_href(base, channel_id, report.reported_message_id.as_deref()))
                    class="text-blue-600 text-xs hover:underline" {
                    (channel_prefix) (channel_label)
                }
            } @else {
                span class="text-neutral-500 text-xs" { "Channel: " (channel_label) }
            }
            @if report.reported_channel_nsfw == Some(true) {
                span class="self-start" { (badge("NSFW", BadgeVariant::Danger)) }
            }
        }
    }
}

fn reported_webhook_author(webhook: &ReportedWebhook<'_>) -> Markup {
    html! {
        div class="flex flex-col items-start gap-0.5" {
            (webhook_identity(webhook))
            a href=(webhook.reports_href) title="Reports about this webhook"
                class="font-mono text-blue-600 text-xs [overflow-wrap:normal] hover:underline" {
                "Webhook ID: " (webhook.id)
            }
            @if let Some(ref creator) = webhook.creator {
                a href=(creator.href) title=(creator.id) data-report-webhook-creator=(creator.id)
                    class="text-blue-600 text-xs hover:underline" {
                    "Webhook creator: " (creator.label)
                }
                @if creator.bot || creator.account_deleted {
                    span class="inline-flex flex-wrap items-center gap-1" { (webhook_creator_badges(creator)) }
                }
            }
        }
    }
}

fn reported_user_avatar(
    config: &AdminConfig,
    report: &ReportEntry,
    user_id: &str,
    label: &str,
) -> Markup {
    let url = user_avatar_url(
        config,
        user_id,
        report.reported_user_avatar_hash.as_deref(),
        80,
        true,
    );
    html! {
        img src=(url) alt=(format!("{label}'s avatar")) class="h-8 w-8 max-w-none rounded-full object-cover";
    }
}

fn render_reports_table(config: &AdminConfig, reports: &[ReportEntry]) -> Markup {
    let base = &config.base_path;
    let headers = &[
        "Reported At",
        "Type / Category",
        "Reporter",
        "Reported",
        "Status",
        "Actions",
    ];
    let rows = html! {
        @for report in reports {
            tr class="hover:bg-neutral-50 transition-colors" {
                td class="hidden whitespace-nowrap px-3 py-3 text-neutral-600 text-sm xl:table-cell" {
                    (format_admin_timestamp(&report.reported_at))
                }
                td class="px-3 py-3 text-sm" {
                    div class="flex flex-col items-start gap-1" {
                        span class="text-neutral-600 text-xs xl:hidden" data-report-reported-at-compact=(report.report_id) {
                            (format_admin_timestamp(&report.reported_at))
                        }
                        @let (type_label, type_variant) = format_report_type(report.report_type);
                        (badge(type_label, type_variant))
                        @let reason_label = report.reason.as_deref().map(|reason| report.reason_label.as_deref().unwrap_or(reason));
                        @let repeats = report.category.as_deref().zip(reason_label).is_some_and(|(category, label)| reason_repeats_category(category, label));
                        @let priority = report.reason_highest_priority == Some(true);
                        @let category_class = if repeats && priority {
                            "font-medium text-red-700 text-xs"
                        } else {
                            "text-neutral-600 text-xs"
                        };
                        span class=(category_class) data-report-reason=[report.reason.as_deref().filter(|_| repeats)] {
                            @if let Some(category) = &report.category {
                                (report_category(category))
                            } @else {
                                "\u{2014}"
                            }
                        }
                        @if !repeats && let (Some(reason), Some(label)) = (&report.reason, reason_label) {
                            @let reason_class = if priority {
                                "font-medium text-red-700 text-xs"
                            } else {
                                "text-neutral-500 text-xs"
                            };
                            span class=(reason_class) data-report-reason=(reason) {
                                (label)
                            }
                        }
                    }
                }
                td class="px-3 py-3 text-sm [overflow-wrap:anywhere] [&_.whitespace-nowrap]:whitespace-normal" {
                    (reporter_cell(config, report))
                }
                td class="px-3 py-3 text-sm [overflow-wrap:anywhere] [&_.whitespace-nowrap]:whitespace-normal" {
                    (reported_cell(config, report))
                }
                @let (status_label, status_variant) = format_status(report.status);
                td class="hidden whitespace-nowrap px-3 py-3 text-sm xl:table-cell" {
                    span data-status-pill=(report.report_id) {
                        (badge(status_label, status_variant))
                    }
                }
                td class="whitespace-nowrap px-3 py-3 text-sm" {
                    div class="flex flex-col items-start gap-1" {
                        span class="xl:hidden" data-status-pill-compact=(report.report_id) {
                            (badge(status_label, status_variant))
                        }
                        button type="button"
                            data-drawer-open="report-peek"
                            data-drawer-href={(base) "/reports/" (report.report_id) "/fragment"}
                            data-drawer-title={"Report " (report.report_id)}
                            popovertarget="report-peek"
                            hx-get={(base) "/reports/" (report.report_id) "/fragment"}
                            hx-target="#report-peek-body"
                            hx-swap="innerHTML"
                            aria-label={"Peek report " (report.report_id)}
                            class="inline-flex min-h-[36px] items-center justify-center rounded-md \
                                   border border-neutral-300 px-3 py-1.5 font-medium \
                                   text-neutral-700 text-sm transition-colors hover:border-neutral-400 \
                                   hover:bg-neutral-50 focus:outline-none focus-visible:ring-2 \
                                   focus-visible:ring-brand-primary focus-visible:ring-offset-2" {
                            "Peek"
                        }
                        a href={(base) "/reports/" (report.report_id)}
                            class="px-1 font-medium text-blue-600 text-xs hover:underline" {
                            "Details " (PreEscaped("&rarr;"))
                        }
                    }
                }
            }
        }
    };
    html! {
        div data-report-table="true" {
            (table_container(table(html! {
                (table_head(html! {
                    tr {
                        @for (index, header) in headers.iter().enumerate() {
                            th class={"px-3 py-3 text-left text-neutral-600 text-xs uppercase tracking-wider" (match index {
                                0 | 4 => " hidden whitespace-nowrap xl:table-cell",
                                5 => " xl:whitespace-nowrap",
                                _ => " whitespace-nowrap",
                            })} {
                                @if index == 5 {
                                    span class="xl:hidden" { "Status / " }
                                }
                                (header)
                            }
                        }
                    }
                }))
                (table_body(rows))
            })))
        }
    }
}

fn reports_pagination(
    config: &AdminConfig,
    filters: &ReportFilters<'_>,
    page: u32,
    limit: u32,
    total: u64,
) -> Markup {
    let total_pages = total.div_ceil(u64::from(limit)).max(1);
    let next_page = page
        .checked_add(1)
        .filter(|page| u64::from(*page) < total_pages);
    html! {
        div class="mt-4 flex items-center justify-between" {
            @if page > 0 {
                a href=(reports_url(config, filters, page - 1, limit))
                    class="text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
                    (PreEscaped("&larr; Previous"))
                }
            } @else {
                span {}
            }
            span class="text-neutral-500 text-sm" {
                "Page " (u64::from(page) + 1) " of " (total_pages)
            }
            @if let Some(next_page) = next_page {
                a href=(reports_url(config, filters, next_page, limit))
                    class="text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
                    (PreEscaped("Next &rarr;"))
                }
            } @else {
                span {}
            }
        }
    }
}

fn push_param(params: &mut Vec<(String, String)>, key: &str, value: Option<&str>) {
    if let Some(value) = value.filter(|value| !value.is_empty()) {
        params.push((key.to_owned(), value.to_owned()));
    }
}

fn reports_url(config: &AdminConfig, filters: &ReportFilters<'_>, page: u32, limit: u32) -> String {
    let mut params = Vec::new();
    push_param(&mut params, "q", filters.query);
    push_param(&mut params, "status", filters.status);
    push_param(&mut params, "type", filters.report_type);
    push_param(&mut params, "category", filters.category);
    push_param(&mut params, "reason", filters.reason);
    push_param(&mut params, "reporter_id", filters.reporter_id);
    push_param(&mut params, "reported_user_id", filters.reported_user_id);
    push_param(
        &mut params,
        "reported_webhook_id",
        filters.reported_webhook_id,
    );
    push_param(&mut params, "reported_guild_id", filters.reported_guild_id);
    push_param(
        &mut params,
        "reported_channel_id",
        filters.reported_channel_id,
    );
    push_param(&mut params, "guild_context_id", filters.guild_context_id);
    push_param(
        &mut params,
        "resolved_by_admin_id",
        filters.resolved_by_admin_id,
    );
    push_param(&mut params, "sort", Some(filters.sort));
    params.push(("limit".to_owned(), limit.to_string()));
    params.push(("page".to_owned(), page.to_string()));
    let query = params
        .iter()
        .map(|(key, value)| {
            format!(
                "{}={}",
                urlencoding::encode(key),
                urlencoding::encode(value)
            )
        })
        .collect::<Vec<_>>()
        .join("&");
    format!("{}/reports?{}", config.base_path, query)
}
