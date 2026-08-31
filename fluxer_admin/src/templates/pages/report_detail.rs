// SPDX-License-Identifier: AGPL-3.0-or-later

use std::cmp::Ordering;

use crate::{
    acl,
    api::types::ReportEntry,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            data_field::{data_field, data_field_link_mono, data_field_mono, data_field_text},
            form::csrf_input,
            media::{guild_icon_url, initials, user_avatar_url},
            message_list::{Attachment, Message, message_deletion_script, message_list},
            nsfw_indicators::{
                adult_content_badge, channel_nsfw_state_badge, content_warning_badge,
            },
            page_container::page_header_with_back,
            resource_link::{ResourceType, resource_link},
            section_card::section_card,
        },
        layout::admin_layout,
        pages::messages_page::poll_from_value,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};
use serde_json::Value;

fn status_badge(status: i32) -> Markup {
    let (label, variant) = match status {
        0 => ("Pending", BadgeVariant::Warning),
        1 => ("Resolved", BadgeVariant::Success),
        _ => ("Unknown", BadgeVariant::Default),
    };
    html! {
        span class="inline-flex w-fit self-start" {
            (badge(label, variant))
        }
    }
}

fn report_type_label(report_type: i32) -> &'static str {
    match report_type {
        0 => "Message",
        1 => "User",
        2 => "Guild",
        _ => "Unknown",
    }
}

fn reporter_label(report: &ReportEntry) -> String {
    if let Some(tag) = &report.reporter_tag {
        return tag.to_owned();
    }
    if let Some(username) = &report.reporter_username {
        let discriminator = report.reporter_discriminator.as_deref().unwrap_or("0000");
        return format!("{username}#{discriminator}");
    }
    if let Some(email) = &report.reporter_email {
        return email.to_owned();
    }
    "Anonymous".to_owned()
}

fn reported_user_label(report: &ReportEntry) -> String {
    if let Some(tag) = &report.reported_user_tag {
        return tag.to_owned();
    }
    if let Some(username) = &report.reported_user_username {
        let discriminator = report
            .reported_user_discriminator
            .as_deref()
            .unwrap_or("0000");
        return format!("{username}#{discriminator}");
    }
    format!(
        "User {}",
        report.reported_user_id.as_deref().unwrap_or("unknown")
    )
}

fn has_acl(auth: &AuthContext, permission: &str) -> bool {
    auth.admin_user.as_ref().is_some_and(|admin| {
        admin
            .acls
            .iter()
            .any(|acl| acl == permission || acl == crate::acl::WILDCARD)
    })
}

fn basic_info_section(config: &AdminConfig, auth: &AuthContext, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let show_pii = has_acl(auth, acl::REPORT_VIEW_REPORTER_PII);
    html! {
        (section_card(Some("Basic Information"), None, None, html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_mono("Report ID", &report.report_id))
                (data_field_text("Reported At", &format_admin_timestamp(&report.reported_at)))
                (data_field_text("Type", report_type_label(report.report_type)))
                (data_field_text("Category", report.category.as_deref().unwrap_or("")))
                @if let Some(ref reporter_id) = report.reporter_id {
                    (data_field_link_mono("Reporter", &format!("{base}/users/{reporter_id}"), &reporter_label(report)))
                } @else {
                    (data_field_text("Reporter", &reporter_label(report)))
                }
                @if show_pii {
                    @if let Some(ref value) = report.reporter_email {
                        (data_field_text("Reporter Email", value))
                    }
                    @if let Some(ref value) = report.reporter_full_legal_name {
                        (data_field_text("Full Legal Name", value))
                    }
                    @if let Some(ref value) = report.reporter_country_of_residence {
                        (data_field_text("Country of Residence", value))
                    }
                }
                (data_field("Status", status_badge(report.status)))
            }
        }))
    }
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

fn snapshot_note() -> Markup {
    html! {
        span class="text-neutral-500 text-xs italic" { "(at time of report)" }
    }
}

fn guild_content_badges(report: &ReportEntry) -> Markup {
    let fallback = report
        .reported_guild_nsfw_level
        .filter(|_| report.reported_guild_nsfw.is_none())
        .map(|level| level == 3);
    let adult = report.reported_guild_nsfw.or(fallback).unwrap_or(false);
    let show_cw = report.reported_guild_content_warning_level == Some(1);
    if !adult && !show_cw {
        return html! {};
    }
    html! {
        span class="mt-1 inline-flex flex-wrap items-center gap-2" {
            (adult_content_badge(adult, None))
            (content_warning_badge(
                report.reported_guild_content_warning_level,
                report.reported_guild_content_warning_text.as_deref(),
                false,
            ))
            (snapshot_note())
        }
    }
}

fn channel_content_badges(report: &ReportEntry) -> Markup {
    let is_nsfw = report
        .reported_channel_effective_nsfw
        .or(report.reported_channel_nsfw)
        .unwrap_or(false);
    let warning_level = report
        .reported_channel_effective_content_warning_level
        .or(report.reported_channel_content_warning_level);
    let warning_text = report
        .reported_channel_effective_content_warning_text
        .as_deref()
        .or(report.reported_channel_content_warning_text.as_deref());
    if !is_nsfw && warning_level != Some(1) {
        return html! {};
    }
    html! {
        span class="mt-1 inline-flex flex-wrap items-center gap-2" {
            (channel_nsfw_state_badge(
                is_nsfw,
                report.reported_channel_nsfw_override,
                None,
                report.reported_guild_nsfw,
                warning_level,
                warning_text,
                true,
            ))
            (snapshot_note())
        }
    }
}

fn reported_entity_section(config: &AdminConfig, report: &ReportEntry) -> Markup {
    html! {
        (section_card(Some("Reported Entity"), None, None, html! {
            div class="grid grid-cols-1 gap-4" {
                @match report.report_type {
                    0 => {
                        (reported_user_field(config, "User", report))
                        (reported_message_fields(config, report))
                        (reported_guild_field(config, report))
                    }
                    1 => {
                        (reported_user_field(config, "User", report))
                        (reported_guild_field(config, report))
                    }
                    2 => {
                        (reported_guild_field(config, report))
                    }
                    _ => {
                        (data_field_text("Entity", "Unknown"))
                    }
                }
                @if let Some(ref invite) = report.reported_guild_invite_code {
                    (data_field_text("Guild Invite Code", invite))
                }
            }
        }))
    }
}

fn reported_user_field(config: &AdminConfig, label: &str, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if let Some(ref id) = report.reported_user_id {
            (data_field(label, html! {
                a href={(base) "/users/" (id)} class="inline-flex items-center gap-2 text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
                    img
                        src=(user_avatar_url(config, id, report.reported_user_avatar_hash.as_deref(), 80, true))
                        alt=(format!("{}'s avatar", reported_user_label(report)))
                        class="h-8 w-8 rounded-full object-cover";
                    span { (reported_user_label(report)) }
                }
            }))
        } @else {
            (data_field_text(label, &reported_user_label(report)))
        }
    }
}

fn reported_message_fields(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if let Some(ref message_id) = report.reported_message_id {
            @if let Some(ref channel_id) = report.reported_channel_id {
                (data_field_link_mono("Message ID", &message_lookup_href(base, channel_id, Some(message_id)), message_id))
            } @else {
                (data_field_mono("Message ID", message_id))
            }
        }
        @if let Some(ref channel_id) = report.reported_channel_id {
            (data_field_link_mono("Channel ID", &message_lookup_href(base, channel_id, report.reported_message_id.as_deref()), channel_id))
        }
        @if report.reported_channel_name.is_some()
            || report.reported_channel_nsfw == Some(true)
            || report.reported_channel_effective_nsfw == Some(true)
            || report.reported_channel_content_warning_level == Some(1)
            || report.reported_channel_effective_content_warning_level == Some(1) {
            div class="space-y-1" {
                @if let Some(ref name) = report.reported_channel_name {
                    (data_field_text("Channel Name", name))
                }
                (channel_content_badges(report))
            }
        }
    }
}

fn reported_guild_field(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if report.reported_guild_id.is_some()
            || report.reported_guild_name.is_some()
            || report.reported_guild_nsfw == Some(true)
            || report.reported_guild_content_warning_level == Some(1) {
            div class="space-y-1" {
                @if let Some(ref guild_id) = report.reported_guild_id {
                    div class="flex min-w-0 items-center justify-between gap-4 py-2 text-sm" {
                        dt class="font-medium text-neutral-500" { "Guild" }
                        dd class="min-w-0 text-right text-neutral-900" {
                            a href={(base) "/guilds/" (guild_id)}
                                class="inline-flex min-w-0 items-center justify-end gap-2 text-blue-600 hover:underline" {
                                (reported_guild_icon(
                                    config,
                                    report,
                                    guild_id,
                                    report.reported_guild_name.as_deref().unwrap_or(guild_id),
                                ))
                                span class="truncate" {
                                    (report.reported_guild_name.as_deref().unwrap_or(guild_id))
                                }
                            }
                        }
                    }
                } @else if let Some(ref guild_name) = report.reported_guild_name {
                    (data_field_text("Guild", guild_name))
                }
                (guild_content_badges(report))
            }
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
            img src=(url) alt="" class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
        },
        None => html! {
            span class="flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-neutral-200 text-neutral-600 text-xs" {
                (initials(label))
            }
        },
    }
}

fn additional_info_section(report: &ReportEntry) -> Markup {
    html! {
        @if let Some(ref info) = report.additional_info {
            (section_card(Some("Additional Information"), None, None, html! {
                p class="whitespace-pre-wrap break-words text-neutral-700" { (info) }
            }))
        }
    }
}

fn status_card(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Status"), None, None, html! {
            div class="mb-4 flex justify-start" {
                (status_badge(report.status))
            }
            @if let Some(ref resolved_at) = report.resolved_at {
                p class="text-sm text-neutral-500" {
                    span class="font-medium" { "Resolved at: " }
                    (format_admin_timestamp(resolved_at))
                }
            }
            @if let Some(ref resolved_by) = report.resolved_by_admin_id {
                p class="text-sm text-neutral-500" {
                    span class="font-medium" { "Resolved by: " }
                    (resource_link(base, ResourceType::User, resolved_by, html! {
                        (resolved_by)
                    }))
                }
            }
            @if let Some(ref comment) = report.public_comment {
                div class="mt-3 border-neutral-200 border-t pt-3" {
                    p class="mb-1 font-medium text-neutral-700 text-sm" { "Public comment:" }
                    p class="whitespace-pre-wrap break-words text-neutral-600 text-sm" {
                        (comment)
                    }
                }
            }
        }))
    }
}

fn actions_card(config: &AdminConfig, report: &ReportEntry, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Actions"), None, None, html! {
            div class="flex flex-col gap-3" {
                @if report.status == 0 {
                    form method="post"
                        action={(base) "/reports/" (&report.report_id) "/resolve"} {
                        (csrf_input(csrf_token))
                        button type="submit"
                            class="inline-flex w-full items-center justify-center gap-2 \
                                   font-medium rounded-lg bg-neutral-900 text-white \
                                   px-4 py-2 text-sm" {
                            "Resolve Report"
                        }
                    }
                }
                @if report.report_type == 0 || report.report_type == 1 {
                    @if let Some(ref reported_id) = report.reported_user_id {
                        (nav_link(&format!("{base}/users/{reported_id}"), "View Reported User"))
                    }
                }
                @if report.report_type == 2 {
                    @if let Some(ref guild_id) = report.reported_guild_id {
                        (nav_link(&format!("{base}/guilds/{guild_id}"), "View Reported Guild"))
                    }
                }
                @if let Some(ref reporter_id) = report.reporter_id {
                    (nav_link(&format!("{base}/users/{reporter_id}"), "View Reporter"))
                }
                @if report.report_type == 1 {
                    @if let Some(ref channel_id) = report.mutual_dm_channel_id {
                        (nav_link(&message_lookup_href(base, channel_id, None), "View Mutual DM Channel"))
                    }
                }
            }
        }))
    }
}

fn nav_link(href: &str, label: &str) -> Markup {
    html! {
        a href=(href)
            class="label rounded-lg border border-neutral-300 bg-white px-3 py-2 \
                   text-neutral-700 text-center text-sm transition-colors \
                   hover:bg-neutral-50 block" {
            (label)
        }
    }
}

fn message_context_section(
    config: &AdminConfig,
    report: &ReportEntry,
    include_delete: bool,
    csrf_token: Option<&str>,
) -> Markup {
    let Some(values) = &report.message_context else {
        return html! {};
    };
    if report.report_type != 0 || values.is_empty() {
        return html! {};
    }
    let messages = ordered_messages(values);
    if messages.is_empty() {
        return html! {};
    }
    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200 bg-white" {
            div class="px-4 pt-4 pb-2 sm:px-6 sm:pt-6" {
                h2 class="font-semibold text-base text-neutral-900" { "Message Context" }
            }
            div class="py-2" {
                (message_list(
                    config,
                    &config.base_path,
                    &messages,
                    include_delete,
                    report.reported_message_id.as_deref(),
                ))
            }
        }
        @if let Some(csrf_token) = csrf_token {
            (message_deletion_script(csrf_token))
        }
    }
}

pub fn report_detail_page(
    config: &AdminConfig,
    auth: &AuthContext,
    report: &ReportEntry,
    csrf_token: &str,
    is_htmx: bool,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        div class="space-y-6" {
            (page_header_with_back(
                "Report Details",
                None,
                &format!("{base}/reports"),
                Some("Back to Reports"),
            ))
            div class="grid grid-cols-1 gap-6 lg:grid-cols-3" {
            div class="space-y-6 lg:col-span-2" {
                (basic_info_section(config, auth, report))
                (reported_entity_section(config, report))
                (message_context_section(config, report, true, Some(csrf_token)))
                (additional_info_section(report))
            }
            div class="space-y-6" {
                (status_card(config, report))
                (actions_card(config, report, csrf_token))
            }
        }
        }
    };
    if is_htmx {
        content
    } else {
        admin_layout(config, auth, "Report Details", "reports", None, content)
    }
}

pub fn report_detail_fragment(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        div data-report-fragment="" class="space-y-4" {
            (basic_info_section_fragment(config, report))
            (reported_entity_section(config, report))
            (message_context_section(config, report, false, None))
            (additional_info_section(report))
            a href={(base) "/reports/" (&report.report_id)}
                class="inline-flex min-h-[44px] w-full items-center justify-center \
                       rounded-lg bg-neutral-900 px-4 py-2 font-medium text-sm \
                       text-white transition-colors hover:bg-neutral-800" {
                "Open full report"
            }
        }
    }
}

fn basic_info_section_fragment(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Basic Information"), None, None, html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_mono("Report ID", &report.report_id))
                (data_field_text("Reported At", &format_admin_timestamp(&report.reported_at)))
                (data_field_text("Type", report_type_label(report.report_type)))
                (data_field_text("Category", report.category.as_deref().unwrap_or("")))
                @if let Some(ref reporter_id) = report.reporter_id {
                    (data_field_link_mono("Reporter", &format!("{base}/users/{reporter_id}"), &reporter_label(report)))
                } @else {
                    (data_field_text("Reporter", &reporter_label(report)))
                }
                (data_field("Status", status_badge(report.status)))
            }
        }))
    }
}

fn ordered_messages(values: &[Value]) -> Vec<Message> {
    let mut messages: Vec<Message> = values.iter().map(message_from_value).collect();
    messages.sort_by(compare_message_ids);
    messages
}

fn message_from_value(value: &Value) -> Message {
    let attachments = value
        .get("attachments")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .map(attachment_from_value)
        .collect();
    Message {
        id: value.get("id").and_then(value_id).unwrap_or_default(),
        content: value
            .get("content")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        timestamp: value
            .get("timestamp")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        author_id: value
            .get("author_id")
            .and_then(value_id)
            .unwrap_or_default(),
        author_username: value
            .get("author_username")
            .and_then(Value::as_str)
            .unwrap_or("Unknown")
            .to_owned(),
        author_global_name: value
            .get("author_global_name")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        author_discriminator: value
            .get("author_discriminator")
            .and_then(value_id)
            .unwrap_or_else(|| "0000".to_owned()),
        author_avatar: value
            .get("author_avatar")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        channel_id: value
            .get("channel_id")
            .and_then(value_id)
            .unwrap_or_default(),
        channel_nsfw: value.get("channel_nsfw").and_then(Value::as_bool),
        channel_content_warning_level: value
            .get("channel_content_warning_level")
            .and_then(Value::as_i64)
            .map(|n| n as i32),
        channel_content_warning_text: value
            .get("channel_content_warning_text")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        guild_nsfw: value.get("guild_nsfw").and_then(Value::as_bool),
        poll: value.get("poll").map(poll_from_value),
        attachments,
    }
}

fn attachment_from_value(value: &Value) -> Attachment {
    Attachment {
        id: value.get("id").and_then(value_id).unwrap_or_default(),
        url: value
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        filename: value
            .get("filename")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        nsfw: value.get("nsfw").and_then(Value::as_bool),
        content_type: value
            .get("content_type")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        width: value.get("width").and_then(Value::as_u64).map(|n| n as u32),
        height: value
            .get("height")
            .and_then(Value::as_u64)
            .map(|n| n as u32),
        size: value.get("size").and_then(Value::as_u64),
        ncmec_status: value
            .get("ncmec_status")
            .and_then(Value::as_str)
            .unwrap_or("not_submitted")
            .to_owned(),
        ncmec_report_id: value
            .get("ncmec_report_id")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        ncmec_failure_reason: value
            .get("ncmec_failure_reason")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
    }
}

fn value_id(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        _ => None,
    }
}

fn compare_message_ids(left: &Message, right: &Message) -> Ordering {
    match (left.id.parse::<u128>(), right.id.parse::<u128>()) {
        (Ok(l), Ok(r)) => l.cmp(&r),
        _ => left.id.cmp(&right.id),
    }
}
