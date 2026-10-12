// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::ReportEntry,
    config::AdminConfig,
    templates::components::{
        badge::BadgeVariant,
        media::{guild_icon_url, initials},
    },
};

use maud::{Markup, html};

const REPORT_CATEGORIES: &[(&str, &str)] = &[
    ("harassment", "Harassment or bullying"),
    ("hate_speech", "Hate speech"),
    ("spam", "Spam"),
    ("illegal_activity", "Illegal activity"),
    ("impersonation", "Impersonation"),
    ("child_safety", "Child safety concerns"),
    ("other", "Other"),
    ("violent_content", "Violent or graphic content"),
    ("nsfw_violation", "NSFW policy violation"),
    ("doxxing", "Sharing personal information"),
    ("self_harm", "Self-harm or suicide"),
    ("malicious_links", "Malicious links"),
    ("spam_account", "Spam account"),
    ("underage_user", "Underage user"),
    ("inappropriate_profile", "Inappropriate profile"),
    ("raid_coordination", "Raid coordination"),
    ("malware_distribution", "Malware distribution"),
    ("extremist_community", "Extremist community"),
];

pub fn report_category_label(category: &str) -> &str {
    REPORT_CATEGORIES
        .iter()
        .find_map(|(key, label)| (*key == category).then_some(*label))
        .unwrap_or(category)
}

pub fn reason_repeats_category(category: &str, reason_label: &str) -> bool {
    report_category_label(category).trim().to_lowercase() == reason_label.trim().to_lowercase()
}

pub fn report_category_options(selected: &str) -> Vec<(&str, &str)> {
    let mut options = std::iter::once(("", "All"))
        .chain(REPORT_CATEGORIES.iter().copied())
        .collect::<Vec<_>>();
    if !options.iter().any(|(key, _)| *key == selected) {
        options.push((selected, selected));
    }
    options
}

pub fn report_category(category: &str) -> Markup {
    html! {
        span data-report-category=(category) title=(category) {
            (report_category_label(category))
        }
    }
}

pub(crate) fn report_status(status: i32) -> (&'static str, BadgeVariant) {
    match status {
        0 => ("Pending", BadgeVariant::Warning),
        1 => ("Resolved", BadgeVariant::Success),
        _ => ("Unknown", BadgeVariant::Default),
    }
}

pub(crate) fn report_type(report_type: i32) -> (&'static str, BadgeVariant) {
    match report_type {
        0 => ("Message", BadgeVariant::Info),
        1 => ("User", BadgeVariant::Default),
        2 => ("Guild", BadgeVariant::Warning),
        _ => ("Unknown", BadgeVariant::Default),
    }
}

pub(crate) fn message_lookup_href(
    base: &str,
    channel_id: &str,
    message_id: Option<&str>,
) -> String {
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

pub(crate) fn reported_guild_icon(
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
