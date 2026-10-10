// SPDX-License-Identifier: AGPL-3.0-or-later

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
