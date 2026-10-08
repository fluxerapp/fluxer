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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_categories_get_their_label() {
        assert_eq!(
            report_category_label("harassment"),
            "Harassment or bullying"
        );
        assert_eq!(report_category_label("spam"), "Spam");
        assert_eq!(report_category_label("spam_account"), "Spam account");
        assert_eq!(
            report_category_label("extremist_community"),
            "Extremist community"
        );
    }

    #[test]
    fn unknown_categories_fall_back_to_the_key() {
        assert_eq!(report_category_label("future_value"), "future_value");
        assert_eq!(report_category_label(""), "");
    }

    #[test]
    fn reasons_that_repeat_the_category_label_are_detected() {
        assert!(reason_repeats_category("spam", "Spam"));
        assert!(reason_repeats_category("spam", "spam"));
        assert!(reason_repeats_category(
            "harassment",
            "Harassment or bullying"
        ));
        assert!(reason_repeats_category(
            "harassment",
            " harassment OR bullying "
        ));
        assert!(!reason_repeats_category("harassment", "Hate speech"));
        assert!(!reason_repeats_category(
            "child_safety",
            "Child sexual abuse material"
        ));
        assert!(reason_repeats_category("future_value", "future_value"));
        assert!(!reason_repeats_category("future_value", "Spam"));
    }

    #[test]
    fn every_category_key_is_unique_and_labelled() {
        let mut keys = REPORT_CATEGORIES
            .iter()
            .map(|(key, _)| *key)
            .collect::<Vec<_>>();
        keys.sort_unstable();
        keys.dedup();
        assert_eq!(keys.len(), REPORT_CATEGORIES.len());
        assert_eq!(keys.len(), 18);
        for (key, label) in REPORT_CATEGORIES {
            assert_ne!(key, label);
        }
    }

    #[test]
    fn options_start_with_all_and_keep_an_unknown_selection() {
        let options = report_category_options("");
        assert_eq!(options.first(), Some(&("", "All")));
        assert_eq!(options.len(), 19);
        assert_eq!(report_category_options("spam").len(), 19);
        let unknown = report_category_options("future_value");
        assert_eq!(unknown.len(), 20);
        assert_eq!(unknown.last(), Some(&("future_value", "future_value")));
    }

    #[test]
    fn category_markup_shows_the_label_and_keeps_the_key() {
        let markup = report_category("doxxing").into_string();
        assert!(
            markup.contains(r#"data-report-category="doxxing""#),
            "{markup}"
        );
        assert!(
            markup.contains(">Sharing personal information<"),
            "{markup}"
        );
        let unknown = report_category("future_value").into_string();
        assert!(
            unknown.contains(r#"data-report-category="future_value""#),
            "{unknown}"
        );
        assert!(unknown.contains(">future_value<"), "{unknown}");
    }
}
