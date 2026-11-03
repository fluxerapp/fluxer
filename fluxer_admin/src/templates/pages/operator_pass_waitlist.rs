// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::generated::types::{
        AdminOperatorPassPriceFeedbackCurrency, AdminOperatorPassWaitlistFollowupStats,
        AdminOperatorPassWaitlistFollowupTextEntry, AdminOperatorPassWaitlistStatsResponse,
        OperatorPassPriceFeedbackSchema, OperatorPassWaitlistAboutKeySchema,
        OperatorPassWaitlistFollowupKeySchema,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            alert::{AlertVariant, alert},
            empty_state::empty_state_full,
            error_display::error_alert,
            page_container::{card_with_header, page_header},
            table::{data_table, table_cell, table_row},
        },
        layout::admin_layout,
        pages::operator_pass_wall::format_time,
    },
};
use maud::{Markup, html};

pub fn price_feedback_label(feedback: &OperatorPassPriceFeedbackSchema) -> &'static str {
    match feedback {
        OperatorPassPriceFeedbackSchema::TooHigh => "More than I'd pay",
        OperatorPassPriceFeedbackSchema::AboutRight => "About right",
        OperatorPassPriceFeedbackSchema::CouldPayMore => "I'd pay more",
    }
}

pub const FOLLOWUP_CSV_PATH: &str = "/operator-pass-waitlist/followup.csv";

pub fn followup_key_label(key: &OperatorPassWaitlistFollowupKeySchema) -> &'static str {
    match key {
        OperatorPassWaitlistFollowupKeySchema::WallSpot => "A spot on the Operators' wall",
        OperatorPassWaitlistFollowupKeySchema::Badge => "The Operator badge",
        OperatorPassWaitlistFollowupKeySchema::OperatorsChannel => "The Operators channel",
        OperatorPassWaitlistFollowupKeySchema::EarlyTesting => "Testing new features early",
        OperatorPassWaitlistFollowupKeySchema::YearlyList => "A name on the yearly Operators list",
        OperatorPassWaitlistFollowupKeySchema::InstanceMark => "An Operator mark on your instance",
        OperatorPassWaitlistFollowupKeySchema::Other => "Something else",
    }
}

pub fn about_key_label(key: &OperatorPassWaitlistAboutKeySchema) -> &'static str {
    match key {
        OperatorPassWaitlistAboutKeySchema::SelfHostingNew => "I'm new to self-hosting",
        OperatorPassWaitlistAboutKeySchema::SelfHostingExperienced => "I've self-hosted before",
        OperatorPassWaitlistAboutKeySchema::BotsNew => "I'm new to building bots",
        OperatorPassWaitlistAboutKeySchema::BotsExperienced => "I've built bots before",
        OperatorPassWaitlistAboutKeySchema::SupportFluxer => "I mainly want to support Fluxer",
        OperatorPassWaitlistAboutKeySchema::CommunityOwner => "I run a community on Fluxer",
    }
}

fn about_table(followup: &AdminOperatorPassWaitlistFollowupStats) -> Markup {
    data_table(
        &["Description", "Confirmed", "All signups"],
        html! {
            @for about in &followup.about {
                (table_row(html! {
                    (table_cell(true, html! { (about_key_label(&about.key)) " (" (about.key) ")" }))
                    (table_cell(false, html! { (about.confirmed) }))
                    (table_cell(false, html! { (about.all) }))
                }))
            }
        },
    )
}

fn joined_keys<T: ToString>(keys: &[T], separator: &str) -> String {
    keys.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(separator)
}

fn followup_table(followup: &AdminOperatorPassWaitlistFollowupStats) -> Markup {
    data_table(
        &["Option", "Confirmed", "All signups"],
        html! {
            @for pick in &followup.picks {
                (table_row(html! {
                    (table_cell(true, html! { (followup_key_label(&pick.key)) " (" (pick.key) ")" }))
                    (table_cell(false, html! { (pick.confirmed) }))
                    (table_cell(false, html! { (pick.all) }))
                }))
            }
            (table_row(html! {
                (table_cell(true, html! { "Wrote their own words" }))
                (table_cell(false, html! { (followup.other_text.confirmed) }))
                (table_cell(false, html! { (followup.other_text.all) }))
            }))
            (table_row(html! {
                (table_cell(true, html! { "Answered the question" }))
                (table_cell(false, html! { (followup.answered.confirmed) }))
                (table_cell(false, html! { (followup.answered.all) }))
            }))
        },
    )
}

fn followup_texts(texts: &[AdminOperatorPassWaitlistFollowupTextEntry]) -> Markup {
    html! {
        @if texts.is_empty() {
            (empty_state_full(None, "No answers in their own words yet.", None))
        } @else {
            ul class="flex flex-col divide-y divide-neutral-200" {
                @for entry in texts {
                    li class="flex flex-col gap-1 py-3" {
                        p class="whitespace-pre-wrap break-words text-neutral-900 text-sm" { (entry.other_text) }
                        p class="text-neutral-600 text-xs" {
                            (format_time(Some(&entry.answered_at))) ", " (entry.status) ", picked "
                            (joined_keys(&entry.picks, ", "))
                            @if !entry.about.is_empty() {
                                ", describes themselves as " (joined_keys(&entry.about, ", "))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn followup_content(
    base: &str,
    followup: &AdminOperatorPassWaitlistFollowupStats,
    texts: Option<&[AdminOperatorPassWaitlistFollowupTextEntry]>,
) -> Markup {
    html! {
        (card_with_header("Which of these describe you?", about_table(followup)))
        (card_with_header("Most important to them", followup_table(followup)))
        (card_with_header("Answers in their own words", html! {
            p class="mb-3 text-sm text-neutral-600" {
                "Shown as plain text, for staff only. "
                a class="underline" href={(base) (FOLLOWUP_CSV_PATH)} hx-boost="false" download { "Download the counts and answers as CSV" }
            }
            @match texts {
                Some(texts) => { (followup_texts(texts)) }
                None => { (error_alert("Could not load the answers in their own words.")) }
            }
        }))
    }
}

fn csv_field(value: &str) -> String {
    let guarded = if value.starts_with(['=', '+', '-', '@', '\t', '\r']) {
        format!("'{value}")
    } else {
        value.to_owned()
    };
    format!("\"{}\"", guarded.replace('"', "\"\""))
}

fn csv_row(fields: &[&str]) -> String {
    let mut row = fields
        .iter()
        .map(|field| csv_field(field))
        .collect::<Vec<_>>()
        .join(",");
    row.push_str("\r\n");
    row
}

pub fn followup_csv(
    stats: &AdminOperatorPassWaitlistStatsResponse,
    texts: &[AdminOperatorPassWaitlistFollowupTextEntry],
) -> String {
    let mut csv = csv_row(&[
        "kind",
        "option",
        "confirmed",
        "all",
        "answered_at",
        "status",
        "picks",
        "about",
        "other_text",
    ]);
    for pick in &stats.followup.picks {
        csv.push_str(&csv_row(&[
            "count",
            &pick.key.to_string(),
            &pick.confirmed.to_string(),
            &pick.all.to_string(),
            "",
            "",
            "",
            "",
            "",
        ]));
    }
    for about in &stats.followup.about {
        csv.push_str(&csv_row(&[
            "about_count",
            &about.key.to_string(),
            &about.confirmed.to_string(),
            &about.all.to_string(),
            "",
            "",
            "",
            "",
            "",
        ]));
    }
    let other_text = &stats.followup.other_text;
    let answered = &stats.followup.answered;
    for (label, confirmed, all) in [
        ("own_words", other_text.confirmed, other_text.all),
        ("answered", answered.confirmed, answered.all),
    ] {
        csv.push_str(&csv_row(&[
            "count",
            label,
            &confirmed.to_string(),
            &all.to_string(),
            "",
            "",
            "",
            "",
            "",
        ]));
    }
    for entry in texts {
        csv.push_str(&csv_row(&[
            "answer",
            "",
            "",
            "",
            &serde_json::to_value(entry.answered_at)
                .ok()
                .and_then(|value| value.as_str().map(str::to_owned))
                .unwrap_or_default(),
            &entry.status.to_string(),
            &joined_keys(&entry.picks, " "),
            &joined_keys(&entry.about, " "),
            &entry.other_text,
        ]));
    }
    csv
}

fn status_table(stats: &AdminOperatorPassWaitlistStatsResponse) -> Markup {
    let rows = [
        ("Pending", stats.by_status.pending),
        ("Confirmed", stats.by_status.confirmed),
        ("Unsubscribed", stats.by_status.unsubscribed),
        ("Bounced", stats.by_status.bounced),
        ("Total", stats.total),
    ];
    data_table(
        &["Status", "Signups"],
        html! {
            @for (label, count) in rows {
                (table_row(html! {
                    (table_cell(false, html! { (label) }))
                    (table_cell(false, html! { (count) }))
                }))
            }
        },
    )
}

fn currency_table(distribution: &AdminOperatorPassPriceFeedbackCurrency) -> Markup {
    data_table(
        &["Answer", "Confirmed", "All signups"],
        html! {
            @for answer in &distribution.answers {
                (table_row(html! {
                    (table_cell(true, html! { (price_feedback_label(&answer.feedback)) }))
                    (table_cell(false, html! { (answer.confirmed) }))
                    (table_cell(false, html! { (answer.all) }))
                }))
            }
            (table_row(html! {
                (table_cell(true, html! { "Total" }))
                (table_cell(false, html! { (distribution.confirmed) }))
                (table_cell(false, html! { (distribution.all) }))
            }))
        },
    )
}

fn stats_content(
    base: &str,
    stats: &AdminOperatorPassWaitlistStatsResponse,
    texts: Option<&[AdminOperatorPassWaitlistFollowupTextEntry]>,
) -> Markup {
    html! {
        div class="flex flex-col gap-6" {
            @if !stats.standard_price_set {
                (alert(
                    AlertVariant::Warning,
                    Some("No standard price is set"),
                    html! {
                        "When the early bird price ends, the pass keeps selling at the early bird price. "
                        "Set FLUXER_OPERATOR_PASS_STANDARD_PRICE_* for every currency on the API and the marketing site to change it."
                    },
                ))
            }
            (card_with_header("Signups", status_table(stats)))
            (followup_content(base, &stats.followup, texts))
            @let answered: Vec<_> = stats.price_feedback.iter().filter(|entry| entry.all > 0).collect();
            @if answered.is_empty() {
                (card_with_header("Price answers", empty_state_full(
                    None,
                    "No price answers yet.",
                    Some("Answers appear here once people join the waitlist."),
                )))
            }
            @for distribution in answered {
                (card_with_header(
                    &format!("Early bird price answers in {}", distribution.currency),
                    currency_table(distribution),
                ))
            }
            @if stats.unanswered.all > 0 {
                p class="text-sm text-neutral-600" {
                    "Signups without a price answer: " (stats.unanswered.all)
                    " (" (stats.unanswered.confirmed) " confirmed)"
                }
            }
        }
    }
}

pub fn operator_pass_waitlist_page(
    config: &AdminConfig,
    auth: &AuthContext,
    stats: Option<&AdminOperatorPassWaitlistStatsResponse>,
    texts: Option<&[AdminOperatorPassWaitlistFollowupTextEntry]>,
) -> Markup {
    let content = html! {
        (page_header(
            "Operator Pass Waitlist",
            Some("Signups by status, how they describe themselves, what is most important to them, and how people felt about the early bird price they saw, per currency. No addresses."),
        ))
        @match stats {
            Some(stats) => { (stats_content(&config.base_path, stats, texts)) }
            None => {
                (error_alert(
                    "Could not load waitlist stats. The API request failed or this account lacks the operator_pass:waitlist_stats ACL.",
                ))
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Operator Pass Waitlist",
        "operator-pass-waitlist",
        None,
        content,
    )
}
