// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::generated::types::{
        AdminPaymentDisputeProductSchema, AdminPaymentDisputeReviewResponse,
        AdminPaymentDisputeReviewStatusSchema,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{
                FORM_SELECT_CLASS, FORM_TEXTAREA_CLASS, csrf_input, danger_button,
                secondary_button, submit_button,
            },
            page_container::{card_with_header, page_header},
            resource_link::{ResourceType, resource_link},
            table::{data_table, table_cell, table_row},
        },
        layout::admin_layout,
        pages::operator_pass_wall::format_time,
    },
};
use chrono::{DateTime, Utc};
use maud::{Markup, html};
use std::cmp::Reverse;

type Review = AdminPaymentDisputeReviewResponse;

const REFRESH_TARGET: &str = "#payment-disputes";

pub fn format_amount(minor: i64, currency: &str) -> String {
    let sign = if minor < 0 { "-" } else { "" };
    let minor = minor.unsigned_abs();
    format!("{sign}{}.{:02} {currency}", minor / 100, minor % 100)
}

fn product_label(product: AdminPaymentDisputeProductSchema) -> &'static str {
    match product {
        AdminPaymentDisputeProductSchema::OperatorPass => "Operator Pass",
        AdminPaymentDisputeProductSchema::Plutonium => "Plutonium",
    }
}

pub fn is_warning_incomplete(review: &Review) -> bool {
    review.status == AdminPaymentDisputeReviewStatusSchema::Unjustified
        && review.warned_at.is_none()
        && review.reversed_at.is_none()
}

pub fn status_label(review: &Review) -> String {
    let label = review_status_label(review);
    match review.dispute_purchase_at {
        Some(_) if review.closed_at.is_none() => {
            format!("{label}, purchase while open")
        }
        _ => label,
    }
}

fn review_status_label(review: &Review) -> String {
    match review.status {
        AdminPaymentDisputeReviewStatusSchema::Open if review.blockers.is_empty() => {
            "Not reviewed".to_owned()
        }
        AdminPaymentDisputeReviewStatusSchema::Open => {
            let blockers: Vec<String> = review.blockers.iter().map(ToString::to_string).collect();
            format!(
                "Not reviewed, cannot be unjustified: {}",
                blockers.join(", ")
            )
        }
        AdminPaymentDisputeReviewStatusSchema::Justified if review.reversed_at.is_some() => {
            "Justified after an appeal".to_owned()
        }
        AdminPaymentDisputeReviewStatusSchema::Justified => "Justified".to_owned(),
        AdminPaymentDisputeReviewStatusSchema::Unjustified if review.warned_at.is_some() => {
            "Unjustified, warned".to_owned()
        }
        AdminPaymentDisputeReviewStatusSchema::Unjustified => {
            let sent = |at: &Option<DateTime<Utc>>| if at.is_some() { "sent" } else { "not sent" };
            format!(
                "Unjustified, warning incomplete (email {}, system message {})",
                sent(&review.warning_email_sent_at),
                sent(&review.warning_dm_sent_at)
            )
        }
    }
}

pub fn open_reviews(reviews: &[Review]) -> Vec<&Review> {
    let mut open: Vec<&Review> = reviews
        .iter()
        .filter(|review| review.closed_at.is_none())
        .collect();
    open.sort_by_key(|review| {
        (
            review.warning_deadline.is_none(),
            Reverse(review.warning_deadline),
            Reverse(review.disputed_at),
        )
    });
    open
}

pub fn reversible_closed_reviews(reviews: &[Review]) -> Vec<&Review> {
    reviews
        .iter()
        .filter(|review| {
            review.closed_at.is_some()
                && review.status == AdminPaymentDisputeReviewStatusSchema::Unjustified
                && review.reversed_at.is_none()
        })
        .collect()
}

fn note_field(name: &str, placeholder: &str, max_length: u16, required: bool) -> Markup {
    html! {
        textarea name=(name) rows="2" placeholder=(placeholder) maxlength=(max_length)
            required[required] class=(FORM_TEXTAREA_CLASS) {}
    }
}

fn holder_note_field() -> Markup {
    html! {
        label class="flex flex-col gap-1 text-sm text-neutral-700" {
            "Note to the account holder"
            (note_field("note", "Why the dispute is unjustified or justified", 500, false))
            span class="text-xs text-neutral-600" {
                "The account holder sees this note in the warning email and system message, and in the enforcement notice if the account is closed. Up to 500 characters."
            }
        }
    }
}

fn decision_form(base: &str, csrf_token: &str, review: &Review) -> Markup {
    html! {
        form method="post" action={(base) "/payment-disputes/" (review.dispute_id) "/decision"}
            class="flex min-w-64 flex-col gap-2" data-admin-refresh-on-success=(REFRESH_TARGET) {
            (csrf_input(csrf_token))
            label class="text-sm text-neutral-700" {
                "Decision"
                select name="decision" class=(FORM_SELECT_CLASS) {
                    option value="justified" { "Justified" }
                    @if review.warnable {
                        option value="unjustified" { "Unjustified, warn the account" }
                    }
                }
            }
            (holder_note_field())
            div { (submit_button("Record decision")) }
        }
    }
}

fn retry_form(base: &str, csrf_token: &str, review: &Review) -> Markup {
    html! {
        form method="post" action={(base) "/payment-disputes/" (review.dispute_id) "/decision"}
            data-admin-refresh-on-success=(REFRESH_TARGET) {
            (csrf_input(csrf_token))
            input type="hidden" name="decision" value="unjustified";
            (secondary_button("Send the missing warning"))
        }
    }
}

fn reversal_form(base: &str, csrf_token: &str, review: &Review) -> Markup {
    html! {
        form method="post" action={(base) "/payment-disputes/" (review.dispute_id) "/reversal"}
            class="flex min-w-64 flex-col gap-2" data-admin-refresh-on-success=(REFRESH_TARGET) {
            (csrf_input(csrf_token))
            (note_field("note", "What the appeal showed", 2000, true))
            div { (danger_button("Reverse after an appeal")) }
        }
    }
}

fn actions(base: &str, csrf_token: &str, review: &Review) -> Markup {
    html! {
        div class="flex flex-col gap-3" {
            @if review.status == AdminPaymentDisputeReviewStatusSchema::Open && review.closed_at.is_none() {
                (decision_form(base, csrf_token, review))
            }
            @if is_warning_incomplete(review) && review.closed_at.is_none() {
                (retry_form(base, csrf_token, review))
            }
            @if review.status == AdminPaymentDisputeReviewStatusSchema::Unjustified && review.reversed_at.is_none() {
                (reversal_form(base, csrf_token, review))
            }
        }
    }
}

fn review_rows(
    base: &str,
    csrf_token: &str,
    reviews: &[&Review],
    can_decide: bool,
    deadline_column: bool,
) -> Markup {
    html! {
        @for review in reviews {
            (table_row(html! {
                (table_cell(false, html! {
                    div class="flex flex-col gap-1" {
                        span class="font-mono text-xs" { (review.dispute_id) }
                        span class="text-xs text-neutral-600" {
                            (product_label(review.product))
                            @if review.is_gift { ", gift" }
                            @if let Some(number) = &review.invoice_number { ", invoice " (number) }
                        }
                    }
                }))
                (table_cell(false, resource_link(
                    base,
                    ResourceType::User,
                    review.purchaser_id.as_str(),
                    html! { (review.purchaser_id.as_str()) },
                )))
                (table_cell(false, html! { (format_amount(review.amount_minor, &review.currency)) }))
                (table_cell(false, html! { (review.reason) }))
                (table_cell(true, html! { (format_time(review.evidence_due_by.as_ref())) }))
                @if deadline_column {
                    (table_cell(true, html! { (format_time(review.warning_deadline.as_ref())) }))
                } @else {
                    (table_cell(true, html! { (format_time(review.closed_at.as_ref())) }))
                }
                (table_cell(false, html! {
                    div class="flex flex-col gap-1" {
                        span { (status_label(review)) }
                        @if let Some(decision_id) = review.enforcement_decision_id.as_deref() {
                            a href={(base) "/decisions/" (decision_id)} class="text-xs text-blue-600 hover:underline" {
                                "Decision " (decision_id)
                            }
                        } @else if let Some(decision_id) = review.awaited_decision_id.as_deref() {
                            span class="text-xs text-neutral-600" {
                                "Closure waits for "
                                a href={(base) "/decisions/" (decision_id)} class="text-blue-600 hover:underline" {
                                    "decision " (decision_id)
                                }
                                " to end"
                            }
                        }
                    }
                }))
                @if can_decide {
                    (table_cell(false, actions(base, csrf_token, review)))
                }
            }))
        }
    }
}

fn reviews_table(
    base: &str,
    csrf_token: &str,
    reviews: &[&Review],
    can_decide: bool,
    deadline_column: bool,
) -> Markup {
    let mut headers = vec![
        "Dispute",
        "Purchaser",
        "Amount",
        "Reason",
        "Evidence due",
        if deadline_column {
            "Warning deadline"
        } else {
            "Closed"
        },
        "Status",
    ];
    if can_decide {
        headers.push("Actions");
    }
    data_table(
        &headers,
        review_rows(base, csrf_token, reviews, can_decide, deadline_column),
    )
}

fn reviews_content(base: &str, csrf_token: &str, reviews: &[Review], can_decide: bool) -> Markup {
    let open = open_reviews(reviews);
    let reversible = reversible_closed_reviews(reviews);
    html! {
        div class="flex flex-col gap-6" {
            @if open.is_empty() {
                (card_with_header("Open reviews", empty_state_full(
                    None,
                    "No open payment disputes.",
                    Some("A review appears here when a dispute opens and stays until the dispute closes."),
                )))
            } @else {
                (card_with_header("Open reviews", reviews_table(base, csrf_token, &open, can_decide, true)))
            }
            @if !reversible.is_empty() {
                (card_with_header(
                    "Closed unjustified reviews",
                    reviews_table(base, csrf_token, &reversible, can_decide, false),
                ))
            }
        }
    }
}

pub fn payment_disputes_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    reviews: Option<&[Review]>,
    can_decide: bool,
) -> Markup {
    let content = html! {
        (page_header(
            "Payment Disputes",
            Some("Open payment disputes, the latest warning deadline first. Unjustified warns the account by email and system message, and only a dispute that ends with the money returned after the warning date has a consequence: for Plutonium an enforcement decision closes the account, and for the Operator Pass the bonus is removed permanently under section 8, so the account can't buy or receive a pass again."),
        ))
        div id="payment-disputes" {
            @match reviews {
                Some(reviews) => { (reviews_content(&config.base_path, csrf_token, reviews, can_decide)) }
                None => {
                    (error_alert(
                        "Could not load payment dispute reviews. The API request failed or this account lacks the user:lookup ACL.",
                    ))
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Payment Disputes",
        "payment-disputes",
        None,
        content,
    )
}
