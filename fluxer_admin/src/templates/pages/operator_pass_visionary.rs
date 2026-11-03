// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        generated::types::{
            AdminOperatorPassVisionaryActivationResponse, AdminOperatorPassVisionaryNoticeResponse,
        },
        types::FlashMessage,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert,
            form::{FORM_INPUT_CLASS, csrf_input, submit_button},
            page_container::{card_with_header, detail_row, page_header},
        },
        layout::admin_layout,
        pages::operator_pass_wall::format_time,
    },
};
use maud::{Markup, html};

pub const VISIONARY_PATH: &str = "/operator-pass-visionary";
pub const VISIONARY_CSV_PATH: &str = "/operator-pass-visionary/activations.csv";
pub const DELIVERIES_CSV_PATH: &str = "/operator-pass-visionary/deliveries.csv";

fn counts(status: &AdminOperatorPassVisionaryNoticeResponse) -> Markup {
    let skipped = &status.skipped;
    html! {
        (detail_row("Will get the email on the next run", html! { (status.recipients) }))
        (detail_row("Already emailed", html! { (status.emailed) }))
        (detail_row("Got the system DM", html! { (status.dm_sent) }))
        (detail_row("Already turned it on", html! { (skipped.turned_on) }))
        (detail_row("No working email, DM only", html! { (skipped.no_email) }))
        (detail_row("No Visionary now", html! { (skipped.not_visionary) }))
        (detail_row("Deleted, disabled or spam", html! { (skipped.inactive) }))
        (detail_row("Deletion pending", html! { (skipped.pending_deletion) }))
        (detail_row("Removed under section 8", html! { (skipped.removed) }))
        (detail_row("No Visionary payment on record, the pass shows the Plutonium start date", html! { (status.date_unknown) }))
        (detail_row("Run going now", html! { (if status.running { "Yes" } else { "No" }) }))
    }
}

pub fn operator_pass_visionary_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    status: Option<&AdminOperatorPassVisionaryNoticeResponse>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header(
            "Visionary Email",
            Some("The one email that tells every Visionary how to turn on the Operator Pass that comes with Visionary. It never sends on its own, and each account gets it at most once."),
        ))
        div class="flex flex-col gap-6" {
            @match status {
                None => {
                    (error_alert("Could not load the email status. The API request failed or this account lacks the operator_pass:visionary_notice ACL."))
                }
                Some(status) => {
                    (card_with_header("Status", counts(status)))
                    (card_with_header("Send", html! {
                        p class="mb-3 text-sm text-neutral-600" {
                            "Type the number of accounts the next run will email, as shown above. The send is refused when a fresh count differs. Sending again later only reaches accounts not reached yet."
                        }
                        form method="post" action={(base) (VISIONARY_PATH) "/send"} class="flex flex-col gap-3" {
                            (csrf_input(csrf_token))
                            label class="flex flex-col gap-1 text-sm" {
                                "Accounts to email"
                                input type="number" name="expected_recipients" min="0" required class=(FORM_INPUT_CLASS);
                            }
                            label class="flex items-center gap-2 text-sm" {
                                input type="checkbox" name="include_dm" value="true";
                                "Also send each reached account one system DM"
                            }
                            div { (submit_button("Send the email")) }
                        }
                    }))
                }
            }
            (card_with_header("For the books", html! {
                div class="flex flex-col gap-4" {
                    a class="underline text-sm" href={(base) (VISIONARY_CSV_PATH)} hx-boost="false" download {
                        "Download every turn-on as CSV"
                    }
                    form method="get" action={(base) (DELIVERIES_CSV_PATH)} hx-boost="false" class="flex flex-col gap-3" {
                        p class="text-sm text-neutral-600" {
                            "One row per pass for every purchase delivered in the month, in UTC. Save it as operator-pass-leveranser-YYYY-MM.csv in the books."
                        }
                        label class="flex flex-col gap-1 text-sm" {
                            "Month of delivery"
                            input type="month" name="month" required pattern="[0-9]{4}-[0-9]{2}" class=(FORM_INPUT_CLASS);
                        }
                        div { (submit_button("Download deliveries as CSV")) }
                    }
                }
            }))
        }
    };
    admin_layout(
        config,
        auth,
        "Visionary Email",
        "operator-pass-visionary",
        flash,
        content,
    )
}

pub fn visionary_activation_card(
    activation: &AdminOperatorPassVisionaryActivationResponse,
) -> Markup {
    html! {
        (detail_row("Turned on", html! { (format_time(activation.activated_at.as_ref())) }))
        (detail_row("Visionary date on the pass", html! { (format_time(activation.visionary_since.as_ref())) }))
        (detail_row("Certificate number", html! {
            @match activation.certificate_number { Some(number) => { (number) } None => { "Not set" } }
        }))
        (detail_row("Terms version", html! { (activation.terms_version.as_deref().unwrap_or("Not set")) }))
        (detail_row("Ended", html! {
            (format_time(activation.ended_at.as_ref()))
            @if let Some(reason) = &activation.end_reason { ", " (reason) }
        }))
        (detail_row("Email handed to the provider", html! { (format_time(activation.notice_emailed_at.as_ref())) }))
        (detail_row("Last skip reason", html! { (activation.notice_skip_reason.as_deref().unwrap_or("None")) }))
        (detail_row("System DM", html! { (format_time(activation.notice_dm_sent_at.as_ref())) }))
    }
}
