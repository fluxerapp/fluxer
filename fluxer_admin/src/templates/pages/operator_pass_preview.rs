// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        generated::types::{
            AdminOperatorPassPreviewPurchase, AdminOperatorPassPreviewResponse,
            AdminOperatorPassResetPlan, OperatorPassPurchaseStatusSchema,
        },
        types::FlashMessage,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert,
            form::{FORM_INPUT_CLASS, csrf_input, danger_button, submit_button},
            page_container::{card_with_header, detail_row, page_header},
        },
        layout::admin_layout,
        pages::operator_pass_wall::format_time,
    },
};
use maud::{Markup, html};

pub const PREVIEW_PATH: &str = "/operator-pass-preview";

pub struct PreviewPageAccess {
    pub can_manage: bool,
    pub can_reset: bool,
}

fn refusal_text(plan: &AdminOperatorPassResetPlan) -> &'static str {
    match plan
        .refused_reason
        .as_ref()
        .map(ToString::to_string)
        .as_deref()
    {
        Some("non_preview_purchases") => {
            "Refused: a purchase exists that was not made through a preview entry, so real customer data would be lost."
        }
        Some("other_holders") => {
            "Refused: an account holds passes that no preview purchase explains."
        }
        Some("visionary_activations") => "Refused: a Visionary holder turned on the Operator Pass.",
        Some("other_certificates") => {
            "Refused: a certificate number belongs to a pass outside the preview purchases."
        }
        Some("billing_documents") => "Refused: a preview purchase has a billing document.",
        _ => "The reset can run.",
    }
}

fn entries_card(
    base: &str,
    csrf_token: &str,
    preview: &AdminOperatorPassPreviewResponse,
    can_manage: bool,
) -> Markup {
    html! {
        div class="flex flex-col gap-4" {
            p class="text-sm text-neutral-600" {
                "Until the sale opens for everyone, requests from these addresses see the sale open in the app, the API and on /pass?test=1, and every checkout from them costs nothing. IPv6 addresses change often, so add the /64 range of your network."
            }
            @if preview.sale_open {
                p class="text-sm text-neutral-600" { "The sale is open for everyone, so these entries do nothing now." }
            }
            @if preview.entries.is_empty() && preview.config_values.is_empty() {
                p class="text-sm text-neutral-600" { "There are no entries." }
            }
            @for entry in &preview.entries {
                div class="rounded-lg border border-neutral-200 p-3" {
                    (detail_row("Address or range", html! { code { (entry.value) } }))
                    (detail_row("Label", html! { (entry.label) }))
                    (detail_row("Expires", html! { (format_time(entry.expires_at.as_ref())) }))
                    (detail_row("Added", html! { (format_time(Some(&entry.created_at))) }))
                    @if can_manage {
                        form method="post" action={(base) (PREVIEW_PATH) "/ips/" (entry.id) "/remove"} class="mt-3" {
                            (csrf_input(csrf_token))
                            (danger_button("Remove"))
                        }
                    }
                }
            }
            @for value in &preview.config_values {
                div class="rounded-lg border border-neutral-200 p-3" {
                    (detail_row("Address or range", html! { code { (value) } }))
                    (detail_row("Set in", html! { "FLUXER_OPERATOR_PASS_PREVIEW_IPS" }))
                }
            }
            @if can_manage {
                form method="post" action={(base) (PREVIEW_PATH) "/ips"} class="flex flex-col gap-3" {
                    (csrf_input(csrf_token))
                    label class="flex flex-col gap-1 text-sm" {
                        "IPv4 or IPv6 address, or a CIDR range"
                        input type="text" name="value" required maxlength="64" class=(FORM_INPUT_CLASS);
                    }
                    label class="flex flex-col gap-1 text-sm" {
                        "Label"
                        input type="text" name="label" required maxlength="100" class=(FORM_INPUT_CLASS);
                    }
                    label class="flex flex-col gap-1 text-sm" {
                        "Expires, in UTC (optional)"
                        input type="datetime-local" name="expires_at" class=(FORM_INPUT_CLASS);
                    }
                    div { (submit_button("Add entry")) }
                }
            }
        }
    }
}

fn purchase_card(
    base: &str,
    csrf_token: &str,
    purchase: &AdminOperatorPassPreviewPurchase,
    can_manage: bool,
) -> Markup {
    let numbers = purchase
        .certificate_numbers
        .iter()
        .map(|number| format!("{number:06}"))
        .collect::<Vec<_>>()
        .join(", ");
    html! {
        div class="rounded-lg border border-neutral-200 p-3" {
            (detail_row("Purchase", html! { (purchase.purchase_id) }))
            (detail_row("Account", html! {
                a class="underline" href={(base) "/users/" (purchase.purchaser_id.to_string())} { (purchase.purchaser_id.to_string()) }
            }))
            (detail_row("Entry", html! { (purchase.label) }))
            (detail_row("Status", html! { (purchase.status.label()) }))
            (detail_row("Passes", html! { (purchase.quantity) }))
            (detail_row("Delivered", html! { (format_time(purchase.delivered_at.as_ref())) }))
            (detail_row("Certificate numbers", html! { @if numbers.is_empty() { "None" } @else { (numbers) } }))
            @if can_manage && matches!(
                purchase.status,
                OperatorPassPurchaseStatusSchema::Pending
                    | OperatorPassPurchaseStatusSchema::Completed
                    | OperatorPassPurchaseStatusSchema::Disputed
            ) {
                form method="post" action={(base) (PREVIEW_PATH) "/purchases/" (purchase.purchase_id) "/cancel"} class="mt-3" {
                    (csrf_input(csrf_token))
                    (danger_button("Cancel this preview purchase"))
                }
            }
        }
    }
}

fn reset_card(base: &str, csrf_token: &str, plan: &AdminOperatorPassResetPlan) -> Markup {
    html! {
        div class="flex flex-col gap-3" {
            p class="text-sm text-neutral-600" {
                "Removes every preview purchase with its passes, certificate pages and numbers, gift offers, wall entries, the Fluxer Labs rows they created and the Labs role they gave, and starts certificate numbering again at 000001. The waitlist stays. It is refused whenever any purchase was not made through a preview entry."
            }
            (detail_row("Preview purchases", html! { (plan.preview_purchases) }))
            (detail_row("Other purchases", html! { (plan.non_preview_purchases) }))
            (detail_row("Passes", html! { (plan.passes) }))
            (detail_row("Certificate numbers", html! { (plan.certificate_numbers) }))
            (detail_row("Accounts", html! { (plan.users) }))
            p class="text-sm" { (refusal_text(plan)) }
            @if plan.allowed {
                form method="post" action={(base) (PREVIEW_PATH) "/reset"} class="flex flex-col gap-3" {
                    (csrf_input(csrf_token))
                    label class="flex flex-col gap-1 text-sm" {
                        "Type RESET to confirm"
                        input type="text" name="confirm" required autocomplete="off" pattern="RESET" class=(FORM_INPUT_CLASS);
                    }
                    div { (danger_button("Reset Operator Pass data")) }
                }
            }
        }
    }
}

pub fn operator_pass_preview_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    access: &PreviewPageAccess,
    preview: Option<&AdminOperatorPassPreviewResponse>,
    plan: Option<&AdminOperatorPassResetPlan>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header(
            "Operator Pass Preview",
            Some("Open the sale early for chosen IP addresses, buy passes there at no cost, and clean up before the sale opens."),
        ))
        div class="flex flex-col gap-6" {
            @match preview {
                None if !access.can_manage => {}
                None => {
                    (error_alert("Could not load the preview entries. The API request failed or this account lacks the operator_pass:preview ACL."))
                }
                Some(preview) => {
                    (card_with_header("Preview entries", entries_card(base, csrf_token, preview, access.can_manage)))
                    (card_with_header("Preview purchases", html! {
                        div class="flex flex-col gap-4" {
                            @if preview.purchases.is_empty() {
                                p class="text-sm text-neutral-600" { "There are no preview purchases." }
                            }
                            @for purchase in &preview.purchases {
                                (purchase_card(base, csrf_token, purchase, access.can_manage))
                            }
                        }
                    }))
                }
            }
            @if access.can_reset {
                @match plan {
                    None => {
                        (error_alert("Could not load the reset plan. The API request failed or this account lacks the operator_pass:reset ACL."))
                    }
                    Some(plan) => {
                        (card_with_header("Reset Operator Pass data", reset_card(base, csrf_token, plan)))
                    }
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Operator Pass Preview",
        "operator-pass-preview",
        flash,
        content,
    )
}
