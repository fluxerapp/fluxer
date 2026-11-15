// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{PendingRegistrationResponse, RegistrationUrlResponse},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert, form::csrf_input, page_container::page_header,
            table::empty_state,
        },
        layout::admin_layout,
        layout_sidebar_nav::PENDING_REGISTRATIONS_KEY,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

use super::instance_registration::{
    CompactButtonVariant, admin_issued_registration_url, compact_button, compact_copy_button,
};

pub fn pending_registrations_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    pending_registrations: Option<&[PendingRegistrationResponse]>,
    registration_urls: &[RegistrationUrlResponse],
) -> Markup {
    let content = html! {
        (page_header(
            "Pending Registrations",
            Some("Approve or reject accounts waiting for a registration decision. Rejecting deletes the account."),
        ))
        @match pending_registrations {
            Some(pending_registrations) => {
                (pending_registration_list(config, csrf_token, pending_registrations, registration_urls))
            }
            None => (error_alert("Failed to load pending registrations.")),
        }
    };
    admin_layout(
        config,
        auth,
        "Pending Registrations",
        PENDING_REGISTRATIONS_KEY,
        None,
        content,
    )
}

pub fn pending_registration_list(
    config: &AdminConfig,
    csrf_token: &str,
    pending_registrations: &[PendingRegistrationResponse],
    registration_urls: &[RegistrationUrlResponse],
) -> Markup {
    let base = &config.base_path;
    html! {
        div id="pending-registration-list" {
            @if pending_registrations.is_empty() {
                (empty_state("No pending registrations."))
            } @else {
                div class="overflow-x-auto rounded-lg border border-neutral-200" {
                    table class="min-w-[940px] divide-y divide-neutral-200 text-sm" {
                        thead class="bg-neutral-50" {
                            tr {
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Applicant" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Requested" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Registration URL" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "IP" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Action" }
                            }
                        }
                        tbody class="divide-y divide-neutral-200 bg-white" {
                            @for pending in pending_registrations {
                                (pending_registration_row(config, base, csrf_token, pending, registration_urls))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn pending_registration_row(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    pending: &PendingRegistrationResponse,
    registration_urls: &[RegistrationUrlResponse],
) -> Markup {
    let account_name = pending_registration_account_name(pending);
    let requested_at = format_admin_timestamp(&pending.requested_at);
    let client_ip = pending.client_ip.as_deref().unwrap_or("None");
    let approve_action = format!("{base}/pending-registrations/approve");
    let reject_action = format!("{base}/pending-registrations/reject");
    html! {
        tr {
            td class="max-w-[20rem] px-4 py-3 align-top" {
                p class="font-medium text-neutral-900" { (account_name) }
                @if let Some(global_name) = pending.global_name.as_deref() {
                    p class="truncate text-xs text-neutral-500" title=(global_name) { (global_name) }
                }
                @if let Some(email) = pending.email.as_deref() {
                    p class="truncate text-xs text-neutral-500" title=(email) { (email) }
                }
                p class="whitespace-nowrap text-xs text-neutral-500" { "ID: " (&pending.user_id) }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (requested_at)
            }
            td class="px-4 py-3 align-top text-neutral-700" {
                @if let Some(registration_url_id) = pending.registration_url_id.as_deref() {
                    p class="whitespace-nowrap text-xs text-neutral-500" {
                        (registration_url_name(registration_urls, registration_url_id))
                    }
                    div class="mt-1" {
                        (compact_copy_button(&admin_issued_registration_url(config, registration_url_id), "Copy URL"))
                    }
                } @else {
                    span class="text-neutral-500" { "None" }
                }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (client_ip)
            }
            td class="px-4 py-3 align-top" {
                div class="flex flex-nowrap gap-2" {
                    (decision_form(&approve_action, csrf_token, &pending.user_id, "Approve", CompactButtonVariant::Neutral, None))
                    (decision_form(
                        &reject_action,
                        csrf_token,
                        &pending.user_id,
                        "Reject",
                        CompactButtonVariant::Danger,
                        Some("Reject this registration and delete the account?"),
                    ))
                }
            }
        }
    }
}

fn decision_form(
    action: &str,
    csrf_token: &str,
    user_id: &str,
    label: &str,
    variant: CompactButtonVariant,
    confirm: Option<&str>,
) -> Markup {
    html! {
        form method="post" action=(action)
            hx-post=(action)
            hx-target="#pending-registration-list"
            hx-swap="outerHTML"
            hx-push-url="false"
            hx-confirm=[confirm]
            data-admin-allow-swap="true" {
            (csrf_input(csrf_token))
            input type="hidden" name="user_id" value=(user_id);
            (compact_button(label, variant))
        }
    }
}

fn pending_registration_account_name(pending: &PendingRegistrationResponse) -> String {
    if pending.discriminator == 0 {
        pending.username.clone()
    } else {
        format!("{}#{:04}", pending.username, pending.discriminator)
    }
}

fn registration_url_name(registration_urls: &[RegistrationUrlResponse], id: &str) -> String {
    registration_urls
        .iter()
        .find(|url| url.id == id)
        .and_then(|url| url.label.clone())
        .unwrap_or_else(|| format!("ID: {id}"))
}
