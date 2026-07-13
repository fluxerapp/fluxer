// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        InstanceConfigResponse, InstanceRegistrationResponse, PendingRegistrationResponse,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::csrf_input, page_container::page_header, section_card::section_card_simple,
            table::empty_state,
        },
        layout::admin_layout,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

pub fn pending_guilds_users_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    instance_config: Option<&InstanceConfigResponse>,
) -> Markup {
    // println!("{}", &instance_config.registration.pending_registrations.len());
    println!(
        "{}",
        &instance_config
            .unwrap()
            .registration
            .pending_registrations
            .len()
    );

    let content = html! {
        (page_header("Pending Registrations", Some("Review pending users awaiting approval")))
        div class="space-y-10" {
            @if let Some(instance_config) = instance_config {
                (html! {
                        (registration_config_section(
                            config,
                            csrf_token,
                            &instance_config.registration,
                        ))
                    })
            } @else {
                (section_card_simple("Pending Registrations", html! {
                    p class="text-sm text-red-600" { "Failed to load instance configuration." }
                }))
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Pending Registrations",
        "pending",
        None,
        content,
    )
}

fn registration_config_section(
    config: &AdminConfig,
    csrf_token: &str,
    registration: &InstanceRegistrationResponse,
) -> Markup {
    pending_registration_list(config, csrf_token, &registration.pending_registrations)
}

pub fn pending_registration_list(
    config: &AdminConfig,
    csrf_token: &str,
    pending_registrations: &[PendingRegistrationResponse],
) -> Markup {
    let base = &config.base_path;
    html! {
        div id="pending-registration-list" class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Pending Registrations" }
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
                                (pending_registration_row(config, base, csrf_token, pending))
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
) -> Markup {
    let account_name = pending_registration_account_name(pending);
    let requested_at = format_admin_timestamp(&pending.requested_at);
    let email = pending.email.as_deref().unwrap_or("None");
    let link_id = pending.registration_url_id.as_deref().unwrap_or("None");
    let client_ip = pending.client_ip.as_deref().unwrap_or("None");
    let approve_action = format!("{base}/instance-config?action=approve_pending_registration");
    let reject_action = format!("{base}/instance-config?action=reject_pending_registration");
    html! {
        tr {
            td class="max-w-[20rem] px-4 py-3 align-top" {
                p class="font-medium text-neutral-900" { (account_name) }
                @if let Some(global_name) = pending.global_name.as_deref() {
                    p class="truncate text-xs text-neutral-500" title=(global_name) { (global_name) }
                }
                p class="truncate text-xs text-neutral-500" title=(email) { (email) }
                p class="whitespace-nowrap text-xs text-neutral-500" { "ID: " (&pending.user_id) }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (requested_at)
            }
            td class="px-4 py-3 align-top text-neutral-700" {
                @if let Some(registration_url_id) = pending.registration_url_id.as_deref() {
                    @let full_url = admin_issued_registration_url(config, registration_url_id);
                    p class="whitespace-nowrap text-xs text-neutral-500" { "ID: " (registration_url_id) }
                    div class="mt-1" {
                        (compact_copy_button(&full_url, "Copy URL"))
                    }
                } @else {
                    span class="text-neutral-500" { (link_id) }
                }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (client_ip)
            }
            td class="px-4 py-3 align-top" {
                div class="flex flex-nowrap gap-2" {
                    form method="post" action=(&approve_action)
                        hx-post=(&approve_action)
                        hx-target="#pending-registration-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(&pending.user_id);
                        (compact_button("Approve", false))
                    }
                    form method="post" action=(&reject_action)
                        hx-post=(&reject_action)
                        hx-target="#pending-registration-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(&pending.user_id);
                        (compact_button("Reject", true))
                    }
                }
            }
        }
    }
}

fn admin_issued_registration_url(config: &AdminConfig, code: &str) -> String {
    format!(
        "{}/register?registration_url={}",
        config.web_app_endpoint.trim_end_matches('/'),
        urlencoding::encode(code)
    )
}

fn compact_copy_button(value: &str, label: &str) -> Markup {
    html! {
        button type="button"
            class="inline-flex h-8 shrink-0 items-center justify-center rounded-lg border border-neutral-300 bg-neutral-50 px-3 text-xs font-medium text-neutral-700 transition-all hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-brand-primary/20"
            data-copy-value=(value)
            onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
            (label)
        }
    }
}

fn compact_button(label: &str, danger: bool) -> Markup {
    let variant_class = if danger {
        "bg-red-600 text-white hover:bg-red-700 focus:ring-red-500/30"
    } else {
        "bg-neutral-900 text-white hover:bg-neutral-800 focus:ring-brand-primary/20"
    };
    html! {
        button type="submit"
            class={"inline-flex h-8 shrink-0 items-center justify-center rounded-lg px-3 text-xs font-medium transition-all focus:outline-none focus:ring-2 " (variant_class)} {
            (label)
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
