// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{
        AccountIdentityMode, EmailDeliveryMode, EmailDeliveryState, InstanceIntegrationsResponse,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{
            Checkbox, FORM_INPUT_CLASS, form_actions, form_field_group, select_input,
            submit_button, text_input,
        },
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

use super::{EMAIL_DELIVERY_CHOICES, choice_options, inherit_select};

fn secret_badge(label: &str, is_set: bool) -> Markup {
    if is_set {
        badge(&format!("{label} set"), BadgeVariant::Success)
    } else {
        badge(&format!("{label} missing"), BadgeVariant::Default)
    }
}

fn password_input(name: &str, label: &str, helper: Option<&str>) -> Markup {
    form_field_group(
        label,
        name,
        false,
        None,
        helper,
        html! {
            input type="password" id=(name) name=(name) value="" class=(FORM_INPUT_CLASS)
                autocomplete="new-password";
        },
    )
}

const PEM_PLACEHOLDER: &str = "-----BEGIN PRIVATE KEY-----\nMIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQg...\n-----END PRIVATE KEY-----";

fn private_key_input(name: &str, label: &str, helper: &str) -> Markup {
    form_field_group(
        label,
        name,
        false,
        None,
        Some(helper),
        html! {
            textarea id=(name) name=(name) rows="5" placeholder=(PEM_PLACEHOLDER) class={(FORM_INPUT_CLASS) " font-mono text-xs"}
                autocomplete="off" spellcheck="false" {}
        },
    )
}

fn smtp_tls_mode_summary(mode: &str) -> &str {
    match mode {
        "implicit" => "implicit TLS",
        "starttls" => "required STARTTLS",
        "opportunistic" => "STARTTLS when the server offers it",
        "none" => "no encryption",
        other => other,
    }
}

fn email_delivery_select(delivery: Option<EmailDeliveryMode>) -> Markup {
    let options = choice_options(EMAIL_DELIVERY_CHOICES);
    let options: Vec<(&str, &str)> = std::iter::once(("", "Use the environment setting"))
        .chain(
            options
                .iter()
                .map(|(value, label)| (value.as_str(), *label)),
        )
        .collect();
    let selected = delivery.map(|mode| mode.to_string()).unwrap_or_default();
    select_input(
        "integration_email_delivery",
        "Delivery",
        &options,
        &selected,
    )
}

pub(super) fn integrations_config_section(
    base: &str,
    config_form: ConfigForm<'_>,
    integrations: &InstanceIntegrationsResponse,
    account_identity: AccountIdentityMode,
    product_name: &str,
) -> Markup {
    let smtp_port = integrations
        .email
        .smtp
        .port
        .map(|port| port.to_string())
        .unwrap_or_default();
    section_card_with_description(
        "Runtime Integrations",
        "Configure optional providers without changing environment variables. Blank secret fields keep the current value.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_integrations"} {
                (config_form.fields())
                div class="space-y-8" {
                    div class="space-y-4" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "GIF provider" }
                            (secret_badge("KLIPY key", integrations.gif.klipy_api_key_set))
                        }
                        div class="grid grid-cols-1 gap-4" {
                            (password_input("integration_klipy_api_key", "KLIPY API key", Some("Leave blank to keep the current key.")))
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "YouTube Data API" }
                            (secret_badge("API key", integrations.youtube.api_key_set))
                        }
                        (password_input("integration_youtube_api_key", "YouTube API key", Some("Leave blank to keep the current key.")))
                        p class="text-xs text-neutral-500" {
                            "The key needs YouTube Data API v3 enabled and a daily quota above 0. \
                             Restrict it by IP address or not at all. Website restrictions block it."
                        }
                    }

                    @if account_identity.is_username() {
                        div class="space-y-4 border-t border-neutral-200 pt-6" {
                            div class="flex flex-wrap items-center gap-2" {
                                h3 class="text-sm font-semibold text-neutral-900" { "Email delivery" }
                                (email_delivery_state_badge(integrations.email.delivery_state))
                            }
                            p class="text-xs text-neutral-500" {
                                "People sign in with a username, so the instance never sends email \
                                 and email delivery cannot be turned on."
                            }
                        }
                    } @else {
                        div class="space-y-4 border-t border-neutral-200 pt-6" {
                            div class="flex flex-wrap items-center gap-2" {
                                h3 class="text-sm font-semibold text-neutral-900" { "Email delivery" }
                                (email_delivery_state_badge(integrations.email.delivery_state))
                                @if integrations.email.effective_disable_new_ip_authorization {
                                    (badge("IP auth disabled", BadgeVariant::Warning))
                                } @else {
                                    (badge("IP auth required", BadgeVariant::Default))
                                }
                                @if integrations.email.smtp.password_set || integrations.email.smtp.username.is_some() {
                                    (secret_badge("SMTP password", integrations.email.smtp.password_set))
                                }
                            }
                            input type="hidden" name="integration_email_present" value="1";
                            (email_delivery_select(integrations.email.delivery))
                            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                                (text_input(
                                    "integration_email_from_email",
                                    "From email",
                                    integrations.email.from_email.as_deref().unwrap_or(""),
                                    "notifications@example.com",
                                ))
                                (text_input(
                                    "integration_email_from_name",
                                    "From name",
                                    integrations.email.from_name.as_deref().unwrap_or(""),
                                    product_name,
                                ))
                                (text_input(
                                    "integration_smtp_host",
                                    "SMTP host",
                                    integrations.email.smtp.host.as_deref().unwrap_or(""),
                                    "smtp.example.com",
                                ))
                                (text_input(
                                    "integration_smtp_port",
                                    "SMTP port",
                                    &smtp_port,
                                    "587",
                                ))
                                (text_input(
                                    "integration_smtp_username",
                                    "SMTP username",
                                    integrations.email.smtp.username.as_deref().unwrap_or(""),
                                    "user@example.com",
                                ))
                                (password_input("integration_smtp_password", "SMTP password", Some("Leave blank to keep the current password.")))
                            }
                            (select_input("integration_smtp_tls_mode", "SMTP encryption", &[
                                ("", "Automatic"),
                                ("implicit", "Implicit TLS"),
                                ("starttls", "STARTTLS, required"),
                                ("opportunistic", "STARTTLS, when the server offers it"),
                                ("none", "None"),
                            ], integrations.email.smtp.tls_mode.as_deref().unwrap_or("")))
                            p class="text-xs text-neutral-500" {
                                "Automatic follows FLUXER_EMAIL_SMTP_TLS_MODE when it is set, then the older FLUXER_EMAIL_SMTP_SECURE, where true means implicit TLS on port 465 and required STARTTLS on every other port. With neither set it uses implicit TLS on port 465 and requires STARTTLS on every other port."
                            }
                            p class="text-xs text-neutral-500" {
                                "Leave the username and password empty for a relay that accepts mail without signing in."
                            }
                            @if let Some(mode) = integrations.email.smtp.effective_tls_mode.as_deref() {
                                p class="text-xs text-neutral-500" {
                                    "Outgoing email currently uses " (smtp_tls_mode_summary(mode))
                                    @if let Some(port) = integrations.email.smtp.port { " on port " (port) }
                                    "."
                                }
                            }
                            (Checkbox {
                                name: "integration_email_disable_new_ip_authorization",
                                value: "true",
                                label: "Disable new IP login authorization",
                                checked: integrations.email.disable_new_ip_authorization,
                                enabled: true,
                            }.render())
                            (text_input(
                                "smtp_test_send_to",
                                "Send a test message to (optional)",
                                "",
                                "you@example.com",
                            ))
                            div class="flex flex-wrap gap-2" {
                                button type="submit"
                                    formaction={(base) "/instance-config?action=test_smtp"}
                                    class="inline-flex w-fit items-center justify-center gap-2 rounded-lg border border-neutral-300 bg-neutral-50 px-4 py-2 font-medium text-base text-neutral-700 transition-all duration-150 hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white" {
                                    span { "Save and test SMTP" }
                                }
                            }
                            p class="text-xs text-neutral-500" {
                                "The test saves this form first, then connects with the saved settings, the same ones outgoing email uses. With an address above it also sends a short message from the from address and shows what the server answered."
                            }
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "Bluesky OAuth" }
                            @if integrations.bluesky.effective_enabled {
                                (badge("Effective: enabled", BadgeVariant::Success))
                            } @else {
                                (badge("Effective: disabled", BadgeVariant::Default))
                            }
                            (badge(&format!("{} signing key(s)", integrations.bluesky.key_count), BadgeVariant::Default))
                        }
                        (inherit_select(
                            "integration_bluesky_enabled",
                            "Bluesky OAuth",
                            integrations.bluesky.enabled,
                        ))
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "integration_bluesky_client_name",
                                "Client name",
                                integrations.bluesky.client_name.as_deref().unwrap_or(""),
                                "Fluxer",
                            ))
                            (text_input(
                                "integration_bluesky_client_uri",
                                "Client URL",
                                integrations.bluesky.client_uri.as_deref().unwrap_or(""),
                                "https://example.com",
                            ))
                            (text_input(
                                "integration_bluesky_logo_uri",
                                "Logo URL",
                                integrations.bluesky.logo_uri.as_deref().unwrap_or(""),
                                "https://example.com/logo.png",
                            ))
                            (text_input(
                                "integration_bluesky_tos_uri",
                                "Terms URL",
                                integrations.bluesky.tos_uri.as_deref().unwrap_or(""),
                                "https://example.com/terms",
                            ))
                            (text_input(
                                "integration_bluesky_policy_uri",
                                "Privacy URL",
                                integrations.bluesky.policy_uri.as_deref().unwrap_or(""),
                                "https://example.com/privacy",
                            ))
                            (text_input("integration_bluesky_key_id", "New signing key ID", "", "atproto-key-1"))
                            (private_key_input("integration_bluesky_private_key", "New private key", "Paste the whole PEM file, including the BEGIN and END lines. Provide this only when adding or replacing runtime Bluesky keys."))
                        }
                    }

                    (form_actions(html! {
                        (submit_button("Save runtime integrations"))
                    }))
                }
            }
        },
    )
}

fn email_delivery_state_badge(state: EmailDeliveryState) -> Markup {
    match state {
        EmailDeliveryState::On => badge("Sending email", BadgeVariant::Success),
        EmailDeliveryState::OffDisabled => badge("Not sending: turned off", BadgeVariant::Default),
        EmailDeliveryState::OffIncomplete => {
            badge("Not sending: settings incomplete", BadgeVariant::Warning)
        }
        EmailDeliveryState::OffUsernameMode => {
            badge("Not sending: username sign-in", BadgeVariant::Default)
        }
    }
}
