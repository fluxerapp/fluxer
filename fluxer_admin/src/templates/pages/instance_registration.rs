// SPDX-License-Identifier: AGPL-3.0-or-later

use super::instance_config::{
    REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD, allowed_domains_input,
    format_optional_admin_timestamp,
};
use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{AccountIdentityMode, InstanceRegistrationResponse, RegistrationUrlResponse},
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{
            Checkbox, FORM_INPUT_CLASS, NumberField, csrf_input, form_actions, form_field_group,
            select_input, submit_button, text_input,
        },
        section_card::section_card_with_description,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

pub(super) fn registration_config_section(
    config: &AdminConfig,
    config_form: ConfigForm<'_>,
    registration: &InstanceRegistrationResponse,
    account_identity: AccountIdentityMode,
) -> Markup {
    let base = &config.base_path;
    section_card_with_description(
        "Registration Controls",
        "Control public registration and admin-issued registration URLs.",
        html! {
            div class="space-y-8" {
                (registration_policy_form(base, config_form, registration, account_identity))
                (create_registration_url_form(base, config_form.csrf_token))
                (registration_url_list(config, config_form.csrf_token, &registration.urls))
            }
        },
    )
}

fn registration_policy_form(
    base: &str,
    config_form: ConfigForm<'_>,
    registration: &InstanceRegistrationResponse,
    account_identity: AccountIdentityMode,
) -> Markup {
    html! {
        div class="space-y-4" {
            h3 class="text-sm font-semibold text-neutral-900" { "Registration Policy" }
            form method="post" action={(base) "/instance-config?action=update_registration"} {
                (config_form.fields())
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (select_input("registration_mode", "Mode", &[
                            ("open", "Open"),
                            ("approval", "Approval required"),
                            ("closed", "Closed"),
                        ], registration.mode.as_str()))
                        div class="flex items-end pb-1" {
                            (Checkbox {
                                name: "admin_registration_urls_enabled",
                                value: "true",
                                label: "Admin registration URLs enabled",
                                checked: registration.admin_registration_urls_enabled,
                                enabled: true,
                            }.render())
                        }
                    }
                    @if !account_identity.is_username() {
                        (allowed_domains_input(
                            REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD,
                            &registration.allowed_email_domains.join("\n"),
                            "Only email addresses on these domains (one per line) can sign up, claim an account or become an account's new email. People with an admin-issued registration URL can use any domain. Leave empty to allow any domain.",
                        ))
                    }
                    (form_actions(html! {
                        (submit_button("Save Registration Policy"))
                    }))
                }
            }
        }
    }
}

fn create_registration_url_form(base: &str, csrf_token: &str) -> Markup {
    let action = format!("{base}/instance-config?action=create_registration_url");
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Create Admin-Issued Registration URL" }
            form method="post" action=(&action)
                hx-post=(&action)
                hx-target="#registration-url-list"
                hx-swap="outerHTML"
                hx-push-url="false"
                data-admin-allow-swap="true" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4" {
                        (text_input(
                            "registration_url_label",
                            "Label",
                            "",
                            "Invite batch, partner, or support case",
                        ))
                        (NumberField {
                            name: "registration_url_expires_in_days",
                            label: "Expires In Days",
                            value: "",
                            min: Some(1),
                            max: None,
                            step: "1",
                            helper: Some("Leave blank for no expiration."),
                            placeholder: None,
                        }.render())
                        (NumberField {
                            name: "registration_url_max_uses",
                            label: "Max Uses",
                            value: "",
                            min: Some(1),
                            max: Some(1_000_000),
                            step: "1",
                            helper: Some("Leave blank for unlimited uses."),
                            placeholder: None,
                        }.render())
                        div class="flex items-end pb-1" {
                            (Checkbox {
                                name: "registration_url_approval_required",
                                value: "true",
                                label: "Approval required",
                                checked: false,
                                enabled: true,
                            }.render())
                        }
                    }
                    (form_field_group(
                        "Join a community after sign-up (optional)",
                        "registration_url_invite_code",
                        false,
                        None,
                        Some("An invite code or invite link. People who sign up through this URL accept the invite once their account exists."),
                        html! {
                            input type="text" id="registration_url_invite_code" name="registration_url_invite_code"
                                placeholder="abc123" class=(FORM_INPUT_CLASS);
                        },
                    ))
                    (form_actions(html! {
                        (submit_button("Create Registration URL"))
                    }))
                }
            }
        }
    }
}

pub(crate) fn registration_url_list(
    config: &AdminConfig,
    csrf_token: &str,
    urls: &[RegistrationUrlResponse],
) -> Markup {
    let base = &config.base_path;
    html! {
        div id="registration-url-list" class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Existing Registration URLs" }
            @if urls.is_empty() {
                p class="text-sm text-neutral-500" { "No admin-issued registration URLs." }
            } @else {
                div class="overflow-x-auto rounded-lg border border-neutral-200" {
                    table class="min-w-[1100px] divide-y divide-neutral-200 text-sm" {
                        thead class="bg-neutral-50" {
                            tr {
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Label / URL" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Created" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Expires" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Uses" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Status" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Approval" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Action" }
                            }
                        }
                        tbody class="divide-y divide-neutral-200 bg-white" {
                            @for url in urls {
                                (registration_url_row(config, base, csrf_token, url))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn registration_url_row(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    url: &RegistrationUrlResponse,
) -> Markup {
    let label = url.label.as_deref().unwrap_or("Unlabeled");
    let full_url = admin_issued_registration_url(config, &url.id);
    let created_at = format_admin_timestamp(&url.created_at);
    let expires_at = format_optional_admin_timestamp(url.expires_at.as_deref(), "Never");
    let uses = registration_url_uses_text(url);
    let status = registration_url_status(url);
    let approval = if url.approval_required {
        "Required"
    } else {
        "Not required"
    };
    let revoke_action = format!("{base}/instance-config?action=revoke_registration_url");
    let remove_action = format!("{base}/instance-config?action=remove_registration_url");
    let rename_action = format!("{base}/instance-config?action=rename_registration_url");
    html! {
        tr {
            td class="min-w-[28rem] px-4 py-3 align-top" {
                p class="font-medium text-neutral-900" { (label) }
                (copyable_registration_url(&full_url, "Copy URL"))
                p class="mt-1 whitespace-nowrap text-xs text-neutral-500" { "ID: " (&url.id) }
                @if let Some(invite_code) = url.invite_code.as_deref() {
                    p class="mt-1 whitespace-nowrap text-xs text-neutral-500" { "Joins invite: " (invite_code) }
                }
                form method="post" action=(&rename_action)
                    hx-post=(&rename_action)
                    hx-target="#registration-url-list"
                    hx-swap="outerHTML"
                    hx-push-url="false"
                    data-admin-allow-swap="true"
                    class="mt-2 flex items-center gap-2" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="registration_url_id" value=(&url.id);
                    input type="text" name="registration_url_label" maxlength="120"
                        value=(url.label.as_deref().unwrap_or(""))
                        aria-label="New label" placeholder="New label"
                        class="h-8 min-w-0 flex-1 rounded-lg border border-neutral-300 bg-white px-3 py-1.5 text-xs text-neutral-800";
                    (compact_button("Rename", CompactButtonVariant::Neutral))
                }
            }
            td class="px-4 py-3 align-top text-neutral-700" {
                p class="whitespace-nowrap" { (created_at) }
                p class="whitespace-nowrap text-xs text-neutral-500" { "By: " (&url.created_by_user_id) }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (expires_at)
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (uses)
            }
            td class="px-4 py-3 align-top whitespace-nowrap" {
                (badge(status.label(), status.variant()))
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (approval)
            }
            td class="px-4 py-3 align-top" {
                @if status != RegistrationUrlStatus::Revoked {
                    form method="post" action=(&revoke_action)
                        hx-post=(&revoke_action)
                        hx-target="#registration-url-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="registration_url_id" value=(&url.id);
                        (compact_button("Revoke", CompactButtonVariant::Danger))
                    }
                } @else {
                    form method="post" action=(&remove_action)
                        hx-post=(&remove_action)
                        hx-target="#registration-url-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        hx-confirm="Remove this registration URL from the list?"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="registration_url_id" value=(&url.id);
                        (compact_button("Remove", CompactButtonVariant::Danger))
                    }
                }
            }
        }
    }
}

pub(super) fn admin_issued_registration_url(config: &AdminConfig, code: &str) -> String {
    format!(
        "{}/register?registration_url={}",
        config.web_app_endpoint.trim_end_matches('/'),
        urlencoding::encode(code)
    )
}

fn copyable_registration_url(value: &str, label: &str) -> Markup {
    html! {
        div class="mt-2 flex items-center gap-2" {
            input type="url" readonly value=(value)
                aria-label="Registration URL"
                class="h-8 min-w-0 flex-1 rounded-lg border border-neutral-300 bg-neutral-50 px-3 py-1.5 text-xs text-neutral-800";
            (compact_copy_button(value, label))
        }
    }
}

pub(super) fn compact_copy_button(value: &str, label: &str) -> Markup {
    html! {
        button type="button"
            class="inline-flex h-8 shrink-0 items-center justify-center rounded-lg border border-neutral-300 bg-neutral-50 px-3 text-xs font-medium text-neutral-700 transition-all hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-brand-primary/20"
            data-copy-value=(value)
            onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
            (label)
        }
    }
}

pub(super) enum CompactButtonVariant {
    Neutral,
    Danger,
}

pub(super) fn compact_button(label: &str, variant: CompactButtonVariant) -> Markup {
    let variant_class = match variant {
        CompactButtonVariant::Danger => {
            "bg-red-600 text-white hover:bg-red-700 focus:ring-red-500/30"
        }
        CompactButtonVariant::Neutral => {
            "bg-neutral-900 text-white hover:bg-neutral-800 focus:ring-brand-primary/20"
        }
    };
    html! {
        button type="submit"
            class={"inline-flex h-8 shrink-0 items-center justify-center rounded-lg px-3 text-xs font-medium transition-all focus:outline-none focus:ring-2 " (variant_class)} {
            (label)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RegistrationUrlStatus {
    Active,
    Revoked,
    Expired,
    Exhausted,
}

impl RegistrationUrlStatus {
    fn label(self) -> &'static str {
        match self {
            Self::Active => "Active",
            Self::Revoked => "Revoked",
            Self::Expired => "Expired",
            Self::Exhausted => "Exhausted",
        }
    }

    fn variant(self) -> BadgeVariant {
        match self {
            Self::Active => BadgeVariant::Success,
            Self::Revoked => BadgeVariant::Danger,
            Self::Expired | Self::Exhausted => BadgeVariant::Warning,
        }
    }
}

fn registration_url_status(url: &RegistrationUrlResponse) -> RegistrationUrlStatus {
    if url.revoked_at.is_some() {
        return RegistrationUrlStatus::Revoked;
    }
    if url.expires_at.as_deref().is_some_and(timestamp_is_past) {
        return RegistrationUrlStatus::Expired;
    }
    if url
        .max_uses
        .is_some_and(|max_uses| url.use_count >= max_uses)
    {
        return RegistrationUrlStatus::Exhausted;
    }
    RegistrationUrlStatus::Active
}

fn timestamp_is_past(iso: &str) -> bool {
    time::OffsetDateTime::parse(iso, &time::format_description::well_known::Rfc3339)
        .or_else(|_| {
            time::OffsetDateTime::parse(
                iso,
                &time::format_description::well_known::Iso8601::DEFAULT,
            )
        })
        .map(|timestamp| timestamp < time::OffsetDateTime::now_utc())
        .unwrap_or(false)
}

fn registration_url_uses_text(url: &RegistrationUrlResponse) -> String {
    match url.max_uses {
        Some(max_uses) => format!("{} / {}", url.use_count, max_uses),
        None => format!("{} / unlimited", url.use_count),
    }
}
