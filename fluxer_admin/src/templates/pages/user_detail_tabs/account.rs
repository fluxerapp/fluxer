// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{
        AccountIdentityMode, AdminUser, PasswordResetLinkResponse, UserSession, WebAuthnCredential,
    },
    config::AdminConfig,
    templates::components::{
        alert::{AlertVariant, alert},
        form::{checkbox, csrf_input, form_actions, submit_button},
        page_container::card_with_header,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

const INPUT_CLS: &str = "block w-full rounded-md border border-neutral-300 px-3 py-2 \
                          text-sm shadow-sm focus:border-brand-primary focus:outline-none \
                          focus:ring-1 focus:ring-brand-primary";
const BTN_CLS: &str = "w-full inline-flex items-center justify-center rounded-md \
                        bg-brand-primary px-4 py-2 text-sm font-medium text-white \
                        shadow-sm hover:bg-brand-primary-dark";

pub struct AccountTabOptions<'a> {
    pub admin_acls: &'a [String],
    pub account_identity: AccountIdentityMode,
    pub password_reset_link: Option<&'a PasswordResetLinkResponse>,
}

pub fn account_tab(
    config: &AdminConfig,
    user: &AdminUser,
    sessions: &[UserSession],
    webauthn_credentials: &[WebAuthnCredential],
    csrf_token: &str,
    options: &AccountTabOptions<'_>,
) -> Markup {
    let base = &config.base_path;
    let username_sign_in = options.account_identity.is_username();
    let can_create_reset_link = username_sign_in
        && acl::has_permission(options.admin_acls, acl::USER_CREATE_PASSWORD_RESET_LINK);
    let can_revoke_recovery_kit = username_sign_in
        && !user.bot
        && acl::has_permission(options.admin_acls, acl::USER_DELETE_RECOVERY_KIT);
    html! {
        div class="space-y-6" {
            @if can_create_reset_link {
                (password_reset_link_card(base, user, csrf_token, options.password_reset_link))
            }
            (edit_account_card(base, user, csrf_token, username_sign_in))
            (sessions_card(config, sessions))
            (quick_actions_card(base, user, csrf_token, username_sign_in, can_revoke_recovery_kit))
            (clear_fields_card(base, user, csrf_token))
            (user_status_card(base, user, csrf_token))
            (security_actions_card(base, user, csrf_token))
            (webauthn_credentials_card(base, user, webauthn_credentials, csrf_token))
        }
    }
}

fn password_reset_link_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    link: Option<&PasswordResetLinkResponse>,
) -> Markup {
    let action_url = format!(
        "{base}/users/{}?action=create_password_reset_link&tab=account",
        user.id
    );
    html! {
        (card_with_header("Password Reset Link", html! {
            div class="space-y-4" {
                (password_reset_link_result(link))
                p class="text-sm text-neutral-600" {
                    "Create a one-time link that lets this user choose a new password. \
                     Hand it to them yourself. It works once and expires after an hour."
                }
                form method="post"
                    action=(&action_url)
                    data-admin-result-form="true"
                    hx-post=(&action_url)
                    hx-target={"#" (PASSWORD_RESET_LINK_RESULT_ID)}
                    hx-swap="outerHTML"
                    hx-push-url="false" {
                    (csrf_input(csrf_token))
                    button type="submit" class=(BTN_CLS) { "Create Password Reset Link" }
                }
            }
        }))
    }
}

pub const PASSWORD_RESET_LINK_RESULT_ID: &str = "password-reset-link-result";

pub fn password_reset_link_result(link: Option<&PasswordResetLinkResponse>) -> Markup {
    html! {
        div id=(PASSWORD_RESET_LINK_RESULT_ID) hx-history=[link.is_some().then_some("false")] {
            @if let Some(link) = link {
                (alert(AlertVariant::Success, Some("Password reset link created"), html! {
                    div class="flex flex-col gap-2" {
                        p class="text-sm" {
                            "Copy this link now. It is shown only once."
                        }
                        div class="flex items-center gap-2" {
                            input type="url" readonly value=(link.url)
                                aria-label="Password reset link"
                                class="h-8 min-w-0 flex-1 rounded-lg border border-green-200 bg-white px-3 py-1.5 text-xs text-neutral-900";
                            button type="button"
                                class="inline-flex h-8 shrink-0 items-center justify-center rounded-lg border border-neutral-300 bg-neutral-50 px-3 text-xs font-medium text-neutral-700 hover:border-neutral-400 hover:text-neutral-900"
                                data-copy-value=(link.url)
                                onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
                                "Copy Link"
                            }
                        }
                        p class="text-xs" {
                            "Expires " (format_admin_timestamp(&link.expires_at))
                        }
                    }
                }))
            }
        }
    }
}

fn edit_account_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    username_sign_in: bool,
) -> Markup {
    html! {
        (card_with_header("Edit Account Information", html! {
            div class="grid gap-4 md:grid-cols-2" {
                (post_form(base, &user.id, "change_username", "account",
                    "Are you sure you want to change this user\\'s username?", html! {
                    p class="text-sm font-medium text-neutral-700" { "Change Username:" }
                    input type="text" name="username" placeholder="New username"
                        required class=(INPUT_CLS);
                    @if !crate::utils::user_tag::unique_usernames() || user.bot {
                        input type="text" name="discriminator"
                            placeholder="Discriminator (optional)" inputmode="numeric" pattern="[0-9]{1,4}" maxlength="4"
                            class=(INPUT_CLS);
                    }
                    (form_actions(html! {
                        (submit_button("Change Username"))
                    }))
                }, csrf_token))
                @if !username_sign_in {
                    (post_form(base, &user.id, "change_email", "account",
                        "Are you sure you want to change this user\\'s email address?", html! {
                        p class="text-sm font-medium text-neutral-700" { "Change Email:" }
                        input type="email" name="email" placeholder="New email address"
                            required class=(INPUT_CLS);
                        (form_actions(html! {
                            (submit_button("Change Email"))
                        }))
                    }, csrf_token))
                }
                (post_form(base, &user.id, "change_dob", "account",
                    "Are you sure you want to change this user\\'s date of birth?", html! {
                    p class="text-sm font-medium text-neutral-700" { "Change Date of Birth:" }
                    input type="date" name="date_of_birth"
                        value=(user.date_of_birth.as_deref().unwrap_or(""))
                        required class=(INPUT_CLS);
                    (form_actions(html! {
                        (submit_button("Change Date of Birth"))
                    }))
                }, csrf_token))
            }
        }))
    }
}

fn sessions_card(config: &AdminConfig, sessions: &[UserSession]) -> Markup {
    let base = &config.base_path;
    let active: Vec<_> = sessions.iter().filter(|s| s.deleted_at.is_none()).collect();
    let terminated: Vec<_> = sessions.iter().filter(|s| s.deleted_at.is_some()).collect();
    html! {
        (card_with_header("Sessions", html! {
            @if sessions.is_empty() {
                p class="text-sm text-neutral-500" { "No sessions" }
            } @else {
                div class="space-y-3" {
                    @if !active.is_empty() {
                        p class="text-xs font-medium uppercase tracking-wide text-neutral-500" { "Active" }
                    }
                    @for s in &active {
                        (session_entry(base, s, false))
                    }
                    @if !terminated.is_empty() {
                        p class="mt-4 text-xs font-medium uppercase tracking-wide text-neutral-400" { "Terminated" }
                    }
                    @for s in &terminated {
                        (session_entry(base, s, true))
                    }
                }
            }
        }))
    }
}

fn session_entry(base: &str, s: &UserSession, is_tombstone: bool) -> Markup {
    let border_cls = if is_tombstone {
        "rounded-lg border border-neutral-200 bg-neutral-100/60 p-4 opacity-75"
    } else {
        "rounded-lg border border-neutral-200 bg-neutral-50 p-4"
    };
    html! {
        div class=(border_cls) {
            div class="grid grid-cols-2 gap-x-6 gap-y-3 text-sm md:grid-cols-3" {
                (meta_cell("Session ID", html! {
                    span class="text-xs" { (s.session_id_hash) }
                    @if is_tombstone {
                        span class="ml-2 inline-flex items-center rounded-full bg-neutral-200 px-2 py-0.5 text-xs font-medium text-neutral-600" { "Terminated" }
                    }
                }))
                (meta_cell("Created", html! { (format_admin_timestamp(&s.created_at)) }))
                @if let Some(ref d) = s.deleted_at {
                    (meta_cell("Terminated", html! { (format_admin_timestamp(d)) }))
                } @else {
                    (meta_cell("Last Used", html! { (format_admin_timestamp(&s.approx_last_used_at)) }))
                }
                div class="md:col-span-3" {
                    (meta_cell("IP Address", html! {
                        span class="" { (s.client_ip) }
                        a href={(base) "/users?ip=" (s.client_ip)}
                            class="ml-2 text-xs text-blue-600 no-underline hover:underline" {
                            "Find related users"
                        }
                        @if let Some(ref rev) = s.client_ip_reverse {
                            span class="ml-2 text-neutral-600" { "(" (rev) ")" }
                        }
                    }))
                }
                @if let Some(ref p) = s.client_platform {
                    (meta_cell("Platform", html! { (p) }))
                }
                @if let Some(ref o) = s.client_os {
                    (meta_cell("OS", html! { (o) }))
                }
                @if let Some(ref l) = s.client_location {
                    (meta_cell("Location", html! { (l) }))
                }
            }
        }
    }
}

fn quick_actions_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    username_sign_in: bool,
    can_revoke_recovery_kit: bool,
) -> Markup {
    html! {
        (card_with_header("Quick Actions", html! {
            div class="flex flex-wrap gap-3" {
                @if !user.email_verified && !username_sign_in {
                    (action_form(base, &user.id, "verify_email", "account", None,
                        "Verify Email", csrf_token))
                }
                @if !username_sign_in {
                    (action_form(base, &user.id, "send_password_reset", "account", None,
                        "Send Password Reset", csrf_token))
                }
                @if can_revoke_recovery_kit {
                    (action_form(base, &user.id, "revoke_recovery_kit", "account", None,
                        "Revoke Recovery Kit", csrf_token))
                }
            }
        }))
    }
}

fn clear_fields_card(base: &str, user: &AdminUser, csrf_token: &str) -> Markup {
    let has = user.avatar.is_some()
        || user.banner.is_some()
        || user.bio.is_some()
        || user.pronouns.is_some()
        || user.global_name.is_some();
    if !has {
        return html! {};
    }
    let action_url = format!("{base}/users/{}?action=clear_fields&tab=account", user.id);
    html! {
        (card_with_header("Clear Profile Fields", html! {
            form method="post"
                action=(&action_url)
                hx-post=(&action_url)
                hx-target="#flash-container"
                hx-swap="none"
                hx-push-url="false" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="grid grid-cols-2 gap-3 md:grid-cols-3" {
                        @if user.avatar.is_some() { (checkbox("fields[]", "avatar", "Avatar", false, true)) }
                        @if user.banner.is_some() { (checkbox("fields[]", "banner", "Banner", false, true)) }
                        @if user.bio.is_some() { (checkbox("fields[]", "bio", "Bio", false, true)) }
                        @if user.pronouns.is_some() { (checkbox("fields[]", "pronouns", "Pronouns", false, true)) }
                        @if user.global_name.is_some() { (checkbox("fields[]", "global_name", "Display Name", false, true)) }
                    }
                    (form_actions(html! {
                        (submit_button("Clear Selected Fields"))
                    }))
                }
            }
        }))
    }
}

fn user_status_card(base: &str, user: &AdminUser, csrf_token: &str) -> Markup {
    let is_bot = user.bot;
    let is_sys = user.system;
    html! {
        (card_with_header("User Status", html! {
            div class="grid grid-cols-1 gap-4 md:grid-cols-2" {
                (status_toggle(base, &user.id, "set_bot_status", is_bot, "bot", csrf_token))
                (status_toggle(base, &user.id, "set_system_status", is_sys, "system", csrf_token))
            }
        }))
    }
}

fn security_actions_card(base: &str, user: &AdminUser, csrf_token: &str) -> Markup {
    html! {
        (card_with_header("Security Actions", html! {
            div class="grid grid-cols-1 gap-3 md:grid-cols-2" {
                @if user.has_totp {
                    (action_form(base, &user.id, "disable_mfa", "account",
                        Some("Disable MFA/TOTP for this user?"), "Disable MFA/TOTP", csrf_token))
                }
                (action_form(base, &user.id, "terminate_sessions", "account",
                    Some("Terminate all sessions for this user?"),
                    "Terminate All Sessions", csrf_token))
            }
        }))
    }
}

fn webauthn_credentials_card(
    base: &str,
    user: &AdminUser,
    credentials: &[WebAuthnCredential],
    csrf_token: &str,
) -> Markup {
    if credentials.is_empty() {
        return html! {};
    }
    html! {
        (card_with_header("WebAuthn Credentials", html! {
            div class="overflow-x-auto" {
                table class="w-full text-sm" {
                    thead {
                        tr class="border-neutral-200 border-b text-left" {
                            th class="pb-2 font-medium text-neutral-600" { "Name" }
                            th class="pb-2 font-medium text-neutral-600" { "Created" }
                            th class="pb-2 font-medium text-neutral-600" { "Last Used" }
                            th class="pb-2 font-medium text-neutral-600" {}
                        }
                    }
                    tbody {
                        @for credential in credentials {
                            (webauthn_credential_row(base, user, credential, csrf_token))
                        }
                    }
                }
            }
        }))
    }
}

fn webauthn_credential_row(
    base: &str,
    user: &AdminUser,
    credential: &WebAuthnCredential,
    csrf_token: &str,
) -> Markup {
    let action_url = format!(
        "{base}/users/{}?action=delete_webauthn_credential&tab=account",
        user.id
    );
    html! {
        tr class="border-neutral-100 border-b" {
            td class="py-2 pr-4" {
                p class="text-sm text-neutral-900" { (credential.name) }
            }
            td class="py-2 pr-4" {
                p class="text-sm text-neutral-900" { (format_admin_timestamp(&credential.created_at)) }
            }
            td class="py-2 pr-4" {
                p class="text-sm text-neutral-900" {
                    (credential.last_used_at.as_deref().map_or_else(|| "Never".to_string(), format_admin_timestamp))
                }
            }
            td class="py-2" {
                form method="post"
                    action=(&action_url)
                    hx-post=(&action_url)
                    hx-target="#flash-container"
                    hx-swap="none"
                    hx-push-url="false" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="credential_id" value=(credential.id);
                    button type="submit"
                        class="inline-flex items-center justify-center rounded-md bg-brand-primary px-3 py-1.5 text-sm font-medium text-white shadow-sm hover:bg-brand-primary-dark" {
                        "Delete"
                    }
                }
            }
        }
    }
}

fn meta_cell(label: &str, value: Markup) -> Markup {
    html! { div { p class="text-sm font-medium text-neutral-500" { (label) } p class="text-sm text-neutral-900" { (value) } } }
}

fn post_form(
    base: &str,
    uid: &str,
    action: &str,
    tab: &str,
    _confirm_msg: &str,
    body: Markup,
    csrf: &str,
) -> Markup {
    let action_url = format!("{base}/users/{uid}?action={action}&tab={tab}");
    html! {
        form method="post"
            action=(&action_url)
            hx-post=(&action_url)
            hx-target="#flash-container"
            hx-swap="none"
            hx-push-url="false"
            class="w-full" {
            (csrf_input(csrf))
            div class="space-y-2" { (body) }
        }
    }
}

fn action_form(
    base: &str,
    uid: &str,
    action: &str,
    tab: &str,
    _confirm_msg: Option<&str>,
    label: &str,
    csrf: &str,
) -> Markup {
    let action_url = format!("{base}/users/{uid}?action={action}&tab={tab}");
    html! {
        form method="post"
            action=(&action_url)
            hx-post=(&action_url)
            hx-target="#flash-container"
            hx-swap="none"
            hx-push-url="false" {
            (csrf_input(csrf))
            button type="submit" class=(BTN_CLS) { (label) }
        }
    }
}

fn status_toggle(
    base: &str,
    uid: &str,
    action: &str,
    active: bool,
    kind: &str,
    csrf: &str,
) -> Markup {
    let status_val = if active { "false" } else { "true" };
    let label = format!(
        "{} {} Status",
        if active { "Remove" } else { "Set" },
        capitalize(kind)
    );
    let action_url = format!("{base}/users/{uid}?action={action}&status={status_val}&tab=account");
    html! {
        form method="post"
            action=(&action_url)
            hx-post=(&action_url)
            hx-target="#flash-container"
            hx-swap="none"
            hx-push-url="false" {
            (csrf_input(csrf))
            input type="hidden" name=(kind) value=(status_val);
            button type="submit" class=(BTN_CLS) { (label) }
        }
    }
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().chain(c).collect(),
    }
}
