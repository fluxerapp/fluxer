// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl, admin_flags,
    api::types::{
        AdminCustomBadgeList, AdminUser, CustomBadgeType, LimitConfigResponse,
        ListUserChangeLogResponse, account_tier_label,
    },
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{Checkbox, csrf_input, form_actions, submit_button},
        page_container::{card_with_header, detail_row},
    },
    templates::pages::{
        badges::{custom_badge_list, custom_badge_picker, no_badges_defined},
        enforcement_decision::suspension_label,
    },
    utils::{
        bigint::format_discriminator,
        timestamps::{format_admin_timestamp, snowflake_creation_date},
        user_tag::user_tag,
    },
};
use maud::{Markup, html};

#[derive(Default)]
pub struct OverviewSources<'a> {
    pub change_log: Option<&'a ListUserChangeLogResponse>,
    pub limit_config: Option<&'a LimitConfigResponse>,
    pub custom_badges: Option<&'a AdminCustomBadgeList>,
}

pub fn overview_tab(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    change_log: Option<&ListUserChangeLogResponse>,
) -> Markup {
    let sources = OverviewSources {
        change_log,
        ..OverviewSources::default()
    };
    render_overview_tab(config, user, admin_acls, csrf_token, &sources, false, false)
}

pub fn overview_tab_with_sources(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    sources: &OverviewSources,
    username_sign_in: bool,
) -> Markup {
    render_overview_tab(
        config,
        user,
        admin_acls,
        csrf_token,
        sources,
        true,
        username_sign_in,
    )
}

fn render_overview_tab(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    sources: &OverviewSources,
    show_traits: bool,
    username_sign_in: bool,
) -> Markup {
    let OverviewSources {
        change_log,
        limit_config,
        custom_badges,
    } = *sources;
    html! {
        div class="space-y-6" {
            (restriction_banner(config, user))
            @if user.temp_banned_until.is_none() && user.enforcement_decision_id.is_none() {
                @if let Some(pending) = &user.pending_deletion_at {
                    div class="rounded-lg border border-orange-200 bg-orange-50 p-4 text-sm text-orange-900" {
                        div class="font-medium" { "Scheduled for deletion: " (format_admin_timestamp(pending)) }
                        @if let Some(code) = user.deletion_reason_code {
                            div class="mt-1" { "Reason code: " (code) }
                        }
                        @if let Some(reason) = &user.deletion_public_reason {
                            div class="mt-1" { "Public reason: " (reason) }
                        }
                        @if let Some(reason) = &user.deletion_audit_log_reason {
                            div class="mt-1" { "Private reason: " (reason) }
                        }
                        div class="mt-1" {
                            "Scheduled by "
                            @match user.deletion_scheduled_by.as_deref() {
                                Some(id) if id == user.id => { "the user" }
                                Some(id) => {
                                    a href={(config.base_path) "/users/" (id)} class="underline" { (id) }
                                }
                                None => { "an unrecorded source" }
                            }
                            @if let Some(at) = user.deletion_scheduled_at.as_deref() {
                                " on " (format_admin_timestamp(at))
                            }
                        }
                    }
                }
            }
            @if let Some(pending) = &user.pending_bulk_message_deletion_at {
                div class="rounded-lg border border-neutral-200 bg-neutral-50 p-4" {
                    div class="font-medium text-neutral-700 text-sm" {
                        "Bulk message deletion scheduled for " (format_admin_timestamp(pending))
                    }
                    @if acl::has_permission(admin_acls, acl::USER_CANCEL_BULK_MESSAGE_DELETION) {
                        form method="post"
                            action={(config.base_path) "/users/" (user.id) "?action=cancel_bulk_message_deletion&tab=overview"} {
                            (csrf_input(csrf_token))
                            button type="submit"
                                class="mt-3 rounded bg-neutral-900 px-4 py-2 font-medium text-sm text-white transition-colors hover:bg-neutral-800" {
                                "Cancel Bulk Message Deletion"
                            }
                        }
                    }
                }
            }
            (card_with_header("Account Information", html! {
                dl class="divide-y divide-neutral-100" {
                    (detail_row("User ID", html! {
                        span class="font-mono text-xs" { (user.id) }
                    }))
                    (detail_row("Created", html! {
                        (snowflake_creation_date(&user.id))
                    }))
                    (detail_row("Username", html! {
                        (user_tag(&user.username, &format_discriminator(&user.discriminator), user.bot))
                    }))
                    (detail_row("Display Name", html! {
                        @if let Some(ref name) = user.global_name {
                            (name)
                        } @else {
                            span class="text-neutral-400" { "Not set" }
                        }
                    }))
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_EMAIL) && !username_sign_in {
                        (detail_row("Email", html! {
                            @if let Some(ref email) = user.email {
                                (email)
                                @if user.email_verified { span class="ml-1 text-green-600" { "verified" } }
                                @else { span class="ml-1 text-red-600" { "unverified" } }
                                @if user.email_bounced { span class="ml-1 text-orange-600" { "(bounced)" } }
                            } @else {
                                span class="text-neutral-400" { "Not set" }
                            }
                        }))
                    }
                    @if let Some(tier) = user.account_tier {
                        (detail_row("Account Tier", html! {
                            (account_tier_label(tier))
                        }))
                    }
                    (detail_row("Sign-in", html! {
                        @if user.claimed {
                            "Can sign in from another device"
                        } @else {
                            span class="text-orange-600" { "Unclaimed, no way back in after sign-out" }
                        }
                        @if !user.has_password { span class="ml-1 text-neutral-500" { "(no password)" } }
                    }))
                    @if user.date_of_birth_required {
                        (detail_row("Date of Birth Step", html! {
                            span class="text-orange-600" { "Waiting for a date of birth" }
                        }))
                    }
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_DOB) {
                        (detail_row("Date of Birth", html! {
                            (user.date_of_birth.as_deref().unwrap_or("Not set"))
                        }))
                    }
                    (detail_row("Locale", html! {
                        (user.locale.as_deref().unwrap_or("Not set"))
                    }))
                    @if let Some(ref bio) = user.bio {
                        (detail_row("Bio", html! { (bio) }))
                    }
                    @if let Some(ref pronouns) = user.pronouns {
                        (detail_row("Pronouns", html! { (pronouns) }))
                    }
                    @if user.bot {
                        (detail_row("Type", html! { (badge("Bot", BadgeVariant::Info)) }))
                    }
                    @if user.system {
                        (detail_row("Type", html! { (badge("System", BadgeVariant::Warning)) }))
                    }
                    (detail_row("Authenticators", html! {
                        @if user.authenticator_types.is_empty() {
                            span class="text-neutral-400" { "None" }
                        } @else {
                            (user.authenticator_types.iter().map(|t| match t {
                                0 => "TOTP",
                                2 => "WebAuthn",
                                _ => "Unknown",
                            }).collect::<Vec<_>>().join(", "))
                        }
                    }))
                    (detail_row("Premium", html! {
                        @match user.premium_type {
                            Some(1) => {
                                "Subscription"
                                @if let Some(ref value) = user.premium_since {
                                    span class="ml-1 text-neutral-500" { " since " (value) }
                                }
                                @if let Some(ref value) = user.premium_until {
                                    span class="ml-1 text-neutral-500" { " until " (value) }
                                }
                            }
                            Some(2) => {
                                "Lifetime"
                                @if let Some(value) = user.premium_lifetime_sequence {
                                    span class="ml-1 text-neutral-500" { " #" (value) }
                                }
                            }
                            _ => { span class="text-neutral-400" { "None" } }
                        }
                    }))
                    @if let Some(ref value) = user.premium_grace_ends_at {
                        (detail_row("Grace Period Ends", html! { (format_admin_timestamp(value)) }))
                    }
                    (detail_row("Last Active", html! {
                        (user.last_active_at.as_deref().map_or_else(|| "Never".to_string(), format_admin_timestamp))
                    }))
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_IP) {
                        (detail_row("Last IP", html! {
                            @if let Some(ip) = &user.last_active_ip {
                                span class="font-mono text-xs" { (ip) }
                                a href={(config.base_path) "/users?ip=" (urlencoding::encode(ip))}
                                    class="ml-2 text-xs text-blue-600 hover:underline" {
                                    "Find related"
                                }
                                @if let Some(reverse) = &user.last_active_ip_reverse {
                                    span class="ml-2 text-neutral-500" { "(" (reverse) ")" }
                                }
                            } @else {
                                span class="text-neutral-400" { "Not recorded" }
                            }
                        }))
                        (detail_row("Location", html! {
                            (user.last_active_location.as_deref().unwrap_or("Unknown"))
                        }))
                    }
                }
            }))
            (flags_card(config, user, admin_acls, csrf_token))
            (acls_card(config, user, admin_acls, csrf_token))
            @if show_traits {
                (traits_card(config, user, admin_acls, csrf_token, limit_config))
                @if let Some(badges) = custom_badges {
                    (custom_badges_card(config, user, admin_acls, csrf_token, badges))
                }
            }
            @if acl::has_permission(admin_acls, acl::USER_VIEW_CONTACT_LOG) {
                (card_with_header("Contact Change Log", html! {
                    @if let Some(log) = change_log {
                        @if log.entries.is_empty() {
                            p class="text-sm text-neutral-500" { "No contact changes recorded." }
                        } @else {
                            dl class="divide-y divide-neutral-100" {
                                @for entry in &log.entries {
                                    div class="py-3 text-sm" {
                                        dt class="font-medium text-neutral-900" {
                                            (entry.field)
                                            span class="ml-2 font-normal text-xs text-neutral-500" { (format_admin_timestamp(&entry.event_at)) }
                                        }
                                        dd class="mt-1 font-mono text-xs text-neutral-700" {
                                            (entry.old_value.as_deref().unwrap_or("null"))
                                            " \u{2192} "
                                            (entry.new_value.as_deref().unwrap_or("null"))
                                        }
                                        @if let Some(ref reason) = entry.reason {
                                            dd class="text-xs text-neutral-500" { (reason) }
                                        }
                                    }
                                }
                            }
                        }
                    } @else {
                        p class="text-sm text-red-700" { "Failed to load change log." }
                    }
                }))
            }
        }
    }
}

fn restriction_banner(config: &AdminConfig, user: &AdminUser) -> Markup {
    let base = &config.base_path;
    html! {
        @match (&user.enforcement_decision_id, &user.temp_banned_until) {
            (Some(decision_id), until) => {
                div class="rounded-lg border border-red-200 bg-red-50 p-4 text-sm font-medium text-red-900" data-restriction-banner="" {
                    @match (until, &user.pending_deletion_at) {
                        (Some(until), _) => { (suspension_label(until)) ". " }
                        (None, Some(pending)) => { "Scheduled for deletion on " (format_admin_timestamp(pending)) ". " }
                        (None, None) => { "Restricted by a decision. " }
                    }
                    a href={(base) "/decisions/" (decision_id)} class="underline" { "Decision " (decision_id) }
                }
            }
            (None, Some(until)) => {
                div class="rounded-lg border border-red-200 bg-red-50 p-4 text-sm font-medium text-red-900" data-restriction-banner="" {
                    (suspension_label(until)) ". Recorded before decisions, so it has no decision."
                }
            }
            (None, None) => {}
        }
    }
}

fn flags_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
) -> Markup {
    let can_update_flags = acl::has_permission(admin_acls, acl::USER_UPDATE_FLAGS);
    let suspended = user.temp_banned_until.is_some();
    html! {
        div class="space-y-6" {
            (flag_form_shell(
                config,
                &user.id,
                "User flags",
                "update_flags",
                csrf_token,
                can_update_flags,
                html! {
                    @for flag in admin_flags::USER_FLAGS {
                        div class="space-y-0.5" {
                            (Checkbox {
                                name: "flags[]",
                                value: &flag.value.to_string(),
                                label: flag.name,
                                checked: user.flags & flag.value != 0,
                                enabled: can_update_flags && admin_flags::user_flag_is_editable(flag.value, user.flags, suspended),
                            }.render())
                            p class="pl-6 text-xs text-neutral-500" { (flag.help) }
                        }
                    }
                },
            ))
            (flag_form_shell(
                config,
                &user.id,
                "Premium flags",
                "update_premium_flags",
                csrf_token,
                can_update_flags,
                html! {
                    @for flag in admin_flags::PREMIUM_FLAGS {
                        (Checkbox {
                            name: "flags[]",
                            value: &flag.value.to_string(),
                            label: flag.name,
                            checked: user.premium_flags & flag.value != 0,
                            enabled: can_update_flags && admin_flags::premium_flag_is_editable(flag.value),
                        }.render())
                    }
                },
            ))
        }
    }
}

fn flag_form_shell(
    config: &AdminConfig,
    user_id: &str,
    title: &str,
    action: &str,
    csrf_token: &str,
    can_edit: bool,
    checkboxes: Markup,
) -> Markup {
    card_with_header(
        title,
        html! {
            form method="post" action={(config.base_path) "/users/" (user_id) "?action=" (action) "&tab=overview"} class="space-y-4" {
                (csrf_input(csrf_token))
                div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                    (checkboxes)
                }
                @if can_edit {
                    p class="text-xs text-neutral-500" {
                        "Restriction flags are read-only. Use the moderation tab."
                    }
                    (form_actions(html! {
                        (submit_button("Save"))
                    }))
                } @else {
                    p class="text-xs text-neutral-500" {
                        "Read-only. Needs the " (acl::USER_UPDATE_FLAGS) " permission."
                    }
                }
            }
        },
    )
}

fn acls_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::ACL_SET_USER);
    card_with_header(
        "ACLs",
        html! {
            @if can_edit {
                form method="post" action={(config.base_path) "/users/" (user.id) "?action=update_acls&tab=overview"} {
                    (csrf_input(csrf_token))
                    div class="space-y-4" {
                        div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                            @for item in acl::ALL_ACLS {
                                @let checked = user.acls.iter().any(|value| value == item);
                                (Checkbox {
                                    name: "acls[]",
                                    value: item,
                                    label: item,
                                    checked,
                                    enabled: true,
                                }.render())
                            }
                        }
                        @for item in &user.acls {
                            @if !acl::ALL_ACLS.iter().any(|known| known == &item.as_str()) {
                                input type="hidden" name="acls[]" value=(item);
                            }
                        }
                        (form_actions(html! {
                            (submit_button("Save ACLs"))
                        }))
                    }
                }
            } @else {
                @if user.acls.is_empty() {
                    p class="text-sm text-neutral-500" { "No ACLs assigned." }
                } @else {
                    div class="flex flex-wrap gap-2" {
                        @for item in &user.acls {
                            (badge(item, BadgeVariant::Default))
                        }
                    }
                }
                p class="mt-4 text-xs text-neutral-500" {
                    "Read-only \u{2014} acl:set:user permission required."
                }
            }
        },
    )
}

fn custom_badges_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    badges: &AdminCustomBadgeList,
) -> Markup {
    let definitions: Vec<_> = badges.of_type(CustomBadgeType::User).collect();
    let assigned: Vec<_> = definitions
        .iter()
        .copied()
        .filter(|badge| user.badge_ids.contains(&badge.id))
        .collect();
    let can_edit = acl::has_permission(admin_acls, acl::USER_UPDATE_BADGES);
    card_with_header(
        "Badges",
        html! {
            @if definitions.is_empty() {
                (no_badges_defined(config, "user badges"))
            } @else if can_edit {
                form method="post" action={(config.base_path) "/users/" (user.id) "?action=update_badges&tab=overview"} {
                    (csrf_input(csrf_token))
                    div class="space-y-4" {
                        (custom_badge_picker(&definitions, &user.badge_ids))
                        (form_actions(html! { (submit_button("Save badges")) }))
                    }
                }
            } @else {
                (custom_badge_list(&assigned))
                p class="mt-2 text-xs text-neutral-500" { "You need the user badges permission to change these." }
            }
        },
    )
}

fn traits_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    limit_config: Option<&LimitConfigResponse>,
) -> Markup {
    let trait_definitions = parse_trait_definitions(limit_config, config.self_hosted);
    let custom_traits = custom_traits(user, &trait_definitions, config.self_hosted);
    let can_edit = acl::has_permission(admin_acls, acl::USER_UPDATE_TRAITS);
    card_with_header(
        "Traits",
        html! {
            div class="space-y-4" {
                @if limit_config.is_some() {
                    @if can_edit {
                        (traits_form(config, user, csrf_token, &trait_definitions, &custom_traits))
                    } @else {
                        (assigned_traits(&user.traits))
                        p class="mt-2 text-xs text-neutral-500" { "Read-only \u{2014} trait update permission required." }
                    }
                } @else {
                    (assigned_traits(&user.traits))
                    p class="text-sm text-red-700" { "Failed to load limit configuration." }
                }
                @if trait_definitions.iter().all(|definition| *definition == PREMIUM_TRAIT) {
                    p class="text-neutral-500 text-xs" {
                        "No trait definitions declared. "
                        a href={(config.base_path) "/instance-config"} class="text-blue-600 underline" {
                            "Open Instance Configuration"
                        }
                    }
                }
            }
        },
    )
}

fn assigned_traits(traits: &[String]) -> Markup {
    html! {
        @if traits.is_empty() {
            p class="text-neutral-500 text-sm" { "No traits assigned." }
        } @else {
            div class="flex flex-wrap gap-2" {
                @for trait_name in traits {
                    (badge(trait_name, BadgeVariant::Default))
                }
            }
        }
    }
}

fn traits_form(
    config: &AdminConfig,
    user: &AdminUser,
    csrf_token: &str,
    trait_definitions: &[&str],
    custom_traits: &[&str],
) -> Markup {
    html! {
        form method="post" action={(config.base_path) "/users/" (user.id) "?action=update_traits&tab=overview"} {
            (csrf_input(csrf_token))
            div class="space-y-4" {
                @if !trait_definitions.is_empty() {
                    div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                        @for definition in trait_definitions {
                            (Checkbox {
                                name: "traits[]",
                                value: definition,
                                label: definition,
                                checked: user.traits.iter().any(|t| t == definition),
                                enabled: true,
                            }.render())
                        }
                    }
                }
                div class="flex flex-col gap-1" {
                    label for="custom-traits" class="text-neutral-500 text-xs" {
                        "Custom traits (comma or newline separated)"
                    }
                    textarea id="custom-traits" name="traits" rows="2"
                        placeholder="beta-tester, experimental"
                        class="w-full rounded-lg border border-neutral-300 bg-white px-3 py-1.5 text-sm text-neutral-900 placeholder:text-neutral-400 focus:border-brand-primary focus:outline-none focus:ring-2 focus:ring-brand-primary/20" {
                        (custom_traits.join("\n"))
                    }
                    p class="text-neutral-500 text-xs" {
                        @if config.self_hosted {
                            "The premium trait gives this account premium limits. Accounts with a \
                             subscription or gift get them without it."
                        } @else {
                            "The premium trait follows each member's subscription or gift and cannot be \
                             assigned here."
                        }
                    }
                }
                (form_actions(html! {
                    (submit_button("Save Traits"))
                }))
            }
        }
    }
}

const PREMIUM_TRAIT: &str = "premium";

fn parse_trait_definitions(
    limit_config: Option<&LimitConfigResponse>,
    self_hosted: bool,
) -> Vec<&str> {
    let Some(response) = limit_config else {
        return Vec::new();
    };
    assignable_trait_definitions(
        response
            .limit_config
            .trait_definitions
            .iter()
            .map(|value| value.trim())
            .filter(|value| !value.is_empty()),
        self_hosted,
    )
}

fn assignable_trait_definitions<'a>(
    definitions: impl Iterator<Item = &'a str>,
    self_hosted: bool,
) -> Vec<&'a str> {
    let mut assignable: Vec<&str> = definitions
        .filter(|value| self_hosted || *value != PREMIUM_TRAIT)
        .collect();
    if self_hosted && !assignable.contains(&PREMIUM_TRAIT) {
        assignable.insert(0, PREMIUM_TRAIT);
    }
    assignable
}

fn custom_traits<'a>(
    user: &'a AdminUser,
    trait_definitions: &[&str],
    self_hosted: bool,
) -> Vec<&'a str> {
    user.traits
        .iter()
        .map(String::as_str)
        .filter(|trait_name| !trait_definitions.contains(trait_name))
        .filter(|trait_name| self_hosted || *trait_name != PREMIUM_TRAIT)
        .collect()
}
