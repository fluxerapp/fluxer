// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    CLEANUP_MODE_CHOICES, DATE_OF_BIRTH_CHOICES, GUILD_JOIN_GATE_CHOICES, IDENTIFIER_FIRST_CHOICES,
    INTERACTION_TIER_CHOICES, LAST_SESSION_CLEANUP_CHOICES, VERIFICATION_BASIS_CHOICES,
    choice_select,
};
use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{
        AccountIdentityConfigResponse, AccountIdentityLockReason, AccountIdentityMode,
        AccountIdentitySettings, AccountsConfigResponse, AuthConfigResponse,
        CLEANUP_INACTIVE_DAYS_RANGE, CaptchaProvider, CleanupMode, DateOfBirthPolicy,
        EmailDeliveryState, GuildJoinGate, INACTIVITY_WARNING_LEAD_DAYS_RANGE,
        INSTANT_ACCOUNTS_ESCALATION_RANGE, InstantAccountAvailability, InteractionTierMode,
        SCHEDULED_DELETION_GRACE_HOURS, TagStyle, VerificationBasis,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{Checkbox, NumberField, form_actions, submit_button},
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

pub(super) fn account_identity_section(account_identity: &AccountIdentityConfigResponse) -> Markup {
    let description = match account_identity.mode {
        AccountIdentityMode::Username => {
            "Members sign in with a username and password. The instance never collects an email \
             address. A member who forgets their password uses their recovery kit or a reset link \
             from an admin."
        }
        AccountIdentityMode::Email => "Members sign in with an email address and password.",
    };
    section_card_with_description(
        "Sign-in Method",
        "How members identify themselves when they sign in.",
        html! {
            div class="space-y-3" {
                div class="flex flex-wrap items-center gap-2" {
                    h3 class="text-sm font-semibold text-neutral-900" {
                        (account_identity.mode.label())
                    }
                    @if account_identity.locked {
                        (badge("Fixed", BadgeVariant::Default))
                    } @else {
                        (badge("Not fixed yet", BadgeVariant::Warning))
                    }
                }
                p class="text-sm text-neutral-600" { (description) }
                @if let Some(reason) = account_identity.lock_reason {
                    p class="text-sm text-neutral-600" {
                        (match reason {
                            AccountIdentityLockReason::SetupConfigured => "It is fixed because setup is finished.",
                            AccountIdentityLockReason::ExistingInstance => {
                                "It is fixed because this instance was in use before username sign-in existed, so it \
                                 stays on email."
                            }
                            AccountIdentityLockReason::AdminExists => {
                                "It is fixed because an administrator account already exists."
                            }
                            AccountIdentityLockReason::AccountsExist => "It is fixed because accounts already exist.",
                        })
                    }
                }
                @if !account_identity.mode.is_username() {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="text-sm font-semibold text-neutral-900" {
                            (account_identity.tag_style.label())
                        }
                    }
                    p class="text-sm text-neutral-600" {
                        @match account_identity.tag_style {
                            TagStyle::None => {
                                "Each name belongs to one person and is shown without a tag."
                            }
                            TagStyle::Random => {
                                "Names have a random tag, like alex#4821, so several people can share a name."
                            }
                        }
                    }
                }
                p class="text-xs text-neutral-500" {
                    @if account_identity.mode.is_username() {
                        "The sign-in method is chosen during setup. It cannot be changed once setup is complete or the first account exists."
                    } @else {
                        "The sign-in method and the username tags are chosen during setup. They cannot be changed once setup is complete or the first account exists."
                    }
                }
            }
        },
    )
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct TierWording {
    missing: &'static str,
    step: &'static str,
}

const LOCAL_CREDENTIAL_WORDING: TierWording = TierWording {
    missing: "without a password or passkey",
    step: "set a password",
};

const CONFIRMED_EMAIL_WORDING: TierWording = TierWording {
    missing: "without a confirmed email",
    step: "add an email",
};

fn tier_wording(
    mode: AccountIdentityMode,
    basis: VerificationBasis,
    delivery_state: EmailDeliveryState,
) -> TierWording {
    let local_credential = mode.is_username()
        || match basis {
            VerificationBasis::Auto => delivery_state != EmailDeliveryState::On,
            VerificationBasis::ConfirmedEmail => false,
            VerificationBasis::LocalCredential => true,
        };
    if local_credential {
        LOCAL_CREDENTIAL_WORDING
    } else {
        CONFIRMED_EMAIL_WORDING
    }
}

fn way_back_in_wording(
    mode: AccountIdentityMode,
    delivery_state: EmailDeliveryState,
) -> &'static str {
    if mode.is_username() {
        "a password, a passkey or a recovery kit"
    } else if delivery_state == EmailDeliveryState::On {
        "a passkey or an email address"
    } else {
        "a passkey, or a password together with an email address"
    }
}

fn instant_account_inactive_reason(
    availability: InstantAccountAvailability,
) -> Option<&'static str> {
    match availability {
        InstantAccountAvailability::Available | InstantAccountAvailability::Disabled => None,
        InstantAccountAvailability::CaptchaOff => Some("Inactive: the proof-of-work check is off"),
        InstantAccountAvailability::RegistrationClosed => Some("Inactive: registration is closed"),
        InstantAccountAvailability::ApprovalRequired => {
            Some("Inactive: new accounts need approval")
        }
    }
}

fn guild_join_gate_inactive_reason(
    accounts: &AccountsConfigResponse,
    guild_visits_enabled: bool,
) -> Option<&'static str> {
    if accounts.guild_join_gate == GuildJoinGate::Off {
        None
    } else if accounts.interaction_tier == InteractionTierMode::Legacy {
        Some("Inactive: the older limits are on")
    } else if !guild_visits_enabled {
        Some("Inactive: community previews are off")
    } else {
        None
    }
}

fn days_field(
    name: &str,
    label: &str,
    value: u64,
    range: &std::ops::RangeInclusive<u32>,
) -> Markup {
    NumberField {
        name,
        label,
        value: &value.to_string(),
        min: Some(*range.start()),
        max: Some(*range.end()),
        step: "1",
        helper: None,
        placeholder: None,
    }
    .render()
}

pub(super) struct AccountsSectionInput<'a> {
    pub(super) auth: &'a AuthConfigResponse,
    pub(super) accounts: &'a AccountsConfigResponse,
    pub(super) identity: AccountIdentitySettings,
    pub(super) delivery_state: EmailDeliveryState,
    pub(super) captcha_provider: CaptchaProvider,
    pub(super) guild_visits_enabled: bool,
}

pub(super) fn accounts_section(
    base: &str,
    config_form: ConfigForm<'_>,
    input: &AccountsSectionInput,
) -> Markup {
    let AccountsSectionInput {
        auth,
        accounts,
        identity,
        delivery_state,
        captcha_provider,
        guild_visits_enabled,
    } = *input;
    let wording = tier_wording(identity.mode, accounts.verification_basis, delivery_state);
    let captcha_on = captcha_provider != CaptchaProvider::None;
    let name_source = if identity.tag_style.is_none() {
        "Their username is made from it"
    } else {
        "Their username and tag are made from it"
    };
    let instant_inactive = auth
        .instant_accounts
        .enabled
        .then(|| instant_account_inactive_reason(auth.instant_account_availability))
        .flatten();
    let join_gate_inactive = guild_join_gate_inactive_reason(accounts, guild_visits_enabled);
    let interaction_tier_label = format!("Accounts {}", wording.missing);
    let unclaimed = &accounts.unclaimed_account_cleanup;
    let dob_missing = &accounts.dob_missing_cleanup;
    let inactivity = &accounts.inactivity_deletion;
    section_card_with_description(
        "Sign-in & new accounts",
        &format!(
            "How people sign in, whether they can start with just a display name, and what an \
             account {} can do.",
            wording.missing
        ),
        html! {
            form method="post" action={(base) "/instance-config?action=update_accounts"} {
                (config_form.fields())
                div class="space-y-6" {
                    @if !identity.mode.is_username() {
                        (choice_select(
                            "auth_identifier_first",
                            "Start sign-in with the email address",
                            IDENTIFIER_FIRST_CHOICES,
                            &auth.identifier_first,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Clients ask for an email first, then offer a password, a passkey or a \
                             code sent by email. Email codes need working email delivery."
                        }
                    }
                    div class="flex flex-wrap items-center gap-2" {
                        (Checkbox {
                            name: "auth_instant_accounts_enabled",
                            value: "true",
                            label: "Let people start with just a display name",
                            checked: auth.instant_accounts.enabled,
                            enabled: captcha_on || auth.instant_accounts.enabled,
                        }.render())
                        @if let Some(reason) = instant_inactive {
                            (badge(reason, BadgeVariant::Warning))
                        }
                    }
                    p class="text-xs text-neutral-500" {
                        @if captcha_on {
                            (format!(
                                "New people type the name others see and start right away. {name_source}, \
                                 and they {} later to keep the account. This works while registration \
                                 is open, or through a registration link that does not need approval.",
                                wording.step
                            ))
                        } @else {
                            "Turn on the proof-of-work check first. Instant accounts need it."
                        }
                    }
                    (NumberField {
                        name: "auth_instant_accounts_pow_escalation_per_hour",
                        label: "Accounts per hour before the check gets harder",
                        value: &auth.instant_accounts.pow_escalation_per_hour.to_string(),
                        min: Some(*INSTANT_ACCOUNTS_ESCALATION_RANGE.start()),
                        max: Some(*INSTANT_ACCOUNTS_ESCALATION_RANGE.end()),
                        step: "1",
                        helper: Some("Every this many instant accounts in the current hour doubles the check's cost for the next one."),
                        placeholder: None,
                    }.render())
                    (choice_select(
                        "accounts_date_of_birth",
                        "Date of birth",
                        DATE_OF_BIRTH_CHOICES,
                        &accounts.date_of_birth,
                    ))
                    p class="text-xs text-neutral-500" {
                        "With \u{201c}Don't ask\u{201d}, new accounts do not store a date of birth and \
                         are treated as adults."
                    }
                    @if !identity.mode.is_username() {
                        (choice_select(
                            "accounts_verification_basis",
                            "What finishes setting up an account",
                            VERIFICATION_BASIS_CHOICES,
                            &accounts.verification_basis,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Turning email delivery on, or choosing \u{201c}A confirmed email\u{201d}, \
                             makes accounts that have a password but an unconfirmed email count as \
                             not set up until they confirm it."
                        }
                    }
                    (choice_select(
                        "accounts_interaction_tier",
                        &interaction_tier_label,
                        INTERACTION_TIER_CHOICES,
                        &accounts.interaction_tier,
                    ))
                    div class="flex flex-col gap-2" {
                        (choice_select(
                            "accounts_guild_join_gate",
                            &format!("Joining a community {}", wording.missing),
                            GUILD_JOIN_GATE_CHOICES,
                            &accounts.guild_join_gate,
                        ))
                        @if let Some(reason) = join_gate_inactive {
                            div { (badge(reason, BadgeVariant::Warning)) }
                        }
                    }
                    p class="text-xs text-neutral-500" {
                        (format!(
                            "Applies only while \u{201c}{interaction_tier_label}\u{201d} is set to \
                             \u{201c}Only allow what does not reach other people\u{201d} and community \
                             previews are on for that person."
                        ))
                    }
                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Accounts with no way back in" }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                            (choice_select(
                                "accounts_unclaimed_cleanup_mode",
                                "Delete",
                                CLEANUP_MODE_CHOICES,
                                &unclaimed.mode,
                            ))
                            (days_field(
                                "accounts_unclaimed_cleanup_inactive_days",
                                "Days without use",
                                unclaimed.inactive_days.get(),
                                &CLEANUP_INACTIVE_DAYS_RANGE,
                            ))
                            (choice_select(
                                "accounts_unclaimed_cleanup_on_last_session_end",
                                "After the last sign-out",
                                LAST_SESSION_CLEANUP_CHOICES,
                                &unclaimed.on_last_session_end,
                            ))
                        }
                        p class="text-xs text-neutral-500" {
                            (format!(
                                "An account has no way back in when it has none of {}. Accounts that \
                                 sign in through SSO always have one. Such an account is deleted after \
                                 {} days without use. Deletion after the last sign-out happens \
                                 {SCHEDULED_DELETION_GRACE_HOURS} hours after it signs out of its \
                                 last session.",
                                way_back_in_wording(identity.mode, delivery_state),
                                unclaimed.inactive_days,
                            ))
                        }
                    }
                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "Accounts that never gave a date of birth" }
                            @if dob_missing.mode != CleanupMode::Off && accounts.date_of_birth == DateOfBirthPolicy::Off {
                                (badge("Inactive: the date of birth is not asked for", BadgeVariant::Warning))
                            }
                        }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (choice_select(
                                "accounts_dob_missing_cleanup_mode",
                                "Delete",
                                CLEANUP_MODE_CHOICES,
                                &dob_missing.mode,
                            ))
                            (days_field(
                                "accounts_dob_missing_cleanup_inactive_days",
                                "Days without use",
                                dob_missing.inactive_days.get(),
                                &CLEANUP_INACTIVE_DAYS_RANGE,
                            ))
                        }
                        p class="text-xs text-neutral-500" {
                            (format!(
                                "Covers accounts created without a date of birth that never entered \
                                 one, after {} days without use. Applies only while the date of birth \
                                 setting is not \u{201c}Don't ask\u{201d}.",
                                dob_missing.inactive_days,
                            ))
                        }
                    }
                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Inactive accounts" }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                            (choice_select(
                                "accounts_inactivity_deletion_mode",
                                "Delete",
                                CLEANUP_MODE_CHOICES,
                                &inactivity.mode,
                            ))
                            (days_field(
                                "accounts_inactivity_deletion_inactive_days",
                                "Days without use",
                                inactivity.inactive_days.get(),
                                &CLEANUP_INACTIVE_DAYS_RANGE,
                            ))
                            (days_field(
                                "accounts_inactivity_deletion_warning_lead_days",
                                "Days between warning and deletion",
                                inactivity.warning_lead_days.get(),
                                &INACTIVITY_WARNING_LEAD_DAYS_RANGE,
                            ))
                        }
                        p class="text-xs text-neutral-500" {
                            (format!(
                                "Sends a warning email after {} days without use and deletes the \
                                 account {} days later unless someone signs in. Nothing is deleted \
                                 without a warning email, so accounts without an email address are kept.",
                                inactivity.inactive_days,
                                inactivity.warning_lead_days,
                            ))
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save"))
                    }))
                }
            }
        },
    )
}
