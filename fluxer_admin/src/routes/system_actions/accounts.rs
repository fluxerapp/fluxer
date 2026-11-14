// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{parse_choice, parse_form_number};
use crate::{
    api::types::{
        AccountsConfigUpdateRequest, AuthConfigUpdateRequest, CLEANUP_INACTIVE_DAYS_RANGE,
        DobMissingCleanupUpdateRequest, INACTIVITY_WARNING_LEAD_DAYS_RANGE,
        INSTANT_ACCOUNTS_ESCALATION_RANGE, InactivityDeletionUpdateRequest,
        InstanceConfigUpdateRequest, InstantAccountsConfigUpdateRequest,
        UnclaimedAccountCleanupUpdateRequest,
    },
    utils::forms::MultiValueForm,
};
use std::num::NonZeroU64;

fn parse_form_days(
    form: &MultiValueForm,
    key: &str,
    label: &str,
    range: &std::ops::RangeInclusive<u32>,
) -> Result<Option<NonZeroU64>, String> {
    Ok(
        parse_form_number(form, key, label, *range.start(), *range.end())?
            .and_then(|days| NonZeroU64::new(u64::from(days))),
    )
}

pub(super) fn build_accounts_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    let unclaimed = UnclaimedAccountCleanupUpdateRequest {
        mode: parse_choice(
            form,
            "accounts_unclaimed_cleanup_mode",
            "Accounts with no way back in",
        )?,
        inactive_days: parse_form_days(
            form,
            "accounts_unclaimed_cleanup_inactive_days",
            "Days without use",
            &CLEANUP_INACTIVE_DAYS_RANGE,
        )?,
        on_last_session_end: parse_choice(
            form,
            "accounts_unclaimed_cleanup_on_last_session_end",
            "After the last sign-out",
        )?,
    };
    let dob_missing = DobMissingCleanupUpdateRequest {
        mode: parse_choice(
            form,
            "accounts_dob_missing_cleanup_mode",
            "Accounts that never gave a date of birth",
        )?,
        inactive_days: parse_form_days(
            form,
            "accounts_dob_missing_cleanup_inactive_days",
            "Days without use",
            &CLEANUP_INACTIVE_DAYS_RANGE,
        )?,
    };
    let inactivity = InactivityDeletionUpdateRequest {
        mode: parse_choice(
            form,
            "accounts_inactivity_deletion_mode",
            "Inactive accounts",
        )?,
        inactive_days: parse_form_days(
            form,
            "accounts_inactivity_deletion_inactive_days",
            "Days without use",
            &CLEANUP_INACTIVE_DAYS_RANGE,
        )?,
        warning_lead_days: parse_form_days(
            form,
            "accounts_inactivity_deletion_warning_lead_days",
            "Days between warning and deletion",
            &INACTIVITY_WARNING_LEAD_DAYS_RANGE,
        )?,
    };
    Ok(InstanceConfigUpdateRequest {
        auth: Some(AuthConfigUpdateRequest {
            identifier_first: parse_choice(form, "auth_identifier_first", "Start sign-in")?,
            instant_accounts: Some(InstantAccountsConfigUpdateRequest {
                enabled: Some(form.bool_value("auth_instant_accounts_enabled")),
                pow_escalation_per_hour: parse_form_number(
                    form,
                    "auth_instant_accounts_pow_escalation_per_hour",
                    "Accounts per hour",
                    *INSTANT_ACCOUNTS_ESCALATION_RANGE.start(),
                    *INSTANT_ACCOUNTS_ESCALATION_RANGE.end(),
                )?,
            }),
        }),
        accounts: Some(AccountsConfigUpdateRequest {
            date_of_birth: parse_choice(form, "accounts_date_of_birth", "Date of birth")?,
            interaction_tier: parse_choice(form, "accounts_interaction_tier", "Account limits")?,
            verification_basis: parse_choice(
                form,
                "accounts_verification_basis",
                "What finishes setting up an account",
            )?,
            guild_join_gate: parse_choice(form, "accounts_guild_join_gate", "Joining a community")?,
            unclaimed_account_cleanup: (unclaimed.mode.is_some()
                || unclaimed.inactive_days.is_some()
                || unclaimed.on_last_session_end.is_some())
            .then_some(unclaimed),
            dob_missing_cleanup: (dob_missing.mode.is_some()
                || dob_missing.inactive_days.is_some())
            .then_some(dob_missing),
            inactivity_deletion: (inactivity.mode.is_some()
                || inactivity.inactive_days.is_some()
                || inactivity.warning_lead_days.is_some())
            .then_some(inactivity),
            ..Default::default()
        }),
        ..Default::default()
    })
}
