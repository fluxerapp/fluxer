// SPDX-License-Identifier: AGPL-3.0-or-later

use super::parse_form_number;
use crate::{
    api::types::{
        DomainMigrationConfigUpdateRequest, EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES,
        EXPERIMENT_MAX_TARGETED_USERS, ExperimentDeliveryConfigUpdateRequest,
        GuildVisitsConfigUpdateRequest, InstanceConfigUpdateRequest,
        VOICE_P2P_MAX_PARTICIPANTS_RANGE, VoiceP2pConfigUpdateRequest,
    },
    utils::{forms::MultiValueForm, snowflake::is_snowflake},
};

const EXPERIMENT_ROLLOUT_BASIS_POINTS_MAX: u32 = 10_000;
const EXPERIMENT_MAX_ROLLOUT_SALT_CHARS: usize = 64;
const EXPERIMENT_MIN_POLL_INTERVAL_SECONDS: u64 = 60;
const EXPERIMENT_MAX_POLL_INTERVAL_SECONDS: u64 = 86_400;
const EXPERIMENT_MAX_POLL_JITTER_PERCENT: u32 = 50;

fn parse_experiment_rollout_salt(
    form: &MultiValueForm,
    key: &str,
) -> Result<Option<String>, String> {
    let Some(raw) = form.first(key) else {
        return Ok(None);
    };
    let salt = raw.trim();
    if salt.is_empty() || salt.encode_utf16().count() > EXPERIMENT_MAX_ROLLOUT_SALT_CHARS {
        return Err(format!(
            "Rollout salt must be between 1 and {EXPERIMENT_MAX_ROLLOUT_SALT_CHARS} characters"
        ));
    }
    if !salt
        .bytes()
        .all(|byte| byte.is_ascii_graphic() || byte == b' ')
    {
        return Err("Rollout salt must use printable ASCII".to_owned());
    }
    Ok(Some(salt.to_owned()))
}

fn parse_experiment_user_ids(value: &str, label: &str) -> Result<Vec<String>, String> {
    let mut ids: Vec<String> = Vec::new();
    for (index, candidate) in value.split([',', '\n', '\r']).enumerate() {
        let candidate = candidate.trim();
        if candidate.is_empty() {
            continue;
        }
        if !is_snowflake(candidate) {
            return Err(format!(
                "{label} entry {} must contain 1 to 20 decimal digits",
                index + 1
            ));
        }
        if ids.iter().any(|existing| existing == candidate) {
            continue;
        }
        if ids.len() == EXPERIMENT_MAX_TARGETED_USERS {
            return Err(format!(
                "{label} must contain at most {EXPERIMENT_MAX_TARGETED_USERS} unique IDs"
            ));
        }
        ids.push(candidate.to_owned());
    }
    Ok(ids)
}

fn parse_experiment_country_codes(value: &str) -> Result<Vec<String>, String> {
    let mut codes: Vec<String> = Vec::new();
    let candidates = value
        .split(|character: char| character == ',' || character.is_whitespace())
        .filter(|candidate| !candidate.is_empty());
    for (index, candidate) in candidates.enumerate() {
        if candidate.len() != 2 || !candidate.bytes().all(|byte| byte.is_ascii_alphabetic()) {
            return Err(format!(
                "Rollout countries entry {} must be a two-letter country code",
                index + 1
            ));
        }
        let code = candidate.to_ascii_uppercase();
        if codes.contains(&code) {
            continue;
        }
        if codes.len() == EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES {
            return Err(format!(
                "Rollout countries must contain at most {EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES} unique codes"
            ));
        }
        codes.push(code);
    }
    Ok(codes)
}

pub(super) fn build_domain_migration_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    Ok(InstanceConfigUpdateRequest {
        domain_migration: Some(DomainMigrationConfigUpdateRequest {
            enabled: Some(form.bool_value("domain_migration_enabled")),
            rollout_basis_points: parse_form_number(
                form,
                "domain_migration_rollout_basis_points",
                "Rollout basis points",
                0,
                EXPERIMENT_ROLLOUT_BASIS_POINTS_MAX,
            )?,
            rollout_country_codes: Some(parse_experiment_country_codes(
                form.first("domain_migration_rollout_country_codes")
                    .unwrap_or_default(),
            )?),
            rollout_salt: parse_experiment_rollout_salt(form, "domain_migration_rollout_salt")?,
            included_user_ids: Some(parse_experiment_user_ids(
                form.first("domain_migration_included_user_ids")
                    .unwrap_or_default(),
                "Included user IDs",
            )?),
            included_guild_ids: Some(parse_experiment_user_ids(
                form.first("domain_migration_included_guild_ids")
                    .unwrap_or_default(),
                "Included guild IDs",
            )?),
            include_premium_users: Some(form.bool_value("domain_migration_include_premium_users")),
            excluded_user_ids: Some(parse_experiment_user_ids(
                form.first("domain_migration_excluded_user_ids")
                    .unwrap_or_default(),
                "Excluded user IDs",
            )?),
            anonymous_rollout_basis_points: parse_form_number(
                form,
                "domain_migration_anonymous_rollout_basis_points",
                "Anonymous rollout basis points",
                0,
                EXPERIMENT_ROLLOUT_BASIS_POINTS_MAX,
            )?,
            standalone_forwarding: Some(form.bool_value("domain_migration_standalone_forwarding")),
        }),
        ..Default::default()
    })
}

pub(super) fn build_voice_p2p_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    Ok(InstanceConfigUpdateRequest {
        voice_p2p: Some(VoiceP2pConfigUpdateRequest {
            enabled: Some(form.bool_value("voice_p2p_enabled")),
            rollout_basis_points: parse_form_number(
                form,
                "voice_p2p_rollout_basis_points",
                "Rollout basis points",
                0,
                EXPERIMENT_ROLLOUT_BASIS_POINTS_MAX,
            )?,
            rollout_country_codes: Some(parse_experiment_country_codes(
                form.first("voice_p2p_rollout_country_codes")
                    .unwrap_or_default(),
            )?),
            rollout_salt: parse_experiment_rollout_salt(form, "voice_p2p_rollout_salt")?,
            included_user_ids: Some(parse_experiment_user_ids(
                form.first("voice_p2p_included_user_ids")
                    .unwrap_or_default(),
                "Included user IDs",
            )?),
            included_guild_ids: Some(parse_experiment_user_ids(
                form.first("voice_p2p_included_guild_ids")
                    .unwrap_or_default(),
                "Included guild IDs",
            )?),
            include_premium_users: Some(form.bool_value("voice_p2p_include_premium_users")),
            excluded_user_ids: Some(parse_experiment_user_ids(
                form.first("voice_p2p_excluded_user_ids")
                    .unwrap_or_default(),
                "Excluded user IDs",
            )?),
            max_participants: parse_form_number(
                form,
                "voice_p2p_max_participants",
                "Maximum participants",
                *VOICE_P2P_MAX_PARTICIPANTS_RANGE.start(),
                *VOICE_P2P_MAX_PARTICIPANTS_RANGE.end(),
            )?,
        }),
        ..Default::default()
    })
}

pub(super) fn build_guild_visits_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    Ok(InstanceConfigUpdateRequest {
        guild_visits: Some(GuildVisitsConfigUpdateRequest {
            enabled: Some(form.bool_value("guild_visits_enabled")),
            user_basis_points: parse_form_number(
                form,
                "guild_visits_user_basis_points",
                "Rollout basis points",
                0,
                EXPERIMENT_ROLLOUT_BASIS_POINTS_MAX,
            )?,
            user_salt: parse_experiment_rollout_salt(form, "guild_visits_user_salt")?,
            included_user_ids: Some(parse_experiment_user_ids(
                form.first("guild_visits_included_user_ids")
                    .unwrap_or_default(),
                "Included user IDs",
            )?),
            excluded_user_ids: Some(parse_experiment_user_ids(
                form.first("guild_visits_excluded_user_ids")
                    .unwrap_or_default(),
                "Excluded user IDs",
            )?),
        }),
        ..Default::default()
    })
}

pub(super) fn build_experiment_delivery_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    Ok(InstanceConfigUpdateRequest {
        experiment_delivery: Some(ExperimentDeliveryConfigUpdateRequest {
            poll_interval_seconds: parse_form_number(
                form,
                "experiment_delivery_poll_interval_seconds",
                "Poll interval",
                EXPERIMENT_MIN_POLL_INTERVAL_SECONDS,
                EXPERIMENT_MAX_POLL_INTERVAL_SECONDS,
            )?,
            poll_jitter_percent: parse_form_number(
                form,
                "experiment_delivery_poll_jitter_percent",
                "Poll jitter",
                0,
                EXPERIMENT_MAX_POLL_JITTER_PERCENT,
            )?,
        }),
        ..Default::default()
    })
}
