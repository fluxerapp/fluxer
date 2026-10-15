// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::plural::count_noun;
use crate::{
    admin_flags,
    api::{
        client::{AdminApiClient, ApiError},
        enforcement::{ClosureRequest, api_error_message},
        generated::{
            snowflake,
            types::{AdminUserFeatureLimitRequestLimit, AdminUserMessageShredRequestEntriesItem},
        },
    },
    middleware::flash::FlashData,
    templates::components::enforcement_fields::{
        BasisSubmission, basis_from_form, decision_from_form, network_blocks_from_form,
        skipped_networks_summary,
    },
    utils::forms::MultiValueForm,
};
use std::collections::HashSet;

pub struct DispatchOutcome {
    pub flash: FlashData,
    pub redirect_params: Vec<(String, String)>,
}

impl DispatchOutcome {
    fn success(message: impl Into<String>) -> Self {
        Self {
            flash: FlashData::success(message),
            redirect_params: Vec::new(),
        }
    }

    fn error(message: impl Into<String>) -> Self {
        Self {
            flash: FlashData::error(message),
            redirect_params: Vec::new(),
        }
    }

    fn from_result<T, E: std::fmt::Display>(
        result: Result<T, E>,
        success_message: impl Into<String>,
        error_message: impl Into<String>,
    ) -> Self {
        match result {
            Ok(_) => Self::success(success_message),
            Err(error) => {
                let error_message = error_message.into();
                tracing::warn!(%error, context = %error_message, "admin API request failed: user action");
                Self::error(error_message)
            }
        }
    }
}

pub async fn dispatch(
    client: &AdminApiClient,
    user_id: &str,
    action: &str,
    form: &MultiValueForm,
) -> DispatchOutcome {
    let get = |k: &str| form.clean(k);
    match action {
        "update_flags" => {
            if has_legacy_flag_delta_fields(form) {
                let add = form.list_values_any(&["add_flags[]", "add_flags"]);
                let remove = form.list_values_any(&["remove_flags[]", "remove_flags"]);
                return DispatchOutcome::from_result(
                    client.update_user_flags(user_id, &add, &remove).await,
                    "User flags updated successfully",
                    "Failed to update user flags",
                );
            }
            let Ok(submitted) = form.parse_list_values::<u64>(&["flags[]", "flags"]) else {
                return DispatchOutcome::error("Invalid user flag value");
            };
            let selected = submitted.iter().copied().collect::<HashSet<_>>();
            let user = match client.get_user_by_id(user_id).await {
                Ok(user) => user,
                Err(error) => {
                    tracing::warn!(%error, user_id, "admin API request failed: load user for flag update");
                    return DispatchOutcome::error("User not found");
                }
            };
            let suspended = user.temp_banned_until.is_some();
            let editable =
                |flag: &u64| admin_flags::user_flag_is_editable(*flag, user.flags, suspended);
            let add = submitted
                .iter()
                .copied()
                .filter(|flag| user.flags & *flag == 0 && editable(flag))
                .map(|flag| flag.to_string())
                .collect::<Vec<_>>();
            let remove = admin_flags::USER_FLAGS
                .iter()
                .map(|flag| flag.value)
                .filter(|flag| {
                    user.flags & *flag != 0 && !selected.contains(flag) && editable(flag)
                })
                .map(|flag| flag.to_string())
                .collect::<Vec<_>>();
            DispatchOutcome::from_result(
                client.update_user_flags(user_id, &add, &remove).await,
                "User flags updated successfully",
                "Failed to update user flags",
            )
        }
        "update_premium_flags" => {
            if has_legacy_flag_delta_fields(form) {
                let Ok(add) = form.parse_list_values::<i32>(&["add_flags[]", "add_flags"]) else {
                    return DispatchOutcome::error("Invalid premium flag value to add");
                };
                let Ok(remove) = form.parse_list_values::<i32>(&["remove_flags[]", "remove_flags"])
                else {
                    return DispatchOutcome::error("Invalid premium flag value to remove");
                };
                return DispatchOutcome::from_result(
                    client.update_premium_flags(user_id, &add, &remove).await,
                    "Premium flags updated successfully",
                    "Failed to update premium flags",
                );
            }
            let Ok(submitted) = form.parse_list_values::<i32>(&["flags[]", "flags"]) else {
                return DispatchOutcome::error("Invalid premium flag value");
            };
            let selected = submitted.iter().copied().collect::<HashSet<_>>();
            let user = match client.get_user_by_id(user_id).await {
                Ok(user) => user,
                Err(error) => {
                    tracing::warn!(%error, user_id, "admin API request failed: load user for premium flag update");
                    return DispatchOutcome::error("User not found");
                }
            };
            let current = user.premium_flags;
            let add = submitted
                .iter()
                .copied()
                .filter(|flag| {
                    current & *flag != *flag && admin_flags::premium_flag_is_editable(*flag)
                })
                .collect::<Vec<_>>();
            let remove = admin_flags::PREMIUM_FLAGS
                .iter()
                .map(|flag| flag.value)
                .filter(|flag| {
                    current & *flag == *flag
                        && !selected.contains(flag)
                        && admin_flags::premium_flag_is_editable(*flag)
                })
                .collect::<Vec<_>>();
            DispatchOutcome::from_result(
                client.update_premium_flags(user_id, &add, &remove).await,
                "Premium flags updated successfully",
                "Failed to update premium flags",
            )
        }
        "update_acls" => {
            let acls = form.list_values_any(&["acls[]", "acls"]);
            DispatchOutcome::from_result(
                client.set_user_acls(user_id, &acls).await,
                "User ACLs updated successfully",
                "Failed to update user ACLs",
            )
        }
        "update_traits" => {
            let traits = form.list_values_any(&["traits[]", "traits"]);
            DispatchOutcome::from_result(
                client.set_user_traits(user_id, &traits).await,
                "User traits updated successfully",
                "Failed to update user traits",
            )
        }
        "disable_mfa" => DispatchOutcome::from_result(
            client.disable_mfa(user_id).await,
            "MFA disabled successfully",
            "Failed to disable MFA",
        ),
        "resend_verification_email" => DispatchOutcome::from_result(
            client.resend_verification_email(user_id).await,
            "Verification email sent successfully",
            "Failed to resend verification email",
        ),
        "verify_email" => DispatchOutcome::from_result(
            client.verify_email(user_id).await,
            "Email verified successfully",
            "Failed to verify email",
        ),
        "terminate_sessions" => DispatchOutcome::from_result(
            client.terminate_user_sessions(user_id).await,
            "User sessions terminated successfully",
            "Failed to terminate user sessions",
        ),
        "clear_fields" => {
            let fields = form.list_values_any(&["fields[]", "fields"]);
            if fields.is_empty() {
                return DispatchOutcome::error("Choose at least one field");
            }
            let basis = match basis_from_form(form) {
                Ok(basis) => basis,
                Err(message) => return DispatchOutcome::error(message),
            };
            match client.clear_user_fields(user_id, &fields, &basis).await {
                Ok(_) => DispatchOutcome::success("Profile fields cleared"),
                Err(error) => {
                    decision_failure("Failed to clear the profile fields", user_id, &error)
                }
            }
        }
        "set_bot_status" => {
            let val = form.bool_value("bot");
            DispatchOutcome::from_result(
                client.set_bot_status(user_id, val).await,
                "Bot status updated successfully",
                "Failed to update bot status",
            )
        }
        "set_system_status" => {
            let val = form.bool_value("system");
            DispatchOutcome::from_result(
                client.set_system_status(user_id, val).await,
                "System status updated successfully",
                "Failed to update system status",
            )
        }
        "change_username" => {
            let Some(username) = get("username") else {
                return DispatchOutcome::error("Username is required");
            };
            let discriminator = get("discriminator");
            DispatchOutcome::from_result(
                client
                    .change_username(user_id, &username, discriminator.as_deref())
                    .await,
                "Username changed successfully",
                "Failed to change username",
            )
        }
        "change_email" => {
            let Some(email) = get("email") else {
                return DispatchOutcome::error("Email is required");
            };
            DispatchOutcome::from_result(
                client.change_email(user_id, &email).await,
                "Email changed successfully",
                "Failed to change email",
            )
        }
        "suspend" => {
            let Ok(Some(duration_hours)) = form.parse_value::<u32>("duration_hours") else {
                return DispatchOutcome::error("Choose a duration");
            };
            let submission = match decision_from_form(form) {
                Ok(submission) => submission,
                Err(message) => return DispatchOutcome::error(message),
            };
            let block_networks = match network_blocks_from_form(form) {
                Ok(block_networks) => block_networks,
                Err(message) => return DispatchOutcome::error(message),
            };
            let remove_evidence = form.bool_value("remove_evidence");
            match client
                .suspend_user(
                    user_id,
                    duration_hours,
                    remove_evidence,
                    block_networks,
                    &submission,
                )
                .await
            {
                Ok(response) => {
                    let done = if duration_hours == 0 {
                        "Account banned."
                    } else {
                        "Account suspended."
                    };
                    DispatchOutcome::success(with_skipped(done, &response.skipped_ips))
                }
                Err(error) => decision_failure("Failed to suspend the account", user_id, &error),
            }
        }
        "warn" => {
            let submission = match decision_from_form(form) {
                Ok(submission) => submission,
                Err(message) => return DispatchOutcome::error(message),
            };
            match client.warn_user(user_id, &submission).await {
                Ok(response) => DispatchOutcome::success(format!(
                    "Warning sent as decision {}",
                    response.decision_id.as_str()
                )),
                Err(error) => decision_failure("Failed to send the warning", user_id, &error),
            }
        }
        "unban" => {
            let Some(internal_note) = get("internal_note") else {
                return DispatchOutcome::error("Add an internal note");
            };
            match client.unban_user(user_id, Some(&internal_note)).await {
                Ok(_) => DispatchOutcome::success("Suspension lifted"),
                Err(error) => decision_failure("Failed to lift the suspension", user_id, &error),
            }
        }
        "ban_ip" => {
            let Some(ip) = get("ip") else {
                return DispatchOutcome::error("IP address is required");
            };
            let Ok(duration) = form.parse_value::<u32>("duration_hours") else {
                return DispatchOutcome::error("Invalid ban duration");
            };
            DispatchOutcome::from_result(
                client.ban_ip(&ip, duration.unwrap_or(0), None).await,
                "IP banned successfully",
                "Failed to ban IP",
            )
        }
        "ban_avatar" => {
            let Some(hash) = get("avatar_hash") else {
                return DispatchOutcome::error("Avatar hash is required");
            };
            DispatchOutcome::from_result(
                client.ban_avatar_hash(&hash, None).await,
                "Avatar hash banned successfully",
                "Failed to ban avatar hash",
            )
        }
        "close" => close_account(client, user_id, form).await,
        "limit_features" => {
            let Some(limit) = form
                .first("limit")
                .and_then(|value| AdminUserFeatureLimitRequestLimit::try_from(value).ok())
            else {
                return DispatchOutcome::error("Choose a limit");
            };
            let ends_at = match (limit, form.first("ends_on")) {
                (AdminUserFeatureLimitRequestLimit::Reports, Some(value)) => match parse_day(value)
                {
                    Some(ends_at) => Some(ends_at),
                    None => return DispatchOutcome::error("Enter the end date as a date"),
                },
                _ => None,
            };
            let submission = match decision_from_form(form) {
                Ok(submission) => submission,
                Err(message) => return DispatchOutcome::error(message),
            };
            match client
                .limit_features(user_id, limit, ends_at, &submission)
                .await
            {
                Ok(_) => DispatchOutcome::success("Feature limited"),
                Err(error) => decision_failure("Failed to limit the feature", user_id, &error),
            }
        }
        "cancel_deletion" => {
            let Some(expected) = get("expected_pending_deletion_at") else {
                return DispatchOutcome::error(
                    "The pending deletion is missing from the form. Reload and review.",
                );
            };
            if !form.bool_value("confirm") {
                return DispatchOutcome::error(
                    "Confirm whose deletion you are canceling before submitting",
                );
            }
            let Some(internal_note) = get("internal_note") else {
                return DispatchOutcome::error("Add an internal note");
            };
            match client
                .cancel_deletion(user_id, &expected, Some(&internal_note))
                .await
            {
                Ok(_) => DispatchOutcome::success("Deletion cancelled"),
                Err(error) => decision_failure("Failed to cancel the deletion", user_id, &error),
            }
        }
        "change_dob" => {
            let Some(dob) = get("date_of_birth") else {
                return DispatchOutcome::error("Date of birth is required");
            };
            DispatchOutcome::from_result(
                client.change_dob(user_id, &dob).await,
                "Date of birth changed successfully",
                "Failed to change date of birth",
            )
        }
        "trigger_archive" => {
            let inc = form.bool_value("include_attachments");
            DispatchOutcome::from_result(
                client.trigger_user_archive(user_id, inc).await,
                "User archive triggered successfully",
                "Failed to trigger user archive",
            )
        }
        "delete_webauthn_credential" => {
            let Some(cred_id) = get("credential_id") else {
                return DispatchOutcome::error("Credential ID is required");
            };
            DispatchOutcome::from_result(
                client.delete_webauthn_credential(user_id, &cred_id).await,
                "WebAuthn credential deleted successfully",
                "Failed to delete WebAuthn credential",
            )
        }
        "send_password_reset" => DispatchOutcome::from_result(
            client.send_password_reset(user_id).await,
            "Password reset sent successfully",
            "Failed to send password reset",
        ),
        "revoke_recovery_kit" => DispatchOutcome::from_result(
            client.revoke_recovery_kit(user_id).await,
            "Recovery kit revoked",
            "Failed to revoke recovery kit",
        ),
        "remove_relationship" => {
            let Some(target_id) = get("target_user_id").or_else(|| get("target_id")) else {
                return DispatchOutcome::error("Target user ID is required");
            };
            let category = get("category").unwrap_or_else(|| "friend".to_owned());
            if !is_relationship_category(&category) {
                return DispatchOutcome::error(format!(
                    "Invalid relationship category: {category}"
                ));
            }
            DispatchOutcome::from_result(
                client
                    .remove_relationship(user_id, &target_id, &category)
                    .await,
                "Relationship removed successfully",
                "Failed to remove relationship",
            )
        }
        "remove_relationships_by_category" => {
            let Some(category) = get("category") else {
                return DispatchOutcome::error("Relationship category is required");
            };
            if !is_relationship_category(&category) {
                return DispatchOutcome::error(format!(
                    "Invalid relationship category: {category}"
                ));
            }
            match client
                .remove_relationships_by_category(user_id, &category)
                .await
            {
                Ok(response) => DispatchOutcome::success(format!(
                    "Removed {} {} relationship(s)",
                    response.removed_count,
                    category.replace('_', " ")
                )),
                Err(error) => {
                    tracing::warn!(%error, user_id, category, "admin API request failed: remove relationships by category");
                    DispatchOutcome::error("Failed to remove relationships")
                }
            }
        }
        "delete_all_messages" => delete_all_messages(client, user_id, form).await,
        "cancel_bulk_message_deletion" => DispatchOutcome::from_result(
            client.cancel_bulk_message_deletion(user_id).await,
            "Bulk message deletion canceled successfully",
            "Failed to cancel bulk message deletion",
        ),
        "message_shred" => {
            let entries = match parse_message_shred_csv(form.first("csv_data").unwrap_or_default())
            {
                Ok(entries) if entries.is_empty() => {
                    return DispatchOutcome::error("The CSV has no channel_id,message_id rows.");
                }
                Ok(entries) => entries,
                Err(error) => return DispatchOutcome::error(error),
            };
            let basis = match basis_from_form(form) {
                Ok(basis) => basis,
                Err(message) => return DispatchOutcome::error(message),
            };
            match client.queue_message_shred(user_id, entries, &basis).await {
                Ok(response) => DispatchOutcome {
                    flash: FlashData::success(removal_message(
                        "Message shred queued",
                        response.decision_id.as_ref().map(|id| id.as_str()),
                    )),
                    redirect_params: vec![("message_shred_job_id".to_owned(), response.job_id)],
                },
                Err(error) => {
                    decision_failure("Failed to queue the message shred", user_id, &error)
                }
            }
        }
        _ => DispatchOutcome::error(format!("Unknown user action: {action}")),
    }
}

fn with_skipped(
    done: &str,
    skipped: &[crate::api::generated::types::SkippedNetworkBlock],
) -> String {
    match skipped_networks_summary(skipped) {
        Some(summary) => format!("{done} {summary}"),
        None => done.to_owned(),
    }
}

async fn close_account(
    client: &AdminApiClient,
    user_id: &str,
    form: &MultiValueForm,
) -> DispatchOutcome {
    let Ok(Some(days_until_deletion)) = form.parse_value::<u32>("days_until_deletion") else {
        return DispatchOutcome::error("Enter the days until deletion");
    };
    let replace_pending_deletion_at = match form.clean("replace_pending_deletion_at") {
        Some(value) => match chrono::DateTime::parse_from_rfc3339(&value) {
            Ok(at) => Some(at.with_timezone(&chrono::Utc)),
            Err(_) => {
                return DispatchOutcome::error(
                    "The pending deletion is missing from the form. Reload and review.",
                );
            }
        },
        None => None,
    };
    let basis = match basis_from_form(form) {
        Ok(basis) => basis,
        Err(message) => return DispatchOutcome::error(message),
    };
    let enforcement = matches!(basis, BasisSubmission::Enforcement(_));
    let block_networks = match network_blocks_from_form(form) {
        Ok(block_networks) => block_networks.filter(|_| enforcement),
        Err(message) => return DispatchOutcome::error(message),
    };
    let request = ClosureRequest {
        basis: basis.moderation_basis(),
        days_until_deletion,
        remove_evidence: enforcement && form.bool_value("remove_evidence"),
        block_networks,
        replace_pending_deletion_at,
        internal_note: basis.internal_note().map(str::to_owned),
    };
    match client.close_account(user_id, &request).await {
        Ok(response) => {
            DispatchOutcome::success(with_skipped("Account closed.", &response.skipped_ips))
        }
        Err(error) => decision_failure("Failed to close the account", user_id, &error),
    }
}

async fn delete_all_messages(
    client: &AdminApiClient,
    user_id: &str,
    form: &MultiValueForm,
) -> DispatchOutcome {
    let basis = if parse_dry_run(form.first("dry_run")) {
        None
    } else {
        match basis_from_form(form) {
            Ok(basis) => Some(basis),
            Err(message) => return DispatchOutcome::error(message),
        }
    };
    let response = match client
        .delete_all_user_messages(user_id, basis.as_ref())
        .await
    {
        Ok(response) => response,
        Err(error) => return decision_failure("Failed to delete the messages", user_id, &error),
    };
    let message_count = u64::try_from(*response.message_count).unwrap_or_default();
    let channel_count = u64::try_from(*response.channel_count).unwrap_or_default();
    if basis.is_none() {
        return DispatchOutcome {
            flash: FlashData::success(format!(
                "Found {} in {}. Choose a basis to delete them.",
                count_noun(message_count, "message", "messages"),
                count_noun(channel_count, "channel", "channels")
            )),
            redirect_params: vec![
                ("delete_all_messages_dry_run".to_owned(), "true".to_owned()),
                (
                    "delete_all_messages_channel_count".to_owned(),
                    channel_count.to_string(),
                ),
                (
                    "delete_all_messages_message_count".to_owned(),
                    message_count.to_string(),
                ),
            ],
        };
    }
    match response.job_id {
        Some(job_id) => DispatchOutcome {
            flash: FlashData::success(removal_message(
                "Delete job queued",
                response.decision_id.as_ref().map(|id| id.as_str()),
            )),
            redirect_params: vec![("message_shred_job_id".to_owned(), job_id)],
        },
        None => DispatchOutcome::success("No messages to delete"),
    }
}

fn removal_message(queued: &str, decision_id: Option<&str>) -> String {
    match decision_id {
        Some(decision_id) => format!("{queued} as decision {decision_id}"),
        None => queued.to_owned(),
    }
}

fn parse_day(value: &str) -> Option<chrono::DateTime<chrono::Utc>> {
    chrono::NaiveDate::parse_from_str(value.trim(), "%Y-%m-%d")
        .ok()?
        .and_hms_opt(0, 0, 0)
        .map(|at| at.and_utc())
}

fn decision_failure(context: &str, user_id: &str, error: &ApiError) -> DispatchOutcome {
    tracing::warn!(%error, user_id, "admin API request failed: {context}");
    match api_error_message(error) {
        Some(message) => DispatchOutcome::error(format!("{context}: {message}")),
        None => DispatchOutcome::error(context.to_owned()),
    }
}

fn has_legacy_flag_delta_fields(form: &MultiValueForm) -> bool {
    ["add_flags[]", "add_flags", "remove_flags[]", "remove_flags"]
        .iter()
        .any(|key| form.contains_key(key))
}

fn is_relationship_category(category: &str) -> bool {
    matches!(
        category,
        "friend" | "incoming_request" | "outgoing_request" | "blocked"
    )
}

fn parse_dry_run(value: Option<&str>) -> bool {
    !matches!(value.map(|value| value.trim().to_ascii_lowercase()), Some(value) if value == "false" || value == "0")
}

fn parse_message_shred_csv(
    csv: &str,
) -> Result<Vec<AdminUserMessageShredRequestEntriesItem>, String> {
    let mut entries = Vec::new();
    for line in csv.trim().lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.eq_ignore_ascii_case("channel_id,message_id") {
            continue;
        }
        let parts: Vec<&str> = trimmed.split(',').collect();
        if parts.len() != 2 {
            return Err(
                "Each row must contain channel_id and message_id separated by a comma".to_owned(),
            );
        }
        entries.push(AdminUserMessageShredRequestEntriesItem {
            channel_id: snowflake(&parse_csv_snowflake(parts[0], "channel_id", trimmed)?),
            message_id: snowflake(&parse_csv_snowflake(parts[1], "message_id", trimmed)?),
        });
    }
    Ok(entries)
}

fn parse_csv_snowflake(value: &str, field: &str, row: &str) -> Result<String, String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(format!("Invalid row format: {row}"));
    }
    match trimmed.parse::<u64>() {
        Ok(parsed) => Ok(parsed.to_string()),
        Err(_) => Err(format!("Invalid {field} on row: {row}")),
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_dry_run, parse_message_shred_csv};

    #[test]
    fn parse_dry_run_defaults_to_preview() {
        assert!(parse_dry_run(None));
        assert!(parse_dry_run(Some("true")));
        assert!(!parse_dry_run(Some("false")));
        assert!(!parse_dry_run(Some("0")));
    }

    #[test]
    fn parse_message_shred_csv_accepts_header_and_rows() {
        let entries = parse_message_shred_csv("channel_id,message_id\n123,456\n789,101112")
            .expect("valid csv");
        assert_eq!(entries.len(), 2);
        assert_eq!(
            serde_json::to_value(&entries[0]).expect("serializable entry"),
            serde_json::json!({"channel_id": "123", "message_id": "456"})
        );
    }

    #[test]
    fn parse_message_shred_csv_rejects_bad_rows() {
        let error = parse_message_shred_csv("123,abc").expect_err("invalid message id");
        assert_eq!(error, "Invalid message_id on row: 123,abc");
        let error = parse_message_shred_csv("123,456,789").expect_err("too many columns");
        assert_eq!(
            error,
            "Each row must contain channel_id and message_id separated by a comma"
        );
    }
}
