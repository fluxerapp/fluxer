// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    admin_flags, api::client::AdminApiClient, middleware::flash::FlashData,
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
            let submitted = parse_u64_list(form, &["flags[]", "flags"]);
            let selected = submitted.iter().copied().collect::<HashSet<_>>();
            let user = match client.get_user_by_id(user_id).await {
                Ok(user) => user,
                Err(error) => {
                    tracing::warn!(%error, user_id, "admin API request failed: load user for flag update");
                    return DispatchOutcome::error("User not found");
                }
            };
            let add = submitted
                .iter()
                .copied()
                .filter(|flag| user.flags & *flag == 0)
                .map(|flag| flag.to_string())
                .collect::<Vec<_>>();
            let remove = admin_flags::USER_FLAGS
                .iter()
                .map(|flag| flag.value)
                .filter(|flag| user.flags & *flag != 0 && !selected.contains(flag))
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
                let add = parse_i32_list(form, &["add_flags[]", "add_flags"]);
                let remove = parse_i32_list(form, &["remove_flags[]", "remove_flags"]);
                return DispatchOutcome::from_result(
                    client.update_premium_flags(user_id, &add, &remove).await,
                    "Premium flags updated successfully",
                    "Failed to update premium flags",
                );
            }
            let submitted = parse_i32_list(form, &["flags[]", "flags"]);
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
                .filter(|flag| current & *flag != *flag)
                .collect::<Vec<_>>();
            let remove = admin_flags::PREMIUM_FLAGS
                .iter()
                .map(|flag| flag.value)
                .filter(|flag| current & *flag == *flag && !selected.contains(flag))
                .collect::<Vec<_>>();
            DispatchOutcome::from_result(
                client.update_premium_flags(user_id, &add, &remove).await,
                "Premium flags updated successfully",
                "Failed to update premium flags",
            )
        }
        "update_suspicious_flags" => {
            let flags = parse_i32_list(form, &["suspicious_flags[]", "suspicious_flags"])
                .into_iter()
                .fold(0, |acc, flag| acc | flag);
            DispatchOutcome::from_result(
                client.update_suspicious_flags(user_id, flags).await,
                "Suspicious activity flags updated successfully",
                "Failed to update suspicious activity flags",
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
        "update_has_verified_phone" => {
            let val = form.bool_value("has_verified_phone");
            DispatchOutcome::from_result(
                client.update_has_verified_phone(user_id, val).await,
                "Phone verification status updated successfully",
                "Failed to update phone verification status",
            )
        }
        "terminate_sessions" => DispatchOutcome::from_result(
            client.terminate_user_sessions(user_id).await,
            "User sessions terminated successfully",
            "Failed to terminate user sessions",
        ),
        "clear_fields" => {
            let f = form.list_values_any(&["fields[]", "fields"]);
            DispatchOutcome::from_result(
                client.clear_user_fields(user_id, &f).await,
                "User fields cleared successfully",
                "Failed to clear user fields",
            )
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
        "temp_ban" => {
            let dur = form
                .parse_u32("duration_hours")
                .or_else(|| form.parse_u32("duration"))
                .unwrap_or(24);
            let reason = get("reason");
            let private = get("private_reason");
            DispatchOutcome::from_result(
                client
                    .temp_ban_user(user_id, dur, reason.as_deref(), private.as_deref())
                    .await,
                "User temporarily banned successfully",
                "Failed to temporarily ban user",
            )
        }
        "unban" => DispatchOutcome::from_result(
            client.unban_user(user_id).await,
            "User unbanned successfully",
            "Failed to unban user",
        ),
        "ban_ip" => {
            let Some(ip) = get("ip") else {
                return DispatchOutcome::error("IP address is required");
            };
            DispatchOutcome::from_result(
                client.ban_ip(&ip).await,
                "IP banned successfully",
                "Failed to ban IP",
            )
        }
        "ban_avatar" => {
            let Some(hash) = get("avatar_hash") else {
                return DispatchOutcome::error("Avatar hash is required");
            };
            DispatchOutcome::from_result(
                client.ban_avatar_hash(&hash).await,
                "Avatar hash banned successfully",
                "Failed to ban avatar hash",
            )
        }
        "schedule_deletion" => {
            let reason_code = form.parse_i32("reason_code").unwrap_or(0);
            let public_reason = get("public_reason");
            let days = form.parse_u32("days_until_deletion").unwrap_or(60);
            DispatchOutcome::from_result(
                client
                    .schedule_deletion(user_id, reason_code, public_reason.as_deref(), days)
                    .await,
                "User deletion scheduled successfully",
                "Failed to schedule user deletion",
            )
        }
        "cancel_deletion" => DispatchOutcome::from_result(
            client.cancel_deletion(user_id).await,
            "User deletion cancelled successfully",
            "Failed to cancel user deletion",
        ),
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
        "delete_all_messages" => {
            let dry_run = parse_dry_run(form.first("dry_run"));
            match client.delete_all_user_messages(user_id, dry_run).await {
                Ok(response) if dry_run => DispatchOutcome {
                    flash: FlashData::success(format!(
                        "Dry run found {} messages across {} channels. Confirm to delete them permanently.",
                        response.message_count, response.channel_count
                    )),
                    redirect_params: vec![
                        ("delete_all_messages_dry_run".to_owned(), "true".to_owned()),
                        (
                            "delete_all_messages_channel_count".to_owned(),
                            response.channel_count.to_string(),
                        ),
                        (
                            "delete_all_messages_message_count".to_owned(),
                            response.message_count.to_string(),
                        ),
                    ],
                },
                Ok(response) => {
                    if let Some(job_id) = response.job_id {
                        DispatchOutcome {
                            flash: FlashData::success(
                                "Delete job queued. Monitor progress in the status panel.",
                            ),
                            redirect_params: vec![("message_shred_job_id".to_owned(), job_id)],
                        }
                    } else {
                        DispatchOutcome::success("No messages found for deletion.")
                    }
                }
                Err(error) => {
                    tracing::warn!(%error, user_id, "admin API request failed: delete all user messages");
                    DispatchOutcome::error("Failed to delete all user messages")
                }
            }
        }
        "cancel_bulk_message_deletion" => DispatchOutcome::from_result(
            client.cancel_bulk_message_deletion(user_id).await,
            "Bulk message deletion cancelled successfully",
            "Failed to cancel bulk message deletion",
        ),
        "refund_payment" => {
            let Some(pi) = form.clean("payment_intent_id") else {
                return DispatchOutcome::error("Payment intent ID is required");
            };
            let amt = form.parse_u64("amount_cents");
            let reason = get("reason");
            DispatchOutcome::from_result(
                client
                    .issue_refund(user_id, &pi, amt, reason.as_deref())
                    .await,
                "Refund issued successfully",
                "Failed to issue refund",
            )
        }
        "refund_policy_cancel_now" => {
            let reason = get("reason");
            DispatchOutcome::from_result(
                client
                    .refund_policy_cancel_now(user_id, reason.as_deref())
                    .await,
                "Refund policy cancellation completed successfully",
                "Failed to apply refund policy cancellation",
            )
        }
        "cancel_subscription" => DispatchOutcome::from_result(
            client.cancel_subscription(user_id).await,
            "Subscription cancelled successfully",
            "Failed to cancel subscription",
        ),
        "cancel_subscription_now" => {
            let reason = get("reason");
            DispatchOutcome::from_result(
                client
                    .cancel_subscription_immediately(user_id, reason.as_deref())
                    .await,
                "Subscription cancelled immediately",
                "Failed to cancel subscription immediately",
            )
        }
        "reactivate_subscription" => DispatchOutcome::from_result(
            client.reactivate_subscription(user_id).await,
            "Subscription reactivated successfully",
            "Failed to reactivate subscription",
        ),
        "end_premium_grace_period" => DispatchOutcome::from_result(
            client.end_premium_grace_period(user_id).await,
            "Premium grace period ended successfully",
            "Failed to end premium grace period",
        ),
        "message_shred" => {
            let csv = form.first("csv_data").unwrap_or_default();
            match parse_message_shred_csv(csv) {
                Ok(entries) if entries.is_empty() => DispatchOutcome::error(
                    "CSV did not contain any valid channel_id,message_id pairs.",
                ),
                Ok(entries) => match client.queue_message_shred(user_id, &entries).await {
                    Ok(response) => {
                        if let Some(job_id) = response.job_id {
                            DispatchOutcome {
                                flash: FlashData::success("Message shred job queued"),
                                redirect_params: vec![("message_shred_job_id".to_owned(), job_id)],
                            }
                        } else {
                            DispatchOutcome::success("Message shred job queued")
                        }
                    }
                    Err(error) => {
                        tracing::warn!(%error, user_id, "admin API request failed: queue message shred job");
                        DispatchOutcome::error("Failed to queue message shred job")
                    }
                },
                Err(error) => DispatchOutcome::error(error),
            }
        }
        _ => DispatchOutcome::error(format!("Unknown user action: {action}")),
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

fn parse_u64_list(form: &MultiValueForm, keys: &[&str]) -> Vec<u64> {
    form.list_values_any(keys)
        .iter()
        .filter_map(|value| value.parse().ok())
        .collect()
}

fn parse_i32_list(form: &MultiValueForm, keys: &[&str]) -> Vec<i32> {
    form.list_values_any(keys)
        .iter()
        .filter_map(|value| value.parse().ok())
        .collect()
}

fn parse_dry_run(value: Option<&str>) -> bool {
    !matches!(value.map(|value| value.trim().to_ascii_lowercase()), Some(value) if value == "false" || value == "0")
}

fn parse_message_shred_csv(csv: &str) -> Result<Vec<serde_json::Value>, String> {
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
        let channel_id = parse_csv_snowflake(parts[0], "channel_id", trimmed)?;
        let message_id = parse_csv_snowflake(parts[1], "message_id", trimmed)?;
        entries.push(serde_json::json!({
            "channel_id": channel_id,
            "message_id": message_id,
        }));
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
        assert_eq!(entries[0]["channel_id"], "123");
        assert_eq!(entries[0]["message_id"], "456");
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
