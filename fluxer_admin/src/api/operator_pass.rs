// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::generated::snowflake;
use super::generated::types::{
    AdminOperatorPassReleaseFileResponse, AdminOperatorPassReleasePreviewResponse,
    AdminOperatorPassReleasePublishRequest, AdminOperatorPassReleasePublishRequestTag,
    AdminOperatorPassUserResponse, AdminOperatorPassWallAsOfResponse,
    AdminOperatorPassWallDecisionResponse, AdminOperatorPassWallEntryResponse,
    AdminOperatorPassWallRevisionResponse, AdminOperatorPassWallStateResponse,
    AdminOperatorPassWithdrawRequest, AdminOperatorPassWithdrawRequestName,
    AdminOperatorPassWithdrawalChannelSchema, OperatorPassPurchaseResponse,
    OperatorPassWallDeclineReasonSchema, SnowflakeStringType,
    WithdrawAdminOperatorPassPurchasePurchaseId,
};
use chrono::{DateTime, Utc};
use reqwest::Method;
use serde::{Deserialize, Serialize};
use serde_json::json;

#[derive(Serialize)]
pub struct WallDecision<'a> {
    pub reason: OperatorPassWallDeclineReasonSchema,
    pub note: &'a str,
}

pub struct PerkRemoval<'a> {
    pub rule: &'a str,
    pub facts: &'a str,
    pub serious: bool,
    pub duration_days: Option<u32>,
}

pub struct PassWithdrawal<'a> {
    pub name: &'a str,
    pub received_at: DateTime<Utc>,
    pub channel: &'a str,
}

#[derive(Deserialize, Clone, Debug)]
pub struct WallTimelineEvent {
    pub occurred_at: DateTime<Utc>,
    pub kind: String,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub reason: Option<String>,
    #[serde(default)]
    pub cause: Option<String>,
    #[serde(default)]
    pub purchase_id: Option<String>,
    #[serde(default)]
    pub release_year: Option<i64>,
    #[serde(default)]
    pub release_tag: Option<String>,
    #[serde(default)]
    pub actor_id: Option<String>,
    #[serde(default)]
    pub pass_count: Option<i64>,
}

#[derive(Deserialize, Clone, Debug)]
pub struct WallTimeline {
    pub entry: Option<AdminOperatorPassWallEntryResponse>,
    pub revisions: Vec<AdminOperatorPassWallRevisionResponse>,
    pub decisions: Vec<AdminOperatorPassWallDecisionResponse>,
    pub events: Vec<WallTimelineEvent>,
}

fn parse<T, E: std::fmt::Display>(value: Result<T, E>) -> ApiResult<T> {
    value.map_err(|e| ApiError::Parse(e.to_string()))
}

impl AdminApiClient {
    pub async fn get_operator_pass_visionary_activations_csv(
        &self,
    ) -> ApiResult<(String, Vec<u8>)> {
        self.get_bytes("/admin/operator-pass/visionary/activations.csv")
            .await
    }

    pub async fn get_operator_pass_deliveries_csv(
        &self,
        month: &str,
    ) -> ApiResult<(String, Vec<u8>)> {
        self.get_bytes(&format!(
            "/admin/operator-pass/deliveries.csv?month={month}"
        ))
        .await
    }

    pub async fn get_user_operator_pass(
        &self,
        user_id: &str,
    ) -> ApiResult<AdminOperatorPassUserResponse> {
        let response = self
            .generated()
            .get_admin_user_operator_pass(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn revert_operator_pass_wall_revision(
        &self,
        user_id: &str,
        revision_id: &str,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/wall/revisions/{revision_id}/revert"),
            None::<&()>,
            None,
        )
        .await
    }

    pub async fn force_operator_pass_wall_fallback(
        &self,
        user_id: &str,
        decision: &WallDecision<'_>,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/wall/force-fallback"),
            Some(decision),
            None,
        )
        .await
    }

    pub async fn clear_operator_pass_wall_quote(
        &self,
        user_id: &str,
        decision: &WallDecision<'_>,
        quote: &str,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/wall/clear-quote"),
            Some(&json!({"reason": decision.reason, "note": decision.note, "quote": quote})),
            None,
        )
        .await
    }

    pub async fn set_operator_pass_wall_business_listing(
        &self,
        user_id: &str,
        name: &str,
        logo_base64: &str,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::PUT,
            &format!("/admin/users/{user_id}/operator-pass/wall/business"),
            Some(&json!({"name": name, "logo": logo_base64})),
            None,
        )
        .await
    }

    pub async fn set_operator_pass_wall_hidden(
        &self,
        user_id: &str,
        hidden: bool,
        reason: Option<&str>,
        note: Option<&str>,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/wall/hide"),
            Some(&json!({"hidden": hidden, "reason": reason, "note": note})),
            None,
        )
        .await
    }

    pub async fn set_operator_pass_wall_legal_hold(
        &self,
        user_id: &str,
        held: bool,
    ) -> ApiResult<AdminOperatorPassWallEntryResponse> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/wall/legal-hold"),
            Some(&json!({"held": held})),
            None,
        )
        .await
    }

    pub async fn send_operator_pass_removal_notice(
        &self,
        user_id: &str,
        rule: &str,
        facts: &str,
    ) -> ApiResult<serde_json::Value> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/removal-notice"),
            Some(&json!({"rule": rule, "facts": facts})),
            None,
        )
        .await
    }

    pub async fn remove_operator_pass_perks(
        &self,
        user_id: &str,
        removal: &PerkRemoval<'_>,
    ) -> ApiResult<serde_json::Value> {
        self.send(
            Method::POST,
            &format!("/admin/users/{user_id}/operator-pass/removal"),
            Some(&json!({
                "rule": removal.rule,
                "facts": removal.facts,
                "automated": false,
                "serious": removal.serious,
                "duration_days": removal.duration_days,
                "notify_user": true,
            })),
            None,
        )
        .await
    }

    pub async fn restore_operator_pass_perks(&self, user_id: &str) -> ApiResult<()> {
        self.send_void(
            Method::DELETE,
            &format!("/admin/users/{user_id}/operator-pass/removal"),
            None::<&()>,
            None,
        )
        .await
    }

    pub async fn withdraw_operator_pass_purchase(
        &self,
        purchase_id: &str,
        withdrawal: &PassWithdrawal<'_>,
    ) -> ApiResult<OperatorPassPurchaseResponse> {
        let purchase_id = parse(WithdrawAdminOperatorPassPurchasePurchaseId::try_from(
            purchase_id,
        ))?;
        let body = AdminOperatorPassWithdrawRequest {
            channel: parse(
                withdrawal
                    .channel
                    .parse::<AdminOperatorPassWithdrawalChannelSchema>(),
            )?,
            name: parse(AdminOperatorPassWithdrawRequestName::try_from(
                withdrawal.name,
            ))?,
            received_at: withdrawal.received_at,
        };
        let response = self
            .generated()
            .withdraw_admin_operator_pass_purchase(&purchase_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn reissue_operator_pass_certificate(
        &self,
        pass_id: &str,
        holder_name: &str,
        reason: &str,
    ) -> ApiResult<()> {
        self.send_void(
            Method::POST,
            &format!("/admin/operator-pass/certificates/{pass_id}/reissue"),
            Some(&json!({"holder_name": holder_name, "reason": reason})),
            None,
        )
        .await
    }

    pub async fn get_operator_pass_wall_timeline(&self, user_id: &str) -> ApiResult<WallTimeline> {
        self.get(
            &format!("/admin/users/{user_id}/operator-pass/wall/timeline"),
            None,
        )
        .await
    }

    pub async fn get_operator_pass_wall_avatar(
        &self,
        sha256: &str,
    ) -> ApiResult<(String, Vec<u8>)> {
        self.get_bytes(&format!("/admin/operator-pass/wall/avatars/{sha256}"))
            .await
    }

    pub async fn get_operator_pass_wall_as_of(
        &self,
        at: &str,
    ) -> ApiResult<AdminOperatorPassWallAsOfResponse> {
        self.get("/admin/operator-pass/wall/as-of", Some(&[("at", at)]))
            .await
    }

    pub async fn get_operator_pass_wall_state(
        &self,
    ) -> ApiResult<AdminOperatorPassWallStateResponse> {
        self.get("/admin/operator-pass/wall/state", None).await
    }

    pub async fn set_operator_pass_wall_frozen(
        &self,
        frozen: bool,
    ) -> ApiResult<AdminOperatorPassWallStateResponse> {
        self.send(
            Method::POST,
            "/admin/operator-pass/wall/freeze",
            Some(&json!({"frozen": frozen})),
            None,
        )
        .await
    }

    pub async fn republish_operator_pass_wall(
        &self,
    ) -> ApiResult<AdminOperatorPassWallStateResponse> {
        self.send(
            Method::POST,
            "/admin/operator-pass/wall/republish",
            None::<&()>,
            None,
        )
        .await
    }

    pub async fn get_operator_pass_release(
        &self,
        year: i64,
    ) -> ApiResult<AdminOperatorPassReleasePreviewResponse> {
        let response = self
            .generated()
            .get_admin_operator_pass_release(year)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn publish_operator_pass_release(
        &self,
        year: i64,
        tag: &str,
        released_at: DateTime<Utc>,
    ) -> ApiResult<AdminOperatorPassReleaseFileResponse> {
        let body = AdminOperatorPassReleasePublishRequest {
            released_at,
            tag: parse(AdminOperatorPassReleasePublishRequestTag::try_from(tag))?,
        };
        let response = self
            .generated()
            .publish_admin_operator_pass_release(year, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }

    pub async fn erase_operator_pass_release_name(
        &self,
        year: i64,
        user_id: &str,
    ) -> ApiResult<AdminOperatorPassReleaseFileResponse> {
        let user_id = parse(SnowflakeStringType::try_from(user_id))?;
        let response = self
            .generated()
            .erase_admin_operator_pass_release_name(year, &user_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(response.into_inner())
    }
}
