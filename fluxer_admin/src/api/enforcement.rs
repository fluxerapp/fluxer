// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::Method;
use serde::Deserialize;

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{AdminUser, UserMutationResponse};

#[derive(Debug, Deserialize)]
pub struct RestrictionResponse {
    pub skipped_ips: Vec<generated_types::SkippedNetworkBlock>,
}

pub struct DecisionSubmission {
    pub decision: generated_types::EnforcementDecisionInput,
    pub internal_note: String,
}

pub struct ClosureRequest {
    pub basis: generated_types::ModerationBasis,
    pub days_until_deletion: u32,
    pub immediate: bool,
    pub remove_evidence: bool,
    pub block_networks: Option<generated_types::NetworkBlockRequest>,
    pub replace_pending_deletion_at: Option<chrono::DateTime<chrono::Utc>>,
    pub internal_note: Option<String>,
}

impl AdminApiClient {
    pub async fn suspend_user(
        &self,
        user_id: &str,
        duration_hours: u32,
        remove_evidence: bool,
        block_networks: Option<generated_types::NetworkBlockRequest>,
        submission: &DecisionSubmission,
    ) -> ApiResult<RestrictionResponse> {
        let body = generated_types::AdminUserBanRequest {
            block_networks,
            decision: submission.decision.clone(),
            duration_hours: i64::from(duration_hours),
            remove_evidence: remove_evidence.then_some(true),
        };
        self.send(
            Method::PUT,
            &format!("/admin/users/{}/ban", urlencoding::encode(user_id)),
            Some(&body),
            Some(&submission.internal_note),
        )
        .await
    }

    pub async fn list_network_block_candidates(
        &self,
        user_id: &str,
    ) -> ApiResult<generated_types::NetworkBlockCandidatesResponse> {
        self.get(
            &format!(
                "/admin/users/{}/network-block-candidates",
                urlencoding::encode(user_id)
            ),
            None,
        )
        .await
    }

    pub async fn warn_user(
        &self,
        user_id: &str,
        submission: &DecisionSubmission,
    ) -> ApiResult<generated_types::AdminUserWarningResponse> {
        let body = generated_types::AdminUserWarningRequest {
            decision: submission.decision.clone(),
        };
        self.send(
            Method::POST,
            &format!("/admin/users/{}/warnings", urlencoding::encode(user_id)),
            Some(&body),
            Some(&submission.internal_note),
        )
        .await
    }

    pub async fn close_account(
        &self,
        user_id: &str,
        request: &ClosureRequest,
    ) -> ApiResult<RestrictionResponse> {
        let body = generated_types::AdminUserDeletionScheduleRequest {
            basis: request.basis.clone(),
            block_networks: request.block_networks.clone(),
            days_until_deletion: crate::api::generated::nonzero_u32(
                request.days_until_deletion,
                "days_until_deletion",
            )
            .map_err(ApiError::Parse)?
            .into(),
            immediate: request.immediate.then_some(true),
            remove_evidence: request.remove_evidence.then_some(true),
            replace_pending_deletion_at: request.replace_pending_deletion_at,
        };
        self.send(
            Method::PUT,
            &format!("/admin/users/{}/deletion", urlencoding::encode(user_id)),
            Some(&body),
            request.internal_note.as_deref(),
        )
        .await
    }

    pub async fn limit_features(
        &self,
        user_id: &str,
        limit: generated_types::AdminUserFeatureLimitRequestLimit,
        ends_at: Option<chrono::DateTime<chrono::Utc>>,
        submission: &DecisionSubmission,
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserFeatureLimitRequest {
            decision: submission.decision.clone(),
            ends_at,
            limit,
        };
        let response: UserMutationResponse = self
            .send(
                Method::PUT,
                &format!(
                    "/admin/users/{}/feature-limits",
                    urlencoding::encode(user_id)
                ),
                Some(&body),
                Some(&submission.internal_note),
            )
            .await?;
        Ok(response.user)
    }

    pub async fn list_review_queue(
        &self,
    ) -> ApiResult<generated_types::EnforcementDecisionListResponse> {
        self.get(
            "/admin/enforcement-decisions",
            Some(&[("review_queue", "true")]),
        )
        .await
    }

    pub async fn list_user_decisions(
        &self,
        user_id: &str,
    ) -> ApiResult<generated_types::EnforcementDecisionListResponse> {
        self.get(
            "/admin/enforcement-decisions",
            Some(&[("subject_type", "user"), ("subject_id", user_id)]),
        )
        .await
    }

    pub async fn get_decision(
        &self,
        decision_id: &str,
    ) -> ApiResult<generated_types::EnforcementDecisionResponse> {
        self.get(
            &format!(
                "/admin/enforcement-decisions/{}",
                urlencoding::encode(decision_id)
            ),
            None,
        )
        .await
    }

    pub async fn end_decision(
        &self,
        decision_id: &str,
        outcome: generated_types::EnforcementDecisionEndRequestOutcome,
        statement: Option<String>,
        internal_note: &str,
    ) -> ApiResult<generated_types::EnforcementDecisionResponse> {
        let body = generated_types::EnforcementDecisionEndRequest { outcome, statement };
        self.send(
            Method::POST,
            &format!(
                "/admin/enforcement-decisions/{}/end",
                urlencoding::encode(decision_id)
            ),
            Some(&body),
            Some(internal_note),
        )
        .await
    }

    pub async fn annotate_decision(
        &self,
        decision_id: &str,
        body: String,
    ) -> ApiResult<generated_types::EnforcementDecisionNoteResponse> {
        let body = generated_types::EnforcementDecisionNoteRequest { body };
        self.send(
            Method::POST,
            &format!(
                "/admin/enforcement-decisions/{}/notes",
                urlencoding::encode(decision_id)
            ),
            Some(&body),
            None,
        )
        .await
    }
}
