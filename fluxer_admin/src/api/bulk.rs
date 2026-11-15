// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::{guild_features, user_flags};

use reqwest::Method;

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::BulkJobResponse;

impl AdminApiClient {
    pub async fn bulk_update_user_flags(
        &self,
        user_ids: &[String],
        add_flags: &[String],
        remove_flags: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::UpdateUserFlags {
            add_flags: user_flags(add_flags)?,
            remove_flags: user_flags(remove_flags)?,
            user_ids: snowflakes(user_ids),
        };
        self.send(
            Method::POST,
            "/admin/bulk-jobs",
            Some(&body),
            audit_log_reason,
        )
        .await
    }

    pub async fn bulk_update_guild_features(
        &self,
        guild_ids: &[String],
        add_features: &[String],
        remove_features: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::UpdateGuildFeatures {
            add_features: guild_features(add_features),
            guild_ids: snowflakes(guild_ids),
            remove_features: guild_features(remove_features),
        };
        self.send(
            Method::POST,
            "/admin/bulk-jobs",
            Some(&body),
            audit_log_reason,
        )
        .await
    }

    pub async fn bulk_add_guild_members(
        &self,
        guild_id: &str,
        user_ids: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::AddGuildMembers {
            guild_id: snowflake(guild_id),
            user_ids: snowflakes(user_ids),
        };
        self.send(
            Method::POST,
            "/admin/bulk-jobs",
            Some(&body),
            audit_log_reason,
        )
        .await
    }

    pub async fn bulk_delete_user_messages(
        &self,
        user_ids: &[String],
        basis: generated_types::BulkModerationBasis,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::DeleteUserMessages {
            basis,
            user_ids: snowflakes(user_ids),
        };
        self.send(
            Method::POST,
            "/admin/bulk-jobs",
            Some(&body),
            audit_log_reason,
        )
        .await
    }

    pub async fn bulk_schedule_user_deletion(
        &self,
        user_ids: &[String],
        days_until_deletion: u32,
        basis: generated_types::BulkModerationBasis,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::ScheduleUserDeletion {
            basis,
            days_until_deletion: crate::api::generated::nonzero_u32(
                days_until_deletion,
                "days_until_deletion",
            )
            .map_err(ApiError::Parse)?
            .into(),
            user_ids: snowflakes(user_ids),
        };
        self.send(
            Method::POST,
            "/admin/bulk-jobs",
            Some(&body),
            audit_log_reason,
        )
        .await
    }
}

fn snowflakes(values: &[String]) -> Vec<generated_types::SnowflakeType> {
    values.iter().map(|value| snowflake(value)).collect()
}
