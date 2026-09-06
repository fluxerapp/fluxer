// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

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
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::UpdateUserFlagsAdminBulkJobCreateRequest {
                add_flags: user_flags(add_flags),
                remove_flags: user_flags(remove_flags),
                task:
                    generated_types::UpdateUserFlagsAdminBulkJobCreateRequestTask::UpdateUserFlags,
                user_ids: snowflakes(user_ids),
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }

    pub async fn bulk_update_suspicious_activity_flags(
        &self,
        user_ids: &[String],
        add_flags: &[String],
        remove_flags: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::UpdateSuspiciousActivityFlagsAdminBulkJobCreateRequest {
                add_flags: add_flags.to_vec(),
                remove_flags: remove_flags.to_vec(),
                task: generated_types::UpdateSuspiciousActivityFlagsAdminBulkJobCreateRequestTask::UpdateSuspiciousActivityFlags,
                user_ids: snowflakes(user_ids),
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }

    pub async fn bulk_update_guild_features(
        &self,
        guild_ids: &[String],
        add_features: &[String],
        remove_features: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::UpdateGuildFeaturesAdminBulkJobCreateRequest {
                add_features: guild_features(add_features),
                guild_ids: snowflakes(guild_ids),
                remove_features: guild_features(remove_features),
                task: generated_types::UpdateGuildFeaturesAdminBulkJobCreateRequestTask::UpdateGuildFeatures,
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }

    pub async fn bulk_add_guild_members(
        &self,
        guild_id: &str,
        user_ids: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::AddGuildMembersAdminBulkJobCreateRequest {
                guild_id: snowflake(guild_id),
                task:
                    generated_types::AddGuildMembersAdminBulkJobCreateRequestTask::AddGuildMembers,
                user_ids: snowflakes(user_ids),
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }

    pub async fn bulk_delete_user_messages(
        &self,
        user_ids: &[String],
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::DeleteUserMessagesAdminBulkJobCreateRequest {
                task:
                    generated_types::DeleteUserMessagesAdminBulkJobCreateRequestTask::DeleteUserMessages,
                user_ids: snowflakes(user_ids),
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }

    pub async fn bulk_schedule_user_deletion(
        &self,
        user_ids: &[String],
        reason_code: u32,
        days_until_deletion: u32,
        public_reason: Option<&str>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<BulkJobResponse> {
        let body = generated_types::AdminBulkJobCreateRequest::from(
            generated_types::ScheduleUserDeletionAdminBulkJobCreateRequest {
                days_until_deletion: Some(
                    crate::api::generated::nonzero_u32(days_until_deletion, "days_until_deletion")
                        .map_err(ApiError::Parse)?,
                ),
                public_reason: public_reason.map(std::borrow::ToOwned::to_owned),
                reason_code: crate::api::generated::deletion_reason_code(
                    i32::try_from(reason_code).map_err(|e| ApiError::Parse(e.to_string()))?,
                    "reason_code",
                )
                .map_err(ApiError::Parse)?,
                task: generated_types::ScheduleUserDeletionAdminBulkJobCreateRequestTask::ScheduleUserDeletion,
                user_ids: snowflakes(user_ids),
            },
        );
        self.post_typed_with_reason("/admin/bulk-jobs", &body, audit_log_reason)
            .await
    }
}

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}

fn snowflakes(values: &[String]) -> Vec<generated_types::SnowflakeType> {
    values
        .iter()
        .cloned()
        .map(generated_types::SnowflakeType::from)
        .collect()
}

fn user_flags(values: &[String]) -> Vec<generated_types::UserFlags> {
    values
        .iter()
        .cloned()
        .map(generated_types::UserFlags::from)
        .collect()
}

fn guild_features(values: &[String]) -> Vec<generated_types::GuildFeatureSchema> {
    values
        .iter()
        .cloned()
        .map(generated_types::GuildFeatureSchema::from)
        .collect()
}
