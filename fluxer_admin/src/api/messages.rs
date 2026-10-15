// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::Method;

use crate::api::generated::{snowflake, types as generated_types};

use crate::templates::components::enforcement_fields::BasisSubmission;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    BrowseChannelResponse, LookupMessageResponse, MessageShredStatusResponse,
    SearchChannelMessagesResponse,
};

impl AdminApiClient {
    pub async fn delete_message(
        &self,
        channel_id: &str,
        message_id: &str,
        basis: generated_types::MessageDeletionBasis,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let body = generated_types::AdminMessageDeleteRequest { basis };
        let _: serde_json::Value = self
            .send(
                Method::DELETE,
                &format!(
                    "/admin/channels/{}/messages/{}",
                    urlencoding::encode(channel_id),
                    urlencoding::encode(message_id)
                ),
                Some(&body),
                audit_log_reason,
            )
            .await?;
        Ok(())
    }

    pub async fn lookup_message(
        &self,
        channel_id: &str,
        message_id: &str,
        context_limit: u32,
    ) -> ApiResult<LookupMessageResponse> {
        let context_limit = context_limit.to_string();
        let response = self
            .generated()
            .get_admin_message(
                &snowflake(channel_id),
                &snowflake(message_id),
                Some(context_limit.as_str()),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn queue_message_shred(
        &self,
        user_id: &str,
        entries: Vec<generated_types::AdminUserMessageShredRequestEntriesItem>,
        basis: &BasisSubmission,
    ) -> ApiResult<generated_types::MessageShredResponse> {
        let body = generated_types::AdminUserMessageShredRequest {
            basis: basis.moderation_basis(),
            entries,
        };
        self.send(
            Method::POST,
            &format!(
                "/admin/users/{}/message-shreds",
                urlencoding::encode(user_id)
            ),
            Some(&body),
            basis.internal_note(),
        )
        .await
    }

    pub async fn delete_all_user_messages(
        &self,
        user_id: &str,
        basis: Option<&BasisSubmission>,
    ) -> ApiResult<generated_types::DeleteAllUserMessagesResponse> {
        let body = generated_types::AdminUserMessageDeleteRequest {
            basis: basis.map(BasisSubmission::moderation_basis),
        };
        let dry_run = if basis.is_some() { "false" } else { "true" };
        self.send(
            Method::DELETE,
            &format!(
                "/admin/users/{}/messages?dry_run={dry_run}",
                urlencoding::encode(user_id)
            ),
            Some(&body),
            basis.and_then(BasisSubmission::internal_note),
        )
        .await
    }

    pub async fn get_message_shred_status(
        &self,
        job_id: &str,
    ) -> ApiResult<MessageShredStatusResponse> {
        let response = self
            .generated()
            .get_admin_message_shred(&snowflake(job_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn lookup_message_by_attachment(
        &self,
        channel_id: &str,
        attachment_id: &str,
        filename: &str,
        context_limit: u32,
    ) -> ApiResult<LookupMessageResponse> {
        let context_limit = context_limit.to_string();
        let filename = generated_types::SearchAdminMessagesFilename::try_from(filename)
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .search_admin_messages(
                Some(&snowflake(attachment_id)),
                &snowflake(channel_id),
                Some(context_limit.as_str()),
                Some(&filename),
                None,
                None,
                None,
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn browse_channel(
        &self,
        channel_id: &str,
        before: Option<&str>,
        after: Option<&str>,
        limit: Option<u32>,
    ) -> ApiResult<BrowseChannelResponse> {
        let after = after.map(snowflake);
        let before = before.map(snowflake);
        let limit = limit.map(|value| value.to_string());
        let response = self
            .generated()
            .list_admin_channel_messages(
                &snowflake(channel_id),
                after.as_ref(),
                before.as_ref(),
                limit.as_deref(),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn search_channel_messages(
        &self,
        channel_id: &str,
        query: &str,
        limit: Option<u32>,
    ) -> ApiResult<SearchChannelMessagesResponse> {
        let limit = limit.map(|value| value.to_string());
        let response = self
            .generated()
            .search_admin_messages(
                None,
                &snowflake(channel_id),
                None,
                None,
                limit.as_deref(),
                None,
                Some(query),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
