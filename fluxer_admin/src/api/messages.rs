// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    BrowseChannelResponse, DeleteAllUserMessagesResponse, LookupMessageResponse,
    MessageShredResponse, MessageShredStatusResponse, NcmecAttachmentSubmitResult,
    SearchChannelMessagesResponse,
};

impl AdminApiClient {
    pub async fn delete_message(
        &self,
        channel_id: &str,
        message_id: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let _: serde_json::Value = self
            .delete_with_reason(
                &format!(
                    "/admin/channels/{}/messages/{}",
                    urlencoding::encode(channel_id),
                    urlencoding::encode(message_id)
                ),
                None,
                audit_log_reason,
            )
            .await?;
        Ok(())
    }

    pub async fn report_attachment_to_ncmec(
        &self,
        channel_id: &str,
        message_id: &str,
        attachment_id: &str,
        filename: &str,
        reporter_full_name: &str,
        source_report_id: Option<&str>,
    ) -> ApiResult<NcmecAttachmentSubmitResult> {
        let body = generated_types::ReportAttachmentToNcmecRequest {
            attachment_id: snowflake(attachment_id),
            channel_id: snowflake(channel_id),
            confirmed_viewed: true,
            filename: filename.to_owned(),
            message_id: snowflake(message_id),
            reporter_full_name:
                generated_types::ReportAttachmentToNcmecRequestReporterFullName::try_from(
                    reporter_full_name,
                )
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            source_report_id: source_report_id.map(snowflake),
        };
        let response = self
            .generated()
            .create_admin_ncmec_report(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
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
        entries: &[serde_json::Value],
    ) -> ApiResult<MessageShredResponse> {
        let entries = entries
            .iter()
            .cloned()
            .map(serde_json::from_value::<generated_types::AdminUserMessageShredRequestEntriesItem>)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::AdminUserMessageShredRequest { entries };
        let response = self
            .generated()
            .shred_admin_user_messages(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_all_user_messages(
        &self,
        user_id: &str,
        dry_run: bool,
    ) -> ApiResult<DeleteAllUserMessagesResponse> {
        let dry_run = if dry_run { "true" } else { "false" };
        let response = self
            .generated()
            .delete_admin_user_messages(&snowflake(user_id), Some(dry_run))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_message_shred_status(
        &self,
        job_id: &str,
    ) -> ApiResult<MessageShredStatusResponse> {
        let response = self
            .generated()
            .get_admin_message_shred(job_id)
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
        let response = self
            .generated()
            .search_admin_messages(
                Some(&snowflake(attachment_id)),
                &snowflake(channel_id),
                Some(context_limit.as_str()),
                Some(filename),
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

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}
