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
        let body = generated_types::DeleteMessageRequest {
            channel_id: snowflake(channel_id),
            message_id: snowflake(message_id),
        };
        let _: serde_json::Value = self
            .post_typed_with_reason("/admin/messages/delete", &body, audit_log_reason)
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
            .report_message_attachment_to_ncmec(&body)
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
        let body = generated_types::LookupMessageRequest {
            channel_id: snowflake(channel_id),
            context_limit: Some(
                crate::api::generated::nonzero_u32(context_limit, "context_limit")
                    .map_err(ApiError::Parse)?,
            ),
            message_id: snowflake(message_id),
        };
        let response = self
            .generated()
            .lookup_message(&body)
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
            .map(serde_json::from_value::<generated_types::MessageShredRequestEntriesItem>)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::MessageShredRequest {
            entries,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .queue_message_shred(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_all_user_messages(
        &self,
        user_id: &str,
        dry_run: bool,
    ) -> ApiResult<DeleteAllUserMessagesResponse> {
        let body = generated_types::DeleteAllUserMessagesRequest {
            dry_run: Some(dry_run),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .delete_all_user_messages(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_message_shred_status(
        &self,
        job_id: &str,
    ) -> ApiResult<MessageShredStatusResponse> {
        let body = generated_types::MessageShredStatusRequest {
            job_id: job_id.to_owned(),
        };
        let response = self
            .generated()
            .get_message_shred_status(&body)
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
        let body = generated_types::LookupMessageByAttachmentRequest {
            attachment_id: snowflake(attachment_id),
            channel_id: snowflake(channel_id),
            context_limit: Some(
                crate::api::generated::nonzero_u32(context_limit, "context_limit")
                    .map_err(ApiError::Parse)?,
            ),
            filename: filename.to_owned(),
        };
        let response = self
            .generated()
            .lookup_message_by_attachment(&body)
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
        let body = generated_types::BrowseChannelRequest {
            after: after.map(snowflake),
            before: before.map(snowflake),
            channel_id: snowflake(channel_id),
            limit: limit
                .map(|value| crate::api::generated::nonzero_u32(value, "limit"))
                .transpose()
                .map_err(ApiError::Parse)?,
        };
        let response = self
            .generated()
            .browse_channel_messages(&body)
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
        let body = generated_types::SearchChannelMessagesRequest {
            channel_id: snowflake(channel_id),
            limit: limit
                .map(|value| crate::api::generated::nonzero_u32(value, "limit"))
                .transpose()
                .map_err(ApiError::Parse)?,
            query: generated_types::SearchChannelMessagesRequestQuery::try_from(query)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .search_channel_messages(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}
