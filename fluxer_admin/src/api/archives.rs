// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{Archive, ArchiveDownloadUrlResponse, ListArchivesResponse};

impl AdminApiClient {
    pub async fn trigger_user_archive(
        &self,
        user_id: &str,
        include_attachments: bool,
    ) -> ApiResult<Archive> {
        let body = generated_types::AdminArchiveCreateRequest {
            include_attachments: include_attachments.then_some(true),
        };
        let response = self
            .generated()
            .create_admin_user_archive(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn trigger_guild_archive(
        &self,
        guild_id: &str,
        include_attachments: bool,
    ) -> ApiResult<Archive> {
        let body = generated_types::AdminArchiveCreateRequest {
            include_attachments: include_attachments.then_some(true),
        };
        let response = self
            .generated()
            .create_admin_guild_archive(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_archives(
        &self,
        subject_type: &str,
        subject_id: Option<&str>,
        include_expired: bool,
        requested_by: Option<&str>,
    ) -> ApiResult<ListArchivesResponse> {
        let subject_id = subject_id.filter(|id| !id.is_empty());
        let search_every_subject_type = subject_type == "all" && subject_id.is_some();
        let subject_types: &[&str] = if search_every_subject_type {
            &["user", "guild"]
        } else {
            std::slice::from_ref(&subject_type)
        };
        let mut archives = Vec::new();
        for &subject_type in subject_types {
            let query_params = [
                ("subject_type", subject_type),
                ("subject_id", subject_id.unwrap_or_default()),
                ("requested_by", requested_by.unwrap_or_default()),
                (
                    "include_expired",
                    if include_expired { "true" } else { "false" },
                ),
            ];
            match self.get("/admin/archives", Some(&query_params)).await {
                Ok(ListArchivesResponse { archives: page }) => archives.extend(page),
                Err(ApiError::Http { status: 403, .. }) if search_every_subject_type => {}
                Err(error) => return Err(error),
            }
        }
        Ok(ListArchivesResponse { archives })
    }

    pub async fn get_archive_download_url(
        &self,
        subject_type: &str,
        subject_id: &str,
        archive_id: &str,
    ) -> ApiResult<ArchiveDownloadUrlResponse> {
        let response = self
            .generated()
            .get_admin_archive_download(subject_type, subject_id, archive_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}
