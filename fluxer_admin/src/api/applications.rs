// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiResult};
use super::types::{Application, ApplicationUpdateResponse, LookupApplicationResponse};
use serde::Serialize;

#[derive(Serialize)]
struct TransferApplicationOwnershipRequest<'a> {
    new_owner_id: &'a str,
}

impl AdminApiClient {
    pub async fn lookup_application(&self, application_id: &str) -> ApiResult<Option<Application>> {
        let resp: LookupApplicationResponse = self
            .get(
                &format!(
                    "/admin/applications/{}",
                    urlencoding::encode(application_id)
                ),
                None,
            )
            .await?;
        Ok(resp.application)
    }

    pub async fn list_user_applications(&self, user_id: &str) -> ApiResult<Vec<Application>> {
        let query_params = [("owner_id", user_id)];
        let resp: super::types::ListUserApplicationsResponse =
            self.get("/admin/applications", Some(&query_params)).await?;
        Ok(resp.applications)
    }

    pub async fn list_guild_applications(&self, guild_id: &str) -> ApiResult<Vec<Application>> {
        let query_params = [("guild_id", guild_id)];
        let resp: super::types::ListUserApplicationsResponse =
            self.get("/admin/applications", Some(&query_params)).await?;
        Ok(resp.applications)
    }

    pub async fn transfer_application_ownership(
        &self,
        application_id: &str,
        new_owner_id: &str,
    ) -> ApiResult<ApplicationUpdateResponse> {
        let body = TransferApplicationOwnershipRequest { new_owner_id };
        self.patch_typed_with_reason(
            &format!(
                "/admin/applications/{}",
                urlencoding::encode(application_id)
            ),
            &body,
            None,
        )
        .await
    }
}
