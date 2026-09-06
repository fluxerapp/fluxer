// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{CreateAdminApiKeyResponse, ListAdminApiKeyEntry};

impl AdminApiClient {
    pub async fn create_api_key(
        &self,
        name: &str,
        acls: &[String],
    ) -> ApiResult<CreateAdminApiKeyResponse> {
        let body = generated_types::CreateAdminApiKeyRequest {
            acls: parse_acls(acls)?,
            expires_in_days: None,
            name: generated_types::CreateAdminApiKeyRequestName::try_from(name)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .create_admin_api_key(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_api_keys(&self) -> ApiResult<Vec<ListAdminApiKeyEntry>> {
        let response = self
            .generated()
            .list_admin_api_keys()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn revoke_api_key(&self, key_id: &str) -> ApiResult<()> {
        let key_id = generated_types::SnowflakeType::from(key_id.to_owned());
        self.generated()
            .delete_admin_api_key(&key_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }
}

pub(super) fn parse_acls(acls: &[String]) -> ApiResult<Vec<generated_types::AdminAclType>> {
    acls.iter()
        .map(|acl| {
            generated_types::AdminAclType::try_from(acl.as_str())
                .map_err(|e| ApiError::Parse(e.to_string()))
        })
        .collect()
}
