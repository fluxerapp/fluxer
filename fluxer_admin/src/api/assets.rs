// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiResult};

impl AdminApiClient {
    pub async fn purge_assets(
        &self,
        guild_id: &str,
        ids: &[String],
    ) -> ApiResult<serde_json::Value> {
        let body = generated_types::PurgeGuildAssetsRequest { ids: ids.to_vec() };
        let response = self
            .generated()
            .purge_admin_guild_assets(
                &generated_types::SnowflakeType::from(guild_id.to_owned()),
                &body,
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
