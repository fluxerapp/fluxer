// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::snowflake;

use super::client::{AdminApiClient, ApiResult};
use super::types::ListGuildThreadsResponse;

impl AdminApiClient {
    pub async fn list_guild_threads(&self, guild_id: &str) -> ApiResult<ListGuildThreadsResponse> {
        let response = self
            .generated()
            .list_admin_guild_threads(&snowflake(guild_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_thread_channel(&self, channel_id: &str) -> ApiResult<()> {
        self.generated()
            .delete_admin_thread_channel(&snowflake(channel_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }
}
