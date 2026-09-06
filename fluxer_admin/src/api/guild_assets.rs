// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiResult};
use super::types::{ListGuildEmojisResponse, ListGuildStickersResponse};

impl AdminApiClient {
    pub async fn list_guild_emojis(&self, guild_id: &str) -> ApiResult<ListGuildEmojisResponse> {
        let guild_id = generated_types::SnowflakeType::from(guild_id.to_owned());
        let response = self
            .generated()
            .list_admin_guild_emojis(&guild_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_guild_stickers(
        &self,
        guild_id: &str,
    ) -> ApiResult<ListGuildStickersResponse> {
        let guild_id = generated_types::SnowflakeType::from(guild_id.to_owned());
        let response = self
            .generated()
            .list_admin_guild_stickers(&guild_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
