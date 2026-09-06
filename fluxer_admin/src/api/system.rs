// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    GatewayVoiceStateCountsResponse, GuildMemoryStatsResponse, NodeStatsResponse,
    ReloadAllGuildsResponse,
};

impl AdminApiClient {
    pub async fn get_guild_memory_stats(&self, limit: u32) -> ApiResult<GuildMemoryStatsResponse> {
        let limit = limit.to_string();
        let response = self
            .generated()
            .get_admin_gateway_memory_stats(Some(limit.as_str()))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn reload_all_guilds(
        &self,
        guild_ids: &[String],
    ) -> ApiResult<ReloadAllGuildsResponse> {
        let body = generated_types::ReloadGuildsRequest {
            guild_ids: guild_ids
                .iter()
                .cloned()
                .map(generated_types::SnowflakeType::from)
                .collect(),
        };
        let response = self
            .generated()
            .create_admin_gateway_reload(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_node_stats(&self) -> ApiResult<NodeStatsResponse> {
        let response = self
            .generated()
            .get_admin_gateway_stats()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_gateway_voice_state_counts(
        &self,
    ) -> ApiResult<GatewayVoiceStateCountsResponse> {
        let response = self
            .generated()
            .get_admin_gateway_voice_state_counts()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
