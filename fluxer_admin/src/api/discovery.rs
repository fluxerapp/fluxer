// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::float_to_u64;

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{DiscoveryApplicationResponse, DiscoveryGuild};

impl AdminApiClient {
    pub async fn list_pending_discovery_applications(&self) -> ApiResult<Vec<DiscoveryGuild>> {
        let response = self
            .generated()
            .list_admin_discovery_applications()
            .await
            .map_err(|e| self.generated_error(e))?;
        Vec::from(response.into_inner())
            .into_iter()
            .map(pending_discovery_application)
            .collect()
    }

    pub async fn list_discovery_listed_guilds(&self) -> ApiResult<Vec<DiscoveryGuild>> {
        let response = self
            .generated()
            .list_admin_discovery_listings()
            .await
            .map_err(|e| self.generated_error(e))?;
        Vec::from(response.into_inner())
            .into_iter()
            .map(listed_guild)
            .collect()
    }

    pub async fn approve_discovery_application(
        &self,
        guild_id: &str,
        reason: Option<&str>,
    ) -> ApiResult<DiscoveryApplicationResponse> {
        let guild_id = snowflake(guild_id);
        let body = generated_types::DiscoveryAdminApplicationUpdateRequest::Approved {
            reason: reason
                .map(generated_types::DiscoveryReviewReason::try_from)
                .transpose()
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .update_admin_discovery_application(&guild_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn reject_discovery_application(
        &self,
        guild_id: &str,
        reason: &str,
    ) -> ApiResult<DiscoveryApplicationResponse> {
        let guild_id = snowflake(guild_id);
        let body = generated_types::DiscoveryAdminApplicationUpdateRequest::Rejected {
            reason: generated_types::DiscoveryRejectionReason::try_from(reason)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .update_admin_discovery_application(&guild_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn remove_from_discovery(
        &self,
        guild_id: &str,
        reason: &str,
    ) -> ApiResult<DiscoveryApplicationResponse> {
        let guild_id = snowflake(guild_id);
        let body = generated_types::DiscoveryAdminRemoveRequest {
            reason: generated_types::DiscoveryAdminRemoveRequestReason::try_from(reason)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .delete_admin_discovery_listing(&guild_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

fn pending_discovery_application(
    app: generated_types::DiscoveryAdminPendingApplicationResponse,
) -> ApiResult<DiscoveryGuild> {
    Ok(DiscoveryGuild {
        guild_id: String::from(app.guild_id),
        guild_name: app.guild_name,
        guild_icon: app.guild_icon,
        guild_owner_id: String::from(app.guild_owner_id),
        guild_owner_username: app.guild_owner_username,
        guild_owner_global_name: app.guild_owner_global_name,
        guild_owner_discriminator: app.guild_owner_discriminator,
        guild_member_count: float_to_u64(app.guild_member_count, "guild_member_count")?,
        guild_nsfw_level: app.guild_nsfw_level.map(i32::from),
        description: app.description,
        category_type: float_to_i32(app.category_type, "category_type")?,
        primary_language: app.primary_language,
        custom_tags: app.custom_tags,
        applied_at: app.applied_at,
        approved_at: None,
    })
}

fn listed_guild(
    guild: generated_types::DiscoveryAdminListedGuildResponse,
) -> ApiResult<DiscoveryGuild> {
    Ok(DiscoveryGuild {
        guild_id: String::from(guild.guild_id),
        guild_name: guild.guild_name,
        guild_icon: guild.guild_icon,
        guild_owner_id: String::from(guild.guild_owner_id),
        guild_owner_username: guild.guild_owner_username,
        guild_owner_global_name: guild.guild_owner_global_name,
        guild_owner_discriminator: guild.guild_owner_discriminator,
        guild_member_count: float_to_u64(guild.guild_member_count, "guild_member_count")?,
        guild_nsfw_level: guild.guild_nsfw_level.map(i32::from),
        description: guild.description,
        category_type: float_to_i32(guild.category_type, "category_type")?,
        primary_language: guild.primary_language,
        custom_tags: guild.custom_tags,
        applied_at: guild.applied_at,
        approved_at: guild.approved_at,
    })
}

fn float_to_i32(value: f64, field: &str) -> ApiResult<i32> {
    let parsed = crate::api::generated::number_to_u64(value, field).map_err(ApiError::Parse)?;
    i32::try_from(parsed).map_err(|_| ApiError::Parse(format!("{field} is out of range: {value}")))
}
