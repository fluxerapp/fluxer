// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    DiscoveryApplicationResponse, DiscoveryListedGuild, DiscoveryPendingApplication,
};

impl AdminApiClient {
    pub async fn list_pending_discovery_applications(
        &self,
    ) -> ApiResult<Vec<DiscoveryPendingApplication>> {
        let response = self
            .generated()
            .list_admin_discovery_applications()
            .await
            .map_err(|e| self.generated_error(e))?;
        response
            .into_inner()
            .into_iter()
            .map(pending_discovery_application)
            .collect()
    }

    pub async fn list_discovery_listed_guilds(&self) -> ApiResult<Vec<DiscoveryListedGuild>> {
        let response = self
            .generated()
            .list_admin_discovery_listings()
            .await
            .map_err(|e| self.generated_error(e))?;
        response
            .into_inner()
            .into_iter()
            .map(listed_guild)
            .collect()
    }

    pub async fn approve_discovery_application(
        &self,
        guild_id: &str,
        reason: Option<&str>,
    ) -> ApiResult<DiscoveryApplicationResponse> {
        let guild_id = generated_types::SnowflakeType::from(guild_id.to_owned());
        let body = generated_types::DiscoveryAdminApplicationUpdateRequest::from(
            generated_types::ApprovedDiscoveryAdminApplicationUpdateRequest {
                reason: reason
                    .map(generated_types::ApprovedDiscoveryAdminApplicationUpdateRequestReason::try_from)
                    .transpose()
                    .map_err(|e| ApiError::Parse(e.to_string()))?,
                status: generated_types::ApprovedDiscoveryAdminApplicationUpdateRequestStatus::Approved,
            },
        );
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
        let guild_id = generated_types::SnowflakeType::from(guild_id.to_owned());
        let body = generated_types::DiscoveryAdminApplicationUpdateRequest::from(
            generated_types::RejectedDiscoveryAdminApplicationUpdateRequest {
                reason:
                    generated_types::RejectedDiscoveryAdminApplicationUpdateRequestReason::try_from(
                        reason,
                    )
                    .map_err(|e| ApiError::Parse(e.to_string()))?,
                status:
                    generated_types::RejectedDiscoveryAdminApplicationUpdateRequestStatus::Rejected,
            },
        );
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
        let guild_id = generated_types::SnowflakeType::from(guild_id.to_owned());
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
) -> ApiResult<DiscoveryPendingApplication> {
    Ok(DiscoveryPendingApplication {
        guild_id: String::from(app.guild_id),
        guild_name: Some(app.guild_name),
        guild_icon: app.guild_icon,
        guild_owner_id: Some(String::from(app.guild_owner_id)),
        guild_owner_username: app.guild_owner_username,
        guild_owner_global_name: app.guild_owner_global_name,
        guild_owner_discriminator: app.guild_owner_discriminator,
        guild_member_count: Some(float_to_u64(app.guild_member_count, "guild_member_count")?),
        guild_nsfw_level: app.guild_nsfw_level.map(i32::from),
        guild_features: app.guild_features,
        description: Some(app.description),
        category_type: Some(float_to_i32(app.category_type, "category_type")?),
        primary_language: app.primary_language,
        custom_tags: app.custom_tags,
        member_count: None,
        applied_at: Some(app.applied_at),
        category: None,
    })
}

fn listed_guild(
    guild: generated_types::DiscoveryAdminListedGuildResponse,
) -> ApiResult<DiscoveryListedGuild> {
    Ok(DiscoveryListedGuild {
        guild_id: String::from(guild.guild_id),
        guild_name: Some(guild.guild_name),
        guild_icon: guild.guild_icon,
        guild_owner_id: Some(String::from(guild.guild_owner_id)),
        guild_owner_username: guild.guild_owner_username,
        guild_owner_global_name: guild.guild_owner_global_name,
        guild_owner_discriminator: guild.guild_owner_discriminator,
        guild_member_count: Some(float_to_u64(
            guild.guild_member_count,
            "guild_member_count",
        )?),
        guild_nsfw_level: guild.guild_nsfw_level.map(i32::from),
        guild_features: guild.guild_features,
        description: Some(guild.description),
        category_type: Some(float_to_i32(guild.category_type, "category_type")?),
        primary_language: guild.primary_language,
        custom_tags: guild.custom_tags,
        member_count: None,
        applied_at: Some(guild.applied_at),
        approved_at: guild.approved_at,
        listed_at: None,
        category: None,
    })
}

fn float_to_u64(value: f64, field: &str) -> ApiResult<u64> {
    crate::api::generated::number_to_u64(value, field).map_err(ApiError::Parse)
}

fn float_to_i32(value: f64, field: &str) -> ApiResult<i32> {
    let parsed = crate::api::generated::number_to_u64(value, field).map_err(ApiError::Parse)?;
    i32::try_from(parsed).map_err(|_| ApiError::Parse(format!("{field} is out of range: {value}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_lossy_discovery_numeric_fields() {
        assert!(float_to_u64(10.5, "guild_member_count").is_err());
        assert!(float_to_i32(9_999_999_999.0, "category_type").is_err());
    }
}
