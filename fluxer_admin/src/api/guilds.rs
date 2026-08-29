// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;
use serde::Deserialize;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    GuildAuditLogResponse, GuildDetailInfo, GuildInfo, GuildUpdateResponse,
    ListGuildMembersResponse, LookupGuildResponse, SearchGuildsResponse, SearchReportsResponse,
    SuccessResponse,
};

impl AdminApiClient {
    pub async fn search_guilds(
        &self,
        query: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchGuildsResponse> {
        let body = generated_types::SearchGuildsRequest {
            limit: Some(
                crate::api::generated::nonzero_u32(limit, "limit").map_err(ApiError::Parse)?,
            ),
            offset: Some(i64::from(offset)),
            query: Some(query.to_owned()),
        };
        let response = self
            .generated()
            .search_guilds(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        search_guilds_response(response.into_inner())
    }

    pub async fn get_guild_by_id(&self, guild_id: &str) -> ApiResult<GuildInfo> {
        let body = generated_types::LookupGuildRequest {
            guild_id: snowflake(guild_id),
        };
        let response = self
            .generated()
            .lookup_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupGuildResponse = self.generated_value(response.into_inner())?;
        resp.guild
            .map(GuildInfo::from)
            .ok_or_else(|| super::client::ApiError::Http {
                status: 404,
                message: "Guild not found".to_owned(),
            })
    }

    pub async fn lookup_guild(&self, guild_id: &str) -> ApiResult<Option<GuildDetailInfo>> {
        let body = generated_types::LookupGuildRequest {
            guild_id: snowflake(guild_id),
        };
        let response = self
            .generated()
            .lookup_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupGuildResponse = self.generated_value(response.into_inner())?;
        Ok(resp.guild)
    }

    pub async fn update_guild_features(
        &self,
        guild_id: &str,
        add_features: &[String],
        remove_features: &[String],
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::UpdateGuildFeaturesRequest {
            add_features: guild_features(add_features),
            guild_id: snowflake(guild_id),
            remove_features: guild_features(remove_features),
        };
        let response = self
            .generated()
            .update_guild_features(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let body = generated_types::DeleteGuildRequest {
            guild_id: snowflake(guild_id),
        };
        let response = self
            .generated()
            .admin_delete_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn transfer_guild_ownership(
        &self,
        guild_id: &str,
        new_owner_id: &str,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::TransferGuildOwnershipRequest {
            guild_id: snowflake(guild_id),
            new_owner_id: snowflake(new_owner_id),
        };
        let response = self
            .generated()
            .admin_transfer_guild_ownership(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_guild_members(
        &self,
        guild_id: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<ListGuildMembersResponse> {
        let body = generated_types::ListGuildMembersRequest {
            guild_id: snowflake(guild_id),
            limit: Some(
                crate::api::generated::nonzero_u32(limit, "limit").map_err(ApiError::Parse)?,
            ),
            offset: Some(i64::from(offset)),
        };
        let response = self
            .generated()
            .admin_list_guild_members(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn ban_guild_member(&self, guild_id: &str, user_id: &str) -> ApiResult<()> {
        let body = generated_types::BanGuildMemberRequest {
            ban_duration_seconds: None,
            delete_message_days: None,
            delete_message_seconds: None,
            guild_id: snowflake(guild_id),
            reason: None,
            user_id: snowflake(user_id),
        };
        self.generated()
            .admin_ban_guild_member(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn kick_guild_member(&self, guild_id: &str, user_id: &str) -> ApiResult<()> {
        let body = generated_types::KickGuildMemberRequest {
            guild_id: snowflake(guild_id),
            user_id: snowflake(user_id),
        };
        self.generated()
            .kick_guild_member(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn list_guild_audit_logs(
        &self,
        guild_id: &str,
        limit: Option<u32>,
        before: Option<&str>,
    ) -> ApiResult<GuildAuditLogResponse> {
        let body = generated_types::ListGuildAuditLogsRequest {
            action_type: None,
            after: None,
            before: before.map(snowflake),
            guild_id: snowflake(guild_id),
            limit: limit
                .map(i32::try_from)
                .transpose()
                .map_err(|e| ApiError::Parse(e.to_string()))?
                .map(generated_types::Int32Type::from),
            user_id: None,
        };
        let response = self
            .generated()
            .list_guild_audit_logs_admin(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn clear_guild_fields(&self, guild_id: &str, fields: &[String]) -> ApiResult<()> {
        let fields = fields
            .iter()
            .map(generated_types::ClearGuildFieldsRequestFieldsItem::try_from)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::ClearGuildFieldsRequest {
            fields,
            guild_id: snowflake(guild_id),
        };
        self.generated()
            .clear_guild_fields(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn update_guild_settings(
        &self,
        guild_id: &str,
        settings: &serde_json::Value,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = guild_settings_request(guild_id, settings)?;
        let response = self
            .generated()
            .update_guild_settings(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        guild_update_response(response.into_inner())
    }

    pub async fn update_guild_name(
        &self,
        guild_id: &str,
        name: &str,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::UpdateGuildNameRequest {
            guild_id: snowflake(guild_id),
            name: name.to_owned(),
        };
        let response = self
            .generated()
            .update_guild_name(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_guild_vanity(
        &self,
        guild_id: &str,
        vanity: Option<&str>,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::UpdateGuildVanityRequest {
            guild_id: snowflake(guild_id),
            vanity_url_code: vanity.map(std::borrow::ToOwned::to_owned),
        };
        let response = self
            .generated()
            .update_guild_vanity(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn reload_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let body = generated_types::ReloadGuildRequest {
            guild_id: snowflake(guild_id),
        };
        let response = self
            .generated()
            .reload_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn shutdown_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let body = generated_types::ShutdownGuildRequest {
            guild_id: snowflake(guild_id),
        };
        let response = self
            .generated()
            .shutdown_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn force_add_user_to_guild(
        &self,
        user_id: &str,
        guild_id: &str,
    ) -> ApiResult<SuccessResponse> {
        let body = generated_types::ForceAddUserToGuildRequest {
            guild_id: snowflake(guild_id),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .force_add_user_to_guild(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn search_reports_by_guild(
        &self,
        guild_id: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchReportsResponse> {
        let body = generated_types::SearchReportsRequest {
            category: None,
            guild_context_id: None,
            limit: Some(
                crate::api::generated::nonzero_u32(limit, "limit").map_err(ApiError::Parse)?,
            ),
            offset: Some(i64::from(offset)),
            query: None,
            report_type: None,
            reported_channel_id: None,
            reported_guild_id: Some(snowflake(guild_id)),
            reported_user_id: None,
            reporter_id: None,
            resolved_by_admin_id: None,
            sort_by: None,
            sort_order: None,
            status: None,
        };
        let response = self
            .generated()
            .search_reports(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let response = response.into_inner();
        Ok(SearchReportsResponse {
            reports: self.generated_value(response.reports)?,
            total: crate::api::generated::number_to_u64(response.total, "total")
                .map_err(ApiError::Parse)?,
            offset: crate::api::generated::number_to_u64(response.offset, "offset")
                .map_err(ApiError::Parse)?,
            limit: crate::api::generated::number_to_u64(response.limit, "limit")
                .map_err(ApiError::Parse)?,
        })
    }
}

#[derive(Deserialize)]
struct GuildSettingsPatch {
    content_warning_level: Option<generated_types::ContentWarningLevel>,
    content_warning_text: Option<String>,
    default_message_notifications: Option<generated_types::DefaultMessageNotifications>,
    disabled_operations: Option<generated_types::GuildOperations>,
    explicit_content_filter: Option<generated_types::GuildExplicitContentFilter>,
    mfa_level: Option<generated_types::GuildMfaLevel>,
    nsfw: Option<bool>,
    nsfw_level: Option<generated_types::NsfwLevel>,
    verification_level: Option<generated_types::GuildVerificationLevel>,
}

fn search_guilds_response(
    response: generated_types::SearchGuildsResponse,
) -> ApiResult<SearchGuildsResponse> {
    Ok(SearchGuildsResponse {
        guilds: response
            .guilds
            .into_iter()
            .map(guild_admin_response)
            .collect::<ApiResult<Vec<_>>>()?,
        total: crate::api::generated::number_to_u64(response.total, "total")
            .map_err(ApiError::Parse)?,
    })
}

fn guild_admin_response(response: generated_types::GuildAdminResponse) -> ApiResult<GuildInfo> {
    Ok(GuildInfo {
        id: String::from(response.id),
        name: response.name,
        icon: response.icon,
        banner: response.banner,
        owner_id: String::from(response.owner_id),
        owner_username: response.owner_username,
        owner_global_name: response.owner_global_name,
        owner_discriminator: response.owner_discriminator,
        member_count: crate::api::generated::i64_to_u64(
            i64::from(response.member_count),
            "member_count",
        )
        .map_err(ApiError::Parse)?,
        features: response.features.into_iter().map(String::from).collect(),
        nsfw_level: response.nsfw_level.map(i32::from),
        nsfw: response.nsfw,
        content_warning_level: response.content_warning_level.map(i32::from),
        content_warning_text: response.content_warning_text,
        description: None,
        vanity_url_code: None,
    })
}

fn guild_update_response(
    response: generated_types::GuildUpdateResponse,
) -> ApiResult<GuildUpdateResponse> {
    let guild = response.guild;
    Ok(GuildUpdateResponse {
        guild: GuildInfo {
            id: String::from(guild.id),
            name: guild.name,
            icon: guild.icon,
            banner: guild.banner,
            owner_id: String::from(guild.owner_id),
            owner_username: None,
            owner_global_name: None,
            owner_discriminator: None,
            member_count: crate::api::generated::i64_to_u64(
                i64::from(i32::from(guild.member_count)),
                "member_count",
            )
            .map_err(ApiError::Parse)?,
            features: guild.features,
            nsfw_level: guild.nsfw_level.map(i32::from),
            nsfw: guild.nsfw,
            content_warning_level: guild.content_warning_level.map(i32::from),
            content_warning_text: guild.content_warning_text,
            description: None,
            vanity_url_code: None,
        },
    })
}

fn guild_settings_request(
    guild_id: &str,
    settings: &serde_json::Value,
) -> ApiResult<generated_types::UpdateGuildSettingsRequest> {
    let patch = serde_json::from_value::<GuildSettingsPatch>(settings.clone())
        .map_err(|e| ApiError::Parse(e.to_string()))?;
    Ok(generated_types::UpdateGuildSettingsRequest {
        content_warning_level: patch.content_warning_level,
        content_warning_text: patch.content_warning_text,
        default_message_notifications: patch.default_message_notifications,
        disabled_operations: patch.disabled_operations,
        explicit_content_filter: patch.explicit_content_filter,
        guild_id: snowflake(guild_id),
        mfa_level: patch.mfa_level,
        nsfw: patch.nsfw,
        nsfw_level: patch.nsfw_level,
        verification_level: patch.verification_level,
    })
}

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}

fn guild_features(values: &[String]) -> Vec<generated_types::GuildFeatureSchema> {
    values
        .iter()
        .cloned()
        .map(generated_types::GuildFeatureSchema::from)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guild_settings_adapter_preserves_dynamic_patch_fields() {
        let settings = serde_json::json!({
            "disabled_operations": 5,
            "nsfw": true,
            "verification_level": 2,
        });
        let request = guild_settings_request("123", &settings).unwrap();
        let json = serde_json::to_value(request).unwrap();
        assert_eq!(json["guild_id"], "123");
        assert_eq!(json["disabled_operations"], 5);
        assert_eq!(json["nsfw"], true);
        assert_eq!(json["verification_level"], 2);
    }

    #[test]
    fn guild_search_adapter_rejects_lossy_totals() {
        let response = generated_types::SearchGuildsResponse {
            guilds: Vec::new(),
            total: 1.5,
        };
        assert!(search_guilds_response(response).is_err());
    }
}
