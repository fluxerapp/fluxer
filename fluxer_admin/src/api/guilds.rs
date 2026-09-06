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
        let limit = limit.to_string();
        let offset = offset.to_string();
        let response = self
            .generated()
            .list_admin_guilds(Some(limit.as_str()), Some(offset.as_str()), Some(query))
            .await
            .map_err(|e| self.generated_error(e))?;
        search_guilds_response(response.into_inner())
    }

    pub async fn get_guild_by_id(&self, guild_id: &str) -> ApiResult<GuildInfo> {
        let response = self
            .generated()
            .get_admin_guild(&snowflake(guild_id))
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
        let response = self
            .generated()
            .get_admin_guild(&snowflake(guild_id))
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
        let body = generated_types::UpdateGuildRequest {
            add_features: guild_features(add_features),
            remove_features: guild_features(remove_features),
            ..Default::default()
        };
        let response = self
            .generated()
            .update_admin_guild(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let response = self
            .generated()
            .delete_admin_guild(&snowflake(guild_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn transfer_guild_ownership(
        &self,
        guild_id: &str,
        new_owner_id: &str,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::UpdateGuildRequest {
            new_owner_id: Some(snowflake(new_owner_id)),
            ..Default::default()
        };
        let response = self
            .generated()
            .update_admin_guild(&snowflake(guild_id), &body)
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
        let limit = limit.to_string();
        let offset = offset.to_string();
        let response = self
            .generated()
            .list_admin_guild_members(
                &snowflake(guild_id),
                Some(limit.as_str()),
                Some(offset.as_str()),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn ban_guild_member(&self, guild_id: &str, user_id: &str) -> ApiResult<()> {
        let body = generated_types::BanGuildMemberBody::default();
        self.generated()
            .ban_admin_guild_member(&snowflake(guild_id), &snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn kick_guild_member(&self, guild_id: &str, user_id: &str) -> ApiResult<()> {
        self.generated()
            .kick_admin_guild_member(&snowflake(guild_id), &snowflake(user_id))
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
        let before = before.map(snowflake);
        let limit = limit
            .map(i32::try_from)
            .transpose()
            .map_err(|e| ApiError::Parse(e.to_string()))?
            .map(generated_types::Int32Type::from);
        let response = self
            .generated()
            .list_admin_guild_audit_logs(
                &snowflake(guild_id),
                None,
                None,
                before.as_ref(),
                limit.as_ref(),
                None,
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn clear_guild_fields(&self, guild_id: &str, fields: &[String]) -> ApiResult<()> {
        let fields = fields
            .iter()
            .map(|field| generated_types::UpdateGuildRequestFieldsItem::try_from(field.as_str()))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::UpdateGuildRequest {
            fields,
            ..Default::default()
        };
        self.generated()
            .update_admin_guild(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn update_guild_settings(
        &self,
        guild_id: &str,
        settings: &serde_json::Value,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = guild_settings_request(settings)?;
        let response = self
            .generated()
            .update_admin_guild(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        guild_update_response(response.into_inner())
    }

    pub async fn update_guild_name(
        &self,
        guild_id: &str,
        name: &str,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = generated_types::UpdateGuildRequest {
            name: Some(name.to_owned()),
            ..Default::default()
        };
        let response = self
            .generated()
            .update_admin_guild(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_guild_vanity(
        &self,
        guild_id: &str,
        vanity: Option<&str>,
    ) -> ApiResult<GuildUpdateResponse> {
        let body = serde_json::json!({"vanity_url_code": vanity});
        self.patch(
            &format!("/admin/guilds/{}", urlencoding::encode(guild_id)),
            Some(&body),
        )
        .await
    }

    pub async fn reload_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let response = self
            .generated()
            .create_admin_guild_reload(&snowflake(guild_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn shutdown_guild(&self, guild_id: &str) -> ApiResult<SuccessResponse> {
        let response = self
            .generated()
            .create_admin_guild_shutdown(&snowflake(guild_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn force_add_user_to_guild(
        &self,
        user_id: &str,
        guild_id: &str,
    ) -> ApiResult<SuccessResponse> {
        let response = self
            .generated()
            .add_admin_guild_member(&snowflake(guild_id), &snowflake(user_id))
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
        self.search_reports(
            None,
            None,
            None,
            None,
            None,
            None,
            Some(guild_id),
            None,
            None,
            None,
            None,
            None,
            limit,
            offset,
        )
        .await
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
    settings: &serde_json::Value,
) -> ApiResult<generated_types::UpdateGuildRequest> {
    let patch = serde_json::from_value::<GuildSettingsPatch>(settings.clone())
        .map_err(|e| ApiError::Parse(e.to_string()))?;
    Ok(generated_types::UpdateGuildRequest {
        content_warning_level: patch.content_warning_level,
        content_warning_text: patch.content_warning_text,
        default_message_notifications: patch.default_message_notifications,
        disabled_operations: patch.disabled_operations,
        explicit_content_filter: patch.explicit_content_filter,
        mfa_level: patch.mfa_level,
        nsfw: patch.nsfw,
        nsfw_level: patch.nsfw_level,
        verification_level: patch.verification_level,
        ..Default::default()
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
        let request = guild_settings_request(&settings).unwrap();
        let json = serde_json::to_value(request).unwrap();
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
