// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    AdminUser, BadgeMutationResponse, BuiltinBadgeIcons, GuildUpdateResponse, ListBadgesResponse,
    SuccessResponse, UserMutationResponse,
};

impl AdminApiClient {
    pub async fn get_public_badges(&self) -> ApiResult<ListBadgesResponse> {
        self.get("/badges", None).await
    }

    pub async fn list_badges(&self) -> ApiResult<ListBadgesResponse> {
        let response = self
            .generated()
            .list_admin_badges()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn create_badge(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<BadgeMutationResponse> {
        let body = serde_json::from_value::<generated_types::CreateBadgeRequest>(params.clone())
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .create_admin_badge(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_badge(
        &self,
        badge_id: &str,
        params: &serde_json::Value,
    ) -> ApiResult<BadgeMutationResponse> {
        serde_json::from_value::<generated_types::UpdateBadgeRequest>(params.clone())
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        self.patch_with_reason(
            &format!("/admin/badges/{}", urlencoding::encode(badge_id)),
            Some(params),
            None,
        )
        .await
    }

    pub async fn delete_badge(&self, badge_id: &str) -> ApiResult<SuccessResponse> {
        let response = self
            .generated()
            .delete_admin_badge(&snowflake(badge_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_builtin_badge_icon(
        &self,
        badge: &str,
        icon: Option<&str>,
    ) -> ApiResult<BuiltinBadgeIcons> {
        let badge = badge
            .parse::<generated_types::BuiltinBadgeEnum>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let icon = icon
            .map(str::parse::<generated_types::UpdateBuiltinBadgeIconRequestIcon>)
            .transpose()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .update_admin_builtin_badge_icon(
                badge,
                &generated_types::UpdateBuiltinBadgeIconRequest { icon },
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_user_badges(
        &self,
        user_id: &str,
        badge_ids: &[String],
    ) -> ApiResult<AdminUser> {
        let body = generated_types::BadgeAssignmentRequest {
            badge_ids: badge_ids.iter().map(|id| snowflake(id)).collect(),
        };
        let response = self
            .generated()
            .update_admin_user_badges(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_guild_badges(
        &self,
        guild_id: &str,
        badge_ids: &[String],
    ) -> ApiResult<GuildUpdateResponse> {
        self.patch_with_reason(
            &format!("/admin/guilds/{}", urlencoding::encode(guild_id)),
            Some(&serde_json::json!({"badge_ids": badge_ids})),
            None,
        )
        .await
    }
}
