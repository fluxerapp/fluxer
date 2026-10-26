// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    AdminCustomBadge, AdminCustomBadgeList, AdminUser, CustomBadgeFields, CustomBadgeType,
    UserMutationResponse,
};

fn badge_ids(ids: &[String]) -> generated_types::CustomBadgeIdsType {
    generated_types::CustomBadgeIdsType(ids.iter().map(|id| snowflake(id)).collect())
}

fn parse_error(error: impl std::fmt::Display) -> ApiError {
    ApiError::Parse(error.to_string())
}

impl AdminApiClient {
    pub async fn list_badges(&self) -> ApiResult<AdminCustomBadgeList> {
        let response = self
            .generated()
            .list_admin_badges()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn create_badge(
        &self,
        badge_type: CustomBadgeType,
        fields: CustomBadgeFields,
    ) -> ApiResult<AdminCustomBadge> {
        let image = fields
            .image
            .ok_or_else(|| ApiError::Parse("Choose an image to upload".to_owned()))?;
        let body = generated_types::CreateCustomBadgeRequest {
            type_: match badge_type {
                CustomBadgeType::User => generated_types::CustomBadgeTypeSchema::User,
                CustomBadgeType::Guild => generated_types::CustomBadgeTypeSchema::Guild,
            },
            name: fields.name,
            description: fields.description,
            href: fields.href,
            image: image.as_str().try_into().map_err(parse_error)?,
        };
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
        fields: CustomBadgeFields,
    ) -> ApiResult<AdminCustomBadge> {
        let body = generated_types::UpdateCustomBadgeRequest {
            name: fields.name,
            description: fields.description,
            href: fields.href,
            image: fields
                .image
                .as_deref()
                .map(TryInto::try_into)
                .transpose()
                .map_err(parse_error)?,
        };
        let response = self
            .generated()
            .update_admin_badge(&snowflake(badge_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_badge(&self, badge_id: &str) -> ApiResult<()> {
        self.generated()
            .delete_admin_badge(&snowflake(badge_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn update_user_badges(&self, user_id: &str, ids: &[String]) -> ApiResult<AdminUser> {
        let body = generated_types::UpdateUserCustomBadgesRequest {
            badge_ids: badge_ids(ids),
        };
        let response = self
            .generated()
            .update_admin_user_badges(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_guild_badges(&self, guild_id: &str, ids: &[String]) -> ApiResult<()> {
        let body = generated_types::UpdateGuildRequest {
            badge_ids: Some(badge_ids(ids)),
            ..Default::default()
        };
        self.generated()
            .update_admin_guild(&snowflake(guild_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }
}
