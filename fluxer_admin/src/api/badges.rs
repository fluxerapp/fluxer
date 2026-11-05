// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    AdminCustomBadge, AdminCustomBadgeList, AdminUser, BadgeAssignment, BadgeListQuery, BadgeMove,
    BadgeSettings, CustomBadgeFields, CustomBadgeType, UserMutationResponse,
};

const LOOKUP_CHUNK: usize = 100;
const ASSIGNMENT_CANDIDATES: usize = 25;
const ASSIGNMENT_CANDIDATE_PAGE: u32 = 50;
const ASSIGNMENT_SEARCH_PAGES: usize = 4;
const ASSIGNMENT_PICKS_MAX: usize = 100;

pub fn parse_badge_picks(value: Option<&str>) -> Vec<String> {
    let mut picks: Vec<String> = Vec::new();
    for id in value.unwrap_or("").split(',').map(str::trim) {
        let valid =
            !id.is_empty() && id.len() <= 20 && id.bytes().all(|byte| byte.is_ascii_digit());
        if valid && !picks.iter().any(|pick| pick == id) {
            picks.push(id.to_owned());
        }
        if picks.len() >= ASSIGNMENT_PICKS_MAX {
            break;
        }
    }
    picks
}

fn generated_type(badge_type: CustomBadgeType) -> generated_types::CustomBadgeTypeSchema {
    match badge_type {
        CustomBadgeType::User => generated_types::CustomBadgeTypeSchema::User,
        CustomBadgeType::Guild => generated_types::CustomBadgeTypeSchema::Guild,
    }
}

fn badge_ids(ids: &[String]) -> generated_types::CustomBadgeIdsType {
    generated_types::CustomBadgeIdsType(ids.iter().map(|id| snowflake(id)).collect())
}

fn parse_error(error: impl std::fmt::Display) -> ApiError {
    ApiError::Parse(error.to_string())
}

impl AdminApiClient {
    pub async fn list_badges(&self, query: &BadgeListQuery) -> ApiResult<AdminCustomBadgeList> {
        let after = query
            .after
            .as_deref()
            .map(str::parse::<generated_types::CustomBadgeCursorType>)
            .transpose()
            .map_err(parse_error)?;
        let limit = query.limit.map(|limit| limit.to_string());
        let response = self
            .generated()
            .list_admin_badges(
                after.as_ref(),
                None,
                limit.as_deref(),
                query.query.as_deref(),
                query.badge_type.map(generated_type),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn find_badges(
        &self,
        badge_type: CustomBadgeType,
        ids: &[String],
    ) -> ApiResult<Vec<AdminCustomBadge>> {
        let mut found = Vec::new();
        for chunk in ids.chunks(LOOKUP_CHUNK) {
            let ids: generated_types::ListAdminBadgesIds =
                chunk.join(",").parse().map_err(parse_error)?;
            let response = self
                .generated()
                .list_admin_badges(
                    None,
                    Some(&ids),
                    None,
                    None,
                    Some(generated_type(badge_type)),
                )
                .await
                .map_err(|e| self.generated_error(e))?;
            let list: AdminCustomBadgeList = self.generated_value(response.into_inner())?;
            found.extend(list.badges);
        }
        Ok(found)
    }

    pub async fn load_badge_assignment(
        &self,
        badge_type: CustomBadgeType,
        assigned_ids: &[String],
        query: Option<&str>,
        picked_ids: &[String],
    ) -> ApiResult<BadgeAssignment> {
        let query = query
            .map(str::trim)
            .filter(|query| !query.is_empty())
            .map(|query| query.chars().take(64).collect::<String>());
        let assigned = self.find_badges(badge_type, assigned_ids).await?;
        let wanted: Vec<String> = picked_ids
            .iter()
            .filter(|id| !assigned.iter().any(|held| &held.id == *id))
            .cloned()
            .collect();
        let found = self.find_badges(badge_type, &wanted).await?;
        let picked: Vec<AdminCustomBadge> = wanted
            .iter()
            .filter_map(|id| found.iter().find(|badge| &badge.id == id).cloned())
            .collect();
        let mut candidates: Vec<AdminCustomBadge> = Vec::new();
        let mut after = None;
        let mut next = None;
        let mut settings = None;
        for _ in 0..ASSIGNMENT_SEARCH_PAGES {
            let list = self
                .list_badges(&BadgeListQuery {
                    badge_type: Some(badge_type),
                    query: query.clone(),
                    after: after.take(),
                    limit: Some(ASSIGNMENT_CANDIDATE_PAGE),
                })
                .await?;
            settings.get_or_insert(list.settings);
            candidates.extend(list.badges.into_iter().filter(|badge| {
                !assigned.iter().any(|held| held.id == badge.id)
                    && !picked.iter().any(|pick| pick.id == badge.id)
            }));
            next = list.next;
            if candidates.len() > ASSIGNMENT_CANDIDATES || next.is_none() {
                break;
            }
            after.clone_from(&next);
        }
        let settings = settings.unwrap_or_default();
        let more_candidates = next.is_some() || candidates.len() > ASSIGNMENT_CANDIDATES;
        Ok(BadgeAssignment {
            assigned,
            picked,
            search_incomplete: next.is_some() && candidates.len() <= ASSIGNMENT_CANDIDATES,
            candidates: candidates.into_iter().take(ASSIGNMENT_CANDIDATES).collect(),
            more_candidates,
            query,
            limit: match badge_type {
                CustomBadgeType::User => settings.max_per_user,
                CustomBadgeType::Guild => settings.max_per_guild,
            },
        })
    }

    pub async fn move_badge(
        &self,
        badge_id: &str,
        direction: BadgeMove,
    ) -> ApiResult<AdminCustomBadge> {
        let body = generated_types::MoveCustomBadgeRequest {
            badge_id: snowflake(badge_id),
            direction: Some(match direction {
                BadgeMove::Up => generated_types::CustomBadgeMoveDirectionSchema::Up,
                BadgeMove::Down => generated_types::CustomBadgeMoveDirectionSchema::Down,
                BadgeMove::First => generated_types::CustomBadgeMoveDirectionSchema::First,
                BadgeMove::Last => generated_types::CustomBadgeMoveDirectionSchema::Last,
            }),
            after_id: None,
        };
        let response = self
            .generated()
            .move_admin_badge(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_badge_settings(&self, settings: BadgeSettings) -> ApiResult<BadgeSettings> {
        let limit = |value: u32| std::num::NonZeroU64::new(u64::from(value));
        let body = generated_types::UpdateCustomBadgeSettingsRequest {
            max_per_user: limit(settings.max_per_user),
            max_per_guild: limit(settings.max_per_guild),
        };
        let response = self
            .generated()
            .update_admin_badge_settings(&body)
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
            type_: generated_type(badge_type),
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

    pub async fn replace_built_in_badge_image(
        &self,
        kind: &str,
        image: &str,
    ) -> ApiResult<AdminCustomBadgeList> {
        let body = generated_types::ReplaceBuiltInBadgeImageRequest {
            image: image.try_into().map_err(parse_error)?,
        };
        let kind: generated_types::BuiltInBadgeKindSchema = kind.parse().map_err(parse_error)?;
        let response = self
            .generated()
            .replace_admin_built_in_badge_image(kind, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn reset_built_in_badge_image(&self, kind: &str) -> ApiResult<AdminCustomBadgeList> {
        let kind: generated_types::BuiltInBadgeKindSchema = kind.parse().map_err(parse_error)?;
        let response = self
            .generated()
            .reset_admin_built_in_badge_image(kind)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn set_built_in_badge_hidden(
        &self,
        kind: &str,
        hidden: bool,
    ) -> ApiResult<AdminCustomBadgeList> {
        let kind: generated_types::HideableBuiltInBadgeKindSchema =
            kind.parse().map_err(parse_error)?;
        let client = self.generated();
        let response = if hidden {
            client.hide_admin_built_in_badge(kind).await
        } else {
            client.show_admin_built_in_badge(kind).await
        }
        .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
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
