// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    AdminUser, AdminUserMeResponse, GuildInfo, ListUserGuildsResponse, LookupUserResponse,
    SearchUsersResponse, TerminateSessionsResponse, UserMutationResponse,
};

impl AdminApiClient {
    pub async fn search_users(
        &self,
        query: Option<&str>,
        email: Option<&str>,
        last_active_ip: Option<&str>,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchUsersResponse> {
        let limit = limit.to_string();
        let offset = offset.to_string();
        let response = self
            .generated()
            .list_admin_users(
                nonempty(email),
                nonempty(last_active_ip),
                Some(limit.as_str()),
                Some(offset.as_str()),
                nonempty(query),
                None,
                None,
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        let response = response.into_inner();
        Ok(SearchUsersResponse {
            users: self.generated_value(response.users)?,
            total: response.total as u64,
        })
    }

    pub async fn lookup_user(&self, query: &str) -> ApiResult<Option<AdminUser>> {
        let response = self
            .generated()
            .list_admin_users(None, None, None, None, None, Some(query), None)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupUserResponse = self.generated_value(response.into_inner())?;
        Ok(resp.users.into_iter().next())
    }

    pub async fn lookup_users_by_ids(&self, user_ids: &[String]) -> ApiResult<Vec<AdminUser>> {
        if user_ids.is_empty() {
            return Ok(vec![]);
        }
        let query_params: Vec<(&str, &str)> = user_ids
            .iter()
            .map(|user_id| ("user_id", user_id.as_str()))
            .collect();
        let resp: LookupUserResponse = self.get("/admin/users", Some(&query_params)).await?;
        Ok(resp.users)
    }

    pub async fn get_user_by_id(&self, user_id: &str) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .get_admin_user(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupUserResponse = self.generated_value(response.into_inner())?;
        resp.users
            .into_iter()
            .next()
            .ok_or_else(|| super::client::ApiError::Http {
                status: 404,
                message: "User not found".to_owned(),
            })
    }

    pub async fn get_current_admin(&self) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .get_current_admin_user()
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: AdminUserMeResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_user_flags(
        &self,
        user_id: &str,
        add_flags: &[String],
        remove_flags: &[String],
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserFlagsUpdateRequest {
            add_flags: user_flags(add_flags),
            remove_flags: user_flags(remove_flags),
        };
        let response = self
            .generated()
            .update_admin_user_flags(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn get_user_guilds(
        &self,
        user_id: &str,
        limit: Option<u32>,
        before: Option<&str>,
        after: Option<&str>,
        with_counts: Option<bool>,
    ) -> ApiResult<Vec<GuildInfo>> {
        let after = after.map(snowflake);
        let before = before.map(snowflake);
        let limit = limit.unwrap_or(200).to_string();
        let with_counts = bool_param(with_counts.unwrap_or(true));
        let response = self
            .generated()
            .list_admin_user_guilds(
                &snowflake(user_id),
                after.as_ref(),
                before.as_ref(),
                Some(limit.as_str()),
                Some(with_counts),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: ListUserGuildsResponse = self.generated_value(response.into_inner())?;
        Ok(resp.guilds)
    }

    pub async fn list_user_sessions(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserSessionsResponse> {
        let response = self
            .generated()
            .list_admin_user_sessions(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn terminate_user_sessions(
        &self,
        user_id: &str,
    ) -> ApiResult<TerminateSessionsResponse> {
        let response = self
            .generated()
            .terminate_admin_user_sessions(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_user_relationships(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserRelationshipsResponse> {
        let response = self
            .generated()
            .list_admin_user_relationships(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_user_dm_channels(
        &self,
        user_id: &str,
        before: Option<&str>,
        after: Option<&str>,
        limit: Option<u32>,
    ) -> ApiResult<super::types::ListUserDmChannelsResponse> {
        let after = after.map(snowflake);
        let before = before.map(snowflake);
        let limit = limit.unwrap_or(50).to_string();
        let response = self
            .generated()
            .list_admin_user_dm_channels(
                &snowflake(user_id),
                after.as_ref(),
                before.as_ref(),
                Some(limit.as_str()),
                Some(generated_types::AdminUserDmChannelType::Dm),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_user_group_dm_channels(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserGroupDmChannelsResponse> {
        let response = self
            .generated()
            .list_admin_user_dm_channels(
                &snowflake(user_id),
                None,
                None,
                None,
                Some(generated_types::AdminUserDmChannelType::GroupDm),
            )
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_premium_flags(
        &self,
        user_id: &str,
        add_flags: &[i32],
        remove_flags: &[i32],
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserPremiumFlagsUpdateRequest {
            add_flags: premium_flags(add_flags),
            remove_flags: premium_flags(remove_flags),
        };
        let response = self
            .generated()
            .update_admin_user_premium_flags(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_suspicious_flags(&self, user_id: &str, flags: i32) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserSuspiciousActivityFlagsRequest {
            flags: generated_types::SuspiciousActivityFlags::from(flags),
        };
        let response = self
            .generated()
            .update_admin_user_suspicious_activity_flags(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_user_acls(&self, user_id: &str, acls: &[String]) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserAclsRequest {
            acls: super::admin_api_keys::parse_acls(acls)?,
        };
        let response = self
            .generated()
            .set_admin_user_acls(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_user_traits(&self, user_id: &str, traits: &[String]) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserTraitsRequest {
            traits: traits.to_vec(),
        };
        let response = self
            .generated()
            .set_admin_user_traits(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn disable_mfa(&self, user_id: &str) -> ApiResult<()> {
        self.generated()
            .disable_admin_user_mfa(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn resend_verification_email(&self, user_id: &str) -> ApiResult<()> {
        self.generated()
            .resend_admin_user_verification_email(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn verify_email(&self, user_id: &str) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .verify_admin_user_email(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_has_verified_phone(
        &self,
        user_id: &str,
        has_verified_phone: bool,
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserPhoneVerificationRequest { has_verified_phone };
        let response = self
            .generated()
            .update_admin_user_phone_verification(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn clear_user_fields(
        &self,
        user_id: &str,
        fields: &[String],
    ) -> ApiResult<AdminUser> {
        let fields = fields
            .iter()
            .map(|field| {
                generated_types::AdminUserClearFieldsRequestFieldsItem::try_from(field.as_str())
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::AdminUserClearFieldsRequest { fields };
        let response = self
            .generated()
            .clear_admin_user_profile_fields(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_bot_status(&self, user_id: &str, is_bot: bool) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserBotStatusRequest { bot: is_bot };
        let response = self
            .generated()
            .set_admin_user_bot_status(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_system_status(&self, user_id: &str, is_system: bool) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserSystemStatusRequest { system: is_system };
        let response = self
            .generated()
            .set_admin_user_system_status(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn change_username(
        &self,
        user_id: &str,
        username: &str,
        discriminator: Option<&str>,
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserUsernameUpdateRequest {
            discriminator: discriminator
                .map(generated_types::DiscriminatorType::try_from)
                .transpose()
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            username: generated_types::UsernameType::try_from(username)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .update_admin_user_username(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn change_email(&self, user_id: &str, email: &str) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserEmailUpdateRequest {
            email: generated_types::EmailType::from(email.to_owned()),
        };
        let response = self
            .generated()
            .update_admin_user_email(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn temp_ban_user(
        &self,
        user_id: &str,
        duration_hours: u32,
        reason: Option<&str>,
        private_reason: Option<&str>,
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserBanRequest {
            duration_hours: i32::try_from(duration_hours)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            reason: reason.map(std::borrow::ToOwned::to_owned),
        };
        let resp: UserMutationResponse = self
            .put_typed_with_reason(
                &format!("/admin/users/{}/ban", urlencoding::encode(user_id)),
                &body,
                private_reason,
            )
            .await?;
        Ok(resp.user)
    }

    pub async fn unban_user(&self, user_id: &str) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .unban_admin_user(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn schedule_deletion(
        &self,
        user_id: &str,
        reason_code: i32,
        public_reason: Option<&str>,
        days_until_deletion: u32,
    ) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserDeletionScheduleRequest {
            days_until_deletion: Some(
                crate::api::generated::nonzero_u32(days_until_deletion, "days_until_deletion")
                    .map_err(ApiError::Parse)?,
            ),
            public_reason: public_reason.map(std::borrow::ToOwned::to_owned),
            reason_code: crate::api::generated::deletion_reason_code(reason_code, "reason_code")
                .map_err(ApiError::Parse)?,
        };
        let response = self
            .generated()
            .schedule_admin_user_deletion(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn cancel_deletion(&self, user_id: &str) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .cancel_admin_user_deletion(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn change_dob(&self, user_id: &str, dob: &str) -> ApiResult<AdminUser> {
        let body = generated_types::AdminUserDobUpdateRequest {
            date_of_birth: dob.to_owned(),
        };
        let response = self
            .generated()
            .update_admin_user_date_of_birth(&snowflake(user_id), &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn send_password_reset(&self, user_id: &str) -> ApiResult<()> {
        self.generated()
            .send_admin_user_password_reset(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn remove_relationship(
        &self,
        user_id: &str,
        target_id: &str,
        category: &str,
    ) -> ApiResult<()> {
        let category = generated_types::RemoveAdminUserRelationshipCategory::try_from(category)
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        self.generated()
            .remove_admin_user_relationship(&snowflake(user_id), target_id, category)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn remove_relationships_by_category(
        &self,
        user_id: &str,
        category: &str,
    ) -> ApiResult<super::types::RemoveRelationshipsResponse> {
        let category = generated_types::ClearAdminUserRelationshipsCategory::try_from(category)
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .clear_admin_user_relationships(&snowflake(user_id), category)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_webauthn_credential(
        &self,
        user_id: &str,
        credential_id: &str,
    ) -> ApiResult<()> {
        self.generated()
            .delete_admin_user_webauthn_credential(&snowflake(user_id), credential_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn list_user_change_log(
        &self,
        user_id: &str,
        limit: Option<u32>,
    ) -> ApiResult<super::types::ListUserChangeLogResponse> {
        let limit = limit.unwrap_or(50).to_string();
        let response = self
            .generated()
            .list_admin_user_change_log(&snowflake(user_id), Some(limit.as_str()), None)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_webauthn_credentials(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::WebAuthnCredentialListResponse> {
        let response = self
            .generated()
            .list_admin_user_webauthn_credentials(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn cancel_bulk_message_deletion(&self, user_id: &str) -> ApiResult<AdminUser> {
        let response = self
            .generated()
            .cancel_admin_user_message_deletion(&snowflake(user_id))
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }
}

fn nonempty(value: Option<&str>) -> Option<&str> {
    value.filter(|value| !value.is_empty())
}

fn bool_param(value: bool) -> &'static str {
    if value { "true" } else { "false" }
}

fn snowflake(value: &str) -> generated_types::SnowflakeType {
    generated_types::SnowflakeType::from(value.to_owned())
}

fn user_flags(values: &[String]) -> Vec<generated_types::UserFlags> {
    values
        .iter()
        .cloned()
        .map(generated_types::UserFlags::from)
        .collect()
}

fn premium_flags(values: &[i32]) -> Vec<generated_types::PremiumFlags> {
    values
        .iter()
        .map(|value| generated_types::PremiumFlags::from(*value))
        .collect()
}
