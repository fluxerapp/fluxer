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
        let body = generated_types::SearchUsersRequest {
            email: nonempty_string(email),
            last_active_ip: nonempty_string(last_active_ip),
            limit: Some(
                crate::api::generated::nonzero_u32(limit, "limit").map_err(ApiError::Parse)?,
            ),
            offset: Some(i64::from(offset)),
            query: nonempty_string(query),
        };
        let response = self
            .generated()
            .search_users(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let response = response.into_inner();
        Ok(SearchUsersResponse {
            users: self.generated_value(response.users)?,
            total: response.total as u64,
        })
    }

    pub async fn lookup_user(&self, query: &str) -> ApiResult<Option<AdminUser>> {
        let body = generated_types::LookupUserRequest::Query(query.to_owned());
        let response = self
            .generated()
            .lookup_user(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupUserResponse = self.generated_value(response.into_inner())?;
        Ok(resp.users.into_iter().next())
    }

    pub async fn lookup_users_by_ids(&self, user_ids: &[String]) -> ApiResult<Vec<AdminUser>> {
        if user_ids.is_empty() {
            return Ok(vec![]);
        }
        let body = generated_types::LookupUserRequest::UserIds(
            user_ids
                .iter()
                .cloned()
                .map(generated_types::SnowflakeType::from)
                .collect(),
        );
        let response = self
            .generated()
            .lookup_user(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: LookupUserResponse = self.generated_value(response.into_inner())?;
        Ok(resp.users)
    }

    pub async fn get_user_by_id(&self, user_id: &str) -> ApiResult<AdminUser> {
        let body = generated_types::LookupUserRequest::Query(user_id.to_owned());
        let response = self
            .generated()
            .lookup_user(&body)
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
            .get_authenticated_admin_user()
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
        let body = generated_types::UpdateUserFlagsRequest {
            add_flags: user_flags(add_flags),
            remove_flags: user_flags(remove_flags),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .update_user_flags(&body)
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
        let body = generated_types::ListUserGuildsRequest {
            after: after.map(|id| generated_types::SnowflakeType::from(id.to_owned())),
            before: before.map(|id| generated_types::SnowflakeType::from(id.to_owned())),
            limit: Some(
                crate::api::generated::nonzero_u32(limit.unwrap_or(200), "limit")
                    .map_err(ApiError::Parse)?,
            ),
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
            with_counts: Some(with_counts.unwrap_or(true)),
        };
        let response = self
            .generated()
            .list_user_guilds(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: ListUserGuildsResponse = self.generated_value(response.into_inner())?;
        Ok(resp.guilds)
    }

    pub async fn list_user_sessions(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserSessionsResponse> {
        let body = generated_types::ListUserSessionsRequest {
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .list_user_sessions(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn terminate_user_sessions(
        &self,
        user_id: &str,
    ) -> ApiResult<TerminateSessionsResponse> {
        let body = generated_types::TerminateSessionsRequest {
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .terminate_user_sessions(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_user_relationships(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserRelationshipsResponse> {
        let body = generated_types::ListUserRelationshipsRequest {
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .admin_list_user_relationships(&body)
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
        let body = generated_types::ListUserDmChannelsRequest {
            after: after.map(|id| generated_types::SnowflakeType::from(id.to_owned())),
            before: before.map(|id| generated_types::SnowflakeType::from(id.to_owned())),
            limit: Some(
                crate::api::generated::nonzero_u32(limit.unwrap_or(50), "limit")
                    .map_err(ApiError::Parse)?,
            ),
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .list_user_dm_channels(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_user_group_dm_channels(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::ListUserGroupDmChannelsResponse> {
        let body = generated_types::ListUserGroupDmChannelsRequest {
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .list_user_group_dm_channels(&body)
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
        let body = generated_types::UpdatePremiumFlagsRequest {
            add_flags: premium_flags(add_flags),
            remove_flags: premium_flags(remove_flags),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .update_user_premium_flags(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn update_suspicious_flags(&self, user_id: &str, flags: i32) -> ApiResult<AdminUser> {
        let body = generated_types::UpdateSuspiciousActivityFlagsRequest {
            flags: generated_types::SuspiciousActivityFlags::from(flags),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .update_suspicious_activity_flags(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_user_acls(&self, user_id: &str, acls: &[String]) -> ApiResult<AdminUser> {
        let body = generated_types::SetUserAclsRequest {
            acls: acls.to_vec(),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .set_user_acls(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_user_traits(&self, user_id: &str, traits: &[String]) -> ApiResult<AdminUser> {
        let body = generated_types::SetUserTraitsRequest {
            traits: traits.to_vec(),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .set_user_traits(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn disable_mfa(&self, user_id: &str) -> ApiResult<()> {
        let body = generated_types::DisableMfaRequest {
            user_id: snowflake(user_id),
        };
        self.generated()
            .disable_user_mfa(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn resend_verification_email(&self, user_id: &str) -> ApiResult<()> {
        let body = generated_types::ResendVerificationEmailRequest {
            user_id: snowflake(user_id),
        };
        self.generated()
            .admin_resend_verification_email(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn verify_email(&self, user_id: &str) -> ApiResult<AdminUser> {
        let body = generated_types::VerifyUserEmailRequest {
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .verify_user_email(&body)
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
        let body = generated_types::UpdateHasVerifiedPhoneRequest {
            has_verified_phone,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .update_user_has_verified_phone(&body)
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
            .map(generated_types::ClearUserFieldsRequestFieldsItem::try_from)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::ClearUserFieldsRequest {
            fields,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .clear_user_fields(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_bot_status(&self, user_id: &str, is_bot: bool) -> ApiResult<AdminUser> {
        let body = generated_types::SetUserBotStatusRequest {
            bot: is_bot,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .set_user_bot_status(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn set_system_status(&self, user_id: &str, is_system: bool) -> ApiResult<AdminUser> {
        let body = generated_types::SetUserSystemStatusRequest {
            system: is_system,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .set_user_system_status(&body)
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
        let body = generated_types::ChangeUsernameRequest {
            discriminator: discriminator
                .map(generated_types::DiscriminatorType::try_from)
                .transpose()
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            user_id: snowflake(user_id),
            username: generated_types::UsernameType::try_from(username)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .change_user_username(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn change_email(&self, user_id: &str, email: &str) -> ApiResult<AdminUser> {
        let body = generated_types::ChangeEmailRequest {
            email: generated_types::EmailType::from(email.to_owned()),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .change_user_email(&body)
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
        let body = generated_types::TempBanUserRequest {
            duration_hours: i32::try_from(duration_hours)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            reason: reason.map(std::borrow::ToOwned::to_owned),
            user_id: snowflake(user_id),
        };
        let resp: UserMutationResponse = self
            .post_typed_with_reason("/admin/users/temp-ban", &body, private_reason)
            .await?;
        Ok(resp.user)
    }

    pub async fn unban_user(&self, user_id: &str) -> ApiResult<AdminUser> {
        let body = generated_types::DisableMfaRequest {
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .unban_user(&body)
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
        let body = generated_types::ScheduleAccountDeletionRequest {
            days_until_deletion: Some(
                crate::api::generated::nonzero_u32(days_until_deletion, "days_until_deletion")
                    .map_err(ApiError::Parse)?,
            ),
            public_reason: public_reason.map(std::borrow::ToOwned::to_owned),
            reason_code,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .schedule_account_deletion(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn cancel_deletion(&self, user_id: &str) -> ApiResult<AdminUser> {
        let body = generated_types::DisableMfaRequest {
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .cancel_account_deletion(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn change_dob(&self, user_id: &str, dob: &str) -> ApiResult<AdminUser> {
        let body = generated_types::ChangeDobRequest {
            date_of_birth: dob.to_owned(),
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .change_user_dob(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }

    pub async fn send_password_reset(&self, user_id: &str) -> ApiResult<()> {
        let body = generated_types::SendPasswordResetRequest {
            user_id: snowflake(user_id),
        };
        self.generated()
            .send_password_reset(&body)
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
        let body = generated_types::RemoveUserRelationshipRequest {
            category: generated_types::RemoveUserRelationshipRequestCategory::try_from(category)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            target_user_id: snowflake(target_id),
            user_id: snowflake(user_id),
        };
        self.generated()
            .remove_user_relationship(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn remove_relationships_by_category(
        &self,
        user_id: &str,
        category: &str,
    ) -> ApiResult<super::types::RemoveRelationshipsResponse> {
        let body = generated_types::RemoveUserRelationshipsByCategoryRequest {
            category: generated_types::RemoveUserRelationshipsByCategoryRequestCategory::try_from(
                category,
            )
            .map_err(|e| ApiError::Parse(e.to_string()))?,
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .remove_user_relationships_by_category(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_webauthn_credential(
        &self,
        user_id: &str,
        credential_id: &str,
    ) -> ApiResult<()> {
        let body = generated_types::DeleteWebAuthnCredentialRequest {
            credential_id: credential_id.to_owned(),
            user_id: snowflake(user_id),
        };
        self.generated()
            .delete_user_webauthn_credential(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        Ok(())
    }

    pub async fn list_user_change_log(
        &self,
        user_id: &str,
        limit: Option<u32>,
    ) -> ApiResult<super::types::ListUserChangeLogResponse> {
        let body = generated_types::ListUserChangeLogRequest {
            limit: Some(
                crate::api::generated::nonzero_u32(limit.unwrap_or(50), "limit")
                    .map_err(ApiError::Parse)?,
            ),
            page_token: None,
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .get_user_change_log(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_webauthn_credentials(
        &self,
        user_id: &str,
    ) -> ApiResult<super::types::WebAuthnCredentialListResponse> {
        let body = generated_types::ListWebAuthnCredentialsRequest {
            user_id: generated_types::SnowflakeType::from(user_id.to_owned()),
        };
        let response = self
            .generated()
            .list_user_webauthn_credentials(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn cancel_bulk_message_deletion(&self, user_id: &str) -> ApiResult<AdminUser> {
        let body = generated_types::CancelBulkMessageDeletionRequest {
            user_id: snowflake(user_id),
        };
        let response = self
            .generated()
            .admin_cancel_bulk_message_deletion(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        let resp: UserMutationResponse = self.generated_value(response.into_inner())?;
        Ok(resp.user)
    }
}

fn nonempty_string(value: Option<&str>) -> Option<String> {
    value
        .filter(|value| !value.is_empty())
        .map(std::borrow::ToOwned::to_owned)
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
