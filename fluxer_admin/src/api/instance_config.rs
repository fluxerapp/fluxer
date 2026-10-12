// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::Method;

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    AccountIdentitySettings, CreateRegistrationUrlRequest, CreateRegistrationUrlResponse,
    InstanceAccountIdentityDiscovery, InstanceConfigResponse, InstanceConfigUpdateRequest,
    InstanceEmailSmtpTestResponse, InstancePremiumDiscovery,
};

impl AdminApiClient {
    pub async fn get_instance_config(&self) -> ApiResult<InstanceConfigResponse> {
        self.get("/admin/instance/config", None).await
    }

    pub async fn get_instance_premium_discovery(&self) -> ApiResult<InstancePremiumDiscovery> {
        self.get("/.well-known/fluxer", None).await
    }

    pub async fn get_instance_account_identity(&self) -> ApiResult<AccountIdentitySettings> {
        let discovery: InstanceAccountIdentityDiscovery =
            self.get("/.well-known/fluxer", None).await?;
        let mode = discovery.features.account_identity;
        Ok(AccountIdentitySettings {
            mode,
            tag_style: discovery.features.tag_style,
        })
    }

    pub async fn update_instance_config(
        &self,
        update: &InstanceConfigUpdateRequest,
    ) -> ApiResult<InstanceConfigResponse> {
        self.send(Method::PATCH, "/admin/instance/config", Some(update), None)
            .await
    }

    pub async fn test_saved_instance_smtp_config(
        &self,
    ) -> ApiResult<InstanceEmailSmtpTestResponse> {
        self.send(
            Method::POST,
            "/admin/instance/config/smtp-tests",
            Some(&serde_json::json!({})),
            None,
        )
        .await
    }

    pub async fn create_registration_url(
        &self,
        request: &CreateRegistrationUrlRequest,
    ) -> ApiResult<CreateRegistrationUrlResponse> {
        self.send(
            Method::POST,
            "/admin/instance/registration-urls",
            Some(request),
            None,
        )
        .await
    }

    pub async fn revoke_registration_url(&self, id: &str) -> ApiResult<InstanceConfigResponse> {
        self.send(
            Method::DELETE,
            &format!(
                "/admin/instance/registration-urls/{}",
                urlencoding::encode(id)
            ),
            None::<&serde_json::Value>,
            None,
        )
        .await
    }

    pub async fn approve_pending_registration(
        &self,
        user_id: &str,
    ) -> ApiResult<InstanceConfigResponse> {
        self.decide_pending_registration(user_id, "approved").await
    }

    pub async fn reject_pending_registration(
        &self,
        user_id: &str,
    ) -> ApiResult<InstanceConfigResponse> {
        self.decide_pending_registration(user_id, "rejected").await
    }

    async fn decide_pending_registration(
        &self,
        user_id: &str,
        status: &str,
    ) -> ApiResult<InstanceConfigResponse> {
        let body = serde_json::json!({"status": status});
        self.send(
            Method::PATCH,
            &format!(
                "/admin/instance/pending-registrations/{}",
                urlencoding::encode(user_id)
            ),
            Some(&body),
            None,
        )
        .await
    }
}
