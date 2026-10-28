// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::Method;

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    AccountIdentitySettings, BrandingAssetKind, BrandingAssetUploadRequest,
    CreateRegistrationUrlRequest, CreateRegistrationUrlResponse, InstanceAccountIdentityDiscovery,
    InstanceConfigResponse, InstanceConfigUpdateRequest, InstanceEmailSmtpTestResponse,
    InstancePremiumDiscovery, PendingRegistrationListResponse, PendingRegistrationResponse,
    PendingRegistrationStatus,
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
            emails_enabled: discovery.features.emails_enabled,
            username_from_provider: discovery.sso.username_from_provider,
        })
    }

    pub async fn update_instance_config(
        &self,
        update: &InstanceConfigUpdateRequest,
    ) -> ApiResult<InstanceConfigResponse> {
        self.send(Method::PATCH, "/admin/instance/config", Some(update), None)
            .await
    }

    pub async fn upload_branding_asset(
        &self,
        kind: BrandingAssetKind,
        image: Option<String>,
    ) -> ApiResult<InstanceConfigResponse> {
        self.send(
            Method::POST,
            "/admin/instance/config/branding-assets",
            Some(&BrandingAssetUploadRequest { kind, image }),
            None,
        )
        .await
    }

    pub async fn test_saved_instance_smtp_config(
        &self,
        send_to: Option<&str>,
    ) -> ApiResult<InstanceEmailSmtpTestResponse> {
        let body = match send_to {
            Some(address) => serde_json::json!({"send_to": address}),
            None => serde_json::json!({}),
        };
        self.send(
            Method::POST,
            "/admin/instance/config/smtp-tests",
            Some(&body),
            None,
        )
        .await
    }

    pub async fn rename_registration_url(
        &self,
        id: &str,
        label: Option<&str>,
    ) -> ApiResult<InstanceConfigResponse> {
        self.send(
            Method::PATCH,
            &format!(
                "/admin/instance/registration-urls/{}",
                urlencoding::encode(id)
            ),
            Some(&serde_json::json!({"label": label})),
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

    pub async fn list_pending_registrations(&self) -> ApiResult<Vec<PendingRegistrationResponse>> {
        let response: PendingRegistrationListResponse = self
            .get("/admin/instance/pending-registrations", None)
            .await?;
        Ok(response.pending_registrations)
    }

    pub async fn decide_pending_registration(
        &self,
        user_id: &str,
        status: PendingRegistrationStatus,
    ) -> ApiResult<Vec<PendingRegistrationResponse>> {
        let body = serde_json::json!({"status": status.as_str()});
        let response: PendingRegistrationListResponse = self
            .send(
                Method::PATCH,
                &format!(
                    "/admin/instance/pending-registrations/{}",
                    urlencoding::encode(user_id)
                ),
                Some(&body),
                None,
            )
            .await?;
        Ok(response.pending_registrations)
    }
}
