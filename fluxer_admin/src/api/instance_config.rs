// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    CreateRegistrationUrlRequest, CreateRegistrationUrlResponse, InstanceConfigResponse,
    InstanceConfigUpdateRequest, InstanceEmailSmtpTestRequest, InstanceEmailSmtpTestResponse,
};

impl AdminApiClient {
    pub async fn get_instance_config(&self) -> ApiResult<InstanceConfigResponse> {
        self.get("/admin/instance/config", None).await
    }

    pub async fn update_instance_config(
        &self,
        update: &InstanceConfigUpdateRequest,
    ) -> ApiResult<InstanceConfigResponse> {
        self.patch_typed_with_reason("/admin/instance/config", update, None)
            .await
    }

    pub async fn test_instance_smtp_config(
        &self,
        request: &InstanceEmailSmtpTestRequest,
    ) -> ApiResult<InstanceEmailSmtpTestResponse> {
        self.post_typed("/admin/instance/config/smtp-tests", request)
            .await
    }

    pub async fn create_registration_url(
        &self,
        request: &CreateRegistrationUrlRequest,
    ) -> ApiResult<CreateRegistrationUrlResponse> {
        self.post_typed("/admin/instance/registration-urls", request)
            .await
    }

    pub async fn revoke_registration_url(&self, id: &str) -> ApiResult<InstanceConfigResponse> {
        self.delete_with_reason(
            &format!(
                "/admin/instance/registration-urls/{}",
                urlencoding::encode(id)
            ),
            None,
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
        self.patch_with_reason(
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
