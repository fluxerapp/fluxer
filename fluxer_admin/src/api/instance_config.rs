// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::types::PendingRegistrationResponse;

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    CreateRegistrationUrlRequest, CreateRegistrationUrlResponse, InstanceConfigResponse,
    InstanceConfigUpdateRequest, InstanceEmailSmtpTestRequest, InstanceEmailSmtpTestResponse,
    PendingRegistrationActionRequest, RegistrationUrlActionRequest,
};

impl AdminApiClient {
    pub async fn get_instance_config(&self) -> ApiResult<InstanceConfigResponse> {
        self.post("/admin/instance-config/get", None).await
    }

    pub async fn update_instance_config(
        &self,
        update: &InstanceConfigUpdateRequest,
    ) -> ApiResult<InstanceConfigResponse> {
        self.post_typed("/admin/instance-config/update", update)
            .await
    }

    pub async fn test_instance_smtp_config(
        &self,
        request: &InstanceEmailSmtpTestRequest,
    ) -> ApiResult<InstanceEmailSmtpTestResponse> {
        self.post_typed("/admin/instance-config/integrations/smtp/test", request)
            .await
    }

    pub async fn create_registration_url(
        &self,
        request: &CreateRegistrationUrlRequest,
    ) -> ApiResult<CreateRegistrationUrlResponse> {
        self.post_typed("/admin/instance-config/registration-urls/create", request)
            .await
    }

    pub async fn revoke_registration_url(&self, id: &str) -> ApiResult<InstanceConfigResponse> {
        let request = RegistrationUrlActionRequest { id: id.to_owned() };
        self.post_typed("/admin/instance-config/registration-urls/revoke", &request)
            .await
    }

    pub async fn get_pending_registrations(&self) -> ApiResult<Vec<PendingRegistrationResponse>> {
        self.post_typed("/admin/instance-config/pending-registrations/get", &())
            .await
    }

    pub async fn approve_pending_registration(
        &self,
        user_id: &str,
    ) -> ApiResult<InstanceConfigResponse> {
        let request = PendingRegistrationActionRequest {
            user_id: user_id.to_owned(),
        };
        self.post_typed(
            "/admin/instance-config/pending-registrations/approve",
            &request,
        )
        .await
    }

    pub async fn reject_pending_registration(
        &self,
        user_id: &str,
    ) -> ApiResult<InstanceConfigResponse> {
        let request = PendingRegistrationActionRequest {
            user_id: user_id.to_owned(),
        };
        self.post_typed(
            "/admin/instance-config/pending-registrations/reject",
            &request,
        )
        .await
    }
}
