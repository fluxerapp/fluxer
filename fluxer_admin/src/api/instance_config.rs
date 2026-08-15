// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiResult};
use super::types::{
    CreateRegistrationUrlRequest, CreateRegistrationUrlResponse, InstanceConfigResponse,
    InstanceConfigUpdateRequest, InstanceEmailSmtpTestRequest, InstanceEmailSmtpTestResponse,
    RegistrationUrlActionRequest,
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
}
