// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    CreateVoiceRegionResponse, CreateVoiceServerResponse, DeleteVoiceResponse,
    GetVoiceRegionResponse, GetVoiceServerResponse, ListVoiceRegionsResponse,
    ListVoiceServersResponse, UpdateVoiceRegionResponse, UpdateVoiceServerResponse,
};

impl AdminApiClient {
    pub async fn list_voice_regions(
        &self,
        include_servers: bool,
    ) -> ApiResult<ListVoiceRegionsResponse> {
        let response = self
            .generated()
            .list_admin_voice_regions(Some(bool_param(include_servers)))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_voice_region(
        &self,
        id: &str,
        include_servers: bool,
    ) -> ApiResult<GetVoiceRegionResponse> {
        let response = self
            .generated()
            .get_admin_voice_region(id, Some(bool_param(include_servers)))
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn create_voice_region(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<CreateVoiceRegionResponse> {
        let body =
            serde_json::from_value::<generated_types::CreateVoiceRegionRequest>(params.clone())
                .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .create_admin_voice_region(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_voice_region(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<UpdateVoiceRegionResponse> {
        let region_id = required_field(params, "id")?;
        let body =
            serde_json::from_value::<generated_types::UpdateVoiceRegionRequest>(params.clone())
                .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .update_admin_voice_region(&region_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_voice_region(&self, id: &str) -> ApiResult<DeleteVoiceResponse> {
        let response = self
            .generated()
            .delete_admin_voice_region(id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn list_voice_servers(&self, region_id: &str) -> ApiResult<ListVoiceServersResponse> {
        let response = self
            .generated()
            .list_admin_voice_servers(region_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_voice_server(
        &self,
        region_id: &str,
        server_id: &str,
    ) -> ApiResult<GetVoiceServerResponse> {
        let response = self
            .generated()
            .get_admin_voice_server(region_id, server_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn create_voice_server(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<CreateVoiceServerResponse> {
        let region_id = required_field(params, "region_id")?;
        let body =
            serde_json::from_value::<generated_types::CreateVoiceServerRequest>(params.clone())
                .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .create_admin_voice_server(&region_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn update_voice_server(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<UpdateVoiceServerResponse> {
        let region_id = required_field(params, "region_id")?;
        let server_id = required_field(params, "server_id")?;
        let body =
            serde_json::from_value::<generated_types::UpdateVoiceServerRequest>(params.clone())
                .map_err(|e| ApiError::Parse(e.to_string()))?;
        let response = self
            .generated()
            .update_admin_voice_server(&region_id, &server_id, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn delete_voice_server(
        &self,
        region_id: &str,
        server_id: &str,
    ) -> ApiResult<DeleteVoiceResponse> {
        let response = self
            .generated()
            .delete_admin_voice_server(region_id, server_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

fn bool_param(value: bool) -> &'static str {
    if value { "true" } else { "false" }
}

fn required_field(params: &serde_json::Value, field: &str) -> ApiResult<String> {
    params
        .get(field)
        .and_then(serde_json::Value::as_str)
        .map(std::borrow::ToOwned::to_owned)
        .ok_or_else(|| ApiError::Parse(format!("{field} is required")))
}
