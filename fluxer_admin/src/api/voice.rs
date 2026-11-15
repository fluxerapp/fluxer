// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::bool_param;

use reqwest::Method;

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    CreateVoiceJoinTokenResponse, CreateVoiceRegionResponse, CreateVoiceServerResponse,
    DeleteVoiceResponse, GetVoiceRegionResponse, ListVoiceJoinTokensResponse,
    ListVoiceRegionsResponse, ListVoiceServersResponse, UpdateVoiceRegionResponse,
    UpdateVoiceServerResponse,
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
        let body = voice_request_body(params, &["id"])?;
        validate_against::<generated_types::UpdateVoiceRegionRequestBody>(&body)?;
        self.send(
            Method::PATCH,
            &format!("/admin/voice/regions/{}", urlencoding::encode(&region_id)),
            Some(&body),
            None,
        )
        .await
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

    pub async fn create_voice_server(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<CreateVoiceServerResponse> {
        let region_id = required_field(params, "region_id")?;
        paired_coordinates(params)?;
        let body = serde_json::from_value::<generated_types::CreateVoiceServerRequestBody>(
            voice_request_body(params, &["region_id"])?,
        )
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
        paired_coordinates(params)?;
        let body = voice_request_body(params, &["region_id", "server_id"])?;
        validate_against::<generated_types::UpdateVoiceServerRequestBody>(&body)?;
        self.send(
            Method::PATCH,
            &format!(
                "/admin/voice/regions/{}/servers/{}",
                urlencoding::encode(&region_id),
                urlencoding::encode(&server_id)
            ),
            Some(&body),
            None,
        )
        .await
    }

    pub async fn create_voice_join_token(
        &self,
        params: &serde_json::Value,
    ) -> ApiResult<CreateVoiceJoinTokenResponse> {
        validate_against::<generated_types::CreateVoiceJoinTokenRequest>(params)?;
        self.send(Method::POST, "/admin/voice/join-tokens", Some(params), None)
            .await
    }

    pub async fn list_voice_join_tokens(&self) -> ApiResult<ListVoiceJoinTokensResponse> {
        let response = self
            .generated()
            .list_admin_voice_join_tokens()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn revoke_voice_join_token(&self, token_id: &str) -> ApiResult<DeleteVoiceResponse> {
        self.send(
            Method::DELETE,
            &format!("/admin/voice/join-tokens/{}", urlencoding::encode(token_id)),
            None::<&serde_json::Value>,
            None,
        )
        .await
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

fn validate_against<T: serde::de::DeserializeOwned>(params: &serde_json::Value) -> ApiResult<()> {
    serde_json::from_value::<T>(params.clone())
        .map(drop)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn voice_request_body(
    params: &serde_json::Value,
    path_fields: &[&str],
) -> ApiResult<serde_json::Value> {
    let mut body = params
        .as_object()
        .ok_or_else(|| ApiError::Parse("voice request body must be an object".to_owned()))?
        .clone();
    for field in path_fields {
        body.remove(*field);
    }
    Ok(body.into())
}

fn paired_coordinates(params: &serde_json::Value) -> ApiResult<()> {
    let has_coordinate = |field: &str| params.get(field).is_some_and(|value| !value.is_null());
    if has_coordinate("latitude") == has_coordinate("longitude") {
        Ok(())
    } else {
        Err(ApiError::Parse(
            "latitude and longitude must both be set or both be left empty".to_owned(),
        ))
    }
}

fn required_field(params: &serde_json::Value, field: &str) -> ApiResult<String> {
    params
        .get(field)
        .and_then(serde_json::Value::as_str)
        .map(std::borrow::ToOwned::to_owned)
        .ok_or_else(|| ApiError::Parse(format!("{field} is required")))
}
