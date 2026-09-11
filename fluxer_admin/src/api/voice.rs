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
        let body = voice_request_body(params, &["id"])?;
        validate_against::<generated_types::UpdateVoiceRegionRequestBody>(&body)?;
        self.patch_with_reason(
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
        self.patch_with_reason(
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn region_update_body_preserves_explicit_restriction_clears() {
        let expected = json!({
            "required_guild_features": [],
            "allowed_guild_ids": [],
            "allowed_user_ids": [],
        });
        let mut params = expected.clone();
        params["id"] = json!("eu");
        let body = voice_request_body(&params, &["id"]).expect("region body");
        validate_against::<generated_types::UpdateVoiceRegionRequestBody>(&body)
            .expect("valid region body");
        assert_eq!(body, expected);
    }

    #[test]
    fn server_update_body_preserves_clears_and_omitted_restrictions() {
        for expected in [
            json!({
                "required_guild_features": [],
                "allowed_guild_ids": [],
                "allowed_user_ids": [],
                "soft_connection_limit": null,
                "latitude": null,
                "longitude": null,
            }),
            json!({"is_active": false}),
        ] {
            let mut params = expected.clone();
            params["region_id"] = json!("eu");
            params["server_id"] = json!("primary");
            let body =
                voice_request_body(&params, &["region_id", "server_id"]).expect("server body");
            validate_against::<generated_types::UpdateVoiceServerRequestBody>(&body)
                .expect("valid server body");
            assert_eq!(body, expected);
        }
    }

    #[test]
    fn create_server_body_keeps_the_server_id_and_rejects_missing_fields() {
        let expected = json!({
            "server_id": "primary",
            "endpoint": "wss://voice.example.com",
            "api_key": "key",
            "api_secret": "secret",
        });
        let mut params = expected.clone();
        params["region_id"] = json!("eu");
        let body = voice_request_body(&params, &["region_id"]).expect("server body");
        validate_against::<generated_types::CreateVoiceServerRequestBody>(&body)
            .expect("valid server body");
        assert_eq!(body, expected);
        assert!(
            validate_against::<generated_types::CreateVoiceServerRequestBody>(&json!({})).is_err()
        );
    }
}
