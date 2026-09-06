// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient,
    middleware::{
        auth::AuthContext,
        flash::{self, FlashData},
    },
    state::AppState,
    utils::forms::MultiValueForm,
};
use axum::{
    extract::{Query, Request, State},
    response::{IntoResponse, Redirect, Response},
};

use super::ActionQuery;

fn flash_from_level(level: &str, msg: &str) -> FlashData {
    if level == "success" {
        FlashData::success(msg)
    } else {
        FlashData::error(msg)
    }
}

pub(crate) fn build_region_body(form: &MultiValueForm) -> serde_json::Value {
    let mut body = serde_json::Map::new();
    if let Some(id) = form.clean("id") {
        body.insert("id".into(), id.into());
    }
    if let Some(name) = form.clean("name") {
        body.insert("name".into(), name.into());
    }
    if let Some(emoji) = form.clean("emoji") {
        body.insert("emoji".into(), emoji.into());
    }
    if let Some(lat) = form
        .clean("latitude")
        .and_then(|value| value.parse::<f64>().ok())
    {
        body.insert("latitude".into(), lat.into());
    }
    if let Some(lng) = form
        .clean("longitude")
        .and_then(|value| value.parse::<f64>().ok())
    {
        body.insert("longitude".into(), lng.into());
    }
    body.insert("is_default".into(), form.bool_value("is_default").into());
    body.insert("vip_only".into(), form.bool_value("vip_only").into());
    body.insert(
        "required_guild_features".into(),
        form.list_values_any(&["required_guild_features[]", "required_guild_features"])
            .into(),
    );
    body.insert(
        "allowed_guild_ids".into(),
        form.list_values_any(&["allowed_guild_ids[]", "allowed_guild_ids"])
            .into(),
    );
    serde_json::Value::Object(body)
}

pub(crate) fn build_server_body(form: &MultiValueForm) -> serde_json::Value {
    let mut body = serde_json::Map::new();
    if let Some(v) = form.clean("region_id") {
        body.insert("region_id".into(), v.into());
    }
    if let Some(v) = form.clean("server_id") {
        body.insert("server_id".into(), v.into());
    }
    if let Some(v) = form.clean("endpoint") {
        body.insert("endpoint".into(), v.into());
    }
    if let Some(v) = form.clean("api_key") {
        body.insert("api_key".into(), v.into());
    }
    if let Some(v) = form.clean("api_secret") {
        body.insert("api_secret".into(), v.into());
    }
    if let Some(lat) = form
        .clean("latitude")
        .and_then(|value| value.parse::<f64>().ok())
    {
        body.insert("latitude".into(), lat.into());
    }
    if let Some(lng) = form
        .clean("longitude")
        .and_then(|value| value.parse::<f64>().ok())
    {
        body.insert("longitude".into(), lng.into());
    }
    body.insert("is_active".into(), form.bool_value("is_active").into());
    body.insert("vip_only".into(), form.bool_value("vip_only").into());
    body.insert(
        "required_guild_features".into(),
        form.list_values_any(&["required_guild_features[]", "required_guild_features"])
            .into(),
    );
    body.insert(
        "allowed_guild_ids".into(),
        form.list_values_any(&["allowed_guild_ids[]", "allowed_guild_ids"])
            .into(),
    );
    serde_json::Value::Object(body)
}

pub(crate) async fn voice_regions_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => return Redirect::to(&format!("{base}/voice-regions")).into_response(),
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");

    let (msg, level) = match action {
        "create" => {
            let body = build_region_body(&form);
            if body.get("id").and_then(|v| v.as_str()).is_some() {
                match client.create_voice_region(&body).await {
                    Ok(_) => ("Voice region created".to_string(), "success"),
                    Err(e) => (format!("Failed to create region: {e:?}"), "error"),
                }
            } else {
                ("Region ID is required".to_string(), "error")
            }
        }
        "update" => {
            let body = build_region_body(&form);
            if body.get("id").and_then(|v| v.as_str()).is_some() {
                match client.update_voice_region(&body).await {
                    Ok(_) => ("Voice region updated".to_string(), "success"),
                    Err(e) => (format!("Failed to update region: {e:?}"), "error"),
                }
            } else {
                ("Region ID is required".to_string(), "error")
            }
        }
        "delete" => {
            if let Some(id) = form.clean("id") {
                match client.delete_voice_region(&id).await {
                    Ok(_) => ("Voice region deleted".to_string(), "success"),
                    Err(e) => (format!("Failed to delete region: {e:?}"), "error"),
                }
            } else {
                ("Region ID is required".to_string(), "error")
            }
        }
        _ => ("Unknown action".to_string(), "error"),
    };

    flash::redirect_with_flash(
        &format!("{base}/voice-regions"),
        flash_from_level(level, &msg),
        config.secure_cookies(),
    )
}

pub(crate) async fn voice_servers_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => return Redirect::to(&format!("{base}/voice-servers")).into_response(),
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");

    let (msg, level) = match action {
        "create" => {
            let body = build_server_body(&form);
            let has_ids = body.get("region_id").and_then(|v| v.as_str()).is_some()
                && body.get("server_id").and_then(|v| v.as_str()).is_some();
            if has_ids {
                match client.create_voice_server(&body).await {
                    Ok(_) => ("Voice server created".to_string(), "success"),
                    Err(e) => (format!("Failed to create server: {e:?}"), "error"),
                }
            } else {
                ("Region ID and Server ID are required".to_string(), "error")
            }
        }
        "update" => {
            let body = build_server_body(&form);
            let has_ids = body.get("region_id").and_then(|v| v.as_str()).is_some()
                && body.get("server_id").and_then(|v| v.as_str()).is_some();
            if has_ids {
                match client.update_voice_server(&body).await {
                    Ok(_) => ("Voice server updated".to_string(), "success"),
                    Err(e) => (format!("Failed to update server: {e:?}"), "error"),
                }
            } else {
                ("Region ID and Server ID are required".to_string(), "error")
            }
        }
        "delete" => {
            if let (Some(region_id), Some(server_id)) =
                (form.clean("region_id"), form.clean("server_id"))
            {
                match client.delete_voice_server(&region_id, &server_id).await {
                    Ok(_) => ("Voice server deleted".to_string(), "success"),
                    Err(e) => (format!("Failed to delete server: {e:?}"), "error"),
                }
            } else {
                ("Region ID and Server ID are required".to_string(), "error")
            }
        }
        _ => ("Unknown action".to_string(), "error"),
    };

    let redirect_url = if let Some(region_id) = form.clean("region_id") {
        format!(
            "{base}/voice-servers?region_id={}",
            urlencoding::encode(&region_id)
        )
    } else {
        format!("{base}/voice-servers")
    };
    flash::redirect_with_flash(
        &redirect_url,
        flash_from_level(level, &msg),
        config.secure_cookies(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_region_body_preserves_repeated_lists() {
        let form = MultiValueForm::parse(
            b"id=us-east&name=US%20East&is_default=on&required_guild_features%5B%5D=VIP&required_guild_features%5B%5D=VOICE&allowed_guild_ids%5B%5D=1&allowed_guild_ids%5B%5D=2",
        );
        let body = build_region_body(&form);
        assert_eq!(body["id"], serde_json::json!("us-east"));
        assert_eq!(body["is_default"], serde_json::json!(true));
        assert_eq!(
            body["required_guild_features"],
            serde_json::json!(["VIP", "VOICE"])
        );
        assert_eq!(body["allowed_guild_ids"], serde_json::json!(["1", "2"]));
    }

    #[test]
    fn build_server_body_preserves_single_and_repeated_values() {
        let form = MultiValueForm::parse(
            b"region_id=us-east&server_id=s1&endpoint=wss%3A%2F%2Fvoice.example&is_active=true&vip_only=true&required_guild_features=VIP%2CVOICE&allowed_guild_ids%5B%5D=1&allowed_guild_ids%5B%5D=2",
        );
        let body = build_server_body(&form);
        assert_eq!(body["region_id"], serde_json::json!("us-east"));
        assert_eq!(body["endpoint"], serde_json::json!("wss://voice.example"));
        assert_eq!(body["is_active"], serde_json::json!(true));
        assert_eq!(body["vip_only"], serde_json::json!(true));
        assert_eq!(
            body["required_guild_features"],
            serde_json::json!(["VIP", "VOICE"])
        );
        assert_eq!(body["allowed_guild_ids"], serde_json::json!(["1", "2"]));
    }
}
