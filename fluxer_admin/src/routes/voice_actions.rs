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
    insert_cleaned_fields(&mut body, form, &["id", "name", "emoji"]);
    body.insert("is_default".into(), form.bool_value("is_default").into());
    body.insert("vip_only".into(), form.bool_value("vip_only").into());
    insert_submitted_list(&mut body, form, "required_guild_features");
    insert_submitted_list(&mut body, form, "allowed_guild_ids");
    serde_json::Value::Object(body)
}

fn insert_cleaned_fields(
    body: &mut serde_json::Map<String, serde_json::Value>,
    form: &MultiValueForm,
    fields: &[&str],
) {
    for &field in fields {
        if let Some(value) = form.clean(field) {
            body.insert(field.to_owned(), value.into());
        }
    }
    for field in ["latitude", "longitude"] {
        if let Some(value) = form
            .clean(field)
            .and_then(|value| value.parse::<f64>().ok())
        {
            body.insert(field.to_owned(), value.into());
        }
    }
}

fn insert_submitted_list(
    body: &mut serde_json::Map<String, serde_json::Value>,
    form: &MultiValueForm,
    field: &str,
) {
    let repeated = format!("{field}[]");
    if !form.contains_key(&repeated) && !form.contains_key(field) {
        return;
    }
    body.insert(
        field.to_owned(),
        form.list_values_any(&[repeated.as_str(), field]).into(),
    );
}

pub(crate) fn build_server_body(form: &MultiValueForm) -> serde_json::Value {
    let mut body = serde_json::Map::new();
    insert_cleaned_fields(
        &mut body,
        form,
        &[
            "region_id",
            "server_id",
            "endpoint",
            "api_key",
            "api_secret",
        ],
    );
    body.insert("is_active".into(), form.bool_value("is_active").into());
    if let Some(raw) = form.first("soft_connection_limit") {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            body.insert("soft_connection_limit".into(), serde_json::Value::Null);
        } else if let Ok(limit) = trimmed.parse::<i64>()
            && limit > 0
        {
            body.insert("soft_connection_limit".into(), limit.into());
        }
    }
    if let Some(raw) = form.first("notes") {
        let trimmed = raw.trim();
        body.insert(
            "notes".into(),
            if trimmed.is_empty() {
                serde_json::Value::Null
            } else {
                trimmed.into()
            },
        );
    }
    body.insert("vip_only".into(), form.bool_value("vip_only").into());
    insert_submitted_list(&mut body, form, "required_guild_features");
    insert_submitted_list(&mut body, form, "allowed_guild_ids");
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
