// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiError, ApiResult},
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
    },
    state::AppState,
    templates,
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Query, Request, State},
    response::{Html, IntoResponse, Redirect, Response},
    routing::get,
};

use super::ActionQuery;

pub fn router() -> Router<AppState> {
    Router::new().route("/badges", get(badges_page).post(badges_post))
}

async fn badges_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    flash: Option<axum::Extension<FlashData>>,
    request: Request,
) -> Response {
    let config = state.config();
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let csrf_token = csrf::get_csrf_token(&request);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.list_badges().await;
    if let Ok(badges) = &result {
        state.remember_badges(badges.clone());
    }
    let error = result.as_ref().err().map(|e| format!("{e:?}"));
    let markup = templates::pages::badges::badges_page(
        config,
        &auth.0,
        result.as_ref().ok(),
        error.as_deref(),
        flash.as_ref(),
        &csrf_token,
    );
    Html(markup.into_string()).into_response()
}

pub(crate) fn build_badge_body(form: &MultiValueForm) -> serde_json::Value {
    let mut body = serde_json::Map::new();
    for field in ["type", "name", "tooltip", "icon"] {
        if let Some(value) = form.clean(field) {
            body.insert(field.into(), value.into());
        }
    }
    if form.contains_key("url") {
        body.insert(
            "url".into(),
            form.clean("url")
                .map_or(serde_json::Value::Null, serde_json::Value::from),
        );
    }
    if let Some(position) = form.parse_u32("position") {
        body.insert("position".into(), position.into());
    }
    serde_json::Value::Object(body)
}

fn required(form: &MultiValueForm, field: &str) -> ApiResult<String> {
    form.clean(field)
        .ok_or_else(|| ApiError::Parse(format!("{field} is required")))
}

async fn apply_badge_action(
    client: &AdminApiClient,
    action: &str,
    form: &MultiValueForm,
) -> ApiResult<&'static str> {
    match action {
        "create" => client
            .create_badge(&build_badge_body(form))
            .await
            .map(|_| "Badge created"),
        "update" => client
            .update_badge(&required(form, "id")?, &build_badge_body(form))
            .await
            .map(|_| "Badge updated"),
        "delete" => client
            .delete_badge(&required(form, "id")?)
            .await
            .map(|_| "Badge deleted"),
        "update_builtin" => client
            .update_builtin_badge_icon(&required(form, "badge")?, Some(&required(form, "icon")?))
            .await
            .map(|_| "Badge icon updated"),
        "reset_builtin" => client
            .update_builtin_badge_icon(&required(form, "badge")?, None)
            .await
            .map(|_| "Default badge icon restored"),
        _ => Err(ApiError::Parse("Unknown action".to_owned())),
    }
}

async fn badges_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let redirect = format!("{}/badges", config.base_path);
    let Some(form) = MultiValueForm::from_request(request).await else {
        return Redirect::to(&redirect).into_response();
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match apply_badge_action(&client, aq.action.as_deref().unwrap_or(""), &form).await {
        Ok(message) => FlashData::success(message),
        Err(e) => FlashData::error(format!("Failed to update badges: {e:?}")),
    };
    flash::redirect_with_flash(&redirect, flash, config.secure_cookies())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn badge_body_keeps_submitted_fields_and_clears_blank_urls() {
        let form = MultiValueForm::parse(
            b"type=user&name=+Staff+&tooltip=Staff&icon=%3Csvg%2F%3E&url=&position=2",
        );
        assert_eq!(
            build_badge_body(&form),
            json!({
                "type": "user",
                "name": "Staff",
                "tooltip": "Staff",
                "icon": "<svg/>",
                "url": null,
                "position": 2,
            })
        );
    }

    #[test]
    fn badge_body_omits_fields_that_were_not_submitted() {
        let form = MultiValueForm::parse(b"name=Partner");
        assert_eq!(build_badge_body(&form), json!({"name": "Partner"}));
    }
}
