// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::{AdminApiClient, ApiError},
        types::{BADGE_IMAGE_MAX_BYTES, BADGE_IMAGE_TYPES, CustomBadgeFields, CustomBadgeType},
    },
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
    },
    state::AppState,
    templates,
};
use axum::{
    Router,
    extract::{DefaultBodyLimit, Multipart, Path, Request, State},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use base64::{Engine as _, engine::general_purpose::STANDARD};

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/badges", get(badges_page).post(create_badge))
        .route("/badges/{badge_id}", post(update_badge))
        .route("/badges/{badge_id}/delete", post(delete_badge))
        .layer(DefaultBodyLimit::max(BADGE_IMAGE_MAX_BYTES + 64 * 1024))
}

async fn badges_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.list_badges().await;
    if let Err(error) = &result {
        tracing::warn!(%error, "admin API request failed: list badges");
    }
    let markup =
        templates::pages::badges::badges_page(config, &auth.0, result.as_ref().ok(), &csrf_token);
    Html(markup.into_string()).into_response()
}

struct BadgeForm {
    badge_type: Option<CustomBadgeType>,
    fields: CustomBadgeFields,
}

fn optional_text(value: String) -> Option<String> {
    let trimmed = value.trim();
    (!trimmed.is_empty()).then(|| trimmed.to_owned())
}

async fn read_badge_form(mut multipart: Multipart) -> Result<BadgeForm, &'static str> {
    let mut badge_type = None;
    let mut name = String::new();
    let mut description = None;
    let mut href = None;
    let mut image = None;
    while let Some(field) = multipart
        .next_field()
        .await
        .map_err(|_| "Invalid form data")?
    {
        let field_name = field.name().unwrap_or_default().to_owned();
        if field_name == "image" {
            let content_type = field.content_type().unwrap_or_default().to_owned();
            let bytes = field
                .bytes()
                .await
                .map_err(|_| "The image must be under 10 MB")?;
            if bytes.is_empty() {
                continue;
            }
            if !BADGE_IMAGE_TYPES.contains(&content_type.as_str()) {
                return Err("Choose a PNG, JPEG, GIF, WebP or SVG image");
            }
            image = Some(format!(
                "data:{content_type};base64,{}",
                STANDARD.encode(&bytes)
            ));
            continue;
        }
        let value = field.text().await.map_err(|_| "Invalid form data")?;
        match field_name.as_str() {
            "type" => badge_type = CustomBadgeType::parse(value.trim()),
            "name" => name = value.trim().to_owned(),
            "description" => description = optional_text(value),
            "href" => href = optional_text(value),
            _ => {}
        }
    }
    if name.is_empty() {
        return Err("Enter a name for the badge");
    }
    Ok(BadgeForm {
        badge_type,
        fields: CustomBadgeFields {
            name,
            description,
            href,
            image,
        },
    })
}

fn failure(action: &str, error: &ApiError) -> FlashData {
    tracing::warn!(%error, "admin API request failed: {action} badge");
    match error
        .validation_message()
        .or_else(|| error.detail_message())
    {
        Some(message) => FlashData::error(format!("Failed to {action} the badge: {message}")),
        None => FlashData::error(format!("Failed to {action} the badge")),
    }
}

async fn create_badge(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    multipart: Multipart,
) -> Response {
    let config = state.config();
    let back = format!("{}/badges", config.base_path);
    let form = match read_badge_form(multipart).await {
        Ok(form) => form,
        Err(message) => return flash::redirect_error(config, &back, message),
    };
    let Some(badge_type) = form.badge_type else {
        return flash::redirect_error(config, &back, "Choose who the badge is for");
    };
    if form.fields.image.is_none() {
        return flash::redirect_error(config, &back, "Choose an image to upload");
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.create_badge(badge_type, form.fields).await {
        Ok(_) => FlashData::success("Badge created"),
        Err(error) => failure("create", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn update_badge(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(badge_id): Path<String>,
    multipart: Multipart,
) -> Response {
    let config = state.config();
    let back = format!("{}/badges", config.base_path);
    let form = match read_badge_form(multipart).await {
        Ok(form) => form,
        Err(message) => return flash::redirect_error(config, &back, message),
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.update_badge(&badge_id, form.fields).await {
        Ok(_) => FlashData::success("Badge saved"),
        Err(error) => failure("save", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn delete_badge(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(badge_id): Path<String>,
) -> Response {
    let config = state.config();
    let back = format!("{}/badges", config.base_path);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.delete_badge(&badge_id).await {
        Ok(()) => FlashData::success("Badge deleted"),
        Err(error) => failure("delete", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}
