// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::{AdminApiClient, ApiError},
        types::{
            BADGE_IMAGE_MAX_BYTES, BADGE_IMAGE_TYPES, BadgeListQuery, BadgeMove, BadgeSettings,
            CustomBadgeFields, CustomBadgeType,
        },
    },
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
    },
    state::AppState,
    templates::{
        self,
        pages::badges::{BADGE_PAGE_SIZE, BadgePageState},
    },
};
use axum::{
    Form, Router,
    extract::{DefaultBodyLimit, Multipart, Path, Query, Request, State},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use base64::{Engine as _, engine::general_purpose::STANDARD};
use serde::Deserialize;

#[derive(Debug, Default, Deserialize)]
pub struct BadgePageQuery {
    #[serde(default, rename = "type")]
    badge_type: Option<String>,
    #[serde(default)]
    q: Option<String>,
    #[serde(default)]
    after: Option<String>,
    #[serde(default)]
    trail: Option<String>,
}

impl BadgePageQuery {
    fn back(&self, config: &crate::config::AdminConfig) -> String {
        format!(
            "{}/badges{}",
            config.base_path,
            self.state().current_query()
        )
    }

    fn state(&self) -> BadgePageState {
        BadgePageState::parse(
            self.badge_type.as_deref(),
            self.q.as_deref(),
            self.after.as_deref(),
            self.trail.as_deref(),
        )
    }
}

#[derive(Debug, Deserialize)]
struct BadgeSettingsForm {
    #[serde(default)]
    max_per_user: Option<String>,
    #[serde(default)]
    max_per_guild: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/badges", get(badges_page).post(create_badge))
        .route("/badges/settings", post(update_settings))
        .route("/badges/{badge_id}", post(update_badge))
        .route("/badges/{badge_id}/delete", post(delete_badge))
        .route("/badges/{badge_id}/move/{direction}", post(move_badge))
        .route("/badges/built-in/{kind}", post(replace_built_in_image))
        .route("/badges/built-in/{kind}/reset", post(reset_built_in_image))
        .layer(DefaultBodyLimit::max(BADGE_IMAGE_MAX_BYTES + 64 * 1024))
}

async fn badges_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<BadgePageQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let page = query.state();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client
        .list_badges(&BadgeListQuery {
            badge_type: Some(page.badge_type),
            query: page.query.clone(),
            after: page.after.clone(),
            limit: Some(BADGE_PAGE_SIZE),
        })
        .await;
    if let Err(error) = &result {
        tracing::warn!(%error, "admin API request failed: list badges");
    }
    let markup = templates::pages::badges::badges_page(
        config,
        &auth.0,
        result.as_ref().ok(),
        &page,
        &csrf_token,
    );
    Html(markup.into_string()).into_response()
}

fn parse_limit(value: Option<&str>, ceiling: u32) -> Option<u32> {
    value
        .map(str::trim)
        .and_then(|value| value.parse::<u32>().ok())
        .filter(|value| (1..=ceiling).contains(value))
}

async fn update_settings(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<BadgePageQuery>,
    Form(form): Form<BadgeSettingsForm>,
) -> Response {
    let config = state.config();
    let back = query.back(config);
    let (Some(max_per_user), Some(max_per_guild)) = (
        parse_limit(
            form.max_per_user.as_deref(),
            crate::api::types::BADGES_PER_USER_CEILING,
        ),
        parse_limit(
            form.max_per_guild.as_deref(),
            crate::api::types::BADGES_PER_GUILD_CEILING,
        ),
    ) else {
        return flash::redirect_error(
            config,
            &back,
            "Enter a limit from 1 to 1000 for users and from 1 to 200 for guilds",
        );
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .update_badge_settings(BadgeSettings {
            max_per_user,
            max_per_guild,
        })
        .await
    {
        Ok(_) => FlashData::success("Badge limits saved"),
        Err(error) => failure("save the limits of", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
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
    Query(query): Query<BadgePageQuery>,
    multipart: Multipart,
) -> Response {
    let config = state.config();
    let back = query.back(config);
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
    Query(query): Query<BadgePageQuery>,
    multipart: Multipart,
) -> Response {
    let config = state.config();
    let back = query.back(config);
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
    Query(query): Query<BadgePageQuery>,
) -> Response {
    let config = state.config();
    let back = query.back(config);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.delete_badge(&badge_id).await {
        Ok(()) => FlashData::success("Badge deleted"),
        Err(error) => failure("delete", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn replace_built_in_image(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(kind): Path<String>,
    Query(query): Query<BadgePageQuery>,
    mut multipart: Multipart,
) -> Response {
    let config = state.config();
    let back = query.back(config);
    let mut image = None;
    while let Ok(Some(field)) = multipart.next_field().await {
        if field.name() != Some("image") {
            continue;
        }
        let content_type = field.content_type().unwrap_or_default().to_owned();
        let Ok(bytes) = field.bytes().await else {
            return flash::redirect_error(config, &back, "The image must be under 10 MB");
        };
        if bytes.is_empty() {
            continue;
        }
        if !BADGE_IMAGE_TYPES.contains(&content_type.as_str()) {
            return flash::redirect_error(
                config,
                &back,
                "Choose a PNG, JPEG, GIF, WebP or SVG image",
            );
        }
        image = Some(format!(
            "data:{content_type};base64,{}",
            STANDARD.encode(&bytes)
        ));
    }
    let Some(image) = image else {
        return flash::redirect_error(config, &back, "Choose an image to upload");
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.replace_built_in_badge_image(&kind, &image).await {
        Ok(_) => FlashData::success("Badge image replaced"),
        Err(error) => failure("replace the image of", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn reset_built_in_image(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(kind): Path<String>,
    Query(query): Query<BadgePageQuery>,
) -> Response {
    let config = state.config();
    let back = query.back(config);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.reset_built_in_badge_image(&kind).await {
        Ok(_) => FlashData::success("Badge uses the built-in image again"),
        Err(error) => failure("reset the image of", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn move_badge(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path((badge_id, direction)): Path<(String, String)>,
    Query(query): Query<BadgePageQuery>,
) -> Response {
    let config = state.config();
    let back = query.back(config);
    let Some(direction) = BadgeMove::parse(&direction) else {
        return flash::redirect_error(config, &back, "Choose up, down, top or bottom");
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.move_badge(&badge_id, direction).await {
        Ok(_) => FlashData::success("Badge moved"),
        Err(error) => failure("move", &error),
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}
