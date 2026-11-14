// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{instance_config_response, remember_premium_branding, render_fragment_with_toast};
use crate::{
    api::{
        client::AdminApiClient,
        types::{BRANDING_ASSET_MAX_IMAGE_CHARS, BrandingAssetKind},
    },
    middleware::{auth::AuthContext, flash::FlashData, htmx},
    state::AppState,
    templates,
    utils::forms::clean_string,
};
use axum::{body, extract::Request, http::HeaderMap, response::Response};
use url::form_urlencoded;

const BRANDING_ASSET_FORM_BYTES: usize = BRANDING_ASSET_MAX_IMAGE_CHARS * 3 + 64 * 1024;

#[derive(Clone, Copy)]
pub(super) enum Action {
    Upload,
    Remove,
}

pub(super) async fn post(
    state: &AppState,
    headers: &HeaderMap,
    auth: &AuthContext,
    action: Action,
    request: Request,
) -> Response {
    let config = state.config();
    let form = body::to_bytes(request.into_body(), BRANDING_ASSET_FORM_BYTES)
        .await
        .ok()
        .map(|bytes| BrandingAssetForm::parse(&bytes));
    let (kind, image) = match build_branding_asset_change(form, action) {
        Ok(change) => change,
        Err(message) => {
            return instance_config_response(config, headers, FlashData::error(message));
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.session);
    let result = client.upload_branding_asset(kind, image).await;
    remember_premium_branding(state, &result);
    let instance_config = match result {
        Ok(instance_config) => instance_config,
        Err(error) => {
            return instance_config_response(
                config,
                headers,
                branding_asset_failure(kind, action, &error),
            );
        }
    };
    let flash = branding_asset_success(kind, action);
    if htmx::targets(
        headers,
        &templates::pages::instance_branding::branding_asset_field_id(kind),
    ) {
        return render_fragment_with_toast(
            templates::pages::instance_branding::branding_asset_field(
                config,
                kind,
                kind.url(&instance_config.app_public.branding),
            ),
            &flash,
        );
    }
    instance_config_response(config, headers, flash)
}

struct BrandingAssetForm {
    kind: Option<String>,
    image: Option<String>,
}

impl BrandingAssetForm {
    fn parse(bytes: &[u8]) -> Self {
        let mut form = Self {
            kind: None,
            image: None,
        };
        for (key, value) in form_urlencoded::parse(bytes) {
            let slot = match key.as_ref() {
                "kind" => &mut form.kind,
                "image" => &mut form.image,
                _ => continue,
            };
            if slot.is_none() {
                *slot = Some(value.into_owned());
            }
        }
        form
    }
}

fn build_branding_asset_change(
    form: Option<BrandingAssetForm>,
    action: Action,
) -> Result<(BrandingAssetKind, Option<String>), &'static str> {
    let form = form.ok_or("Invalid form data")?;
    let kind = form
        .kind
        .as_deref()
        .and_then(BrandingAssetKind::parse)
        .ok_or("Unknown branding asset")?;
    if matches!(action, Action::Remove) {
        return Ok((kind, None));
    }
    let image = form
        .image
        .as_deref()
        .and_then(clean_string)
        .ok_or("Choose an image to upload")?;
    Ok((kind, Some(image)))
}

fn branding_asset_success(kind: BrandingAssetKind, action: Action) -> FlashData {
    let verb = match action {
        Action::Upload => "uploaded",
        Action::Remove => "removed",
    };
    FlashData::success(format!("{} {verb}", kind.label()))
}

fn branding_asset_failure(
    kind: BrandingAssetKind,
    action: Action,
    error: &crate::api::client::ApiError,
) -> FlashData {
    let verb = match action {
        Action::Upload => "upload",
        Action::Remove => "remove",
    };
    let label = kind.label().to_lowercase();
    tracing::warn!(%error, kind = kind.as_str(), "admin API request failed: {verb} branding asset");
    match error.validation_message() {
        Some(message) => FlashData::error(format!("Failed to {verb} the {label}: {message}")),
        None => FlashData::error(format!("Failed to {verb} the {label}")),
    }
}
