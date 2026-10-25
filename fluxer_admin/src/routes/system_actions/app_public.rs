// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        AppBrandingConfigUpdateRequest, AppLegalConfigUpdateRequest, AppPublicConfigUpdateRequest,
        AppSetupConfigUpdateRequest, CUSTOM_CSS_MAX_CHARS, InstanceConfigUpdateRequest,
    },
    middleware::flash::FlashData,
    templates::pages::instance_branding::{
        CUSTOM_CSS_FIELD, TRUSTED_DOMAINS_FIELD, UPLOAD_LIMIT_MESSAGE_FIELD,
    },
    utils::{
        forms::{MultiValueForm, clean_string},
        plural::group_digits,
    },
};

pub(super) fn build_app_public_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, FlashData> {
    let optional = |key: &str| Some(form.clean(key));
    let custom_css = if form.contains_key(CUSTOM_CSS_FIELD) {
        let css = form
            .first(CUSTOM_CSS_FIELD)
            .and_then(|value| clean_string(&value.replace("\r\n", "\n")));
        let length = css.as_deref().map_or(0, |css| css.encode_utf16().count());
        if length > CUSTOM_CSS_MAX_CHARS {
            let message = format!(
                "Custom CSS can be up to {} characters. This has {}.",
                group_digits(CUSTOM_CSS_MAX_CHARS),
                group_digits(length)
            );
            return Err(
                FlashData::error(message.clone()).with_field_error(CUSTOM_CSS_FIELD, message)
            );
        }
        Some(css)
    } else {
        None
    };
    Ok(InstanceConfigUpdateRequest {
        app_public: Some(AppPublicConfigUpdateRequest {
            branding: Some(AppBrandingConfigUpdateRequest {
                product_name: form.clean("app_product_name"),
                icon_url: optional("app_icon_url"),
                symbol_url: optional("app_symbol_url"),
                logo_url: optional("app_logo_url"),
                wordmark_url: optional("app_wordmark_url"),
                favicon_url: optional("app_favicon_url"),
                theme_color: optional("app_theme_color"),
                status_page_url: optional("app_status_page_url"),
                status_page_incident_history_url: optional("app_status_page_incident_history_url"),
                favicon_dark_url: form
                    .contains_key("app_favicon_dark_url")
                    .then(|| form.clean("app_favicon_dark_url")),
                sign_in_backdrop_url: form
                    .contains_key("app_sign_in_backdrop_url")
                    .then(|| form.clean("app_sign_in_backdrop_url")),
                custom_css,
                upload_limit_message: form
                    .contains_key(UPLOAD_LIMIT_MESSAGE_FIELD)
                    .then(|| form.clean(UPLOAD_LIMIT_MESSAGE_FIELD)),
                trusted_domains: form
                    .contains_key(TRUSTED_DOMAINS_FIELD)
                    .then(|| form.list_values(TRUSTED_DOMAINS_FIELD)),
                ..Default::default()
            }),
            setup: Some(AppSetupConfigUpdateRequest {
                configured: Some(form.bool_value("app_setup_configured")),
            }),
            legal: None,
        }),
        ..Default::default()
    })
}

pub(super) fn build_app_legal_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let optional = |key: &str| Some(form.clean(key));
    InstanceConfigUpdateRequest {
        app_public: Some(AppPublicConfigUpdateRequest {
            branding: None,
            setup: None,
            legal: Some(AppLegalConfigUpdateRequest {
                terms_url: optional("app_terms_url"),
                privacy_url: optional("app_privacy_url"),
                guidelines_url: optional("app_guidelines_url"),
            }),
        }),
        ..Default::default()
    }
}
