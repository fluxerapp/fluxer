// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::AdminApiClient,
        types::{
            CAPTCHA_COST_RANGE, CAPTCHA_MAX_COUNTER_RANGE, CaptchaConfigUpdateRequest,
            GatewayRolloutConfigUpdateRequest, GatewayRolloutMode,
            InstanceAttachmentDecayUpdateRequest, InstanceConfigUpdateRequest,
            InstanceMediaUpdateRequest, InstanceRegistrationConfigUpdateRequest,
            PushRelayConfigUpdateRequest, RegistrationMode, VoiceE2eeScope,
        },
    },
    config::AdminConfig,
    middleware::{
        csrf::CsrfToken,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates::pages::{
        instance_branding::CUSTOM_CSS_FIELD,
        instance_config::{
            LOADED_CONFIG_FIELD, REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD,
            SSO_ALLOWED_DOMAINS_FIELD,
        },
    },
    utils::forms::MultiValueForm,
};
use axum::{
    extract::{Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
};
use maud::Markup;
use serde::Deserialize;

mod accounts;
mod app_public;
mod branding_assets;
mod experiments;
mod integrations;
mod notices;
mod policy;
mod registration_urls;
mod sso;

#[derive(Deserialize)]
pub(super) struct ActionQuery {
    pub(super) action: Option<String>,
    pub(super) rule: Option<String>,
}

pub(super) fn redirect_back_with_flash(
    base: &str,
    path: &str,
    fd: FlashData,
    secure: bool,
) -> Response {
    flash::redirect_with_flash(&format!("{base}{path}"), fd, secure)
}

pub async fn gateway_post(
    State(state): State<AppState>,
    auth: axum::Extension<crate::middleware::auth::AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return redirect_back_with_flash(
                base,
                "/gateway",
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = if aq.action.as_deref() == Some("reload_all") {
        let ids = form.list_values_any(&["guild_ids[]", "guild_ids"]);
        match client.reload_all_guilds(&ids).await {
            Ok(_) => FlashData::success("Gateway action completed"),
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: reload all guilds");
                FlashData::error("Failed to reload gateway guilds")
            }
        }
    } else {
        FlashData::error("Unknown gateway action")
    };
    redirect_back_with_flash(base, "/gateway", flash, config.secure_cookies())
}

pub async fn search_index_post(
    State(state): State<AppState>,
    auth: axum::Extension<crate::middleware::auth::AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return redirect_back_with_flash(
                base,
                "/search-index",
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let index_type = form.clean("index_type");
    let guild_id = form.clean("guild_id");
    if let Some(idx_type) = index_type {
        return match client
            .refresh_search_index(&idx_type, guild_id.as_deref())
            .await
        {
            Ok(result) => {
                let job_id = &result.job_id;
                flash::redirect_with_flash(
                    &format!("{base}/search-index?job_id={job_id}"),
                    FlashData::success("Search index refresh started"),
                    config.secure_cookies(),
                )
            }
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: refresh search index");
                redirect_back_with_flash(
                    base,
                    "/search-index",
                    FlashData::error("Failed to start search index refresh"),
                    config.secure_cookies(),
                )
            }
        };
    }
    redirect_back_with_flash(
        base,
        "/search-index",
        FlashData::error("Index type is required"),
        config.secure_cookies(),
    )
}

enum UpdateFeedback {
    Standard,
    Field(&'static str, &'static str),
    Premium,
    Billing,
}

pub async fn instance_config_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<crate::middleware::auth::AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let branding_action = match aq.action.as_deref() {
        Some("upload_branding_asset") => Some(branding_assets::Action::Upload),
        Some("remove_branding_asset") => Some(branding_assets::Action::Remove),
        _ => None,
    };
    if let Some(action) = branding_action {
        return branding_assets::post(&state, &headers, &auth.0, action, request).await;
    }
    let config = state.config();
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            let flash = FlashData::error("Invalid form data");
            return instance_config_response(config, &headers, flash);
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let flash = 'action: {
        match action {
            "update_integrations" => {
                break 'action integrations::run(&client, &form, integrations::Action::Update)
                    .await;
            }
            "test_smtp" => {
                break 'action integrations::run(&client, &form, integrations::Action::Test).await;
            }
            _ => {}
        }
        let registration_action = match action {
            "create_registration_url" => Some(registration_urls::Action::Create),
            "rename_registration_url" => Some(registration_urls::Action::Rename),
            "revoke_registration_url" => Some(registration_urls::Action::Revoke),
            "remove_registration_url" => Some(registration_urls::Action::Remove),
            _ => None,
        };
        if let Some(action) = registration_action {
            match registration_urls::run(&client, &form, &headers, config, &csrf.0.0, action).await
            {
                registration_urls::Outcome::Flash(flash) => break 'action flash,
                registration_urls::Outcome::Response(response) => return response,
            }
        }
        let update = match action {
            "update_sso" => sso::build_sso_update(&form).map_err(FlashData::error),
            "update_gateway_rollout" => Ok(build_gateway_rollout_update(&form)),
            "update_registration" => Ok(build_registration_update(&form)),
            "update_app_public" => app_public::build_app_public_update(&form),
            "update_app_legal" => Ok(app_public::build_app_legal_update(&form)),
            "update_policy" => Ok(policy::build_policy_update(&form)),
            "update_media" => Ok(build_media_update(&form)),
            "update_billing" => {
                super::billing_actions::build_billing_update(&form).map_err(FlashData::error)
            }
            "update_push_relay" => Ok(build_push_relay_update(&form)),
            "update_domain_migration" => {
                experiments::build_domain_migration_update(&form).map_err(FlashData::error)
            }
            "update_voice_p2p" => {
                experiments::build_voice_p2p_update(&form).map_err(FlashData::error)
            }
            "update_captcha" => build_captcha_update(&form).map_err(FlashData::error),
            "update_accounts" => accounts::build_accounts_update(&form).map_err(FlashData::error),
            "update_guild_visits" => {
                experiments::build_guild_visits_update(&form).map_err(FlashData::error)
            }
            "update_experiment_delivery" => {
                experiments::build_experiment_delivery_update(&form).map_err(FlashData::error)
            }
            "disable_single_community" => Ok(policy::build_single_community_update(
                &form,
                policy::CommunityAction::Disable,
            )),
            "enable_single_community" => Ok(policy::build_single_community_update(
                &form,
                policy::CommunityAction::Enable,
            )),
            "update_single_community" => Ok(policy::build_single_community_update(
                &form,
                policy::CommunityAction::Update,
            )),
            "update_notice" => notices::build_notice_update(&form).map_err(FlashData::error),
            _ => Err(FlashData::error("Unknown instance config action")),
        };
        let update = match update {
            Ok(update) => update,
            Err(flash) => break 'action flash,
        };
        if let Some(loaded) = form
            .first(LOADED_CONFIG_FIELD)
            .filter(|loaded| !loaded.is_empty())
            && let Ok(current) = client.get_instance_config().await
            && current.changed_since(loaded, &update)
        {
            break 'action FlashData::error(
                "Someone else changed these settings after you opened this page. Reload the page to see their change, then make yours again.",
            );
        }
        let feedback = match action {
            "update_sso" => UpdateFeedback::Field("allowed_domains", SSO_ALLOWED_DOMAINS_FIELD),
            "update_registration" => UpdateFeedback::Field(
                "allowed_email_domains",
                REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD,
            ),
            "update_app_public" => {
                UpdateFeedback::Field("app_public.branding.custom_css", CUSTOM_CSS_FIELD)
            }
            "update_policy" => UpdateFeedback::Premium,
            "update_billing" => UpdateFeedback::Billing,
            _ => UpdateFeedback::Standard,
        };
        let result = client.update_instance_config(&update).await;
        if matches!(feedback, UpdateFeedback::Premium | UpdateFeedback::Billing) {
            remember_premium_branding(&state, &result);
        }
        let warnings = result
            .as_ref()
            .map(|saved| saved.warnings.clone())
            .unwrap_or_default();
        let flash = match feedback {
            UpdateFeedback::Field(api_path, field) => field_result(result, api_path, field),
            UpdateFeedback::Billing => super::billing_actions::billing_result(result),
            UpdateFeedback::Standard | UpdateFeedback::Premium => instance_config_result(result),
        };
        with_save_warnings(flash, &warnings)
    };
    state.forget_instance_settings();
    instance_config_response(config, &headers, flash)
}

fn instance_config_response(
    config: &AdminConfig,
    headers: &HeaderMap,
    flash: FlashData,
) -> Response {
    if htmx::is_htmx_request(headers) {
        return htmx::toast_response(&flash);
    }
    redirect_back_with_flash(
        &config.base_path,
        "/instance-config",
        flash,
        config.secure_cookies(),
    )
}

fn remember_premium_branding(
    state: &AppState,
    result: &Result<crate::api::types::InstanceConfigResponse, crate::api::client::ApiError>,
) {
    if let Ok(instance_config) = result {
        state.remember_instance_config_branding(instance_config);
    }
}

fn render_fragment_with_toast(markup: Markup, flash: &FlashData) -> Response {
    let mut response = Html(markup.into_string()).into_response();
    htmx::add_toast_header(&mut response, flash);
    response
}

fn with_api_reason(summary: &str, error: &crate::api::client::ApiError) -> String {
    match error.detail_message() {
        Some(reason) => format!("{summary}: {reason}"),
        None => summary.to_owned(),
    }
}

fn with_save_warnings(flash: FlashData, warnings: &[String]) -> FlashData {
    if warnings.is_empty() {
        return flash;
    }
    FlashData::info(format!("Instance config updated. {}", warnings.join(" ")))
}

fn instance_config_result<T>(result: Result<T, crate::api::client::ApiError>) -> FlashData {
    match result {
        Ok(_) => FlashData::success("Instance config updated"),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: update instance config");
            match error.validation_message() {
                Some(message) => {
                    FlashData::error(format!("Failed to update instance config: {message}"))
                }
                None => FlashData::error("Failed to update instance config"),
            }
        }
    }
}

fn field_result<T>(
    result: Result<T, crate::api::client::ApiError>,
    api_path: &str,
    field: &str,
) -> FlashData {
    let errors = result
        .as_ref()
        .err()
        .map(crate::api::client::ApiError::field_errors)
        .unwrap_or_default();
    let field_error = errors
        .iter()
        .find(|(path, _)| path == api_path)
        .map(|(_, message)| message.clone());
    let flash = instance_config_result(result);
    match field_error {
        Some(message) if errors.len() == 1 => {
            FlashData::error(message.clone()).with_field_error(field, message)
        }
        Some(message) => flash.with_field_error(field, message),
        None => flash,
    }
}

fn build_gateway_rollout_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let get_f64 = |key: &str| {
        form.first(key)
            .and_then(|value| value.trim().parse::<f64>().ok())
    };
    let session_rollout_mode = match form.first("gateway_rollout_session_rollout_mode") {
        Some("random") => Some(GatewayRolloutMode::Random),
        Some("modulo") => Some(GatewayRolloutMode::Modulo),
        _ => None,
    };
    let voice_e2ee_scope = match form.first("gateway_rollout_voice_e2ee_scope") {
        Some("platform_wide") => Some(VoiceE2eeScope::PlatformWide),
        Some("guild_feature_only") => Some(VoiceE2eeScope::GuildFeatureOnly),
        _ => None,
    };
    InstanceConfigUpdateRequest {
        gateway_rollout: Some(GatewayRolloutConfigUpdateRequest {
            session_rollout_percentage: get_f64("gateway_rollout_session_rollout_percentage"),
            session_rollout_mode,
            guild_rollout_percentage: get_f64("gateway_rollout_guild_rollout_percentage"),
            rpc_request_timeout_ms: form.parse_u64("gateway_rollout_rpc_request_timeout_ms"),
            max_concurrent_session_starts: form
                .parse_u64("gateway_rollout_max_concurrent_session_starts"),
            max_concurrent_guild_starts: form
                .parse_u64("gateway_rollout_max_concurrent_guild_starts"),
            voice_e2ee_scope,
        }),
        ..Default::default()
    }
}

fn parse_form_number<T>(
    form: &MultiValueForm,
    key: &str,
    label: &str,
    min: T,
    max: T,
) -> Result<Option<T>, String>
where
    T: std::str::FromStr + Ord + std::fmt::Display,
{
    let Some(raw) = form.first(key) else {
        return Ok(None);
    };
    let invalid = || format!("{label} must be a whole number between {min} and {max}");
    let value = raw.trim().parse::<T>().map_err(|_| invalid())?;
    if value < min || value > max {
        return Err(invalid());
    }
    Ok(Some(value))
}

fn build_push_relay_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    InstanceConfigUpdateRequest {
        push_relay: Some(PushRelayConfigUpdateRequest {
            relay_consent_accepted: Some(form.bool_value("push_relay_consent_accepted")),
        }),
        ..Default::default()
    }
}

fn build_captcha_update(form: &MultiValueForm) -> Result<InstanceConfigUpdateRequest, String> {
    Ok(InstanceConfigUpdateRequest {
        captcha: Some(CaptchaConfigUpdateRequest {
            provider: parse_choice(form, "captcha_provider", "Check")?,
            cost: parse_form_number(
                form,
                "captcha_cost",
                "Cost",
                *CAPTCHA_COST_RANGE.start(),
                *CAPTCHA_COST_RANGE.end(),
            )?,
            max_counter: parse_form_number(
                form,
                "captcha_max_counter",
                "Maximum counter",
                *CAPTCHA_MAX_COUNTER_RANGE.start(),
                *CAPTCHA_MAX_COUNTER_RANGE.end(),
            )?,
        }),
        ..Default::default()
    })
}

fn parse_choice<T: std::str::FromStr>(
    form: &MultiValueForm,
    key: &str,
    label: &str,
) -> Result<Option<T>, String> {
    match form.first(key) {
        None => Ok(None),
        Some(raw) => raw
            .trim()
            .parse()
            .map(Some)
            .map_err(|_| format!("{label} has an unknown value")),
    }
}

fn build_registration_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let mode = match form.first("registration_mode") {
        Some("approval") => Some(RegistrationMode::Approval),
        Some("closed") => Some(RegistrationMode::Closed),
        Some("open") => Some(RegistrationMode::Open),
        _ => None,
    };
    InstanceConfigUpdateRequest {
        registration: Some(InstanceRegistrationConfigUpdateRequest {
            mode,
            admin_registration_urls_enabled: Some(
                form.bool_value("admin_registration_urls_enabled"),
            ),
            allowed_email_domains: form
                .contains_key(REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD)
                .then(|| form.list_values(REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD)),
        }),
        ..Default::default()
    }
}

fn parse_tristate(form: &MultiValueForm, key: &str) -> Option<Option<bool>> {
    match form.first(key) {
        Some("inherit") => Some(None),
        Some("on") => Some(Some(true)),
        Some("off") => Some(Some(false)),
        _ => None,
    }
}

fn parse_inheritable<T: std::str::FromStr>(form: &MultiValueForm, key: &str) -> Option<Option<T>> {
    let value = form.first(key)?.trim();
    if value.is_empty() {
        return Some(None);
    }
    value.parse().ok().map(Some)
}

fn build_media_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    InstanceConfigUpdateRequest {
        media: Some(InstanceMediaUpdateRequest {
            attachment_decay: Some(InstanceAttachmentDecayUpdateRequest {
                enabled: parse_tristate(form, "media_attachment_decay_enabled"),
                min_size_mb: parse_inheritable(form, "media_attachment_decay_min_size_mb"),
                max_size_mb: parse_inheritable(form, "media_attachment_decay_max_size_mb"),
                max_eligible_size_mb: parse_inheritable(
                    form,
                    "media_attachment_decay_max_eligible_size_mb",
                ),
                min_lifetime_days: parse_inheritable(
                    form,
                    "media_attachment_decay_min_lifetime_days",
                ),
                max_lifetime_days: parse_inheritable(
                    form,
                    "media_attachment_decay_max_lifetime_days",
                ),
                curve: parse_inheritable(form, "media_attachment_decay_curve"),
                renew_threshold_days: parse_inheritable(
                    form,
                    "media_attachment_decay_renew_threshold_days",
                ),
                renew_window_days: parse_inheritable(
                    form,
                    "media_attachment_decay_renew_window_days",
                ),
            }),
        }),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_save_with_warnings_shows_them_instead_of_plain_success() {
        let warnings = vec!["Couldn't connect to the sign-in provider.".to_owned()];
        let flash = with_save_warnings(FlashData::success("Instance config updated"), &warnings);
        assert_eq!(flash.flash_type, "info");
        assert_eq!(
            flash.message,
            "Instance config updated. Couldn't connect to the sign-in provider."
        );
    }
}
