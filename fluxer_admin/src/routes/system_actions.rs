// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::AdminApiClient,
        types::{
            AppBrandingConfigUpdateRequest, AppLegalConfigUpdateRequest,
            AppPublicConfigUpdateRequest, AppRegistrationConfigUpdateRequest,
            AppSetupConfigUpdateRequest, CreateRegistrationUrlRequest,
            GatewayRolloutConfigUpdateRequest, GatewayRolloutMode,
            InstanceAttachmentDecayUpdateRequest, InstanceBlueskyIntegrationUpdateRequest,
            InstanceBlueskyKeyIntegrationUpdateRequest, InstanceCaptchaIntegrationUpdateRequest,
            InstanceConfigUpdateRequest, InstanceEmailIntegrationUpdateRequest,
            InstanceEmailSmtpIntegrationUpdateRequest, InstanceEmailSmtpTestRequest,
            InstanceGifIntegrationUpdateRequest, InstanceIntegrationsUpdateRequest,
            InstanceMediaUpdateRequest, InstancePolicyUpdateRequest,
            InstanceRegistrationConfigUpdateRequest, InstanceServicesUpdateRequest,
            InstanceYoutubeIntegrationUpdateRequest, LimitConfigUpdateRequest, LimitRule,
            LimitRuleFilters, PremiumMode, RegistrationMode, SsoConfigUpdateRequest,
            VoiceE2eeScope,
        },
    },
    config::AdminConfig,
    middleware::{
        csrf::CsrfToken,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates,
    utils::forms::{MultiValueForm, clean_string},
};
use axum::{
    extract::{Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
};
use maud::Markup;
use serde::Deserialize;

#[derive(Deserialize)]
pub struct ActionQuery {
    pub action: Option<String>,
    pub rule: Option<String>,
}

pub fn redirect_back_with_flash(base: &str, path: &str, fd: FlashData, prod: bool) -> Response {
    flash::redirect_with_flash(&format!("{base}{path}"), fd, prod)
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
                config.is_production(),
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
    redirect_back_with_flash(base, "/gateway", flash, config.is_production())
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
                config.is_production(),
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
                    config.is_production(),
                )
            }
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: refresh search index");
                redirect_back_with_flash(
                    base,
                    "/search-index",
                    FlashData::error("Failed to start search index refresh"),
                    config.is_production(),
                )
            }
        };
    }
    redirect_back_with_flash(
        base,
        "/search-index",
        FlashData::error("Index type is required"),
        config.is_production(),
    )
}

pub async fn instance_config_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<crate::middleware::auth::AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            let flash = FlashData::error("Invalid form data");
            if htmx::is_htmx_request(&headers) {
                return htmx::toast_response(&flash);
            }
            return redirect_back_with_flash(
                base,
                "/instance-config",
                flash,
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let flash = match action {
        "update_sso" => {
            let update = build_sso_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_gateway_rollout" => {
            let update = build_gateway_rollout_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_registration" => {
            let update = build_registration_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_app_public" => {
            let update = build_app_public_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_app_legal" => {
            let update = build_app_legal_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_app_registration" => {
            let update = build_app_registration_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_policy" => {
            let update = build_policy_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_integrations" => {
            let update = build_integrations_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "update_media" => {
            let update = build_media_update(&form);
            instance_config_result(client.update_instance_config(&update).await)
        }
        "test_smtp" => match build_smtp_test_request(&form) {
            Ok(request) => match client.test_instance_smtp_config(&request).await {
                Ok(response) if response.ok => FlashData::success("SMTP connection verified"),
                Ok(response) => FlashData::error(
                    response
                        .error
                        .unwrap_or_else(|| "SMTP validation failed".to_owned()),
                ),
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: test SMTP config");
                    FlashData::error("Failed to validate SMTP configuration")
                }
            },
            Err(message) => FlashData::error(message),
        },
        "disable_single_community" => {
            let update = build_disable_single_community_update();
            instance_config_result(client.update_instance_config(&update).await)
        }
        "create_registration_url" => match build_create_registration_url_request(&form) {
            Ok(request) => match client.create_registration_url(&request).await {
                Ok(response) => {
                    let flash = FlashData::success("Registration URL created");
                    if htmx::targets(&headers, "registration-url-list") {
                        return match client.get_instance_config().await {
                            Ok(instance_config) => render_registration_url_list_response(
                                config,
                                &csrf.0.0,
                                &instance_config,
                                &flash,
                            ),
                            Err(error) => {
                                tracing::warn!(%error, "admin API request failed: reload registration URLs");
                                htmx::toast_response(&FlashData::error(
                                    "Registration URL created, but failed to reload the list",
                                ))
                            }
                        };
                    }
                    FlashData::success(format!("Registration URL created: {}", response.url))
                }
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: create registration URL");
                    FlashData::error("Failed to create registration URL")
                }
            },
            Err(message) => FlashData::error(message),
        },
        "revoke_registration_url" => match form.clean("registration_url_id") {
            Some(id) => match client.revoke_registration_url(&id).await {
                Ok(instance_config) => {
                    let flash = FlashData::success("Registration URL revoked");
                    if htmx::targets(&headers, "registration-url-list") {
                        return render_registration_url_list_response(
                            config,
                            &csrf.0.0,
                            &instance_config,
                            &flash,
                        );
                    }
                    flash
                }
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: revoke registration URL");
                    FlashData::error("Failed to revoke registration URL")
                }
            },
            None => FlashData::error("Registration URL ID is required"),
        },
        "approve_pending_registration" => match form.clean("user_id") {
            Some(user_id) => match client.approve_pending_registration(&user_id).await {
                Ok(instance_config) => {
                    let flash = FlashData::success("Registration approved");
                    if htmx::targets(&headers, "pending-registration-list") {
                        return render_pending_registration_list_response(
                            config,
                            &csrf.0.0,
                            &instance_config,
                            &flash,
                        );
                    }
                    flash
                }
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: approve pending registration");
                    FlashData::error("Failed to approve registration")
                }
            },
            None => FlashData::error("User ID is required"),
        },
        "reject_pending_registration" => match form.clean("user_id") {
            Some(user_id) => match client.reject_pending_registration(&user_id).await {
                Ok(instance_config) => {
                    let flash = FlashData::success("Registration rejected");
                    if htmx::targets(&headers, "pending-registration-list") {
                        return render_pending_registration_list_response(
                            config,
                            &csrf.0.0,
                            &instance_config,
                            &flash,
                        );
                    }
                    flash
                }
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: reject pending registration");
                    FlashData::error("Failed to reject registration")
                }
            },
            None => FlashData::error("User ID is required"),
        },
        _ => FlashData::error("Unknown instance config action"),
    };
    if htmx::is_htmx_request(&headers) {
        return htmx::toast_response(&flash);
    }
    redirect_back_with_flash(base, "/instance-config", flash, config.is_production())
}

fn render_registration_url_list_response(
    config: &AdminConfig,
    csrf_token: &str,
    instance_config: &crate::api::types::InstanceConfigResponse,
    flash: &FlashData,
) -> Response {
    render_fragment_with_toast(
        templates::pages::instance_config::registration_url_list(
            config,
            csrf_token,
            &instance_config.registration.urls,
        ),
        flash,
    )
}

fn render_pending_registration_list_response(
    config: &AdminConfig,
    csrf_token: &str,
    instance_config: &crate::api::types::InstanceConfigResponse,
    flash: &FlashData,
) -> Response {
    render_fragment_with_toast(
        templates::pages::instance_config::pending_registration_list(
            config,
            csrf_token,
            &instance_config.registration.pending_registrations,
        ),
        flash,
    )
}

fn render_fragment_with_toast(markup: Markup, flash: &FlashData) -> Response {
    let mut response = Html(markup.into_string()).into_response();
    htmx::add_toast_header(&mut response, flash);
    response
}

fn instance_config_result<T, E: std::fmt::Display>(result: Result<T, E>) -> FlashData {
    match result {
        Ok(_) => FlashData::success("Instance config updated"),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: update instance config");
            FlashData::error("Failed to update instance config")
        }
    }
}

fn build_sso_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let flag = |key: &str| form.bool_value(key);
    let get = |key: &str| Some(form.clean(key));
    let new_secret = form.clean("sso_client_secret");
    let clear_secret = flag("sso_clear_client_secret");
    let allowed = form.list_values_any(&["sso_allowed_domains[]", "sso_allowed_domains"]);
    let client_secret = if new_secret.is_some() {
        Some(new_secret)
    } else if clear_secret {
        Some(None)
    } else {
        None
    };
    InstanceConfigUpdateRequest {
        sso: Some(SsoConfigUpdateRequest {
            enabled: Some(flag("sso_enabled")),
            enforced: Some(flag("sso_enforced")),
            auto_provision: Some(flag("sso_auto_provision")),
            display_name: get("sso_display_name"),
            issuer: get("sso_issuer"),
            authorization_url: get("sso_authorization_url"),
            token_url: get("sso_token_url"),
            userinfo_url: get("sso_userinfo_url"),
            jwks_url: get("sso_jwks_url"),
            client_id: get("sso_client_id"),
            client_secret,
            scope: get("sso_scope"),
            allowed_domains: Some(allowed),
            redirect_uri: None,
        }),
        gateway_rollout: None,
        registration: None,
        app_public: None,
        policy: None,
        integrations: None,
        media: None,
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
        sso: None,
        registration: None,
        app_public: None,
        policy: None,
        integrations: None,
        media: None,
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
        gateway_rollout: None,
        registration: Some(InstanceRegistrationConfigUpdateRequest {
            mode,
            admin_registration_urls_enabled: Some(
                form.bool_value("admin_registration_urls_enabled"),
            ),
        }),
        sso: None,
        app_public: None,
        policy: None,
        integrations: None,
        media: None,
    }
}

fn build_app_public_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let optional = |key: &str| Some(form.clean(key));
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: Some(AppPublicConfigUpdateRequest {
            branding: Some(AppBrandingConfigUpdateRequest {
                product_name: form.clean("app_product_name"),
                icon_url: optional("app_icon_url"),
                symbol_url: optional("app_symbol_url"),
                logo_url: optional("app_logo_url"),
                wordmark_url: optional("app_wordmark_url"),
                favicon_url: optional("app_favicon_url"),
                theme_color: optional("app_theme_color"),
            }),
            setup: Some(AppSetupConfigUpdateRequest {
                configured: Some(form.bool_value("app_setup_configured")),
            }),
            legal: None,
            registration: None,
        }),
        policy: None,
        integrations: None,
        media: None,
    }
}

fn build_app_legal_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let optional = |key: &str| Some(form.clean(key));
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: Some(AppPublicConfigUpdateRequest {
            branding: None,
            setup: None,
            legal: Some(AppLegalConfigUpdateRequest {
                terms_url: optional("app_terms_url"),
                privacy_url: optional("app_privacy_url"),
            }),
            registration: None,
        }),
        policy: None,
        integrations: None,
        media: None,
    }
}

fn build_app_registration_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: Some(AppPublicConfigUpdateRequest {
            branding: None,
            setup: None,
            legal: None,
            registration: Some(AppRegistrationConfigUpdateRequest {
                collect_date_of_birth: Some(form.bool_value("app_collect_date_of_birth")),
            }),
        }),
        policy: None,
        integrations: None,
        media: None,
    }
}

fn build_policy_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let direct_messages_disabled = form
        .first("policy_direct_messages_disabled")
        .map(|value| value == "true");
    let premium_mode = match form.first("policy_premium_mode") {
        Some("mirror") => Some(PremiumMode::Mirror),
        Some("everyone") => Some(PremiumMode::Everyone),
        _ => None,
    };
    let services = build_services_update(form);
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: None,
        policy: Some(InstancePolicyUpdateRequest {
            single_community_enabled: None,
            single_community_name: None,
            direct_messages_disabled,
            premium_mode,
            services,
        }),
        integrations: None,
        media: None,
    }
}

fn build_services_update(form: &MultiValueForm) -> Option<InstanceServicesUpdateRequest> {
    let parse_tristate = |key: &str| match form.first(key) {
        Some("inherit") => Some(None),
        Some("on") => Some(Some(true)),
        Some("off") => Some(Some(false)),
        _ => None,
    };
    let gif_enabled = parse_tristate("policy_service_gif");
    let youtube_enabled = parse_tristate("policy_service_youtube");
    let bluesky_enabled = parse_tristate("policy_service_bluesky");
    if gif_enabled.is_none() && youtube_enabled.is_none() && bluesky_enabled.is_none() {
        None
    } else {
        Some(InstanceServicesUpdateRequest {
            gif_enabled,
            youtube_enabled,
            bluesky_enabled,
        })
    }
}

fn build_integrations_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let clean = |key: &str| form.clean(key);
    let smtp_port = form
        .first("integration_smtp_port")
        .and_then(|value| value.trim().parse::<u16>().ok());
    let bluesky_key_id = clean("integration_bluesky_key_id");
    let bluesky_private_key = clean("integration_bluesky_private_key");
    let bluesky_keys = match (bluesky_key_id, bluesky_private_key) {
        (Some(kid), private_key) => Some(vec![InstanceBlueskyKeyIntegrationUpdateRequest {
            kid,
            private_key,
        }]),
        _ => None,
    };
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: None,
        policy: None,
        integrations: Some(InstanceIntegrationsUpdateRequest {
            gif: Some(InstanceGifIntegrationUpdateRequest {
                klipy_api_key: clean("integration_klipy_api_key"),
            }),
            youtube: Some(InstanceYoutubeIntegrationUpdateRequest {
                api_key: clean("integration_youtube_api_key"),
            }),
            captcha: Some(InstanceCaptchaIntegrationUpdateRequest {
                provider: clean("integration_captcha_provider"),
                hcaptcha_site_key: clean("integration_hcaptcha_site_key"),
                hcaptcha_secret_key: clean("integration_hcaptcha_secret_key"),
                turnstile_site_key: clean("integration_turnstile_site_key"),
                turnstile_secret_key: clean("integration_turnstile_secret_key"),
            }),
            email: Some(InstanceEmailIntegrationUpdateRequest {
                enabled: Some(form.bool_value("integration_email_enabled")),
                provider: Some("smtp".to_owned()),
                from_email: clean("integration_email_from_email"),
                from_name: clean("integration_email_from_name"),
                smtp: Some(InstanceEmailSmtpIntegrationUpdateRequest {
                    host: clean("integration_smtp_host"),
                    port: smtp_port,
                    username: clean("integration_smtp_username"),
                    password: clean("integration_smtp_password"),
                    secure: Some(form.bool_value("integration_smtp_secure")),
                }),
                disable_new_ip_authorization: Some(
                    form.bool_value("integration_email_disable_new_ip_authorization"),
                ),
            }),
            bluesky: Some(InstanceBlueskyIntegrationUpdateRequest {
                enabled: Some(form.bool_value("integration_bluesky_enabled")),
                client_name: clean("integration_bluesky_client_name"),
                client_uri: clean("integration_bluesky_client_uri"),
                logo_uri: clean("integration_bluesky_logo_uri"),
                tos_uri: clean("integration_bluesky_tos_uri"),
                policy_uri: clean("integration_bluesky_policy_uri"),
                keys: bluesky_keys,
            }),
        }),
        media: None,
    }
}

fn build_media_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let parse_f64 = |key: &str| {
        form.first(key)
            .and_then(|value| value.trim().parse::<f64>().ok())
    };
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: None,
        policy: None,
        integrations: None,
        media: Some(InstanceMediaUpdateRequest {
            attachment_decay: Some(InstanceAttachmentDecayUpdateRequest {
                enabled: Some(form.bool_value("media_attachment_decay_enabled")),
                min_size_mb: parse_f64("media_attachment_decay_min_size_mb"),
                max_size_mb: parse_f64("media_attachment_decay_max_size_mb"),
                max_eligible_size_mb: parse_f64("media_attachment_decay_max_eligible_size_mb"),
                min_lifetime_days: form.parse_u32("media_attachment_decay_min_lifetime_days"),
                max_lifetime_days: form.parse_u32("media_attachment_decay_max_lifetime_days"),
                curve: parse_f64("media_attachment_decay_curve"),
                renew_threshold_days: form.parse_u32("media_attachment_decay_renew_threshold_days"),
                renew_window_days: form.parse_u32("media_attachment_decay_renew_window_days"),
            }),
        }),
    }
}

fn build_smtp_test_request(form: &MultiValueForm) -> Result<InstanceEmailSmtpTestRequest, String> {
    let host = form
        .clean("integration_smtp_host")
        .ok_or_else(|| "SMTP host is required".to_owned())?;
    let port = form
        .first("integration_smtp_port")
        .and_then(|value| value.trim().parse::<u16>().ok())
        .ok_or_else(|| "SMTP port must be between 1 and 65535".to_owned())?;
    let username = form
        .clean("integration_smtp_username")
        .ok_or_else(|| "SMTP username is required".to_owned())?;
    let password = form
        .clean("integration_smtp_password")
        .ok_or_else(|| "SMTP password is required for validation".to_owned())?;
    Ok(InstanceEmailSmtpTestRequest {
        host,
        port,
        username,
        password,
        secure: form.bool_value("integration_smtp_secure"),
    })
}

fn build_disable_single_community_update() -> InstanceConfigUpdateRequest {
    InstanceConfigUpdateRequest {
        gateway_rollout: None,
        registration: None,
        sso: None,
        app_public: None,
        policy: Some(InstancePolicyUpdateRequest {
            single_community_enabled: Some(false),
            single_community_name: None,
            direct_messages_disabled: None,
            premium_mode: None,
            services: None,
        }),
        integrations: None,
        media: None,
    }
}

fn build_create_registration_url_request(
    form: &MultiValueForm,
) -> Result<CreateRegistrationUrlRequest, &'static str> {
    let label = form.clean("registration_url_label");
    if label
        .as_ref()
        .is_some_and(|value| value.chars().count() > 120)
    {
        return Err("Label must be 120 characters or fewer");
    }
    Ok(CreateRegistrationUrlRequest {
        label,
        expires_at: parse_registration_url_expires_at(form)?,
        max_uses: parse_registration_url_max_uses(form)?,
        approval_required: form.bool_value("registration_url_approval_required"),
    })
}

fn parse_registration_url_expires_at(
    form: &MultiValueForm,
) -> Result<Option<String>, &'static str> {
    let Some(value) = form
        .first("registration_url_expires_in_days")
        .and_then(clean_string)
    else {
        return Ok(None);
    };
    let days = value
        .parse::<i64>()
        .map_err(|_| "Expires in days must be a positive whole number")?;
    if days < 1 {
        return Err("Expires in days must be a positive whole number");
    }
    let expires_at = time::OffsetDateTime::now_utc()
        .checked_add(time::Duration::days(days))
        .ok_or("Expiration is too far in the future")?;
    expires_at
        .format(&time::format_description::well_known::Rfc3339)
        .map(Some)
        .map_err(|_| "Failed to format expiration timestamp")
}

fn parse_registration_url_max_uses(form: &MultiValueForm) -> Result<Option<u64>, &'static str> {
    let Some(value) = form
        .first("registration_url_max_uses")
        .and_then(clean_string)
    else {
        return Ok(None);
    };
    let max_uses = value
        .parse::<u64>()
        .map_err(|_| "Max uses must be a positive whole number")?;
    if max_uses == 0 || max_uses > 1_000_000 {
        return Err("Max uses must be between 1 and 1,000,000");
    }
    Ok(Some(max_uses))
}

fn build_limit_filters(form: &MultiValueForm) -> Option<LimitRuleFilters> {
    let traits = form.list_values_any(&["traits[]", "traits"]);
    let guild_features = form.list_values_any(&["guild_features[]", "guild_features"]);
    if traits.is_empty() && guild_features.is_empty() {
        None
    } else {
        Some(LimitRuleFilters {
            traits,
            guild_features,
        })
    }
}

fn update_limit_rule_values(
    rule: &mut LimitRule,
    form: &MultiValueForm,
    limit_keys: &[String],
    fallback_limits: Option<&std::collections::BTreeMap<String, u64>>,
) {
    let mut limits = std::collections::BTreeMap::new();
    for key in limit_keys {
        if let Some(parsed) = form.parse_u64(key) {
            limits.insert(key.clone(), parsed);
        }
    }
    if limits.is_empty()
        && let Some(defaults) = fallback_limits
    {
        limits.extend(defaults.clone());
    }
    rule.limits = limits;
    rule.filters = build_limit_filters(form);
}

fn limit_config_result<T, E: std::fmt::Display>(
    result: Result<T, E>,
    success_message: &'static str,
    error_message: &'static str,
) -> FlashData {
    match result {
        Ok(_) => FlashData::success(success_message),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: update limit config");
            FlashData::error(error_message)
        }
    }
}

pub async fn limit_config_post(
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
                "/limit-config",
                FlashData::error("Invalid form data"),
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let is_prod = config.is_production();
    let current = match client.get_limit_config().await {
        Ok(current) => current,
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: fetch current limit configuration");
            return redirect_back_with_flash(
                base,
                "/limit-config",
                FlashData::error("Failed to fetch current limit configuration"),
                is_prod,
            );
        }
    };
    let mut limit_config = current.limit_config;
    match action {
        "update" => {
            let rule_id = match aq.rule.as_deref().and_then(clean_string) {
                Some(rule_id) => rule_id,
                None => {
                    return redirect_back_with_flash(
                        base,
                        "/limit-config",
                        FlashData::error("Rule not found"),
                        is_prod,
                    );
                }
            };
            let Some(rule) = limit_config
                .rules
                .iter_mut()
                .find(|rule| rule.id == rule_id)
            else {
                return redirect_back_with_flash(
                    base,
                    "/limit-config",
                    FlashData::error("Rule not found"),
                    is_prod,
                );
            };
            let fallback = current
                .defaults
                .get(&rule_id)
                .or_else(|| current.defaults.get("default"));
            update_limit_rule_values(rule, &form, &current.limit_keys, fallback);
            let request = LimitConfigUpdateRequest { limit_config };
            let result = client.update_limit_config(&request).await;
            let flash = limit_config_result(
                result,
                "Limit configuration updated",
                "Failed to update limit configuration",
            );
            return redirect_back_with_flash(base, "/limit-config", flash, is_prod);
        }
        "delete" => {
            let rule_id = match aq.rule.as_deref().and_then(clean_string) {
                Some(rule_id) => rule_id,
                None => {
                    return redirect_back_with_flash(
                        base,
                        "/limit-config",
                        FlashData::error("Rule not found"),
                        is_prod,
                    );
                }
            };
            if rule_id == "default" {
                return redirect_back_with_flash(
                    base,
                    "/limit-config",
                    FlashData::error("The default rule cannot be deleted"),
                    is_prod,
                );
            }
            let old_len = limit_config.rules.len();
            limit_config.rules.retain(|rule| rule.id != rule_id);
            if limit_config.rules.len() == old_len {
                return redirect_back_with_flash(
                    base,
                    "/limit-config",
                    FlashData::error("Rule not found"),
                    is_prod,
                );
            }
            let request = LimitConfigUpdateRequest { limit_config };
            let result = client.update_limit_config(&request).await;
            let flash =
                limit_config_result(result, "Limit rule deleted", "Failed to delete limit rule");
            return redirect_back_with_flash(base, "/limit-config", flash, is_prod);
        }
        "create" => {
            let rule_id = match form.clean("rule_id") {
                Some(rule_id) => rule_id,
                None => {
                    return redirect_back_with_flash(
                        base,
                        "/limit-config",
                        FlashData::error("Rule ID is required"),
                        is_prod,
                    );
                }
            };
            if rule_id == "default" {
                return redirect_back_with_flash(
                    base,
                    "/limit-config",
                    FlashData::error("The default rule ID is reserved"),
                    is_prod,
                );
            }
            if limit_config.rules.iter().any(|rule| rule.id == rule_id) {
                return redirect_back_with_flash(
                    base,
                    "/limit-config",
                    FlashData::error("Rule ID already exists"),
                    is_prod,
                );
            }
            let limits = current.defaults.get("default").cloned().unwrap_or_default();
            limit_config.rules.push(LimitRule {
                id: rule_id,
                filters: build_limit_filters(&form),
                limits,
                modified_fields: None,
            });
            let request = LimitConfigUpdateRequest { limit_config };
            let result = client.update_limit_config(&request).await;
            let flash =
                limit_config_result(result, "Limit rule created", "Failed to create limit rule");
            return redirect_back_with_flash(base, "/limit-config", flash, is_prod);
        }
        _ => {}
    }
    redirect_back_with_flash(
        base,
        "/limit-config",
        FlashData::success("Limit config updated"),
        is_prod,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_sso_update_keeps_repeated_allowed_domains() {
        let form = MultiValueForm::parse(
            b"sso_enabled=true&sso_auto_provision=on&sso_allowed_domains%5B%5D=example.com&sso_allowed_domains%5B%5D=example.org&sso_display_name= Fluxer ",
        );
        let request = build_sso_update(&form);
        let sso = request.sso.expect("sso update");
        assert_eq!(sso.enabled, Some(true));
        assert_eq!(sso.enforced, Some(false));
        assert_eq!(sso.auto_provision, Some(true));
        assert_eq!(sso.display_name, Some(Some("Fluxer".to_owned())));
        assert_eq!(
            sso.allowed_domains,
            Some(vec!["example.com".to_owned(), "example.org".to_owned()])
        );
    }

    #[test]
    fn build_sso_update_splits_delimited_allowed_domains() {
        let form = MultiValueForm::parse(
            b"sso_enabled=true&sso_auto_provision=on&sso_allowed_domains=example.com%0Aexample.org%2Cexample.net",
        );
        let request = build_sso_update(&form);
        let sso = request.sso.expect("sso update");
        assert_eq!(
            sso.allowed_domains,
            Some(vec![
                "example.com".to_owned(),
                "example.org".to_owned(),
                "example.net".to_owned()
            ])
        );
    }

    #[test]
    fn build_limit_filters_accepts_repeated_and_delimited_values() {
        let form = MultiValueForm::parse(
            b"traits%5B%5D=staff&traits%5B%5D=partner%2Cvip&guild_features=COMMUNITY%0ANEWS",
        );
        let filters = build_limit_filters(&form).expect("filters");
        assert_eq!(
            filters.traits,
            vec!["staff".to_owned(), "partner".to_owned(), "vip".to_owned()]
        );
        assert_eq!(
            filters.guild_features,
            vec!["COMMUNITY".to_owned(), "NEWS".to_owned()]
        );
    }

    #[test]
    fn update_limit_rule_values_reads_checked_limit_keys() {
        let form = MultiValueForm::parse(b"message_send=1&traits%5B%5D=trial");
        let mut rule = LimitRule {
            id: "trial".to_owned(),
            filters: None,
            limits: std::collections::BTreeMap::new(),
            modified_fields: None,
        };
        update_limit_rule_values(&mut rule, &form, &["message_send".to_owned()], None);
        assert_eq!(rule.limits.get("message_send"), Some(&1));
        assert_eq!(
            rule.filters.expect("filters").traits,
            vec!["trial".to_owned()]
        );
    }
}
