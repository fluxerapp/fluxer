// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{render_fragment_with_toast, with_api_reason};
use crate::{
    api::{client::AdminApiClient, types::CreateRegistrationUrlRequest},
    config::AdminConfig,
    middleware::{flash::FlashData, htmx},
    templates,
    utils::forms::{MultiValueForm, clean_string},
};
use axum::{http::HeaderMap, response::Response};

pub(super) enum Action {
    Create,
    Rename,
    Revoke,
    Remove,
}

pub(super) enum Outcome {
    Flash(FlashData),
    Response(Response),
}

pub(super) async fn run(
    client: &AdminApiClient,
    form: &MultiValueForm,
    headers: &HeaderMap,
    config: &AdminConfig,
    csrf_token: &str,
    action: Action,
) -> Outcome {
    match action {
        Action::Create => {
            let request = match build_create_registration_url_request(form) {
                Ok(request) => request,
                Err(message) => return Outcome::Flash(FlashData::error(message)),
            };
            let response = match client.create_registration_url(&request).await {
                Ok(response) => response,
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: create registration URL");
                    return Outcome::Flash(FlashData::error(with_api_reason(
                        "Failed to create registration URL",
                        &error,
                    )));
                }
            };
            let flash = FlashData::success("Registration URL created");
            if !htmx::targets(headers, "registration-url-list") {
                return Outcome::Flash(FlashData::success(format!(
                    "Registration URL created: {}",
                    response.url
                )));
            }
            let instance_config = match client.get_instance_config().await {
                Ok(instance_config) => instance_config,
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: reload registration URLs");
                    return Outcome::Response(htmx::toast_response(&FlashData::error(
                        "Registration URL created, but failed to reload the list",
                    )));
                }
            };
            Outcome::Response(render_registration_url_list_response(
                config,
                csrf_token,
                &instance_config,
                &flash,
            ))
        }
        Action::Rename => {
            let Some(id) = form.clean("registration_url_id") else {
                return Outcome::Flash(FlashData::error("Registration URL ID is required"));
            };
            let label = form.clean("registration_url_label");
            let instance_config = match client.rename_registration_url(&id, label.as_deref()).await
            {
                Ok(instance_config) => instance_config,
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: rename registration URL");
                    return Outcome::Flash(FlashData::error("Failed to rename registration URL"));
                }
            };
            let flash = FlashData::success("Registration URL renamed");
            if htmx::targets(headers, "registration-url-list") {
                return Outcome::Response(render_registration_url_list_response(
                    config,
                    csrf_token,
                    &instance_config,
                    &flash,
                ));
            }
            Outcome::Flash(flash)
        }
        Action::Revoke | Action::Remove => {
            let Some(id) = form.clean("registration_url_id") else {
                return Outcome::Flash(FlashData::error("Registration URL ID is required"));
            };
            let instance_config = match client.revoke_registration_url(&id).await {
                Ok(instance_config) => instance_config,
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: revoke registration URL");
                    return Outcome::Flash(FlashData::error("Failed to revoke registration URL"));
                }
            };
            let flash = FlashData::success(match action {
                Action::Remove => "Registration URL removed",
                _ => "Registration URL revoked",
            });
            if htmx::targets(headers, "registration-url-list") {
                return Outcome::Response(render_registration_url_list_response(
                    config,
                    csrf_token,
                    &instance_config,
                    &flash,
                ));
            }
            Outcome::Flash(flash)
        }
    }
}

fn render_registration_url_list_response(
    config: &AdminConfig,
    csrf_token: &str,
    instance_config: &crate::api::types::InstanceConfigResponse,
    flash: &FlashData,
) -> Response {
    render_fragment_with_toast(
        templates::pages::instance_registration::registration_url_list(
            config,
            csrf_token,
            &instance_config.registration.urls,
        ),
        flash,
    )
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
        invite_code: parse_invite_code(form.clean("registration_url_invite_code"))?,
    })
}

fn parse_invite_code(value: Option<String>) -> Result<Option<String>, &'static str> {
    let Some(value) = value else {
        return Ok(None);
    };
    let code = value
        .trim_end_matches('/')
        .rsplit('/')
        .next()
        .unwrap_or_default()
        .to_owned();
    if code.is_empty()
        || code.len() > 64
        || !code.chars().all(|c| c.is_ascii_alphanumeric() || c == '-')
    {
        return Err("Enter an invite code or an invite link");
    }
    Ok(Some(code))
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
