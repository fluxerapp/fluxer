// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{instance_config_result, parse_tristate, with_api_reason};
use crate::{
    api::{
        client::AdminApiClient,
        types::{
            EmailDeliveryMode, InstanceBlueskyIntegrationUpdateRequest,
            InstanceBlueskyKeyIntegrationUpdateRequest, InstanceConfigUpdateRequest,
            InstanceEmailIntegrationUpdateRequest, InstanceEmailSmtpIntegrationUpdateRequest,
            InstanceEmailSmtpTestResponse, InstanceGifIntegrationUpdateRequest,
            InstanceIntegrationsUpdateRequest, InstanceYoutubeIntegrationUpdateRequest,
        },
    },
    middleware::flash::FlashData,
    utils::forms::MultiValueForm,
};

pub(super) enum Action {
    Update,
    Test,
}

pub(super) async fn run(
    client: &AdminApiClient,
    form: &MultiValueForm,
    action: Action,
) -> FlashData {
    let update = match build_integrations_update(form) {
        Ok(update) => update,
        Err(message) => return FlashData::error(message),
    };
    let result = client.update_instance_config(&update).await;
    let Action::Test = action else {
        return instance_config_result(result);
    };
    if let Err(error) = result {
        return instance_config_result(Err::<(), _>(error));
    }
    match client
        .test_saved_instance_smtp_config(form.clean("smtp_test_send_to").as_deref())
        .await
    {
        Ok(response) => smtp_test_flash(response),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: test SMTP config");
            FlashData::error(with_api_reason(
                "Settings saved, but the SMTP test could not run",
                &error,
            ))
        }
    }
}

fn smtp_test_flash(response: InstanceEmailSmtpTestResponse) -> FlashData {
    if response.ok {
        return match response.server_response {
            Some(reply) if !reply.is_empty() => FlashData::success(format!(
                "Settings saved and the test message was accepted. The server replied: {reply}"
            )),
            Some(_) => FlashData::success("Settings saved and the test message was accepted"),
            None => FlashData::success("Settings saved and SMTP connection verified"),
        };
    }
    let reason = response
        .error
        .unwrap_or_else(|| "the server gave no reason".to_owned());
    FlashData::error(format!(
        "Settings saved, but the SMTP test failed: {reason}"
    ))
}

fn parse_email_delivery(
    form: &MultiValueForm,
) -> Result<Option<Option<EmailDeliveryMode>>, String> {
    match form.first("integration_email_delivery").map(str::trim) {
        None => Ok(None),
        Some("") => Ok(Some(None)),
        Some(raw) => raw
            .parse()
            .map(|mode| Some(Some(mode)))
            .map_err(|_| "Email delivery has an unknown value".to_owned()),
    }
}

fn build_integrations_update(form: &MultiValueForm) -> Result<InstanceConfigUpdateRequest, String> {
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
    let delivery = parse_email_delivery(form)?;
    Ok(InstanceConfigUpdateRequest {
        integrations: Some(InstanceIntegrationsUpdateRequest {
            gif: Some(InstanceGifIntegrationUpdateRequest {
                klipy_api_key: clean("integration_klipy_api_key"),
            }),
            youtube: Some(InstanceYoutubeIntegrationUpdateRequest {
                api_key: clean("integration_youtube_api_key"),
            }),
            email: (form.has_key_starting_with("integration_email_")
                || form.has_key_starting_with("integration_smtp_"))
            .then(|| InstanceEmailIntegrationUpdateRequest {
                delivery,
                from_email: clean("integration_email_from_email"),
                from_name: clean("integration_email_from_name"),
                smtp: Some(InstanceEmailSmtpIntegrationUpdateRequest {
                    host: clean("integration_smtp_host"),
                    port: smtp_port,
                    username: clean("integration_smtp_username"),
                    password: clean("integration_smtp_password"),
                    tls_mode: form
                        .first("integration_smtp_tls_mode")
                        .map(|value| Some(value.trim().to_owned()).filter(|mode| !mode.is_empty())),
                }),
                disable_new_ip_authorization: Some(
                    form.bool_value("integration_email_disable_new_ip_authorization"),
                ),
            }),
            bluesky: Some(InstanceBlueskyIntegrationUpdateRequest {
                enabled: parse_tristate(form, "integration_bluesky_enabled"),
                client_name: clean("integration_bluesky_client_name"),
                client_uri: clean("integration_bluesky_client_uri"),
                logo_uri: clean("integration_bluesky_logo_uri"),
                tos_uri: clean("integration_bluesky_tos_uri"),
                policy_uri: clean("integration_bluesky_policy_uri"),
                keys: bluesky_keys,
            }),
        }),
        ..Default::default()
    })
}
