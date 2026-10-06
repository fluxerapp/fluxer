// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::payload;
use crate::providers::SendOutcome;
use crate::server::AppState;
use crate::subscription::Subscription;
use crate::vendor::{self, ApnsRequest};
use serde_json::Value;

pub async fn send(state: &AppState, sub: &Subscription, envelope: &Value) -> SendOutcome {
    let Some(cfg) = state.cfg.apns.as_ref() else {
        return SendOutcome::permanent("apns_unavailable");
    };
    if sub.endpoint.is_empty() {
        return SendOutcome::permanent("missing_device_token");
    }
    let environment = sub.environment(cfg.default_environment);
    let Some(topic) = cfg.topic_for(sub.app_id(), environment) else {
        return SendOutcome::permanent("apns_topic_missing");
    };

    let headers = payload::apns_delivery_headers(envelope);
    let request = ApnsRequest {
        environment,
        topic,
        device_token: &sub.endpoint,
        headers: &headers,
        body: serde_json::to_vec(&payload::apns_payload(envelope))
            .expect("a json value serialises"),
    };
    match vendor::send_apns(
        &state.apns_http,
        &state.tokens,
        &state.metrics,
        cfg,
        request,
    )
    .await
    {
        Ok(outcome) => SendOutcome::from(outcome),
        Err(error) => SendOutcome::permanent(format!("apns_auth: {error}")),
    }
}
