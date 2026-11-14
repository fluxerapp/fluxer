// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::payload;
use crate::providers::SendOutcome;
use crate::server::AppState;
use crate::subscription::Subscription;
use crate::vendor;
use serde_json::Value;

pub async fn send(state: &AppState, sub: &Subscription, envelope: &Value) -> SendOutcome {
    let Some(cfg) = state.cfg.fcm.as_ref() else {
        return SendOutcome::permanent("fcm_unavailable");
    };
    if sub.endpoint.is_empty() {
        return SendOutcome::permanent("missing_device_token");
    }

    let body = serde_json::to_vec(&payload::fcm_message(&sub.endpoint, envelope))
        .expect("a json value serialises");
    match vendor::send_fcm(
        &state.http,
        &state.tokens,
        &state.metrics,
        cfg,
        cfg.project_id_for(sub.app_id()),
        body,
    )
    .await
    {
        Ok(outcome) => SendOutcome::from(outcome),
        Err(error) => SendOutcome::transient(format!("fcm_auth: {error}")),
    }
}
