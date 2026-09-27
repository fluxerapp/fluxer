// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod apns;
pub mod fcm;
pub mod own_relay;
pub mod web_push;

use crate::metrics::{DeliveryRoute, Provider, SendResult, elapsed_ms};
use crate::server::AppState;
use crate::subscription::{Platform, Subscription};
use crate::vendor::VendorOutcome;
use fluxer_svc::metrics::now_ms;
use serde_json::Value;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SendOutcome {
    Accepted,
    TokenInvalid { reason: &'static str },
    Permanent { reason: String },
    Transient { reason: String },
}

impl SendOutcome {
    pub fn deletes_token(&self) -> bool {
        matches!(self, Self::TokenInvalid { .. })
    }

    pub fn reason(&self) -> &str {
        match self {
            Self::Accepted => "accepted",
            Self::TokenInvalid { reason } => reason,
            Self::Permanent { reason } | Self::Transient { reason } => reason,
        }
    }

    fn permanent(reason: impl Into<String>) -> Self {
        Self::Permanent {
            reason: reason.into(),
        }
    }

    fn transient(reason: impl Into<String>) -> Self {
        Self::Transient {
            reason: reason.into(),
        }
    }
}

impl From<VendorOutcome> for SendOutcome {
    fn from(outcome: VendorOutcome) -> Self {
        match outcome {
            VendorOutcome::Accepted => Self::Accepted,
            VendorOutcome::Unreachable(unreachable) if unreachable.is_permanent() => {
                Self::permanent(unreachable.label())
            }
            VendorOutcome::Unreachable(unreachable) => Self::transient(unreachable.label()),
            VendorOutcome::Refused(refusal) => match refusal.dead_token {
                Some(dead_token) => Self::TokenInvalid {
                    reason: dead_token.label(),
                },
                None if refusal.is_transient() => {
                    Self::transient(format!("http_{}_{}", refusal.status, refusal.reason))
                }
                None => Self::permanent(format!("http_{}_{}", refusal.status, refusal.reason)),
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Route {
    WebPush,
    LegacyApns,
    LegacyFcm,
}

pub fn route_of(sub: &Subscription) -> Option<Route> {
    match sub.platform()? {
        Platform::WebPush | Platform::AndroidUnifiedPush => Some(Route::WebPush),
        Platform::IosApns if sub.is_web_push_registration() => Some(Route::WebPush),
        Platform::AndroidFcm if sub.is_web_push_registration() => Some(Route::WebPush),
        Platform::IosApnsVoip if sub.is_web_push_registration() => Some(Route::WebPush),
        Platform::IosApns => Some(Route::LegacyApns),
        Platform::AndroidFcm => Some(Route::LegacyFcm),
        Platform::IosApnsVoip => None,
    }
}

pub async fn send(state: &AppState, sub: &Subscription, envelope: &Value) -> SendOutcome {
    let (Some(platform), Some(route)) = (sub.platform(), route_of(sub)) else {
        return SendOutcome::permanent("unsupported_platform");
    };
    let started_ms = now_ms();
    let outcome = if relay_consent_missing(state, &sub.endpoint) {
        SendOutcome::permanent("relay_consent_required")
    } else {
        let direct = in_process_hop(&sub.endpoint, &state.cfg.own_relay_hosts);
        match (route, direct) {
            (Route::WebPush, Some(hop)) => {
                let hopped = hop.as_subscription(sub);
                state.metrics.record_own_relay_shortcut();
                apns::send(state, &hopped, envelope).await
            }
            (Route::WebPush, None) => web_push::send(state, sub, envelope).await,
            (Route::LegacyApns, _) => apns::send(state, sub, envelope).await,
            (Route::LegacyFcm, _) => fcm::send(state, sub, envelope).await,
        }
    };
    state.metrics.record_send(
        provider_of(platform),
        result_of(&outcome),
        elapsed_ms(started_ms),
    );
    state
        .metrics
        .record_delivery_route(route_label(route), result_of(&outcome));
    outcome
}

fn relay_consent_missing(state: &AppState, endpoint: &str) -> bool {
    !relay_consent_accepted(state)
        && own_relay::is_managed(endpoint, &state.cfg.managed_relay_hosts)
}

fn relay_consent_accepted(state: &AppState) -> bool {
    state.cfg.relay_consent_accepted
        || state
            .rollout
            .snapshot()
            .is_some_and(|held| held.relay_consent_accepted)
}

fn in_process_hop(endpoint: &str, hosts: &[String]) -> Option<own_relay::Hop> {
    own_relay::parse(endpoint, hosts).filter(|hop| matches!(hop.leg, own_relay::Leg::Apns))
}

fn route_label(route: Route) -> DeliveryRoute {
    match route {
        Route::WebPush => DeliveryRoute::WebPush,
        Route::LegacyApns => DeliveryRoute::LegacyApns,
        Route::LegacyFcm => DeliveryRoute::LegacyFcm,
    }
}

pub fn provider_of(platform: Platform) -> Provider {
    match platform {
        Platform::WebPush => Provider::WebPush,
        Platform::AndroidUnifiedPush => Provider::UnifiedPush,
        Platform::AndroidFcm => Provider::Fcm,
        Platform::IosApns => Provider::Apns,
        Platform::IosApnsVoip => Provider::ApnsVoip,
    }
}

fn result_of(outcome: &SendOutcome) -> SendResult {
    match outcome {
        SendOutcome::Accepted => SendResult::Accepted,
        SendOutcome::TokenInvalid { .. } => SendResult::TokenInvalid,
        SendOutcome::Permanent { .. } => SendResult::Permanent,
        SendOutcome::Transient { .. } => SendResult::Transient,
    }
}

#[cfg(test)]
mod hop_tests {
    use super::*;

    const TOKEN: &str = "3dbc5a5ef1a1c1666afc26f466e1b3ebaaf4c66d92dddeb0fd1b69c49641d4cd";

    fn ours() -> Vec<String> {
        vec!["push.fluxer.com".to_owned()]
    }

    #[test]
    fn only_the_plain_apns_leg_is_delivered_in_process() {
        let apns = format!("https://push.fluxer.com/relay/v1/apns/canary/production/{TOKEN}");
        assert!(in_process_hop(&apns, &ours()).is_some());
    }

    #[test]
    fn an_fcm_relay_endpoint_keeps_its_encrypted_network_hop() {
        let fcm = "https://push.fluxer.com/relay/v1/fcm/canary/tok%3AAPA91bExample";
        assert!(in_process_hop(fcm, &ours()).is_none());
    }

    #[test]
    fn a_pushkit_relay_endpoint_keeps_its_voip_topic() {
        let voip = format!("https://push.fluxer.com/relay/v1/apns-voip/canary/production/{TOKEN}");
        assert!(in_process_hop(&voip, &ours()).is_none());
    }
}

#[cfg(test)]
mod consent_tests {
    use super::*;
    use crate::config::DeliveryConfig;
    use crate::server::AppState;

    const TOKEN: &str = "3dbc5a5ef1a1c1666afc26f466e1b3ebaaf4c66d92dddeb0fd1b69c49641d4cd";
    const NTFY_ENDPOINT: &str = "https://ntfy.sh/upZzH87cT9jJCc?up=1";

    fn managed_endpoint() -> String {
        format!("https://push.fluxer.com/relay/v1/apns/stable/production/{TOKEN}")
    }

    fn state(relay_consent_accepted: bool) -> AppState {
        let cfg = DeliveryConfig::load_from_iter([
            ("FLUXER_INTERNAL_API_ENDPOINT", "http://127.0.0.1:8080"),
            ("FLUXER_GATEWAY_RPC_AUTH_TOKEN", "rpc-token"),
            ("FLUXER_VAPID_EMAIL", "ops@fluxer.com"),
            ("FLUXER_VAPID_PUBLIC_KEY", "public-key"),
            ("FLUXER_VAPID_PRIVATE_KEY", "private-key"),
            (
                "FLUXER_PUSH_SERVICE_RELAY_CONSENT_ACCEPTED",
                if relay_consent_accepted {
                    "true"
                } else {
                    "false"
                },
            ),
        ])
        .expect("the delivery config loads");
        AppState::try_new(cfg).expect("the delivery state builds")
    }

    fn subscription(endpoint: &str) -> Subscription {
        Subscription {
            subscription_id: "sub-1".to_owned(),
            endpoint: endpoint.to_owned(),
            p256dh_key: None,
            auth_key: None,
            platform: Some("web_push".to_owned()),
            app_id: None,
            provider_environment: None,
        }
    }

    async fn outcome_of(relay_consent_accepted: bool, endpoint: &str) -> SendOutcome {
        let state = state(relay_consent_accepted);
        send(&state, &subscription(endpoint), &Value::Null).await
    }

    #[tokio::test]
    async fn a_managed_relay_send_waits_for_the_operator_to_accept_the_notice() {
        assert_eq!(
            outcome_of(false, &managed_endpoint()).await,
            SendOutcome::permanent("relay_consent_required")
        );
    }

    #[tokio::test]
    async fn a_refused_managed_relay_send_keeps_the_registration() {
        assert!(!outcome_of(false, &managed_endpoint()).await.deletes_token());
    }

    #[tokio::test]
    async fn an_accepted_notice_lets_the_managed_relay_send_through() {
        assert_eq!(
            outcome_of(true, &managed_endpoint()).await,
            SendOutcome::permanent("missing_keys")
        );
    }

    #[tokio::test]
    async fn a_unified_push_endpoint_is_sent_whatever_the_operator_accepted() {
        assert_eq!(
            outcome_of(false, NTFY_ENDPOINT).await,
            SendOutcome::permanent("missing_keys")
        );
        assert_eq!(
            outcome_of(true, NTFY_ENDPOINT).await,
            SendOutcome::permanent("missing_keys")
        );
    }

    #[tokio::test]
    async fn a_notice_accepted_in_the_instance_config_lets_the_send_through() {
        let state = state(false);
        state.rollout.update(&serde_json::json!({
            "enabled": true,
            "config_version": 1,
            "relay_consent_accepted": true,
        }));
        assert_eq!(
            send(&state, &subscription(&managed_endpoint()), &Value::Null).await,
            SendOutcome::permanent("missing_keys")
        );
    }

    #[tokio::test]
    async fn an_instance_config_that_has_not_accepted_still_refuses_the_send() {
        let state = state(false);
        state.rollout.update(&serde_json::json!({
            "enabled": true,
            "config_version": 1,
            "relay_consent_accepted": false,
        }));
        assert_eq!(
            send(&state, &subscription(&managed_endpoint()), &Value::Null).await,
            SendOutcome::permanent("relay_consent_required")
        );
    }

    #[tokio::test]
    async fn a_refused_managed_relay_send_is_still_counted() {
        let state = state(false);
        let outcome = send(&state, &subscription(&managed_endpoint()), &Value::Null).await;
        assert_eq!(outcome, SendOutcome::permanent("relay_consent_required"));
        let rendered = state.metrics.render();
        for series in [
            "fluxer_push_sends_total{provider=\"web_push\",result=\"permanent\"} 1",
            "fluxer_push_delivery_routes_total{route=\"web_push\",result=\"permanent\"} 1",
        ] {
            assert!(rendered.contains(series), "{series} must be recorded");
        }
    }
}
