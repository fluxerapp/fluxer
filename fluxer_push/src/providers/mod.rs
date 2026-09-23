// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod apns;
pub mod fcm;
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
    let outcome = match route {
        Route::WebPush => web_push::send(state, sub, envelope).await,
        Route::LegacyApns => apns::send(state, sub, envelope).await,
        Route::LegacyFcm => fcm::send(state, sub, envelope).await,
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
