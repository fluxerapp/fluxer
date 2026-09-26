// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::http::StatusCode;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum Reason {
    BadRequest,
    PayloadTooLarge,
    DeviceTokenInvalid,
    AppUnknown,
    DeviceTokenGone,
    RateLimited,
    ProviderUnavailable,
    RelayUnavailable,
    Internal,
}

impl Reason {
    pub const ALL: [Self; 9] = [
        Self::BadRequest,
        Self::PayloadTooLarge,
        Self::DeviceTokenInvalid,
        Self::AppUnknown,
        Self::DeviceTokenGone,
        Self::RateLimited,
        Self::ProviderUnavailable,
        Self::RelayUnavailable,
        Self::Internal,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::BadRequest => "bad_request",
            Self::PayloadTooLarge => "payload_too_large",
            Self::DeviceTokenInvalid => "device_token_invalid",
            Self::AppUnknown => "app_unknown",
            Self::DeviceTokenGone => "device_token_gone",
            Self::RateLimited => "rate_limited",
            Self::ProviderUnavailable => "provider_unavailable",
            Self::RelayUnavailable => "relay_unavailable",
            Self::Internal => "internal",
        }
    }

    pub fn status(self) -> StatusCode {
        match self {
            Self::BadRequest | Self::DeviceTokenInvalid | Self::AppUnknown => {
                StatusCode::BAD_REQUEST
            }
            Self::PayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
            Self::DeviceTokenGone => StatusCode::GONE,
            Self::RateLimited => StatusCode::TOO_MANY_REQUESTS,
            Self::ProviderUnavailable => StatusCode::BAD_GATEWAY,
            Self::RelayUnavailable => StatusCode::SERVICE_UNAVAILABLE,
            Self::Internal => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Rejection {
    pub reason: Reason,
    pub retry_after: Option<i64>,
}

impl Rejection {
    pub fn new(reason: Reason) -> Self {
        Self {
            reason,
            retry_after: None,
        }
    }

    pub fn after(reason: Reason, seconds: i64) -> Self {
        Self {
            reason,
            retry_after: Some(seconds),
        }
    }

    pub fn body(self) -> String {
        format!("{{\"reason\":\"{}\"}}", self.reason.label())
    }
}

pub const REASON_COUNT: usize = Reason::ALL.len();
