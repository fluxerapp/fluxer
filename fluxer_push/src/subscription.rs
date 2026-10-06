// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{DEFAULT_APP_ID, ProviderEnvironment};
use serde::Deserialize;

const DEFAULT_PLATFORM: &str = "web_push";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Platform {
    WebPush,
    AndroidUnifiedPush,
    AndroidFcm,
    IosApns,
    IosApnsVoip,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Subscription {
    pub subscription_id: String,
    pub endpoint: String,
    pub p256dh_key: Option<String>,
    pub auth_key: Option<String>,
    pub platform: Option<String>,
    pub app_id: Option<String>,
    pub provider_environment: Option<String>,
}

impl Subscription {
    pub fn platform(&self) -> Option<Platform> {
        match self.platform.as_deref().unwrap_or(DEFAULT_PLATFORM) {
            "web_push" => Some(Platform::WebPush),
            "android_unified_push" => Some(Platform::AndroidUnifiedPush),
            "android_fcm" => Some(Platform::AndroidFcm),
            "ios_apns" => Some(Platform::IosApns),
            "ios_apns_voip" => Some(Platform::IosApnsVoip),
            _ => None,
        }
    }

    pub fn is_web_push_registration(&self) -> bool {
        endpoint_is_url(&self.endpoint) && self.has_web_push_keys()
    }

    pub fn app_id(&self) -> &str {
        self.app_id.as_deref().unwrap_or(DEFAULT_APP_ID)
    }

    pub fn environment(&self, default: ProviderEnvironment) -> ProviderEnvironment {
        match self.provider_environment.as_deref() {
            None => default,
            Some("development" | "sandbox") => ProviderEnvironment::Development,
            Some(_) => ProviderEnvironment::Production,
        }
    }

    fn has_web_push_keys(&self) -> bool {
        [self.p256dh_key.as_deref(), self.auth_key.as_deref()]
            .into_iter()
            .all(|key| key.is_some_and(|key| !key.trim().is_empty()))
    }
}

fn endpoint_is_url(endpoint: &str) -> bool {
    let endpoint = endpoint.trim();
    endpoint.starts_with("https://") || endpoint.starts_with("http://")
}
