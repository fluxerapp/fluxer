// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::{AdminApiClient, ApiResultExt},
        types::{AccountIdentityMode, AccountIdentitySettings, PremiumBranding},
    },
    config::AdminConfig,
};
use std::{
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

const PREMIUM_BRANDING_TTL: Duration = Duration::from_secs(60);
const ACCOUNT_IDENTITY_TTL: Duration = Duration::from_secs(60);
const ACCOUNT_IDENTITY_RETRY_TTL: Duration = Duration::from_secs(10);

#[derive(Clone)]
pub struct AppState {
    inner: Arc<AppStateInner>,
}

struct AppStateInner {
    pub config: AdminConfig,
    pub http_client: reqwest::Client,
    premium_branding: Mutex<Option<(Instant, PremiumBranding)>>,
    account_identity: Mutex<Option<(Instant, AccountIdentitySettings)>>,
}

impl AppState {
    pub fn new(config: AdminConfig) -> Self {
        let http_client = reqwest::Client::builder()
            .user_agent(format!("FluxerAdmin/{} (Rust)", config.build_version))
            .build()
            .expect("failed to create HTTP client");
        Self {
            inner: Arc::new(AppStateInner {
                config,
                http_client,
                premium_branding: Mutex::new(None),
                account_identity: Mutex::new(None),
            }),
        }
    }

    pub fn config(&self) -> &AdminConfig {
        &self.inner.config
    }

    pub fn http_client(&self) -> &reqwest::Client {
        &self.inner.http_client
    }

    pub fn cached_premium_branding(&self) -> Option<PremiumBranding> {
        let cache = self
            .inner
            .premium_branding
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        cache
            .as_ref()
            .filter(|(fetched_at, _)| fetched_at.elapsed() < PREMIUM_BRANDING_TTL)
            .map(|(_, branding)| branding.clone())
    }

    pub fn remember_premium_branding(&self, branding: PremiumBranding) {
        *self
            .inner
            .premium_branding
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) = Some((Instant::now(), branding));
    }

    pub async fn premium_branding(&self, client: &AdminApiClient) -> Option<PremiumBranding> {
        if let Some(branding) = self.cached_premium_branding() {
            return Some(branding);
        }
        let branding = PremiumBranding::from_discovery(
            &client
                .get_instance_premium_discovery()
                .await
                .log_error("load premium branding")?,
        );
        self.remember_premium_branding(branding.clone());
        Some(branding)
    }

    pub async fn account_identity(&self, client: &AdminApiClient) -> AccountIdentityMode {
        self.account_identity_settings(client).await.mode
    }

    pub async fn account_identity_settings(
        &self,
        client: &AdminApiClient,
    ) -> AccountIdentitySettings {
        if !self.config().self_hosted {
            return AccountIdentitySettings::default();
        }
        let previous = *self
            .inner
            .account_identity
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some((expires_at, settings)) = previous
            && Instant::now() < expires_at
        {
            return settings;
        }
        let (settings, ttl) = match client
            .get_instance_account_identity()
            .await
            .log_error("load account identity mode")
        {
            Some(settings) => (settings, ACCOUNT_IDENTITY_TTL),
            None => (
                previous.map_or(AccountIdentitySettings::default(), |(_, settings)| settings),
                ACCOUNT_IDENTITY_RETRY_TTL,
            ),
        };
        *self
            .inner
            .account_identity
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) =
            Some((Instant::now() + ttl, settings));
        settings
    }
}

impl axum::extract::FromRef<AppState> for AdminConfig {
    fn from_ref(state: &AppState) -> Self {
        state.inner.config.clone()
    }
}
