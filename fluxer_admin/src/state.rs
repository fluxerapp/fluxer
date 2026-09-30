// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::{AdminApiClient, ApiResultExt},
        types::{ListBadgesResponse, PremiumBranding},
    },
    config::AdminConfig,
};
use std::{
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

const PREMIUM_BRANDING_TTL: Duration = Duration::from_secs(60);
const BADGES_TTL: Duration = Duration::from_secs(60);

#[derive(Clone)]
pub struct AppState {
    inner: Arc<AppStateInner>,
}

struct AppStateInner {
    pub config: AdminConfig,
    pub http_client: reqwest::Client,
    premium_branding: Mutex<Option<(Instant, PremiumBranding)>>,
    badges: Mutex<Option<(Instant, ListBadgesResponse)>>,
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
                badges: Mutex::new(None),
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

    pub fn remember_badges(&self, badges: ListBadgesResponse) {
        *self
            .inner
            .badges
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) = Some((Instant::now(), badges));
    }

    pub async fn badges(&self, client: &AdminApiClient) -> Option<ListBadgesResponse> {
        let cached = self
            .inner
            .badges
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .as_ref()
            .filter(|(fetched_at, _)| fetched_at.elapsed() < BADGES_TTL)
            .map(|(_, badges)| badges.clone());
        if cached.is_some() {
            return cached;
        }
        let badges = client.get_public_badges().await.log_error("load badges")?;
        self.remember_badges(badges.clone());
        Some(badges)
    }
}

impl axum::extract::FromRef<AppState> for AdminConfig {
    fn from_ref(state: &AppState) -> Self {
        state.inner.config.clone()
    }
}
