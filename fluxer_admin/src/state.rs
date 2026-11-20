// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::{AdminApiClient, ApiResultExt},
        types::{
            AccountIdentityMode, AccountIdentitySettings, InstanceConfigResponse, PremiumBranding,
            ReportReasonEntry,
        },
    },
    config::AdminConfig,
    sso::{self, ProviderMetadata},
};
use anyhow::Context as _;
use sha2::{Digest, Sha256};
use std::{
    collections::{HashSet, VecDeque},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

const PREMIUM_BRANDING_TTL: Duration = Duration::from_secs(60);
const ACCOUNT_IDENTITY_TTL: Duration = Duration::from_secs(60);
const ACCOUNT_IDENTITY_RETRY_TTL: Duration = Duration::from_secs(10);
const SSO_DISCOVERY_TTL: Duration = Duration::from_secs(3600);
pub const SIGN_IN_FLOW_TTL: Duration = Duration::from_secs(600);
const MAX_CLAIMED_SIGN_IN_FLOWS: usize = 100_000;

#[derive(Clone)]
pub struct AppState {
    inner: Arc<AppStateInner>,
}

struct AppStateInner {
    pub config: AdminConfig,
    pub http_client: reqwest::Client,
    sso_http_client: reqwest::Client,
    premium_branding: Mutex<Option<(Instant, PremiumBranding)>>,
    account_identity: Mutex<Option<(Instant, AccountIdentitySettings)>>,
    report_reasons: Mutex<Option<Arc<[ReportReasonEntry]>>>,
    sso_provider: Mutex<Option<(Instant, ProviderMetadata)>>,
    claimed_sign_in_flows: Mutex<ClaimedSignInFlows>,
}

#[derive(Default)]
struct ClaimedSignInFlows {
    keys: HashSet<[u8; 32]>,
    by_expiry: VecDeque<(Instant, [u8; 32])>,
}

impl ClaimedSignInFlows {
    fn claim(&mut self, key: [u8; 32], now: Instant) -> bool {
        while let Some(&(expires_at, expired)) = self.by_expiry.front() {
            if expires_at > now {
                break;
            }
            self.by_expiry.pop_front();
            self.keys.remove(&expired);
        }
        if self.keys.contains(&key) {
            return false;
        }
        if self.keys.len() >= MAX_CLAIMED_SIGN_IN_FLOWS {
            return true;
        }
        self.keys.insert(key);
        self.by_expiry.push_back((now + SIGN_IN_FLOW_TTL, key));
        true
    }
}

impl AppState {
    pub fn try_new(config: AdminConfig) -> anyhow::Result<Self> {
        let http_client = reqwest::Client::builder()
            .user_agent(format!("FluxerAdmin/{} (Rust)", config.build_version))
            .no_gzip()
            .no_brotli()
            .no_deflate()
            .http1_only()
            .build()
            .context("failed to create the admin HTTP client")?;
        let sso_http_client = sso::http_client(&config.build_version)?;
        Ok(Self {
            inner: Arc::new(AppStateInner {
                config,
                http_client,
                sso_http_client,
                premium_branding: Mutex::new(None),
                account_identity: Mutex::new(None),
                report_reasons: Mutex::new(None),
                sso_provider: Mutex::new(None),
                claimed_sign_in_flows: Mutex::new(ClaimedSignInFlows::default()),
            }),
        })
    }

    pub fn config(&self) -> &AdminConfig {
        &self.inner.config
    }

    pub fn http_client(&self) -> &reqwest::Client {
        &self.inner.http_client
    }

    pub fn sso_http_client(&self) -> &reqwest::Client {
        &self.inner.sso_http_client
    }

    pub fn claim_sign_in_flow(&self, verifier: &str) -> bool {
        let key: [u8; 32] = Sha256::digest(verifier.as_bytes()).into();
        self.inner
            .claimed_sign_in_flows
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .claim(key, Instant::now())
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

    pub fn remember_instance_config_branding(&self, instance_config: &InstanceConfigResponse) {
        self.merge_instance_config_branding(PremiumBranding::from_instance_config(instance_config));
    }

    fn merge_instance_config_branding(&self, branding: PremiumBranding) {
        let mut cache = self
            .inner
            .premium_branding
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some((fetched_at, previous)) = cache.as_ref() {
            let branding = PremiumBranding {
                discovery_enabled: previous.discovery_enabled,
                ..branding
            };
            *cache = Some((*fetched_at, branding));
        }
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

    pub async fn self_hosted_branding(&self) -> Option<PremiumBranding> {
        if !self.config().self_hosted {
            return None;
        }
        self.premium_branding(&AdminApiClient::anonymous(
            self.http_client(),
            self.config(),
        ))
        .await
    }

    pub fn forget_instance_settings(&self) {
        *self
            .inner
            .premium_branding
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) = None;
        *self
            .inner
            .account_identity
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) = None;
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

    pub async fn sso_provider(&self) -> Option<ProviderMetadata> {
        let sso = self.config().sso.as_ref()?;
        let cached = self
            .inner
            .sso_provider
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .as_ref()
            .filter(|(fetched_at, _)| fetched_at.elapsed() < SSO_DISCOVERY_TTL)
            .map(|(_, metadata)| metadata.clone());
        if let Some(metadata) = cached {
            return Some(metadata);
        }
        let metadata = match sso::discover(self.sso_http_client(), sso).await {
            Ok(metadata) => metadata,
            Err(error) => {
                tracing::error!(%error, issuer = %sso.issuer, "SSO provider discovery failed");
                return None;
            }
        };
        *self
            .inner
            .sso_provider
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) =
            Some((Instant::now(), metadata.clone()));
        Some(metadata)
    }

    pub async fn report_reasons(
        &self,
        client: &AdminApiClient,
    ) -> Option<Arc<[ReportReasonEntry]>> {
        if let Some(reasons) = self.cached_report_reasons() {
            return Some(reasons);
        }
        let reasons: Arc<[ReportReasonEntry]> = client
            .list_report_reasons()
            .await
            .log_error("load report reasons")?
            .reasons
            .into();
        *self
            .inner
            .report_reasons
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) = Some(reasons.clone());
        Some(reasons)
    }

    fn cached_report_reasons(&self) -> Option<Arc<[ReportReasonEntry]>> {
        self.inner
            .report_reasons
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clone()
    }
}

impl axum::extract::FromRef<AppState> for AdminConfig {
    fn from_ref(state: &AppState) -> Self {
        state.inner.config.clone()
    }
}
