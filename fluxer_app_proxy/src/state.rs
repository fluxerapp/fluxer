// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
use crate::discovery_cache::DiscoveryCache;
use crate::invite_meta::InviteMetaResolver;
use fluxer_common::geoip::GeoipResolver;
use std::sync::{Arc, OnceLock};
use std::time::Duration;

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<AppProxyConfig>,
    pub http_client: reqwest::Client,
    pub discovery_cache: Arc<DiscoveryCache>,
    pub geoip: Arc<GeoipResolver>,
    pub invite_meta: Arc<OnceLock<InviteMetaResolver>>,
    pub index_html: Option<Arc<str>>,
}

pub fn build_http_client() -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(5))
        .timeout(Duration::from_secs(30))
        .redirect(reqwest::redirect::Policy::limited(2))
        .no_gzip()
        .no_brotli()
        .no_deflate()
        .build()
}
