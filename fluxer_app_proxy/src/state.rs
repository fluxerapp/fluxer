// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
use crate::csp::CompiledCspPolicy;
use crate::discovery_cache::DiscoveryCache;
use crate::invite_meta::InviteMetaResolver;
use fluxer_common::geoip::GeoipResolver;
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::sync::Semaphore;

pub const MAX_SPA_INDEX_BYTES: usize = 4 * 1024 * 1024;
pub const MAX_RENDERED_SPA_INDEX_BYTES: usize = 8 * 1024 * 1024;
pub const MAX_STATIC_TEXT_FILE_BYTES: usize = 4 * 1024 * 1024;
pub const SPA_DOCUMENT_MEMORY_BUDGET_BYTES: usize = 160 * 1024 * 1024;
pub const SPA_DOCUMENT_RENDER_RESERVATION_BYTES: u32 = 40 * 1024 * 1024;
pub const UPSTREAM_ASSET_RESPONSES_IN_FLIGHT_MAX: usize = 32;
pub const LOCAL_FILE_READS_IN_FLIGHT_MAX: usize = 256;

const _: () = assert!(
    MAX_RENDERED_SPA_INDEX_BYTES <= SPA_DOCUMENT_RENDER_RESERVATION_BYTES as usize,
    "a rendered SPA document must fit inside the memory reserved to render it"
);

#[derive(Clone)]
pub struct AppProxyBudgets {
    pub spa_document_memory: Arc<Semaphore>,
    pub upstream_asset_slots: Arc<Semaphore>,
    pub local_read_slots: Arc<Semaphore>,
}

impl AppProxyBudgets {
    pub fn new() -> Self {
        Self {
            spa_document_memory: Arc::new(Semaphore::new(SPA_DOCUMENT_MEMORY_BUDGET_BYTES)),
            upstream_asset_slots: Arc::new(Semaphore::new(UPSTREAM_ASSET_RESPONSES_IN_FLIGHT_MAX)),
            local_read_slots: Arc::new(Semaphore::new(LOCAL_FILE_READS_IN_FLIGHT_MAX)),
        }
    }
}

impl Default for AppProxyBudgets {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<AppProxyConfig>,
    pub csp: Arc<CompiledCspPolicy>,
    pub http_client: reqwest::Client,
    pub discovery_cache: Arc<DiscoveryCache>,
    pub geoip: Arc<GeoipResolver>,
    pub invite_meta: Arc<OnceLock<InviteMetaResolver>>,
    pub index_html: Option<Arc<str>>,
    pub budgets: AppProxyBudgets,
}

#[derive(Debug)]
pub enum BoundedFileReadError {
    TooLarge { actual: u64, maximum: usize },
    Io(std::io::Error),
}

impl std::fmt::Display for BoundedFileReadError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooLarge { actual, maximum } => write!(
                formatter,
                "file is {actual} bytes, exceeding the {maximum} byte limit"
            ),
            Self::Io(source) => source.fmt(formatter),
        }
    }
}

impl std::error::Error for BoundedFileReadError {}

impl BoundedFileReadError {
    pub fn is_not_found(&self) -> bool {
        matches!(self, Self::Io(source) if source.kind() == std::io::ErrorKind::NotFound)
    }
}

pub async fn read_bounded_file(
    path: &std::path::Path,
    max_bytes: usize,
) -> Result<Vec<u8>, BoundedFileReadError> {
    let file = tokio::fs::File::open(path)
        .await
        .map_err(BoundedFileReadError::Io)?;
    let metadata = file.metadata().await.map_err(BoundedFileReadError::Io)?;
    if !metadata.is_file() {
        return Err(BoundedFileReadError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "not a regular file",
        )));
    }
    let declared_length = metadata.len();
    if declared_length > max_bytes as u64 {
        return Err(BoundedFileReadError::TooLarge {
            actual: declared_length,
            maximum: max_bytes,
        });
    }
    let expected_bytes =
        usize::try_from(declared_length).expect("a length within a usize limit must fit usize");
    let mut bytes = Vec::with_capacity(expected_bytes);
    tokio::io::AsyncReadExt::read_to_end(&mut file.take(declared_length + 1), &mut bytes)
        .await
        .map_err(BoundedFileReadError::Io)?;
    if bytes.len() > expected_bytes {
        return Err(BoundedFileReadError::TooLarge {
            actual: bytes.len() as u64,
            maximum: max_bytes,
        });
    }
    Ok(bytes)
}

pub async fn read_bounded_text_file(
    path: &std::path::Path,
    max_bytes: usize,
) -> Result<String, BoundedFileReadError> {
    let bytes = read_bounded_file(path, max_bytes).await?;
    String::from_utf8(bytes).map_err(|error| {
        BoundedFileReadError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            error.utf8_error(),
        ))
    })
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
