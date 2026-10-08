// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, HttpUrl};
use crate::csp::CompiledCspPolicy;
use crate::local_files::LocalFileStore;
use crate::routes::present_local_asset_prefixes;
use std::sync::Arc;
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::sync::Semaphore;

pub const MAX_SPA_INDEX_BYTES: usize = 4 * 1024 * 1024;
pub const MAX_RENDERED_SPA_INDEX_BYTES: usize = 8 * 1024 * 1024;
pub const MAX_STATIC_TEXT_FILE_BYTES: usize = 4 * 1024 * 1024;
pub const UPSTREAM_ASSET_RESPONSES_IN_FLIGHT_MAX: usize = 32;
pub const LOCAL_FILE_READS_IN_FLIGHT_MAX: usize = 256;

#[derive(Clone)]
pub struct AppProxyBudgets {
    pub upstream_asset_slots: Arc<Semaphore>,
    pub local_read_slots: Arc<Semaphore>,
}

impl AppProxyBudgets {
    pub fn new() -> Self {
        Self {
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

pub const AUTH_ENTRY_SHELL_FILE: &str = "auth-index.html";

#[derive(Clone)]
pub enum SpaIndexSource {
    Bundled {
        shell: Arc<str>,
        auth_entry_shell: Option<Arc<str>>,
    },
    Upstream(Arc<HttpUrl>),
}

impl SpaIndexSource {
    pub fn bundled(shell: &str) -> Self {
        Self::Bundled {
            shell: Arc::from(shell),
            auth_entry_shell: None,
        }
    }

    pub async fn load(config: &AppProxyConfig) -> anyhow::Result<Self> {
        if let Some(index_upstream_url) = &config.index_upstream_url {
            return Ok(Self::Upstream(Arc::new(index_upstream_url.clone())));
        }
        let static_dir = std::path::Path::new(&config.static_dir);
        let shell = read_shell(&static_dir.join("index.html")).await?;
        let auth_entry_path = static_dir.join(AUTH_ENTRY_SHELL_FILE);
        let auth_entry_shell = if tokio::fs::try_exists(&auth_entry_path).await? {
            Some(read_shell(&auth_entry_path).await?)
        } else {
            None
        };
        Ok(Self::Bundled {
            shell,
            auth_entry_shell,
        })
    }

    pub fn is_upstream(&self) -> bool {
        matches!(self, Self::Upstream(_))
    }
}

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<AppProxyConfig>,
    pub csp: Arc<CompiledCspPolicy>,
    pub http_client: reqwest::Client,
    pub spa_index_source: SpaIndexSource,
    pub local_asset_prefixes: Option<Arc<[&'static str]>>,
    pub budgets: AppProxyBudgets,
    pub(crate) local_files: LocalFileStore,
}

impl AppState {
    pub async fn load(
        config: Arc<AppProxyConfig>,
        csp: Arc<CompiledCspPolicy>,
        http_client: reqwest::Client,
    ) -> anyhow::Result<Self> {
        let budgets = AppProxyBudgets::new();
        let local_files =
            LocalFileStore::load(std::path::Path::new(&config.static_dir), &budgets).await?;
        let spa_index_source = SpaIndexSource::load(&config).await?;
        let local_asset_prefixes = (!spa_index_source.is_upstream())
            .then(|| present_local_asset_prefixes(&config.static_dir));
        Ok(Self {
            config,
            csp,
            http_client,
            spa_index_source,
            local_asset_prefixes,
            budgets,
            local_files,
        })
    }
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

async fn read_shell(path: &std::path::Path) -> anyhow::Result<Arc<str>> {
    let html = read_bounded_text_file(path, MAX_SPA_INDEX_BYTES)
        .await
        .map_err(|error| anyhow::anyhow!("failed to read {}: {error}", path.display()))?;
    Ok(Arc::from(html))
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
