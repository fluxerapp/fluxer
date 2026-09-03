// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::ByteBudget,
    config::Config,
    constants,
    http_client::{self, HTTPClientOptions},
    image_transform::AnimationLimits,
    media_limits::MediaLimits,
    metrics,
    nsfw::NSFWClient,
    response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX,
    server::{
        external::{ExternalHintCache, new_external_hint_cache},
        transform::TransformRuntime,
    },
    storage::Store,
};
use std::{num::NonZeroU64, sync::Arc};

pub(in crate::server) struct AppState {
    pub(in crate::server) cfg: Config,
    pub(in crate::server) metrics: Arc<metrics::Metrics>,
    pub(in crate::server) store: Store,
    pub(in crate::server) media: MediaRuntime,
}

pub(in crate::server) struct MediaRuntime {
    nsfw: NSFWClient,
    external_client: http_client::HttpClient,
    external_buffer_bytes: ByteBudget,
    external_hints: ExternalHintCache,
    transforms: TransformRuntime,
}

impl AppState {
    pub(in crate::server) fn try_new(cfg: Config) -> anyhow::Result<Self> {
        let metrics = Arc::new(metrics::Metrics::new());
        let socket_io_timeout =
            NonZeroU64::new(cfg.socket_io_timeout_ms).unwrap_or(NonZeroU64::MIN);
        let store = Store::try_new(cfg.clone(), metrics.storage(), metrics.http_client())?;
        let external_client = http_client::build(
            HTTPClientOptions::new(socket_io_timeout, socket_io_timeout).restrict_to_public(),
            metrics.http_client(),
        )?;
        let nsfw = NSFWClient::new(
            &cfg.media.nsfw_service_endpoint,
            cfg.media.nsfw_threshold,
            metrics.nsfw(),
        )?;
        let media = MediaRuntime::new(&cfg, &metrics, nsfw, external_client)?;
        Ok(Self {
            cfg,
            metrics,
            store,
            media,
        })
    }

    #[cfg(test)]
    pub(in crate::server) fn for_tests(cfg: Config) -> Self {
        let metrics = Arc::new(metrics::Metrics::new());
        let store = Store::new(cfg.clone(), metrics.storage(), metrics.http_client());
        let external_client = http_client::build_default(metrics.http_client());
        let media = MediaRuntime::new(&cfg, &metrics, NSFWClient::disabled(), external_client)
            .expect("test media runtime limits are valid");
        Self {
            cfg,
            metrics,
            store,
            media,
        }
    }
}

impl MediaRuntime {
    fn new(
        cfg: &Config,
        metrics: &Arc<metrics::Metrics>,
        nsfw: NSFWClient,
        external_client: http_client::HttpClient,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            nsfw,
            external_client,
            external_buffer_bytes: ByteBudget::new(external_buffer_budget_bytes(cfg)),
            external_hints: new_external_hint_cache(cfg),
            transforms: TransformRuntime::new(cfg, metrics)?,
        })
    }

    pub(in crate::server) fn limits(&self) -> MediaLimits {
        self.transforms.limits()
    }

    pub(in crate::server) fn animation(&self) -> AnimationLimits {
        self.transforms.animation()
    }

    pub(in crate::server) fn nsfw(&self) -> &NSFWClient {
        &self.nsfw
    }

    pub(in crate::server) fn external_client(&self) -> &http_client::HttpClient {
        &self.external_client
    }

    pub(in crate::server) fn external_buffer_bytes(&self) -> &ByteBudget {
        &self.external_buffer_bytes
    }

    pub(in crate::server) fn external_hints(&self) -> &ExternalHintCache {
        &self.external_hints
    }

    pub(in crate::server) fn transforms(&self) -> &TransformRuntime {
        &self.transforms
    }
}

fn external_buffer_budget_bytes(cfg: &Config) -> usize {
    let concurrent_inputs = cfg
        .media
        .max_native_transforms
        .saturating_add(cfg.media.worker_queue_capacity)
        .max(1);
    constants::MAX_MEDIA_PROXY_BYTES
        .saturating_mul(concurrent_inputs)
        .saturating_add(RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX)
}
