// SPDX-License-Identifier: AGPL-3.0-or-later

mod activity_pub;
mod cache_policy;
mod charset;
mod direct_media;
mod embed_normalizer;
mod html_markdown;
mod html_parser;
mod http_fetch;
mod media_proxy;
mod network_policy;
mod oembed;
mod resolvers;
mod router_impl;
mod sanitizer;
mod shard_impl;
mod text_limits;
mod types;

use fluxer_svc::config::{Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::UnfurlRouter;
use shard_impl::UnfurlShard;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    fluxer_svc::init_tracing();
    let config = ServiceConfig::from_env()?;
    let transport = NatsTransport::connect(&config.nats_url).await?;

    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting unfurl service"
    );

    match config.mode {
        Mode::Router => {
            let router = UnfurlRouter::new(config.cache_max_entries, config.cache_ttl);
            fluxer_svc::router::run_router(&config, router, transport).await
        }
        Mode::Shard => {
            let shard = UnfurlShard::new(config.cache_ttl);
            fluxer_svc::shard::run_shard(&config, shard, transport).await
        }
    }
}
