// SPDX-License-Identifier: AGPL-3.0-or-later

mod klipy;
mod media_proxy;
mod router_impl;
mod shard_impl;
mod types;

use fluxer_svc::config::{Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::GifsRouter;
use shard_impl::GifsShard;

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
        "starting gifs service"
    );

    match config.mode {
        Mode::Router => {
            let router = GifsRouter::new(config.cache_max_entries, config.cache_ttl);
            fluxer_svc::router::run_router(&config, router, transport).await
        }
        Mode::Shard => {
            let shard = GifsShard::new(&config)?;
            fluxer_svc::shard::run_shard(&config, shard, transport).await
        }
    }
}
