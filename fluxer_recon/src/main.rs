// SPDX-License-Identifier: AGPL-3.0-or-later

use std::net::SocketAddr;
use std::sync::Arc;

use tokio::task::JoinSet;

use fluxer_recon::clock::{SharedClock, SystemClock};
use fluxer_recon::config::ReconConfig;
use fluxer_recon::control::{CONTROL_SUBJECT, INSTANCE_SUBJECT, run_control};
use fluxer_recon::gateway::nats::NatsGateway;
use fluxer_recon::metrics::ReconMetrics;
use fluxer_recon::runtime::{
    InstanceIdentity, Shared, boot_assertions, monotonic_now, run_singleton, runtime_tasks,
};
use fluxer_recon::service::{
    GuardedClock, ReconEngine, SharedFleet, build_livekit, connect_topology, internal_endpoint,
    run_census, run_discovery, run_engine,
};
use fluxer_recon::webhook::run_webhook;
use fluxer_svc::config::ServiceConfig;
use fluxer_svc::metrics::ServiceMetrics;
use fluxer_svc::shutdown::{DEFAULT_DRAIN_TIMEOUT, drain_with_timeout, wait_for_shutdown};
use fluxer_svc::transport::NatsTransport;

enum Exit {
    Task(anyhow::Result<()>),
    Signal,
}

fn main() -> anyhow::Result<()> {
    fluxer_svc::init_tracing();

    let service = ServiceConfig::from_env()?;
    let recon = ReconConfig::from_env()?;
    boot_assertions(&service, &recon)?;

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(recon.worker_threads)
        .max_blocking_threads(1)
        .thread_name("recon")
        .enable_all()
        .build()?;

    runtime.block_on(serve(service, recon))
}

async fn serve(service: ServiceConfig, recon: ReconConfig) -> anyhow::Result<()> {
    let transport =
        NatsTransport::connect(&service.nats_url, service.nats_auth_token.as_deref()).await?;

    let clock: SharedClock = GuardedClock::shared(SystemClock::shared());
    let instance = InstanceIdentity::generate();
    let shared = Shared::new(recon.clone(), instance, monotonic_now());

    let metrics = Arc::new(ServiceMetrics::with_additional_renderer(
        ReconMetrics::renderer(shared.metrics()),
    ));
    metrics.init();

    let topology = connect_topology(&service).await?;
    let internal = internal_endpoint(&recon);
    let fleet = SharedFleet::new(build_livekit(&[], &internal)?);

    tracing::info!(
        service = service.service_name,
        listen_addr = %service.listen_addr,
        instance_id = shared.instance().id,
        mode = recon.mode.as_str(),
        worker_threads = recon.worker_threads,
        tick_ms = recon.tick_ms,
        coverage_target_ms = recon.coverage_target_ms,
        topology_backend = topology.backend(),
        livekit_internal_endpoint = internal.url.as_deref().unwrap_or("none"),
        livekit_default_region = internal.default_region_id.as_deref().unwrap_or("none"),
        control_subject = CONTROL_SUBJECT,
        instance_subject = INSTANCE_SUBJECT,
        tasks = ?runtime_tasks(&recon),
        "starting recon service"
    );

    let mut tasks: JoinSet<anyhow::Result<()>> = JoinSet::new();

    let health_addr = service.listen_addr;
    let health_serving = shared.readiness().flag();
    let health_metrics = metrics.clone();
    let health_name = service.service_name.clone();
    tasks.spawn(async move {
        fluxer_svc::server::run_http(health_addr, health_serving, health_metrics, health_name).await
    });

    let engine = ReconEngine::new(
        shared.clone(),
        NatsGateway::new(transport.clone()),
        fleet.clone(),
        clock.clone(),
    );
    tasks.spawn(async move { run_engine(engine).await });

    let census_shared = shared.clone();
    let census_gateway = NatsGateway::new(transport.clone());
    let census_clock = clock.clone();
    tasks.spawn(async move { run_census(census_shared, census_gateway, census_clock).await });

    let discovery_shared = shared.clone();
    let discovery_fleet = fleet.clone();
    let discovery_clock = clock.clone();
    tasks.spawn(async move {
        run_discovery(discovery_shared, discovery_fleet, topology, discovery_clock).await
    });

    let control_shared = shared.clone();
    let control_transport = transport.clone();
    tasks.spawn(async move { run_control(control_shared, control_transport).await });

    let singleton_shared = shared.clone();
    let singleton_transport = transport.clone();
    tasks.spawn(async move { run_singleton(singleton_shared, singleton_transport).await });

    if recon.webhook_enabled {
        let webhook_shared = shared.clone();
        let webhook_clock = clock.clone();
        let webhook_addr = SocketAddr::from((service.listen_addr.ip(), recon.webhook_port));
        tasks.spawn(async move { run_webhook(webhook_shared, webhook_clock, webhook_addr).await });
    }

    let exit = tokio::select! {
        result = tasks.join_next() => Exit::Task(match result {
            Some(Ok(Ok(()))) => Ok(()),
            Some(Ok(Err(error))) => Err(error),
            Some(Err(error)) => Err(error.into()),
            None => Ok(()),
        }),
        () = wait_for_shutdown() => Exit::Signal,
    };

    shared.readiness().begin_shutdown();
    tasks.abort_all();
    drain_with_timeout(
        async { while tasks.join_next().await.is_some() {} },
        DEFAULT_DRAIN_TIMEOUT,
    )
    .await;

    match exit {
        Exit::Task(result) => {
            tracing::warn!("a recon task ended, shutting the service down");
            result
        }
        Exit::Signal => {
            tracing::info!("recon shutdown complete");
            Ok(())
        }
    }
}
