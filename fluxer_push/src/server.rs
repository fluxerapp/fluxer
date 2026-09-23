// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, DeliveryConfig};
use crate::delivery;
use crate::metrics::Metrics;
use crate::relay;
use crate::rollout::{self, RolloutSnapshot, RolloutStore};
use crate::rpc::RpcClient;
use crate::secret::SecretString;
use crate::tokens::TokenCache;
use crate::vendor;
use axum::Router;
use axum::extract::{ConnectInfo, State};
use axum::http::{HeaderValue, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use fluxer_svc::shutdown::{DEFAULT_DRAIN_TIMEOUT, drain_with_timeout, wait_for_shutdown};
use fluxer_svc::transport::NatsTransport;
use std::net::SocketAddr;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;
use tokio::net::TcpListener;
use tokio::sync::{oneshot, watch};
use tokio::task::JoinHandle;
use tracing::info;

const JOB_DRAIN_TIMEOUT: Duration = Duration::from_secs(25);

pub struct Sidecar {
    metrics: Arc<Metrics>,
    serving: AtomicBool,
}

impl Sidecar {
    pub fn new(metrics: Arc<Metrics>) -> Self {
        Self {
            metrics,
            serving: AtomicBool::new(false),
        }
    }

    pub fn set_serving(&self, serving: bool) {
        self.serving.store(serving, Ordering::SeqCst);
    }
}

pub fn sidecar_router(sidecar: Arc<Sidecar>) -> Router {
    Router::new()
        .route("/_health", get(readiness))
        .route("/_healthz", get(async || "OK"))
        .route("/_metrics", get(metrics_handler))
        .with_state(sidecar)
}

pub struct Serving {
    stop: oneshot::Sender<()>,
    served: JoinHandle<std::io::Result<()>>,
}

impl Serving {
    pub async fn stop(self) {
        let _ = self.stop.send(());
        drain_with_timeout(
            async {
                let _ = self.served.await;
            },
            DEFAULT_DRAIN_TIMEOUT,
        )
        .await;
    }
}

pub async fn serve(addr: SocketAddr, router: Router) -> anyhow::Result<Serving> {
    let listener = TcpListener::bind(addr).await?;
    let (stop, stopped) = oneshot::channel::<()>();
    let served = tokio::spawn(async move {
        axum::serve(
            listener,
            router.into_make_service_with_connect_info::<SocketAddr>(),
        )
        .with_graceful_shutdown(async move {
            let _ = stopped.await;
        })
        .await
    });
    Ok(Serving { stop, served })
}

pub struct AppState {
    pub(crate) cfg: DeliveryConfig,
    pub(crate) metrics: Arc<Metrics>,
    pub(crate) sidecar: Arc<Sidecar>,
    pub(crate) rollout: RolloutStore<RolloutSnapshot>,
    pub(crate) rpc: RpcClient,
    pub(crate) http: reqwest::Client,
    pub(crate) web_push_http: reqwest::Client,
    pub(crate) apns_http: reqwest::Client,
    pub(crate) tokens: TokenCache,
    pub(crate) draining: watch::Sender<bool>,
}

impl AppState {
    pub(crate) fn try_new(cfg: DeliveryConfig) -> anyhow::Result<Self> {
        let metrics = Arc::new(Metrics::new());
        let http = vendor::http_client()?;
        Ok(Self {
            rpc: RpcClient::new(&cfg.rpc, http.clone(), Arc::clone(&metrics)),
            rollout: RolloutStore::new(),
            sidecar: Arc::new(Sidecar::new(Arc::clone(&metrics))),
            web_push_http: vendor::web_push_http_client()?,
            apns_http: vendor::apns_http_client()?,
            tokens: TokenCache::new(),
            draining: watch::Sender::new(false),
            cfg,
            metrics,
            http,
        })
    }
}

pub async fn run(cfg: Config) -> anyhow::Result<()> {
    match cfg {
        Config::Delivery(cfg) => run_delivery(*cfg).await,
        Config::Relay(cfg) => relay::run(*cfg).await,
    }
}

async fn run_delivery(cfg: DeliveryConfig) -> anyhow::Result<()> {
    let state = Arc::new(AppState::try_new(cfg)?);
    let addr = state.cfg.bind_addr;
    let serving = serve(addr, sidecar_router(Arc::clone(&state.sidecar))).await?;
    info!(%addr, vapid_subject = state.cfg.vapid.email, "push sidecar listening");

    let transport = NatsTransport::connect(
        &state.cfg.nats.url,
        state.cfg.nats.auth_token.as_ref().map(SecretString::expose),
    )
    .await?;

    state.sidecar.set_serving(true);

    let subscriber = tokio::spawn({
        let transport = transport.clone();
        let state = Arc::clone(&state);
        async move {
            rollout::run_rollout_subscriber(
                transport,
                &state.rpc,
                &state.rollout,
                &state.metrics,
                rollout::RECONCILE_INTERVAL,
            )
            .await
        }
    });
    let jobs = tokio::spawn(delivery::run_job_subscribers(transport, Arc::clone(&state)));

    wait_for_shutdown().await;
    state.sidecar.set_serving(false);
    subscriber.abort();
    drain_jobs(&state, jobs, JOB_DRAIN_TIMEOUT).await;
    serving.stop().await;
    Ok(())
}

async fn drain_jobs(state: &AppState, jobs: JoinHandle<()>, timeout: Duration) {
    state.draining.send_replace(true);
    let running = jobs.abort_handle();
    drain_with_timeout(
        async {
            let _ = jobs.await;
        },
        timeout,
    )
    .await;
    running.abort();
}

async fn readiness(State(sidecar): State<Arc<Sidecar>>) -> impl IntoResponse {
    if sidecar.serving.load(Ordering::SeqCst) {
        (StatusCode::OK, "OK")
    } else {
        (StatusCode::SERVICE_UNAVAILABLE, "NOT READY")
    }
}

async fn metrics_handler(
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    State(sidecar): State<Arc<Sidecar>>,
) -> Response {
    if !is_loopback_peer(&peer) {
        return (StatusCode::FORBIDDEN, "FORBIDDEN").into_response();
    }
    (
        [(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/plain; version=0.0.4; charset=utf-8"),
        )],
        sidecar.metrics.render(),
    )
        .into_response()
}

fn is_loopback_peer(peer: &SocketAddr) -> bool {
    peer.ip().to_canonical().is_loopback()
}
