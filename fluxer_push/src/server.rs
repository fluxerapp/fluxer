// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, DeliveryConfig};
use crate::delivery;
use crate::metrics::Metrics;
use crate::relay;
use crate::relay_consent::{self, RelayConsentStore};
use crate::rpc::RpcClient;
use crate::tokens::TokenCache;
use crate::vendor;
use axum::Router;
use axum::extract::ConnectInfo;
use fluxer_common::secret::SecretString;
use fluxer_svc::shutdown::{DEFAULT_DRAIN_TIMEOUT, drain_with_timeout};
use fluxer_svc::transport::NatsTransport;
use hyper::body::Incoming;
use hyper::service::service_fn;
use hyper_util::rt::{TokioExecutor, TokioIo};
use hyper_util::server::conn::auto::Builder;
use std::io;
use std::net::SocketAddr;
use std::pin::pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{Notify, oneshot, watch};
use tokio::task::JoinHandle;
use tokio::time::{Instant, sleep};
use tokio_util::sync::CancellationToken;
use tower_service::Service;
use tracing::{Instrument as _, error, info};

const JOB_DRAIN_TIMEOUT: Duration = Duration::from_secs(25);
const FIRST_REQUEST_TIMEOUT: Duration = Duration::from_secs(30);
const CONNECTION_IDLE_TIMEOUT: Duration = Duration::from_secs(120);
const CONNECTION_CLOSE_GRACE: Duration = Duration::from_secs(15);
const ACCEPT_ERROR_PAUSE: Duration = Duration::from_secs(1);

pub struct Sidecar {
    metrics: Arc<Metrics>,
    serving: Arc<AtomicBool>,
}

impl Sidecar {
    pub fn new(metrics: Arc<Metrics>) -> Self {
        Self {
            metrics,
            serving: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn router(&self) -> Router {
        let metrics = self.metrics.clone();
        fluxer_svc::server::ops_router(self.serving.clone(), Arc::new(move || metrics.render()))
    }

    pub fn set_serving(&self, serving: bool) {
        self.serving.store(serving, Ordering::SeqCst);
    }
}

pub struct Serving {
    stop: oneshot::Sender<()>,
    served: JoinHandle<()>,
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
    let served = tokio::spawn(accept_connections(listener, router, stopped).in_current_span());
    Ok(Serving { stop, served })
}

async fn accept_connections(
    listener: TcpListener,
    router: Router,
    mut stopped: oneshot::Receiver<()>,
) {
    let stopping = watch::Sender::new(false);
    let (open_tx, open_rx) = watch::channel(());
    loop {
        let (stream, remote_addr) = tokio::select! {
            accepted = listener.accept() => match accepted {
                Ok(accepted) => accepted,
                Err(err) => {
                    pause_after_accept_error(err).await;
                    continue;
                }
            },
            _ = &mut stopped => break,
        };
        tokio::spawn(
            serve_connection(
                stream,
                remote_addr,
                router.clone(),
                stopping.subscribe(),
                open_rx.clone(),
            )
            .in_current_span(),
        );
    }
    stopping.send_replace(true);
    drop(open_rx);
    drop(listener);
    open_tx.closed().await;
}

async fn pause_after_accept_error(err: io::Error) {
    if matches!(
        err.kind(),
        io::ErrorKind::ConnectionRefused
            | io::ErrorKind::ConnectionAborted
            | io::ErrorKind::ConnectionReset
    ) {
        return;
    }
    error!(error = %err, "accept error");
    sleep(ACCEPT_ERROR_PAUSE).await;
}

async fn serve_connection(
    stream: TcpStream,
    remote_addr: SocketAddr,
    router: Router,
    mut stopping: watch::Receiver<bool>,
    _open: watch::Receiver<()>,
) {
    let activity = Arc::new(Notify::new());
    let service = service_fn({
        let activity = Arc::clone(&activity);
        move |mut req: hyper::Request<Incoming>| {
            activity.notify_one();
            req.extensions_mut().insert(ConnectInfo(remote_addr));
            router.clone().call(req).in_current_span()
        }
    });
    let mut builder = Builder::new(TokioExecutor::new());
    builder.http2().enable_connect_protocol();
    let mut conn = pin!(builder.serve_connection_with_upgrades(TokioIo::new(stream), service));
    let mut deadline = pin!(sleep(FIRST_REQUEST_TIMEOUT));
    let mut served_request = false;
    let mut closing = false;
    loop {
        tokio::select! {
            _ = conn.as_mut() => break,
            _ = activity.notified(), if !closing => {
                served_request = true;
                deadline.as_mut().reset(Instant::now() + CONNECTION_IDLE_TIMEOUT);
            }
            _ = stopping.wait_for(|stopping| *stopping), if !closing => {
                conn.as_mut().graceful_shutdown();
                closing = true;
                deadline.as_mut().reset(Instant::now() + CONNECTION_CLOSE_GRACE);
            }
            _ = deadline.as_mut() => {
                if closing || !served_request {
                    break;
                }
                conn.as_mut().graceful_shutdown();
                closing = true;
                deadline.as_mut().reset(Instant::now() + CONNECTION_CLOSE_GRACE);
            }
        }
    }
}

pub struct AppState {
    pub(crate) cfg: DeliveryConfig,
    pub(crate) metrics: Arc<Metrics>,
    pub(crate) sidecar: Arc<Sidecar>,
    pub(crate) relay_consent: RelayConsentStore,
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
        let relay_consent = RelayConsentStore::new(cfg.relay_consent_accepted);
        metrics.record_relay_consent_accepted(relay_consent.accepted());
        let http = vendor::http_client()?;
        Ok(Self {
            rpc: RpcClient::new(&cfg.rpc, http.clone(), Arc::clone(&metrics)),
            relay_consent,
            sidecar: Arc::new(Sidecar::new(Arc::clone(&metrics))),
            web_push_http: vendor::web_push_http_client(&cfg.private_hosts)?,
            apns_http: vendor::apns_http_client()?,
            tokens: TokenCache::new(),
            draining: watch::Sender::new(false),
            cfg,
            metrics,
            http,
        })
    }
}

pub async fn run(cfg: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    match cfg {
        Config::Delivery(cfg) => run_delivery(*cfg, shutdown).await,
        Config::Relay(cfg) => relay::run(*cfg, shutdown).await,
    }
}

async fn run_delivery(cfg: DeliveryConfig, shutdown: CancellationToken) -> anyhow::Result<()> {
    let state = Arc::new(AppState::try_new(cfg)?);
    let addr = state.cfg.bind_addr;
    let serving = serve(addr, state.sidecar.router()).await?;
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
            relay_consent::run_subscriber(
                transport,
                &state.rpc,
                &state.relay_consent,
                &state.metrics,
                relay_consent::RECONCILE_INTERVAL,
            )
            .await
        }
        .in_current_span()
    });
    let jobs = tokio::spawn(
        delivery::run_job_subscribers(transport, Arc::clone(&state)).in_current_span(),
    );

    shutdown.cancelled().await;
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
