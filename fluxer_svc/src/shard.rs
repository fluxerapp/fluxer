// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::metrics::ServiceMetrics;
use crate::transport::{
    Transport, TransportMessage, TransportSubscriber, reply_bytes, reply_json_error,
};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};
use tokio::sync::{Semaphore, TryAcquireError, oneshot};
use tokio::task::JoinSet;
use tracing::{debug, info, warn};

const MAX_SHARD_REQUEST_BYTES: usize = 2 * 1024 * 1024;
const SHARD_DRAIN_LINGER: Duration = Duration::from_millis(500);
const SHARD_ANNOUNCE_TIMEOUT: Duration = Duration::from_secs(1);
const DRAIN_IDLE_UNSUBSCRIBE: Duration = Duration::from_millis(250);
const DRAIN_UNSUBSCRIBE_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WireEncoding {
    Json,
    MessagePack,
}

impl WireEncoding {
    fn detect(payload: &[u8]) -> Self {
        match payload.iter().find(|byte| !byte.is_ascii_whitespace()) {
            Some(b'{') => Self::Json,
            _ => Self::MessagePack,
        }
    }

    fn decode<T: serde::de::DeserializeOwned>(self, payload: &[u8]) -> anyhow::Result<T> {
        match self {
            Self::Json => Ok(serde_json::from_slice(payload)?),
            Self::MessagePack => Ok(rmp_serde::from_slice(payload)?),
        }
    }

    fn encode<T: serde::Serialize>(self, value: &T) -> anyhow::Result<Vec<u8>> {
        match self {
            Self::Json => Ok(serde_json::to_vec(value)?),
            Self::MessagePack => Ok(rmp_serde::to_vec_named(value)?),
        }
    }
}

pub trait ShardService: Send + Sync + 'static {
    type Request: serde::Serialize + serde::de::DeserializeOwned + Send + 'static;
    type Response: serde::Serialize + serde::de::DeserializeOwned + Send + 'static;

    fn service_name(&self) -> &str;
    fn render_prometheus_metrics(&self, _output: &mut String) {}
    fn handle(
        &self,
        request: Self::Request,
    ) -> impl std::future::Future<Output = anyhow::Result<Self::Response>> + Send;
}

pub async fn run_shard<S>(
    config: &ServiceConfig,
    service: S,
    transport: impl Transport,
) -> anyhow::Result<()>
where
    S: ShardService,
{
    let service = Arc::new(service);
    let name = service.service_name().to_owned();
    let subject = format!("svc.{name}.shard.{}", config.shard_id);
    let metrics = shard_metrics(&service);
    let handler_transport = transport.clone();
    let handler_metrics = metrics.clone();
    serve(
        config,
        Endpoint {
            announce: Some((name.clone(), config.shard_id)),
            name,
            subject,
            queue_group: None,
            linger: SHARD_DRAIN_LINGER,
        },
        metrics,
        transport,
        move |msg| {
            handle_shard_request(
                msg,
                handler_transport.clone(),
                service.clone(),
                handler_metrics.clone(),
            )
        },
        crate::shutdown::wait_for_shutdown(),
    )
    .await
}

pub(crate) fn shard_metrics<S: ShardService>(service: &Arc<S>) -> Arc<ServiceMetrics> {
    let metrics_service = service.clone();
    let additional_renderer: Arc<dyn Fn(&mut String) + Send + Sync> =
        Arc::new(move |output| metrics_service.render_prometheus_metrics(output));
    let metrics = Arc::new(ServiceMetrics::with_additional_renderer(
        additional_renderer,
    ));
    metrics.init();
    metrics
}

async fn handle_shard_request<S, T>(
    msg: T::Message,
    transport: T,
    service: Arc<S>,
    metrics: Arc<ServiceMetrics>,
) where
    S: ShardService,
    T: Transport,
{
    if msg.payload().len() > MAX_SHARD_REQUEST_BYTES {
        warn!(
            payload_bytes = msg.payload().len(),
            max_payload_bytes = MAX_SHARD_REQUEST_BYTES,
            "dropping oversized shard request"
        );
        return;
    }
    let request_start = Instant::now();
    metrics.record_request();
    let encoding = WireEncoding::detect(msg.payload());
    let request: S::Request = match encoding.decode(msg.payload()) {
        Ok(r) => r,
        Err(err) => {
            warn!(error = %err, ?encoding, "failed to decode shard request");
            metrics.record_request_error();
            reply_json_error(&msg, &transport, "shard_request_decode_error").await;
            return;
        }
    };

    match service.handle(request).await {
        Ok(response) => {
            metrics.record_request_duration(request_start.elapsed().as_millis() as u64);
            if msg.has_reply() {
                match encoding.encode(&response) {
                    Ok(response_bytes) => {
                        reply_bytes(&msg, &transport, &response_bytes).await;
                    }
                    Err(err) => {
                        warn!(error = %err, ?encoding, "failed to encode shard response");
                        metrics.record_request_error();
                        reply_json_error(&msg, &transport, "encode_error").await;
                    }
                }
            }
        }
        Err(err) => {
            warn!(error = %err, "shard handler returned error");
            metrics.record_request_error();
            metrics.record_request_duration(request_start.elapsed().as_millis() as u64);
            reply_json_error(&msg, &transport, "shard_handler_error").await;
        }
    }
}

pub(crate) struct Endpoint {
    pub(crate) name: String,
    pub(crate) subject: String,
    pub(crate) queue_group: Option<String>,
    pub(crate) announce: Option<(String, u32)>,
    pub(crate) linger: Duration,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum ShardState {
    Draining,
    Ready,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct ShardStateNotice {
    pub(crate) shard_id: u32,
    pub(crate) state: ShardState,
}

pub(crate) fn control_subject(service_name: &str) -> String {
    format!("svc.{service_name}.ctl")
}

async fn announce_state(
    transport: &impl Transport,
    announce: &Option<(String, u32)>,
    state: ShardState,
) {
    let Some((service_name, shard_id)) = announce else {
        return;
    };
    let notice = ShardStateNotice {
        shard_id: *shard_id,
        state,
    };
    let payload = serde_json::to_vec(&notice).expect("shard state notices are JSON-serializable");
    let announce = async {
        transport
            .publish(&control_subject(service_name), &payload)
            .await?;
        transport.flush().await
    };
    match tokio::time::timeout(SHARD_ANNOUNCE_TIMEOUT, announce).await {
        Ok(Ok(())) => {}
        Ok(Err(error)) => warn!(error = %error, ?state, "failed to announce shard state"),
        Err(_) => warn!(?state, "timed out announcing shard state"),
    }
}

pub(crate) async fn serve<T, H, F>(
    config: &ServiceConfig,
    endpoint: Endpoint,
    metrics: Arc<ServiceMetrics>,
    transport: T,
    handle: H,
    shutdown: impl std::future::Future<Output = ()>,
) -> anyhow::Result<()>
where
    T: Transport,
    H: Fn(T::Message) -> F + Send + Sync + 'static,
    F: std::future::Future<Output = ()> + Send + 'static,
{
    let Endpoint {
        name,
        subject,
        queue_group,
        announce,
        linger,
    } = endpoint;
    let health_addr = config.listen_addr;
    let is_serving = Arc::new(AtomicBool::new(false));
    let request_permits = Arc::new(Semaphore::new(config.max_concurrent_requests));

    let mut tasks = JoinSet::new();

    let http_is_serving = is_serving.clone();
    let http_metrics = metrics.clone();
    tasks.spawn(async move {
        crate::server::run_http(health_addr, http_is_serving, http_metrics, name).await
    });

    let listen_is_serving = is_serving.clone();
    let listen_announce = announce.clone();
    let listen_transport = transport.clone();
    let (stop_requests, mut stop_requests_rx) = oneshot::channel::<()>();
    let request_task = tasks.spawn(async move {
        let transport = listen_transport;
        let mut requests = JoinSet::new();
        let mut draining = false;
        let mut unsubscribed = false;
        let mut announced = false;
        let mut unsubscribe_deadline = tokio::time::Instant::now();
        let idle = tokio::time::sleep(DRAIN_IDLE_UNSUBSCRIBE);
        tokio::pin!(idle);
        'listening: loop {
            let subscribe = async {
                match queue_group.as_deref() {
                    Some(queue_group) => transport.subscribe_queue(&subject, queue_group).await,
                    None => transport.subscribe(&subject).await,
                }
            };
            let mut sub = tokio::select! {
                _ = &mut stop_requests_rx => break 'listening,
                result = subscribe => result?,
            };
            transport.flush().await?;
            info!(
                subject,
                max_concurrent_requests = request_permits.available_permits(),
                "listening for requests"
            );
            if !announced {
                announced = true;
                listen_is_serving.store(true, Ordering::SeqCst);
                announce_state(&transport, &listen_announce, ShardState::Ready).await;
            }

            loop {
                tokio::select! {
                    _ = &mut stop_requests_rx, if !draining => {
                        draining = true;
                        unsubscribe_deadline = tokio::time::Instant::now() + DRAIN_UNSUBSCRIBE_TIMEOUT;
                        idle.as_mut().reset(tokio::time::Instant::now() + DRAIN_IDLE_UNSUBSCRIBE);
                        if let Err(error) = sub.drain().await {
                            warn!(error = %error, subject, "failed to drain subscription");
                            unsubscribed = true;
                            unsubscribe(&mut sub, &subject).await;
                        }
                        info!(subject, "draining subscription");
                    }
                    _ = &mut idle, if draining && !unsubscribed => {
                        unsubscribed = true;
                        unsubscribe(&mut sub, &subject).await;
                    }
                    result = requests.join_next(), if !requests.is_empty() => {
                        if let Err(err) = result.expect("nonempty request set") {
                            warn!(error = %err, "request task failed");
                        }
                    }
                    msg_opt = sub.next() => {
                        let Some(msg) = msg_opt else {
                            if draining {
                                break 'listening;
                            }
                            warn!("subscription stream ended, will re-subscribe");
                            break;
                        };
                        if draining {
                            idle.as_mut().reset(
                                unsubscribe_deadline
                                    .min(tokio::time::Instant::now() + DRAIN_IDLE_UNSUBSCRIBE),
                            );
                        }

                        while let Some(result) = requests.try_join_next() {
                            if let Err(err) = result {
                                warn!(error = %err, "request task failed");
                            }
                        }

                        let permit = match request_permits.clone().try_acquire_owned() {
                            Ok(permit) => permit,
                            Err(TryAcquireError::NoPermits) => {
                                debug!("shedding request, no permits available");
                                metrics.record_request();
                                metrics.record_request_error();
                                reply_json_error(&msg, &transport, "overloaded").await;
                                continue;
                            }
                            Err(TryAcquireError::Closed) => return anyhow::Ok(()),
                        };
                        let request = handle(msg);
                        requests.spawn(async move {
                            let _permit = permit;
                            request.await;
                        });
                    }
                }
            }
        }
        while let Some(result) = requests.join_next().await {
            if let Err(err) = result {
                warn!(error = %err, "request task failed while draining");
            }
        }
        if let Err(error) = transport.flush().await {
            warn!(error = %error, subject, "failed to flush replies while draining");
        }
        anyhow::Ok(())
    });

    tokio::select! {
        result = tasks.join_next() => {
            match result {
                Some(Ok(Ok(()))) => Ok(()),
                Some(Ok(Err(error))) => Err(error),
                Some(Err(error)) => Err(error.into()),
                None => Ok(()),
            }
        }
        _ = shutdown => {
            info!("shutting down, beginning graceful drain");

            is_serving.store(false, Ordering::SeqCst);
            announce_state(&transport, &announce, ShardState::Draining).await;
            tokio::time::sleep(linger).await;

            let _ = stop_requests.send(());
            let drain = async {
                while let Some(joined) = tasks.join_next_with_id().await {
                    let task_id = match joined {
                        Ok((task_id, Ok(()))) => task_id,
                        Ok((task_id, Err(error))) => {
                            warn!(error = %error, "service task failed while draining");
                            task_id
                        }
                        Err(error) => {
                            warn!(error = %error, "service task panicked while draining");
                            error.id()
                        }
                    };
                    if task_id == request_task.id() {
                        return;
                    }
                }
            };
            match tokio::time::timeout(crate::shutdown::DEFAULT_DRAIN_TIMEOUT, drain).await {
                Ok(()) => {
                    info!(
                        max_concurrent_requests = config.max_concurrent_requests,
                        "graceful drain completed"
                    );
                }
                Err(_) => {
                    warn!(
                        timeout_secs = crate::shutdown::DEFAULT_DRAIN_TIMEOUT.as_secs(),
                        "drain timeout exceeded, proceeding with shutdown"
                    );
                }
            }

            info!("shutdown complete");
            Ok(())
        }
    }
}

async fn unsubscribe(sub: &mut impl TransportSubscriber, subject: &str) {
    match sub.unsubscribe().await {
        Ok(()) => info!(subject, "unsubscribed while draining"),
        Err(error) => warn!(error = %error, subject, "failed to unsubscribe while draining"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Mode;
    use crate::transport::InMemoryTransport;
    use serde::{Deserialize, Serialize};
    use std::time::Duration;

    #[derive(Serialize, Deserialize)]
    struct MockRequest {
        key: String,
    }

    #[derive(Serialize, Deserialize)]
    struct MockResponse {
        key: String,
    }

    struct EchoShard;

    impl ShardService for EchoShard {
        type Request = MockRequest;
        type Response = MockResponse;

        fn service_name(&self) -> &str {
            "mock"
        }

        async fn handle(&self, request: MockRequest) -> anyhow::Result<MockResponse> {
            Ok(MockResponse { key: request.key })
        }
    }

    #[test]
    fn wire_encoding_detects_json_and_msgpack_payloads() {
        let json = serde_json::to_vec(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();
        let msgpack = rmp_serde::to_vec_named(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();

        assert_eq!(WireEncoding::detect(&json), WireEncoding::Json);
        assert_eq!(
            WireEncoding::detect(b"  \n{\"key\":\"a\"}"),
            WireEncoding::Json
        );
        assert_eq!(WireEncoding::detect(&msgpack), WireEncoding::MessagePack);
        assert_eq!(WireEncoding::detect(b""), WireEncoding::MessagePack);
    }

    #[tokio::test]
    async fn shard_replies_in_the_request_encoding() {
        let transport = InMemoryTransport::new();
        let config = ServiceConfig::for_test(Mode::Shard, 0, 1);
        let shard_transport = transport.clone();
        let shard_task =
            tokio::spawn(async move { run_shard(&config, EchoShard, shard_transport).await });

        tokio::time::sleep(Duration::from_millis(25)).await;

        let msgpack_request = rmp_serde::to_vec_named(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();
        let msgpack_response = transport
            .request("svc.mock.shard.0", &msgpack_request, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(
            rmp_serde::from_slice::<MockResponse>(&msgpack_response)
                .unwrap()
                .key,
            "a"
        );

        let json_request = serde_json::to_vec(&MockRequest {
            key: "b".to_owned(),
        })
        .unwrap();
        let json_response = transport
            .request("svc.mock.shard.0", &json_request, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(json_response, br#"{"key":"b"}"#);

        shard_task.abort();
    }

    #[tokio::test]
    async fn requests_arriving_after_sigterm_are_answered_before_exit() {
        let transport = InMemoryTransport::new();
        let config = ServiceConfig::for_test(Mode::Shard, 0, 1);
        let (shutdown_tx, shutdown_rx) = oneshot::channel::<()>();
        let release = Arc::new(tokio::sync::Notify::new());
        let handler_release = release.clone();
        let handler_transport = transport.clone();
        let server_transport = transport.clone();
        let server = tokio::spawn(async move {
            serve(
                &config,
                Endpoint {
                    name: "mock".to_owned(),
                    subject: "svc.mock.shard.0".to_owned(),
                    queue_group: None,
                    announce: None,
                    linger: Duration::from_millis(50),
                },
                Arc::new(ServiceMetrics::default()),
                server_transport,
                move |msg| {
                    let transport = handler_transport.clone();
                    let release = handler_release.clone();
                    async move {
                        release.notified().await;
                        reply_bytes(&msg, &transport, msg.payload()).await
                    }
                },
                async {
                    let _ = shutdown_rx.await;
                },
            )
            .await
        });
        tokio::time::sleep(Duration::from_millis(25)).await;
        shutdown_tx.send(()).unwrap();
        tokio::time::sleep(Duration::from_millis(10)).await;

        let requests = tokio::spawn(futures::future::join_all((0..8u8).map(|i| {
            let transport = transport.clone();
            async move {
                transport
                    .request("svc.mock.shard.0", &[i], Duration::from_secs(1))
                    .await
            }
        })));
        tokio::time::sleep(Duration::from_millis(100)).await;
        release.notify_waiters();

        for (i, response) in requests.await.unwrap().into_iter().enumerate() {
            assert_eq!(response.unwrap(), vec![i as u8]);
        }
        server.await.unwrap().unwrap();
    }
}
