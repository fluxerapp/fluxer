// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::metrics::ServiceMetrics;
use crate::transport::{
    Transport, TransportMessage, TransportSubscriber, reply_bytes, reply_json_error,
};
use anyhow::Context;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;
use tokio::sync::{Semaphore, TryAcquireError, oneshot};
use tokio::task::JoinSet;
use tracing::{debug, info, warn};

const MAX_SHARD_REQUEST_BYTES: usize = 2 * 1024 * 1024;

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
    let shard_id = config.shard_id;
    let shard_subject = format!("svc.{name}.shard.{shard_id}");
    let health_addr = config.listen_addr;

    let metrics_service = service.clone();
    let additional_renderer: Arc<dyn Fn(&mut String) + Send + Sync> =
        Arc::new(move |output| metrics_service.render_prometheus_metrics(output));
    let metrics = Arc::new(ServiceMetrics::with_additional_renderer(
        additional_renderer,
    ));
    metrics.init();

    let is_serving = Arc::new(AtomicBool::new(false));
    let request_permits = Arc::new(Semaphore::new(config.max_concurrent_requests));

    is_serving.store(true, Ordering::SeqCst);

    let mut tasks = JoinSet::new();

    let http_is_serving = is_serving.clone();
    let http_metrics = metrics.clone();
    let http_name = name.clone();
    tasks.spawn(async move {
        crate::server::run_http(health_addr, http_is_serving, http_metrics, http_name).await
    });

    let shard_transport = transport.clone();
    let shard_service = service.clone();
    let shard_is_serving = is_serving.clone();
    let shard_permits = request_permits.clone();
    let shard_metrics = metrics.clone();
    let (stop_requests, mut stop_requests_rx) = oneshot::channel();
    let request_task = tasks.spawn(async move {
        let mut requests = JoinSet::new();
        'listening: loop {
            let mut sub = tokio::select! {
                _ = &mut stop_requests_rx => break 'listening,
                result = shard_transport.subscribe(&shard_subject) => result?,
            };
            info!(
                subject = shard_subject,
                shard_id,
                max_concurrent_requests = shard_permits.available_permits(),
                "shard listening for requests"
            );

            loop {
                tokio::select! {
                    _ = &mut stop_requests_rx => break 'listening,
                    result = requests.join_next(), if !requests.is_empty() => {
                        if let Err(err) = result.expect("nonempty shard request set") {
                            warn!(error = %err, "shard request task failed");
                        }
                    }
                    msg_opt = sub.next() => {
                        let Some(msg) = msg_opt else {
                            warn!("shard subscription stream ended, will re-subscribe");
                            break;
                        };

                        while let Some(result) = requests.try_join_next() {
                            if let Err(err) = result {
                                warn!(error = %err, "shard request task failed");
                            }
                        }
                        if !shard_is_serving.load(Ordering::SeqCst) {
                            continue;
                        }

                        let transport = shard_transport.clone();
                        let service = shard_service.clone();
                        let is_serving = shard_is_serving.clone();
                        let metrics = shard_metrics.clone();
                        if msg.payload().len() > MAX_SHARD_REQUEST_BYTES {
                            warn!(
                                payload_bytes = msg.payload().len(),
                                max_payload_bytes = MAX_SHARD_REQUEST_BYTES,
                                "dropping oversized shard request"
                            );
                            continue;
                        }
                        let permit = match shard_permits.clone().try_acquire_owned() {
                            Ok(permit) => permit,
                            Err(TryAcquireError::NoPermits) => {
                                debug!("shedding shard request, no permits available");
                                shard_metrics.record_request();
                                shard_metrics.record_request_error();
                                reply_json_error(&msg, &transport, "overloaded").await;
                                continue;
                            }
                            Err(TryAcquireError::Closed) => return anyhow::Ok(()),
                        };
                        requests.spawn(async move {
                            let _permit = permit;
                            if !is_serving.load(Ordering::SeqCst) {
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
                                    reply_json_error(&msg, &transport, "shard_request_decode_error")
                                        .await;
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
                                                warn!(
                                                    error = %err,
                                                    ?encoding,
                                                    "failed to encode shard response"
                                                );
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
                        });
                    }
                }
            }
        }
        while let Some(result) = requests.join_next().await {
            result.context("shard request task failed while draining")?;
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
        _ = crate::shutdown::wait_for_shutdown() => {
            info!("shard shutting down, beginning graceful drain");

            is_serving.store(false, Ordering::SeqCst);

            let _ = stop_requests.send(());
            let drain = async {
                loop {
                    let (task_id, result) = tasks.join_next_with_id().await
                        .expect("running shard listener while draining")
                        .context("shard service task failed while draining")?;
                    result?;
                    if task_id == request_task.id() {
                        return anyhow::Ok(());
                    }
                }
            };
            match tokio::time::timeout(crate::shutdown::DEFAULT_DRAIN_TIMEOUT, drain).await {
                Ok(result) => {
                    result?;
                    info!(
                        max_concurrent_requests = config.max_concurrent_requests,
                        "all in-flight requests drained"
                    );
                    info!("graceful drain completed");
                }
                Err(_) => {
                    warn!(
                        timeout_secs = crate::shutdown::DEFAULT_DRAIN_TIMEOUT.as_secs(),
                        "drain timeout exceeded, proceeding with shutdown"
                    );
                }
            }

            info!("shard shutdown complete");
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{DatabaseBackend, Mode, ServiceConfig};
    use crate::transport::InMemoryTransport;
    use serde::{Deserialize, Serialize};
    use std::time::Duration;
    use tokio::sync::Notify;

    #[derive(Serialize, Deserialize)]
    struct MockRequest {
        key: String,
    }

    #[derive(Serialize, Deserialize)]
    struct MockResponse {
        key: String,
    }

    struct BlockingShard {
        started: Arc<Notify>,
    }

    impl ShardService for BlockingShard {
        type Request = MockRequest;
        type Response = MockResponse;

        fn service_name(&self) -> &str {
            "mock"
        }

        async fn handle(&self, _request: MockRequest) -> anyhow::Result<MockResponse> {
            self.started.notify_one();
            std::future::pending().await
        }
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
        let config = test_config(4);
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
    async fn shard_sheds_requests_when_permits_are_exhausted() {
        let transport = InMemoryTransport::new();
        let started = Arc::new(Notify::new());
        let shard = BlockingShard {
            started: started.clone(),
        };
        let config = test_config(1);
        let shard_transport = transport.clone();
        let shard_task =
            tokio::spawn(async move { run_shard(&config, shard, shard_transport).await });

        tokio::time::sleep(Duration::from_millis(25)).await;

        let payload = rmp_serde::to_vec_named(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();

        let client = {
            let transport = transport.clone();
            let payload = payload.clone();
            tokio::spawn(async move {
                transport
                    .request("svc.mock.shard.0", &payload, Duration::from_secs(10))
                    .await
            })
        };

        tokio::time::timeout(Duration::from_millis(250), started.notified())
            .await
            .expect("shard should start the first request and hold the only permit");

        let response = tokio::time::timeout(
            Duration::from_millis(250),
            transport.request("svc.mock.shard.0", &payload, Duration::from_secs(1)),
        )
        .await
        .expect("shed reply should not wait behind the in-flight request")
        .unwrap();

        assert_eq!(
            serde_json::from_slice::<serde_json::Value>(&response).unwrap(),
            serde_json::json!({"error": "overloaded"})
        );

        client.abort();
        shard_task.abort();
    }

    fn test_config(max_concurrent_requests: usize) -> ServiceConfig {
        ServiceConfig {
            service_name: "mock".to_owned(),
            mode: Mode::Shard,
            database_backend: DatabaseBackend::Postgres,
            shard_id: 0,
            shard_count: 1,
            listen_addr: "127.0.0.1:0".parse().unwrap(),
            nats_url: "memory".to_owned(),
            nats_auth_token: None,
            cache_max_entries: 100,
            cache_ttl: Duration::from_secs(30),
            cache_hard_ttl: Duration::from_secs(600),
            max_concurrent_requests,
            scylla_hosts: Vec::new(),
            scylla_keyspace: "fluxer".to_owned(),
            scylla_username: None,
            scylla_password: None,
            postgres_url: None,
            postgres_host: "127.0.0.1".to_owned(),
            postgres_port: 5432,
            postgres_database: "fluxer".to_owned(),
            postgres_username: "fluxer".to_owned(),
            postgres_password: Some("fluxer".to_owned()),
            postgres_ssl: false,
            postgres_ssl_ca: None,
            postgres_max_connections: 1,
            postgres_kv_table: "fluxer_kv".to_owned(),
            postgres_prepared_statements: true,
        }
    }
}
