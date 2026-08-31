// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::hash_ring::HashRing;
use crate::metrics::{ServiceMetrics, now_ms};
use crate::transport::{Transport, TransportMessage, TransportSubscriber, reply_message};
use moka::future::Cache;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Semaphore, TryAcquireError};
use tokio::task::JoinSet;
use tracing::{debug, info, warn};

pub(crate) const SHARD_REQUEST_TIMEOUT: Duration = Duration::from_secs(5);
const INFLIGHT_TTL: Duration = Duration::from_millis(200);
const INFLIGHT_MAX_ENTRIES: u64 = 10_000;
const MAX_ROUTER_REQUEST_BYTES: usize = 2 * 1024 * 1024;
const LEGACY_SHARD_DECODE_ERROR: &[u8] = br#"{"error":"shard_request_decode_error"}"#;
type InflightKey = (String, String);

pub trait RouterService: Send + Sync + 'static {
    type Request: serde::Serialize + serde::de::DeserializeOwned + Send + Sync + 'static;
    type Response: serde::Serialize + serde::de::DeserializeOwned + Clone + Send + Sync + 'static;

    const CACHES_RESPONSES: bool = true;

    fn service_name(&self) -> &str;
    fn route_key(request: &Self::Request) -> String;
    fn coalesce_key(_request: &Self::Request) -> Option<String> {
        None
    }

    fn l1_lookup(&self, _req: &Self::Request) -> Option<Self::Response> {
        None
    }
    fn l1_insert(&self, _req: &Self::Request, _resp: &Self::Response) {}
    fn l1_invalidate(&self, _key: &str) {}
}

fn shard_subject<S: RouterService>(service: &S, ring: &HashRing, route_key: &str) -> String {
    let shard_id = ring.owner(route_key);
    format!("svc.{}.shard.{shard_id}", service.service_name())
}

async fn forward_to_shard<S: RouterService>(
    transport: &impl Transport,
    service: &S,
    ring: &HashRing,
    request: &S::Request,
    route_key: &str,
) -> anyhow::Result<Vec<u8>> {
    let shard_subject = shard_subject(service, ring, route_key);

    let msgpack_payload = rmp_serde::to_vec_named(request)
        .map_err(|e| anyhow::anyhow!("failed to encode request as msgpack: {e}"))?;

    transport
        .request(&shard_subject, &msgpack_payload, SHARD_REQUEST_TIMEOUT)
        .await
}

async fn forward_to_shard_verbatim<S: RouterService>(
    transport: &impl Transport,
    service: &S,
    ring: &HashRing,
    request: &S::Request,
    route_key: &str,
    payload: &[u8],
) -> anyhow::Result<Vec<u8>> {
    let shard_subject = shard_subject(service, ring, route_key);
    let response_bytes = transport
        .request(&shard_subject, payload, SHARD_REQUEST_TIMEOUT)
        .await?;
    if response_bytes != LEGACY_SHARD_DECODE_ERROR {
        return Ok(response_bytes);
    }

    warn!(
        subject = shard_subject,
        "shard rejected a pass-through request, retrying with the legacy msgpack encoding"
    );
    let legacy_bytes = forward_to_shard::<S>(transport, service, ring, request, route_key).await?;
    match rmp_serde::from_slice::<S::Response>(&legacy_bytes) {
        Ok(response) => serde_json::to_vec(&response).map_err(|err| {
            anyhow::anyhow!("failed to encode legacy shard response as json: {err}")
        }),
        Err(err) => {
            if serde_json::from_slice::<serde_json::Value>(&legacy_bytes).is_ok() {
                Ok(legacy_bytes)
            } else {
                Err(anyhow::anyhow!(
                    "failed to decode legacy shard response: {err}"
                ))
            }
        }
    }
}

async fn dispatch_to_shard<S: RouterService>(
    transport: &impl Transport,
    service: &S,
    ring: &HashRing,
    request: &S::Request,
    route_key: &str,
    payload: &[u8],
) -> anyhow::Result<Vec<u8>> {
    if S::CACHES_RESPONSES {
        forward_to_shard::<S>(transport, service, ring, request, route_key).await
    } else {
        forward_to_shard_verbatim::<S>(transport, service, ring, request, route_key, payload).await
    }
}

async fn handle_router_request<S, T>(
    msg: T::Message,
    transport: T,
    service: Arc<S>,
    ring: Arc<HashRing>,
    inflight: Cache<InflightKey, Vec<u8>>,
    metrics: Arc<ServiceMetrics>,
) where
    S: RouterService,
    T: Transport,
{
    let request_start = now_ms();
    metrics.record_request();
    if msg.payload().len() > MAX_ROUTER_REQUEST_BYTES {
        warn!(
            payload_bytes = msg.payload().len(),
            max_payload_bytes = MAX_ROUTER_REQUEST_BYTES,
            "rejecting oversized router request"
        );
        metrics.record_request_error();
        if msg.has_reply() {
            let error_response =
                serde_json::to_vec(&serde_json::json!({"error": "request_too_large"}))
                    .unwrap_or_default();
            let _ = reply_message(&msg, &transport, &error_response).await;
        }
        return;
    }
    let request: S::Request = match serde_json::from_slice(msg.payload()) {
        Ok(r) => r,
        Err(err) => {
            warn!(error = %err, "failed to decode incoming request");
            metrics.record_request_error();
            if msg.has_reply() {
                let error_response =
                    serde_json::to_vec(&serde_json::json!({"error": "decode_error"}))
                        .unwrap_or_default();
                let _ = reply_message(&msg, &transport, &error_response).await;
            }
            return;
        }
    };
    let request = Arc::new(request);

    if let Some(cached) = service.l1_lookup(&request) {
        metrics.record_cache_hit();
        let elapsed = (now_ms() - request_start).max(0) as u64;
        metrics.record_request_duration(elapsed);
        if msg.has_reply() {
            let response_bytes = serde_json::to_vec(&cached).unwrap_or_default();
            let _ = reply_message(&msg, &transport, &response_bytes).await;
        }
        return;
    }

    let route_key = S::route_key(&request);
    metrics.record_cache_miss();
    metrics.record_shard_forward();

    let coalesce_result = if let Some(coalesce_key) = S::coalesce_key(&request) {
        let forward_transport = transport.clone();
        let forward_service = service.clone();
        let forward_ring = ring.clone();
        let forward_route_key = route_key.clone();
        let forward_request = request.clone();
        let forward_payload = if S::CACHES_RESPONSES {
            Vec::new()
        } else {
            msg.payload().to_vec()
        };
        let inflight_key = (route_key, coalesce_key);
        inflight
            .try_get_with(inflight_key, async move {
                dispatch_to_shard::<S>(
                    &forward_transport,
                    forward_service.as_ref(),
                    forward_ring.as_ref(),
                    &forward_request,
                    &forward_route_key,
                    &forward_payload,
                )
                .await
            })
            .await
            .map_err(|err| anyhow::anyhow!("{err}"))
    } else {
        dispatch_to_shard::<S>(
            &transport,
            service.as_ref(),
            ring.as_ref(),
            &request,
            &route_key,
            msg.payload(),
        )
        .await
    };

    let elapsed = (now_ms() - request_start).max(0) as u64;
    metrics.record_request_duration(elapsed);

    match coalesce_result {
        Ok(response_bytes) => {
            if !S::CACHES_RESPONSES {
                if msg.has_reply() {
                    let _ = reply_message(&msg, &transport, &response_bytes).await;
                }
                return;
            }
            match rmp_serde::from_slice::<S::Response>(&response_bytes) {
                Ok(response) => {
                    service.l1_insert(&request, &response);
                    if msg.has_reply() {
                        let json = serde_json::to_vec(&response).unwrap_or_default();
                        let _ = reply_message(&msg, &transport, &json).await;
                    }
                }
                Err(err) => {
                    debug!(error = %err, "failed to decode shard response");
                    if msg.has_reply() {
                        if serde_json::from_slice::<serde_json::Value>(&response_bytes).is_ok() {
                            let _ = reply_message(&msg, &transport, &response_bytes).await;
                            return;
                        }
                        let error_response =
                            serde_json::to_vec(&serde_json::json!({"error": "shard_decode_error"}))
                                .unwrap_or_default();
                        let _ = reply_message(&msg, &transport, &error_response).await;
                    }
                }
            }
        }
        Err(err) => {
            debug!(error = %err, "shard request failed (coalesced)");
            metrics.record_request_error();
            if msg.has_reply() {
                let error_response =
                    serde_json::to_vec(&serde_json::json!({"error": "shard_unavailable"}))
                        .unwrap_or_default();
                let _ = reply_message(&msg, &transport, &error_response).await;
            }
        }
    }
}

pub async fn run_router<S>(
    config: &ServiceConfig,
    service: S,
    transport: impl Transport,
) -> anyhow::Result<()>
where
    S: RouterService,
{
    let service = Arc::new(service);
    let ring = Arc::new(HashRing::new(config.shard_count));
    let name = service.service_name().to_owned();
    let request_subject = format!("svc.{name}");
    let invalidate_subject = format!("svc.{name}.invalidate.>");
    let queue_group = format!("{name}-router");

    let metrics = Arc::new(ServiceMetrics::default());
    metrics.init();

    let inflight: Cache<InflightKey, Vec<u8>> = Cache::builder()
        .max_capacity(INFLIGHT_MAX_ENTRIES)
        .time_to_live(INFLIGHT_TTL)
        .build();

    let mut tasks = JoinSet::new();

    let health_addr = config.listen_addr;
    let router_serving = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
    let http_serving = router_serving.clone();
    let http_metrics = metrics.clone();
    let http_name = name.clone();
    tasks.spawn(async move {
        crate::server::run_http(health_addr, http_serving, http_metrics, http_name).await
    });

    let req_transport = transport.clone();
    let req_service = service.clone();
    let req_queue = queue_group.clone();
    let req_metrics = metrics.clone();
    let req_permits = Arc::new(Semaphore::new(config.max_concurrent_requests));
    tasks.spawn(async move {
        loop {
            let mut sub = req_transport
                .subscribe_queue(&request_subject, &req_queue)
                .await?;
            info!(
                subject = request_subject,
                max_concurrent_requests = req_permits.available_permits(),
                "router listening for requests"
            );

            loop {
                let msg = tokio::select! {
                    msg_opt = sub.next() => {
                        let Some(msg) = msg_opt else {
                            warn!("router request subscription stream ended, will re-subscribe");
                            break;
                        };
                        msg
                    }
                    _ = req_transport.wait_for_reconnect() => {
                        info!("NATS reconnected, re-subscribing router request listener");
                        break;
                    }
                };

                let permit = match req_permits.clone().try_acquire_owned() {
                    Ok(permit) => permit,
                    Err(TryAcquireError::NoPermits) => {
                        debug!("shedding router request, no permits available");
                        req_metrics.record_request();
                        req_metrics.record_request_error();
                        if msg.has_reply() {
                            let error_response =
                                serde_json::to_vec(&serde_json::json!({"error": "overloaded"}))
                                    .unwrap_or_default();
                            let _ = reply_message(&msg, &req_transport, &error_response).await;
                        }
                        continue;
                    }
                    Err(TryAcquireError::Closed) => return anyhow::Ok(()),
                };
                let transport = req_transport.clone();
                let service = req_service.clone();
                let ring = ring.clone();
                let inflight = inflight.clone();
                let metrics = req_metrics.clone();
                tokio::spawn(async move {
                    let _permit = permit;
                    handle_router_request::<S, _>(msg, transport, service, ring, inflight, metrics)
                        .await;
                });
            }
        }
    });

    let inv_transport = transport.clone();
    let inv_service = service.clone();
    tasks.spawn(async move {
        loop {
            let mut sub = inv_transport.subscribe(&invalidate_subject).await?;
            info!(
                subject = invalidate_subject,
                "router listening for cache invalidations"
            );

            loop {
                tokio::select! {
                    msg_opt = sub.next() => {
                        let Some(msg) = msg_opt else {
                            warn!("router invalidation subscription stream ended, will re-subscribe");
                            break;
                        };
                        let subject = msg.subject().to_owned();
                        let key = subject
                            .strip_prefix(&format!("svc.{name}.invalidate."))
                            .unwrap_or("");
                        if !key.is_empty() {
                            inv_service.l1_invalidate(key);
                        }
                    }
                    _ = inv_transport.wait_for_reconnect() => {
                        info!("NATS reconnected, re-subscribing router invalidation listener");
                        break;
                    }
                }
            }
        }
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
            info!("router shutting down");
            router_serving.store(false, std::sync::atomic::Ordering::SeqCst);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{DatabaseBackend, Mode, ServiceConfig};
    use crate::transport::{InMemoryTransport, Transport, TransportSubscriber, reply_message};
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use tokio::sync::Notify;

    #[derive(Serialize, Deserialize)]
    struct MockRequest {
        key: String,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct MockResponse {
        key: String,
    }

    struct MockRouter;

    impl RouterService for MockRouter {
        type Request = MockRequest;
        type Response = MockResponse;

        fn service_name(&self) -> &str {
            "mock"
        }

        fn route_key(request: &MockRequest) -> String {
            request.key.clone()
        }
    }

    #[test]
    fn coalescing_is_opt_in() {
        let request = MockRequest {
            key: "shared".to_owned(),
        };
        assert_eq!(MockRouter::coalesce_key(&request), None);
    }

    #[tokio::test]
    async fn router_forwards_uncached_requests_concurrently() {
        let transport = InMemoryTransport::new();
        let mut shard_sub = transport.subscribe("svc.mock.shard.0").await.unwrap();

        let started = Arc::new(AtomicUsize::new(0));
        let both_started = Arc::new(Notify::new());
        let released = Arc::new(AtomicBool::new(false));
        let release = Arc::new(Notify::new());

        let shard_transport = transport.clone();
        let shard_started = started.clone();
        let shard_both_started = both_started.clone();
        let shard_released = released.clone();
        let shard_release = release.clone();
        let shard_task = tokio::spawn(async move {
            for _ in 0..2 {
                let msg = shard_sub.next().await.unwrap();
                let reply_transport = shard_transport.clone();
                let reply_released = shard_released.clone();
                let reply_release = shard_release.clone();
                tokio::spawn(async move {
                    while !reply_released.load(Ordering::SeqCst) {
                        reply_release.notified().await;
                    }
                    let response = MockResponse {
                        key: "ok".to_owned(),
                    };
                    let response_bytes = rmp_serde::to_vec_named(&response).unwrap();
                    reply_message(&msg, &reply_transport, &response_bytes)
                        .await
                        .unwrap();
                });

                if shard_started.fetch_add(1, Ordering::SeqCst) + 1 == 2 {
                    shard_both_started.notify_waiters();
                }
            }
        });

        let router_config = test_config(2);
        let router_transport = transport.clone();
        let router_task =
            tokio::spawn(
                async move { run_router(&router_config, MockRouter, router_transport).await },
            );

        tokio::time::sleep(Duration::from_millis(25)).await;

        let request_a = serde_json::to_vec(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();
        let request_b = serde_json::to_vec(&MockRequest {
            key: "b".to_owned(),
        })
        .unwrap();

        let client_a = {
            let transport = transport.clone();
            tokio::spawn(async move {
                transport
                    .request("svc.mock", &request_a, Duration::from_secs(1))
                    .await
            })
        };
        let client_b = {
            let transport = transport.clone();
            tokio::spawn(async move {
                transport
                    .request("svc.mock", &request_b, Duration::from_secs(1))
                    .await
            })
        };

        tokio::time::timeout(Duration::from_millis(250), async {
            while started.load(Ordering::SeqCst) < 2 {
                both_started.notified().await;
            }
        })
        .await
        .expect("router should forward both requests before the first shard reply is released");

        released.store(true, Ordering::SeqCst);
        release.notify_waiters();

        let response_a = client_a.await.unwrap().unwrap();
        let response_b = client_b.await.unwrap().unwrap();
        assert_eq!(
            serde_json::from_slice::<MockResponse>(&response_a).unwrap(),
            MockResponse {
                key: "ok".to_owned()
            }
        );
        assert_eq!(
            serde_json::from_slice::<MockResponse>(&response_b).unwrap(),
            MockResponse {
                key: "ok".to_owned()
            }
        );

        shard_task.await.unwrap();
        router_task.abort();
    }

    static COUNTED_REQUEST_DECODES: AtomicUsize = AtomicUsize::new(0);

    #[derive(Serialize)]
    struct CountedRequest {
        key: String,
    }

    impl<'de> Deserialize<'de> for CountedRequest {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            #[derive(Deserialize)]
            struct Raw {
                key: String,
            }

            let raw = Raw::deserialize(deserializer)?;
            COUNTED_REQUEST_DECODES.fetch_add(1, Ordering::SeqCst);
            Ok(CountedRequest { key: raw.key })
        }
    }

    struct CachingRouter {
        l1: Mutex<HashMap<String, MockResponse>>,
        inserted_keys: Arc<Mutex<Vec<String>>>,
    }

    impl RouterService for CachingRouter {
        type Request = CountedRequest;
        type Response = MockResponse;

        fn service_name(&self) -> &str {
            "cached-mock"
        }

        fn route_key(request: &CountedRequest) -> String {
            request.key.clone()
        }

        fn coalesce_key(request: &CountedRequest) -> Option<String> {
            Some(request.key.clone())
        }

        fn l1_lookup(&self, request: &CountedRequest) -> Option<MockResponse> {
            self.l1.lock().unwrap().get(&request.key).cloned()
        }

        fn l1_insert(&self, request: &CountedRequest, response: &MockResponse) {
            self.inserted_keys.lock().unwrap().push(request.key.clone());
            self.l1
                .lock()
                .unwrap()
                .insert(request.key.clone(), response.clone());
        }
    }

    #[tokio::test]
    async fn router_decodes_each_request_once_and_inserts_it_into_l1() {
        let transport = InMemoryTransport::new();
        let mut shard_sub = transport
            .subscribe("svc.cached-mock.shard.0")
            .await
            .unwrap();

        let shard_transport = transport.clone();
        let shard_task = tokio::spawn(async move {
            while let Some(msg) = shard_sub.next().await {
                let response = MockResponse {
                    key: "ok".to_owned(),
                };
                let response_bytes = rmp_serde::to_vec_named(&response).unwrap();
                reply_message(&msg, &shard_transport, &response_bytes)
                    .await
                    .unwrap();
            }
        });

        let inserted_keys = Arc::new(Mutex::new(Vec::new()));
        let router = CachingRouter {
            l1: Mutex::new(HashMap::new()),
            inserted_keys: inserted_keys.clone(),
        };
        let router_config = test_config(4);
        let router_transport = transport.clone();
        let router_task =
            tokio::spawn(async move { run_router(&router_config, router, router_transport).await });

        tokio::time::sleep(Duration::from_millis(25)).await;

        let request = serde_json::to_vec(&CountedRequest {
            key: "a".to_owned(),
        })
        .unwrap();

        let first = transport
            .request("svc.cached-mock", &request, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(
            serde_json::from_slice::<MockResponse>(&first).unwrap(),
            MockResponse {
                key: "ok".to_owned()
            }
        );
        assert_eq!(inserted_keys.lock().unwrap().as_slice(), ["a".to_owned()]);
        assert_eq!(COUNTED_REQUEST_DECODES.load(Ordering::SeqCst), 1);

        let second = transport
            .request("svc.cached-mock", &request, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(
            serde_json::from_slice::<MockResponse>(&second).unwrap(),
            MockResponse {
                key: "ok".to_owned()
            }
        );
        assert_eq!(inserted_keys.lock().unwrap().as_slice(), ["a".to_owned()]);
        assert_eq!(COUNTED_REQUEST_DECODES.load(Ordering::SeqCst), 2);

        shard_task.abort();
        router_task.abort();
    }

    struct PassThroughRouter;

    impl RouterService for PassThroughRouter {
        type Request = MockRequest;
        type Response = MockResponse;

        const CACHES_RESPONSES: bool = false;

        fn service_name(&self) -> &str {
            "passthrough-mock"
        }

        fn route_key(request: &MockRequest) -> String {
            request.key.clone()
        }

        fn coalesce_key(request: &MockRequest) -> Option<String> {
            Some(request.key.clone())
        }
    }

    #[tokio::test]
    async fn router_forwards_pass_through_requests_and_replies_verbatim() {
        let transport = InMemoryTransport::new();
        let mut shard_sub = transport
            .subscribe("svc.passthrough-mock.shard.0")
            .await
            .unwrap();

        let observed = Arc::new(Mutex::new(Vec::new()));
        let shard_transport = transport.clone();
        let shard_observed = observed.clone();
        let shard_task = tokio::spawn(async move {
            while let Some(msg) = shard_sub.next().await {
                shard_observed.lock().unwrap().push(msg.payload().to_vec());
                reply_message(&msg, &shard_transport, br#"{ "key" : "verbatim" }"#)
                    .await
                    .unwrap();
            }
        });

        let router_config = test_config(4);
        let router_transport = transport.clone();
        let router_task = tokio::spawn(async move {
            run_router(&router_config, PassThroughRouter, router_transport).await
        });

        tokio::time::sleep(Duration::from_millis(25)).await;

        let request = serde_json::to_vec(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();

        let response = transport
            .request("svc.passthrough-mock", &request, Duration::from_secs(1))
            .await
            .unwrap();

        assert_eq!(response, br#"{ "key" : "verbatim" }"#);
        assert_eq!(observed.lock().unwrap().as_slice(), [request]);

        shard_task.abort();
        router_task.abort();
    }

    #[tokio::test]
    async fn router_retries_pass_through_requests_that_legacy_shards_reject() {
        let transport = InMemoryTransport::new();
        let mut shard_sub = transport
            .subscribe("svc.passthrough-mock.shard.0")
            .await
            .unwrap();

        let shard_transport = transport.clone();
        let shard_task = tokio::spawn(async move {
            while let Some(msg) = shard_sub.next().await {
                let response_bytes = match rmp_serde::from_slice::<MockRequest>(msg.payload()) {
                    Ok(_) => rmp_serde::to_vec_named(&MockResponse {
                        key: "legacy".to_owned(),
                    })
                    .unwrap(),
                    Err(_) => serde_json::to_vec(
                        &serde_json::json!({"error": "shard_request_decode_error"}),
                    )
                    .unwrap(),
                };
                reply_message(&msg, &shard_transport, &response_bytes)
                    .await
                    .unwrap();
            }
        });

        let router_config = test_config(4);
        let router_transport = transport.clone();
        let router_task = tokio::spawn(async move {
            run_router(&router_config, PassThroughRouter, router_transport).await
        });

        tokio::time::sleep(Duration::from_millis(25)).await;

        let request = serde_json::to_vec(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();

        let response = transport
            .request("svc.passthrough-mock", &request, Duration::from_secs(1))
            .await
            .unwrap();

        assert_eq!(
            serde_json::from_slice::<MockResponse>(&response).unwrap(),
            MockResponse {
                key: "legacy".to_owned()
            }
        );

        shard_task.abort();
        router_task.abort();
    }

    #[tokio::test]
    async fn router_sheds_requests_when_permits_are_exhausted() {
        let transport = InMemoryTransport::new();
        let mut shard_sub = transport.subscribe("svc.mock.shard.0").await.unwrap();

        let (forwarded_tx, forwarded_rx) = tokio::sync::oneshot::channel();
        let shard_task = tokio::spawn(async move {
            let _msg = shard_sub.next().await.unwrap();
            let _ = forwarded_tx.send(());
            std::future::pending::<()>().await;
        });

        let router_config = test_config(1);
        let router_transport = transport.clone();
        let router_task =
            tokio::spawn(
                async move { run_router(&router_config, MockRouter, router_transport).await },
            );

        tokio::time::sleep(Duration::from_millis(25)).await;

        let request_a = serde_json::to_vec(&MockRequest {
            key: "a".to_owned(),
        })
        .unwrap();
        let request_b = serde_json::to_vec(&MockRequest {
            key: "b".to_owned(),
        })
        .unwrap();

        let client_a = {
            let transport = transport.clone();
            tokio::spawn(async move {
                transport
                    .request("svc.mock", &request_a, Duration::from_secs(10))
                    .await
            })
        };

        tokio::time::timeout(Duration::from_millis(250), forwarded_rx)
            .await
            .expect("router should forward the first request and hold the only permit")
            .unwrap();

        let response_b = tokio::time::timeout(
            Duration::from_millis(250),
            transport.request("svc.mock", &request_b, Duration::from_secs(1)),
        )
        .await
        .expect("shed reply should not wait behind the in-flight request")
        .unwrap();

        assert_eq!(
            serde_json::from_slice::<serde_json::Value>(&response_b).unwrap(),
            serde_json::json!({"error": "overloaded"})
        );

        client_a.abort();
        shard_task.abort();
        router_task.abort();
    }

    fn test_config(max_concurrent_requests: usize) -> ServiceConfig {
        ServiceConfig {
            service_name: "mock".to_owned(),
            mode: Mode::Router,
            database_backend: DatabaseBackend::Postgres,
            shard_id: 0,
            shard_count: 1,
            listen_addr: "127.0.0.1:0".parse().unwrap(),
            nats_url: "memory".to_owned(),
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
