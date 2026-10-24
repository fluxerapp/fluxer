// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::hash_ring::HashRing;
use crate::metrics::ServiceMetrics;
use crate::shard::{Endpoint, ShardState, ShardStateNotice, control_subject, serve};
use crate::transport::{
    NoResponders, Transport, TransportMessage, TransportSubscriber, reply_bytes, reply_json_error,
};
use anyhow::Context;
use futures::future::{BoxFuture, FutureExt, Shared};
use futures::stream::{FuturesUnordered, StreamExt};
use std::collections::HashMap;
use std::future::Future;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::Notify;
use tracing::{debug, warn};

pub const SHARD_REQUEST_TIMEOUT: Duration = Duration::from_secs(5);
const ROUTER_DRAIN_LINGER: Duration = Duration::from_secs(2);
const DRAINING_NOTICE_TTL: Duration = Duration::from_secs(30);
const DRAINING_BROADCAST_TIMEOUT: Duration = Duration::from_millis(500);
const SHARD_RETRY_INTERVAL: Duration = Duration::from_millis(250);
const SHARD_STATE_RESUBSCRIBE_DELAY: Duration = Duration::from_secs(1);
const MAX_BROADCAST_CONCURRENCY: usize = 32;
pub(crate) const MAX_ROUTER_REQUEST_BYTES: usize = 2 * 1024 * 1024;
type InflightKey = (String, String);
type InflightCall<V, E> = Shared<BoxFuture<'static, Result<V, Arc<E>>>>;
pub(crate) type Inflight<V, E> = Arc<Mutex<HashMap<InflightKey, InflightCall<V, E>>>>;

pub trait RouterService: Send + Sync + 'static {
    type Request: serde::Serialize + serde::de::DeserializeOwned + Send + Sync + 'static;
    type Response: serde::Serialize + serde::de::DeserializeOwned + Clone + Send + Sync + 'static;

    const CACHES_RESPONSES: bool = true;

    fn service_name(&self) -> &str;
    fn route_key(request: &Self::Request) -> String;
    fn shard_candidates(&self, request: &Self::Request, ring: &HashRing) -> Vec<u32> {
        ring.ranked(&Self::route_key(request))
    }
    fn coalesce_key(_request: &Self::Request) -> Option<String> {
        None
    }

    fn is_broadcast_request(_request: &Self::Request) -> bool {
        false
    }
    fn is_broadcast_acknowledgement(_response: &Self::Response) -> bool {
        false
    }

    fn l1_lookup(&self, _req: &Self::Request) -> Option<Self::Response> {
        None
    }
    fn l1_insert(&self, _req: &Self::Request, _resp: &Self::Response) {}
}

#[derive(Default)]
struct ShardStates {
    draining: Mutex<HashMap<u32, Instant>>,
    ready: Notify,
}

impl ShardStates {
    fn apply(&self, notice: ShardStateNotice) {
        let mut draining = self.draining.lock().unwrap();
        match notice.state {
            ShardState::Draining => {
                draining.insert(notice.shard_id, Instant::now());
            }
            ShardState::Ready => {
                draining.remove(&notice.shard_id);
                self.ready.notify_waiters();
            }
        }
    }

    fn forget(&self) {
        self.draining.lock().unwrap().clear();
        self.ready.notify_waiters();
    }

    fn is_draining(&self, shard_id: u32) -> bool {
        self.draining
            .lock()
            .unwrap()
            .get(&shard_id)
            .is_some_and(|since| since.elapsed() < DRAINING_NOTICE_TTL)
    }
}

async fn listen_for_shard_states(
    transport: impl Transport,
    service_name: String,
    states: Arc<ShardStates>,
) {
    let subject = control_subject(&service_name);
    loop {
        let mut sub = match transport.subscribe(&subject).await {
            Ok(sub) => sub,
            Err(error) => {
                warn!(error = %error, subject, "failed to subscribe to shard state notices");
                tokio::time::sleep(SHARD_STATE_RESUBSCRIBE_DELAY).await;
                continue;
            }
        };
        loop {
            tokio::select! {
                msg = sub.next() => {
                    let Some(msg) = msg else {
                        break;
                    };
                    match serde_json::from_slice::<ShardStateNotice>(msg.payload()) {
                        Ok(notice) => {
                            debug!(shard_id = notice.shard_id, state = ?notice.state, "shard state notice");
                            states.apply(notice);
                        }
                        Err(error) => warn!(error = %error, "invalid shard state notice"),
                    }
                }
                _ = transport.wait_for_reconnect() => states.forget(),
            }
        }
        warn!(
            subject,
            "shard state subscription stream ended, will re-subscribe"
        );
    }
}

async fn dispatch_to_shard<S: RouterService>(
    transport: &impl Transport,
    service: &S,
    ring: &HashRing,
    states: &ShardStates,
    request: &S::Request,
    payload: &[u8],
) -> anyhow::Result<Vec<u8>> {
    let msgpack_payload;
    let payload = if S::CACHES_RESPONSES {
        msgpack_payload =
            rmp_serde::to_vec_named(request).context("failed to encode request as msgpack")?;
        msgpack_payload.as_slice()
    } else {
        payload
    };
    let candidates = service.shard_candidates(request, ring);
    let deadline = tokio::time::Instant::now() + SHARD_REQUEST_TIMEOUT;
    loop {
        let ready = states.ready.notified();
        tokio::pin!(ready);
        ready.as_mut().enable();
        for &shard_id in &candidates {
            if states.is_draining(shard_id) {
                continue;
            }
            let timeout = deadline.saturating_duration_since(tokio::time::Instant::now());
            if timeout.is_zero() {
                break;
            }
            let subject = format!("svc.{}.shard.{shard_id}", service.service_name());
            match transport.request(&subject, payload, timeout).await {
                Err(error) if error.is::<NoResponders>() => {
                    debug!(shard_id, "shard has no responders");
                }
                result => return result,
            }
        }
        let now = tokio::time::Instant::now();
        if now >= deadline {
            anyhow::bail!("no shard became available for the request");
        }
        tokio::select! {
            _ = ready => {}
            _ = tokio::time::sleep_until(deadline.min(now + SHARD_RETRY_INTERVAL)) => {}
        }
    }
}

async fn forward_to_all_shards<S: RouterService>(
    transport: &impl Transport,
    service: &S,
    ring: &HashRing,
    states: &ShardStates,
    request: &S::Request,
) -> anyhow::Result<Vec<u8>> {
    let payload = rmp_serde::to_vec_named(request)
        .context("failed to encode broadcast request as msgpack")?;
    let payload = payload.as_slice();
    let service_name = service.service_name();
    let deadline = tokio::time::Instant::now() + SHARD_REQUEST_TIMEOUT;
    let request_shard = |shard_id| async move {
        let mut timeout = deadline.saturating_duration_since(tokio::time::Instant::now());
        if timeout.is_zero() {
            anyhow::bail!("broadcast deadline expired before shard {shard_id}");
        }
        let draining = states.is_draining(shard_id);
        if draining {
            timeout = timeout.min(DRAINING_BROADCAST_TIMEOUT);
        }
        let subject = format!("svc.{service_name}.shard.{shard_id}");
        let response_bytes = match transport.request(&subject, payload, timeout).await {
            Ok(response_bytes) => response_bytes,
            Err(error) if draining || error.is::<NoResponders>() => {
                debug!(shard_id, error = %error, "broadcast skipped a shard that is gone");
                return anyhow::Ok(None);
            }
            Err(error) => {
                return Err(error.context(format!("broadcast request to shard {shard_id} failed")));
            }
        };
        let response = rmp_serde::from_slice::<S::Response>(&response_bytes)
            .with_context(|| format!("invalid broadcast response from shard {shard_id}"))?;
        if !S::is_broadcast_acknowledgement(&response) {
            anyhow::bail!("unexpected broadcast acknowledgement from shard {shard_id}");
        }
        anyhow::Ok(Some(response))
    };

    let mut shard_ids = 0..ring.shard_count();
    let mut pending = FuturesUnordered::new();
    for shard_id in shard_ids.by_ref().take(MAX_BROADCAST_CONCURRENCY) {
        pending.push(request_shard(shard_id));
    }

    let mut acknowledgement = None;
    let mut first_error = None;
    while let Some(result) = pending.next().await {
        match result {
            Ok(Some(response)) => acknowledgement = Some(response),
            Ok(None) => {}
            Err(error) => {
                first_error.get_or_insert(error);
            }
        }
        if first_error.is_none()
            && let Some(shard_id) = shard_ids.next()
        {
            pending.push(request_shard(shard_id));
        }
    }
    if let Some(error) = first_error {
        return Err(error);
    }
    let acknowledgement = acknowledgement.context("broadcast request has no configured shards")?;
    if S::CACHES_RESPONSES {
        rmp_serde::to_vec_named(&acknowledgement)
            .context("failed to encode broadcast acknowledgement as msgpack")
    } else {
        serde_json::to_vec(&acknowledgement)
            .context("failed to encode broadcast acknowledgement as json")
    }
}

pub(crate) async fn coalesce<V, E, F>(
    inflight: &Inflight<V, E>,
    key: InflightKey,
    call: F,
) -> Result<V, Arc<E>>
where
    V: Clone + Send + Sync + 'static,
    E: Send + Sync + 'static,
    F: Future<Output = Result<V, E>> + Send + 'static,
{
    let shared = {
        let mut calls = inflight.lock().unwrap();
        calls
            .entry(key.clone())
            .or_insert_with(|| {
                let inflight = inflight.clone();
                async move {
                    let result = call.await.map_err(Arc::new);
                    inflight.lock().unwrap().remove(&key);
                    result
                }
                .boxed()
                .shared()
            })
            .clone()
    };
    shared.await
}

async fn reply_json_response(
    message: &impl TransportMessage,
    transport: &impl Transport,
    response: &impl serde::Serialize,
    metrics: &ServiceMetrics,
) {
    if !message.has_reply() {
        return;
    }
    match serde_json::to_vec(response) {
        Ok(payload) => reply_bytes(message, transport, &payload).await,
        Err(error) => {
            warn!(error = %error, subject = message.subject(), "failed to encode router response");
            metrics.record_request_error();
            reply_json_error(message, transport, "encode_error").await;
        }
    }
}

async fn handle_router_request<S, T>(
    msg: T::Message,
    transport: T,
    service: Arc<S>,
    ring: Arc<HashRing>,
    states: Arc<ShardStates>,
    inflight: Inflight<Vec<u8>, anyhow::Error>,
    metrics: Arc<ServiceMetrics>,
) where
    S: RouterService,
    T: Transport,
{
    let request_start = Instant::now();
    metrics.record_request();
    if msg.payload().len() > MAX_ROUTER_REQUEST_BYTES {
        warn!(
            payload_bytes = msg.payload().len(),
            max_payload_bytes = MAX_ROUTER_REQUEST_BYTES,
            "rejecting oversized router request"
        );
        metrics.record_request_error();
        reply_json_error(&msg, &transport, "request_too_large").await;
        return;
    }
    let request: S::Request = match serde_json::from_slice(msg.payload()) {
        Ok(r) => r,
        Err(err) => {
            warn!(error = %err, "failed to decode incoming request");
            metrics.record_request_error();
            reply_json_error(&msg, &transport, "decode_error").await;
            return;
        }
    };
    let request = Arc::new(request);
    let broadcast = S::is_broadcast_request(&request);

    if S::CACHES_RESPONSES
        && !broadcast
        && let Some(cached) = service.l1_lookup(&request)
    {
        metrics.record_cache_hit();
        metrics.record_request_duration(request_start.elapsed().as_millis() as u64);
        reply_json_response(&msg, &transport, &cached, &metrics).await;
        return;
    }

    let route_key = S::route_key(&request);
    metrics.record_cache_miss();
    metrics.record_shard_forward();

    let coalesce_result = if broadcast {
        forward_to_all_shards::<S>(
            &transport,
            service.as_ref(),
            ring.as_ref(),
            states.as_ref(),
            &request,
        )
        .await
    } else if let Some(coalesce_key) = S::coalesce_key(&request) {
        let forward_transport = transport.clone();
        let forward_service = service.clone();
        let forward_ring = ring.clone();
        let forward_states = states.clone();
        let forward_request = request.clone();
        let forward_payload = if S::CACHES_RESPONSES {
            Vec::new()
        } else {
            msg.payload().to_vec()
        };
        let inflight_key = (route_key, coalesce_key);
        coalesce(&inflight, inflight_key, async move {
            dispatch_to_shard::<S>(
                &forward_transport,
                forward_service.as_ref(),
                forward_ring.as_ref(),
                forward_states.as_ref(),
                &forward_request,
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
            states.as_ref(),
            &request,
            msg.payload(),
        )
        .await
    };

    metrics.record_request_duration(request_start.elapsed().as_millis() as u64);

    match coalesce_result {
        Ok(response_bytes) => {
            if !S::CACHES_RESPONSES {
                if msg.has_reply() {
                    reply_bytes(&msg, &transport, &response_bytes).await;
                }
                return;
            }
            match rmp_serde::from_slice::<S::Response>(&response_bytes) {
                Ok(response) => {
                    if !broadcast {
                        service.l1_insert(&request, &response);
                    }
                    reply_json_response(&msg, &transport, &response, &metrics).await;
                }
                Err(err) => {
                    debug!(error = %err, "failed to decode shard response");
                    if msg.has_reply() {
                        if serde_json::from_slice::<serde_json::Value>(&response_bytes).is_ok() {
                            reply_bytes(&msg, &transport, &response_bytes).await;
                            return;
                        }
                        reply_json_error(&msg, &transport, "shard_decode_error").await;
                    }
                }
            }
        }
        Err(err) => {
            debug!(error = %err, "shard request failed (coalesced)");
            metrics.record_request_error();
            reply_json_error(&msg, &transport, "shard_unavailable").await;
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
    let states = Arc::new(ShardStates::default());
    let name = service.service_name().to_owned();

    let metrics = Arc::new(ServiceMetrics::default());
    metrics.init();

    let inflight = Inflight::<Vec<u8>, anyhow::Error>::default();
    let handler_transport = transport.clone();
    let handler_metrics = metrics.clone();
    let handler_states = states.clone();
    let requests = serve(
        config,
        Endpoint {
            subject: format!("svc.{name}"),
            queue_group: Some(format!("{name}-router")),
            name: name.clone(),
            announce: None,
            linger: ROUTER_DRAIN_LINGER,
        },
        metrics,
        transport.clone(),
        move |msg| {
            handle_router_request::<S, _>(
                msg,
                handler_transport.clone(),
                service.clone(),
                ring.clone(),
                handler_states.clone(),
                inflight.clone(),
                handler_metrics.clone(),
            )
        },
        crate::shutdown::wait_for_shutdown(),
    );

    let listener = tokio::spawn(listen_for_shard_states(transport, name, states));
    let result = requests.await;
    listener.abort();
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Mode;
    use crate::shard::{ShardService, run_shard};
    use crate::transport::InMemoryTransport;
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    enum KvRequest {
        Get { key: String },
        Invalidate,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    enum KvResponse {
        Value { key: String, shard_id: u32 },
        Invalidated,
    }

    struct KvRouter {
        pinned: bool,
    }

    impl RouterService for KvRouter {
        type Request = KvRequest;
        type Response = KvResponse;
        const CACHES_RESPONSES: bool = false;

        fn service_name(&self) -> &str {
            "kv"
        }

        fn route_key(request: &KvRequest) -> String {
            match request {
                KvRequest::Get { key } => key.clone(),
                KvRequest::Invalidate => String::new(),
            }
        }

        fn shard_candidates(&self, request: &KvRequest, ring: &HashRing) -> Vec<u32> {
            let key = Self::route_key(request);
            if self.pinned {
                vec![ring.owner(&key)]
            } else {
                ring.ranked(&key)
            }
        }

        fn is_broadcast_request(request: &KvRequest) -> bool {
            matches!(request, KvRequest::Invalidate)
        }

        fn is_broadcast_acknowledgement(response: &KvResponse) -> bool {
            matches!(response, KvResponse::Invalidated)
        }
    }

    struct KvShard(u32);

    impl ShardService for KvShard {
        type Request = KvRequest;
        type Response = KvResponse;

        fn service_name(&self) -> &str {
            "kv"
        }

        async fn handle(&self, request: KvRequest) -> anyhow::Result<KvResponse> {
            Ok(match request {
                KvRequest::Get { key } => KvResponse::Value {
                    key,
                    shard_id: self.0,
                },
                KvRequest::Invalidate => KvResponse::Invalidated,
            })
        }
    }

    fn start_shard(transport: &InMemoryTransport, shard_id: u32) -> tokio::task::JoinHandle<()> {
        let transport = transport.clone();
        tokio::spawn(async move {
            let config = ServiceConfig::for_test(Mode::Shard, shard_id, 4);
            run_shard(&config, KvShard(shard_id), transport)
                .await
                .unwrap();
        })
    }

    fn start_router(transport: &InMemoryTransport, pinned: bool) -> tokio::task::JoinHandle<()> {
        let transport = transport.clone();
        tokio::spawn(async move {
            let config = ServiceConfig::for_test(Mode::Router, 0, 4);
            run_router(&config, KvRouter { pinned }, transport)
                .await
                .unwrap();
        })
    }

    async fn call(transport: &InMemoryTransport, payload: &[u8]) -> KvResponse {
        let response = transport
            .request("svc.kv", payload, Duration::from_secs(10))
            .await
            .unwrap();
        serde_json::from_slice(&response).unwrap()
    }

    fn value(shard_id: u32) -> KvResponse {
        KvResponse::Value {
            key: "k".to_owned(),
            shard_id,
        }
    }

    const GET: &[u8] = br#"{"Get":{"key":"k"}}"#;

    #[tokio::test(start_paused = true)]
    async fn router_fails_over_from_missing_and_draining_shards() {
        let transport = InMemoryTransport::new();
        let ranked = HashRing::new(4).ranked("k");
        let mut servers: Vec<_> = ranked[1..]
            .iter()
            .map(|&shard_id| start_shard(&transport, shard_id))
            .collect();
        servers.push(start_router(&transport, false));
        tokio::time::sleep(Duration::from_millis(25)).await;

        assert_eq!(call(&transport, GET).await, value(ranked[1]));
        assert_eq!(
            call(&transport, br#""Invalidate""#).await,
            KvResponse::Invalidated
        );

        let notice = format!(r#"{{"shard_id":{},"state":"draining"}}"#, ranked[1]);
        transport
            .publish("svc.kv.ctl", notice.as_bytes())
            .await
            .unwrap();
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert_eq!(call(&transport, GET).await, value(ranked[2]));

        for server in servers {
            server.abort();
        }
    }

    #[tokio::test(start_paused = true)]
    async fn pinned_requests_wait_for_the_owner_to_come_back() {
        let transport = InMemoryTransport::new();
        let owner = HashRing::new(4).owner("k");
        let mut servers: Vec<_> = (0..4)
            .filter(|&shard_id| shard_id != owner)
            .map(|shard_id| start_shard(&transport, shard_id))
            .collect();
        servers.push(start_router(&transport, true));
        tokio::time::sleep(Duration::from_millis(25)).await;

        let request_transport = transport.clone();
        let mut request = tokio::spawn(async move { call(&request_transport, GET).await });
        assert!(
            tokio::time::timeout(Duration::from_secs(2), &mut request)
                .await
                .is_err()
        );

        servers.push(start_shard(&transport, owner));
        assert_eq!(request.await.unwrap(), value(owner));

        for server in servers {
            server.abort();
        }
    }
}
