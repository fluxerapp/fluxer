// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::metrics::ServiceMetrics;
use crate::router::{
    Inflight, MAX_ROUTER_REQUEST_BYTES, RouterService, SHARD_REQUEST_TIMEOUT, coalesce,
};
use crate::shard::{Endpoint, ShardService, serve, shard_metrics};
use crate::transport::{Transport, TransportMessage, reply_bytes, reply_json_error};
use bytes::Bytes;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tracing::warn;

#[derive(Debug)]
enum CallError {
    Timeout,
    Handler(anyhow::Error),
    Encode(serde_json::Error),
}

impl CallError {
    fn code(&self) -> &'static str {
        match self {
            Self::Timeout => "shard_unavailable",
            Self::Handler(_) => "shard_handler_error",
            Self::Encode(_) => "encode_error",
        }
    }
}

struct Combined<R, S> {
    router: R,
    shard: Arc<S>,
    inflight: Inflight<Bytes, CallError>,
}

pub async fn run_combined<R, S>(
    config: &ServiceConfig,
    router: R,
    shard: S,
    transport: impl Transport,
) -> anyhow::Result<()>
where
    R: RouterService,
    R::Request: Clone,
    S: ShardService<Request = R::Request, Response = R::Response>,
{
    if config.shard_count != 1 {
        anyhow::bail!(
            "FLUXER_SVC_MODE=combined needs FLUXER_SVC_SHARD_COUNT=1, got {}",
            config.shard_count
        );
    }
    let name = router.service_name().to_owned();
    let shard = Arc::new(shard);
    let metrics = shard_metrics(&shard);
    let service = Arc::new(Combined {
        router,
        shard,
        inflight: Inflight::default(),
    });
    let handler_transport = transport.clone();
    let handler_metrics = metrics.clone();
    serve(
        config,
        Endpoint {
            subject: format!("svc.{name}"),
            queue_group: Some(format!("{name}-router")),
            name,
            announce: None,
            linger: Duration::ZERO,
        },
        metrics,
        transport,
        move |msg| {
            handle_request(
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

async fn handle_request<R, S, T>(
    msg: T::Message,
    transport: T,
    service: Arc<Combined<R, S>>,
    metrics: Arc<ServiceMetrics>,
) where
    R: RouterService,
    R::Request: Clone,
    S: ShardService<Request = R::Request, Response = R::Response>,
    T: Transport,
{
    let request_start = Instant::now();
    metrics.record_request();
    if msg.payload().len() > MAX_ROUTER_REQUEST_BYTES {
        warn!(
            payload_bytes = msg.payload().len(),
            max_payload_bytes = MAX_ROUTER_REQUEST_BYTES,
            "rejecting oversized request"
        );
        metrics.record_request_error();
        reply_json_error(&msg, &transport, "request_too_large").await;
        return;
    }
    let request: R::Request = match serde_json::from_slice(msg.payload()) {
        Ok(request) => request,
        Err(err) => {
            warn!(error = %err, "failed to decode incoming request");
            metrics.record_request_error();
            reply_json_error(&msg, &transport, "decode_error").await;
            return;
        }
    };
    let broadcast = R::is_broadcast_request(&request);

    if R::CACHES_RESPONSES
        && !broadcast
        && let Some(cached) = service.router.l1_lookup(&request)
    {
        metrics.record_cache_hit();
        let result = serde_json::to_vec(&cached)
            .map(Bytes::from)
            .map_err(|err| Arc::new(CallError::Encode(err)));
        metrics.record_request_duration(request_start.elapsed().as_millis() as u64);
        reply(&msg, &transport, &metrics, result).await;
        return;
    }
    metrics.record_cache_miss();

    let coalesce_key = if broadcast {
        None
    } else {
        R::coalesce_key(&request)
    };
    let result = match coalesce_key {
        Some(coalesce_key) => {
            let key = (R::route_key(&request), coalesce_key);
            let call_service = service.clone();
            coalesce(&service.inflight, key, async move {
                call(&call_service, request, broadcast).await
            })
            .await
        }
        None => call(&service, request, broadcast).await.map_err(Arc::new),
    };
    metrics.record_request_duration(request_start.elapsed().as_millis() as u64);
    reply(&msg, &transport, &metrics, result).await;
}

async fn call<R, S>(
    service: &Combined<R, S>,
    request: R::Request,
    broadcast: bool,
) -> Result<Bytes, CallError>
where
    R: RouterService,
    R::Request: Clone,
    S: ShardService<Request = R::Request, Response = R::Response>,
{
    let cache_request = (R::CACHES_RESPONSES && !broadcast).then(|| request.clone());
    let response = tokio::time::timeout(SHARD_REQUEST_TIMEOUT, service.shard.handle(request))
        .await
        .map_err(|_| CallError::Timeout)?
        .map_err(CallError::Handler)?;
    if let Some(cache_request) = cache_request {
        service.router.l1_insert(&cache_request, &response);
    }
    serde_json::to_vec(&response)
        .map(Bytes::from)
        .map_err(CallError::Encode)
}

async fn reply(
    msg: &impl TransportMessage,
    transport: &impl Transport,
    metrics: &ServiceMetrics,
    result: Result<Bytes, Arc<CallError>>,
) {
    match result {
        Ok(payload) => {
            if msg.has_reply() {
                reply_bytes(msg, transport, &payload).await;
            }
        }
        Err(err) => {
            match err.as_ref() {
                CallError::Timeout => warn!(subject = msg.subject(), "request timed out"),
                CallError::Handler(error) => warn!(error = %error, "handler returned error"),
                CallError::Encode(error) => warn!(error = %error, "failed to encode response"),
            }
            metrics.record_request_error();
            reply_json_error(msg, transport, err.code()).await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Mode;
    use crate::transport::InMemoryTransport;
    use serde::{Deserialize, Serialize};
    use std::time::Duration;

    #[derive(Clone, Serialize, Deserialize)]
    struct EchoRequest {
        key: String,
    }

    #[derive(Clone, Serialize, Deserialize)]
    struct EchoResponse {
        key: String,
    }

    struct EchoRouter;

    impl RouterService for EchoRouter {
        type Request = EchoRequest;
        type Response = EchoResponse;

        fn service_name(&self) -> &str {
            "echo"
        }

        fn route_key(request: &EchoRequest) -> String {
            request.key.clone()
        }

        fn coalesce_key(request: &EchoRequest) -> Option<String> {
            Some(request.key.clone())
        }
    }

    struct EchoShard;

    impl ShardService for EchoShard {
        type Request = EchoRequest;
        type Response = EchoResponse;

        fn service_name(&self) -> &str {
            "echo"
        }

        async fn handle(&self, request: EchoRequest) -> anyhow::Result<EchoResponse> {
            if request.key.is_empty() {
                anyhow::bail!("empty key");
            }
            Ok(EchoResponse { key: request.key })
        }
    }

    #[tokio::test]
    async fn combined_answers_the_service_subject_from_the_shard_handler() {
        let transport = InMemoryTransport::new();
        let server_transport = transport.clone();
        let server = tokio::spawn(async move {
            run_combined(
                &ServiceConfig::for_test(Mode::Combined, 0, 1),
                EchoRouter,
                EchoShard,
                server_transport,
            )
            .await
        });
        tokio::time::sleep(Duration::from_millis(25)).await;

        let response = transport
            .request("svc.echo", br#"{"key":"a"}"#, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(response, br#"{"key":"a"}"#);

        let error = transport
            .request("svc.echo", br#"{"key":""}"#, Duration::from_secs(1))
            .await
            .unwrap();
        assert_eq!(error, br#"{"error":"shard_handler_error"}"#);

        server.abort();
    }
}
