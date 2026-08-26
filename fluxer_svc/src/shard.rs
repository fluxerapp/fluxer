// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use crate::metrics::{ServiceMetrics, now_ms};
use crate::transport::{Transport, TransportMessage, TransportSubscriber, reply_message};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::Semaphore;
use tokio::task::JoinSet;
use tracing::{debug, info, warn};

const MAX_SHARD_REQUEST_BYTES: usize = 2 * 1024 * 1024;

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
    tasks.spawn(async move {
        loop {
            let mut sub = shard_transport.subscribe(&shard_subject).await?;
            info!(
                subject = shard_subject,
                shard_id,
                max_concurrent_requests = shard_permits.available_permits(),
                "shard listening for requests"
            );

            loop {
                tokio::select! {
                    msg_opt = sub.next() => {
                        let Some(msg) = msg_opt else {
                            warn!("shard subscription stream ended, will re-subscribe");
                            break;
                        };

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
                        let raw_payload = msg.payload().to_vec();
                        let permit = match shard_permits.clone().acquire_owned().await {
                            Ok(permit) => permit,
                            Err(_) => return anyhow::Ok(()),
                        };

                        tokio::spawn(async move {
                            let _permit = permit;
                            if !is_serving.load(Ordering::SeqCst) {
                                return;
                            }
                            let request_start = now_ms();
                            metrics.record_request();
                            let request: S::Request = match rmp_serde::from_slice(&raw_payload) {
                                Ok(r) => r,
                                Err(err) => {
                                    warn!(error = %err, "failed to decode shard request");
                                    metrics.record_request_error();
                                    reply_shard_error(&msg, &transport, "shard_request_decode_error")
                                        .await;
                                    return;
                                }
                            };

                            match service.handle(request).await {
                                Ok(response) => {
                                    let elapsed = (now_ms() - request_start).max(0) as u64;
                                    metrics.record_request_duration(elapsed);
                                    if msg.has_reply() {
                                        match rmp_serde::to_vec_named(&response) {
                                            Ok(response_bytes) => {
                                                if let Err(err) =
                                                    reply_message(&msg, &transport, &response_bytes).await
                                                {
                                                    debug!(
                                                        error = %err,
                                                        "failed to send shard reply"
                                                    );
                                                }
                                            }
                                            Err(err) => {
                                                warn!(
                                                    error = %err,
                                                    "failed to encode shard response"
                                                );
                                            }
                                        }
                                    }
                                }
                                Err(err) => {
                                    warn!(error = %err, "shard handler returned error");
                                    metrics.record_request_error();
                                    let elapsed = (now_ms() - request_start).max(0) as u64;
                                    metrics.record_request_duration(elapsed);
                                    reply_shard_error(&msg, &transport, "shard_handler_error").await;
                                }
                            }
                        });
                    }
                    _ = shard_transport.wait_for_reconnect() => {
                        info!("NATS reconnected, re-subscribing shard listener");
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
            info!("shard shutting down, beginning graceful drain");

            is_serving.store(false, Ordering::SeqCst);

            let max_permits = config.max_concurrent_requests;
            let drain_permits = request_permits.clone();
            crate::shutdown::drain_with_timeout(
                async move {
                    if let Ok(_permit) = drain_permits.acquire_many(max_permits as u32).await {
                        info!(
                            max_concurrent_requests = max_permits,
                            "all in-flight requests drained"
                        );
                    }
                },
                crate::shutdown::DEFAULT_DRAIN_TIMEOUT,
            )
            .await;

            info!("shard shutdown complete");
            Ok(())
        }
    }
}

async fn reply_shard_error(msg: &impl TransportMessage, transport: &impl Transport, code: &str) {
    if !msg.has_reply() {
        return;
    }
    let response = serde_json::to_vec(&serde_json::json!({ "error": code })).unwrap_or_default();
    if let Err(err) = reply_message(msg, transport, &response).await {
        debug!(error = %err, "failed to send shard error reply");
    }
}
