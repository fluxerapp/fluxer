// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::BudgetedBytes,
    coalescer::CoalescerError,
    media_process::MediaBytes,
    output_format::OutputFormat,
    server::{response::error::text_with_reason, transform::TransformRuntime},
};
use axum::{http::StatusCode, response::Response};
use std::{future::Future, time::Instant};

pub(in crate::server) struct CachedTransformHit {
    pub(in crate::server) data: BudgetedBytes,
    pub(in crate::server) format: OutputFormat,
}

pub(in crate::server) struct CachedTransformRequest<'a, F> {
    pub(in crate::server) runtime: &'a TransformRuntime,
    pub(in crate::server) cache_key: String,
    pub(in crate::server) format: OutputFormat,
    pub(in crate::server) deadline: Option<Instant>,
    pub(in crate::server) work: F,
}

pub(in crate::server) fn cached_transform_hit(
    runtime: &TransformRuntime,
    cache_key: &str,
) -> Option<CachedTransformHit> {
    let cached = runtime.cache().get(cache_key)?;
    Some(CachedTransformHit {
        data: cached.data,
        format: cached.format,
    })
}

pub(in crate::server) async fn cached_transform<F, Fut>(
    request: CachedTransformRequest<'_, F>,
) -> Result<BudgetedBytes, CoalescerError>
where
    F: FnOnce() -> Fut,
    Fut: Future<Output = anyhow::Result<MediaBytes>>,
{
    let CachedTransformRequest {
        runtime,
        cache_key,
        format,
        deadline,
        work,
    } = request;
    let result = runtime
        .cache()
        .get_or_run(cache_key, format, deadline, work)
        .await;
    if matches!(result, Err(CoalescerError::WorkFailed)) {
        runtime.metrics().record_transform_failure();
    }
    result
}

pub(in crate::server) fn coalescer_failure_response(
    error: CoalescerError,
    timeout_code: &'static str,
) -> Option<Response> {
    let reason = match error {
        CoalescerError::WorkFailed => return None,
        CoalescerError::RequestTimeout => {
            return Some(text_with_reason(
                StatusCode::GATEWAY_TIMEOUT,
                "Gateway Timeout",
                timeout_code,
            ));
        }
        CoalescerError::BufferBudgetExhausted => "transform_output_budget_exhausted",
        CoalescerError::AllocationFailed => "native_transform_allocation_failed",
        CoalescerError::Overloaded => "native_transform_queue_full",
        CoalescerError::Unavailable => "native_transform_unavailable",
        CoalescerError::WorkCancelled => "transform_work_cancelled",
    };
    Some(text_with_reason(
        StatusCode::SERVICE_UNAVAILABLE,
        "Service Unavailable",
        reason,
    ))
}
