// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::external::ExternalMetrics;
use axum::body::{Body, BodyDataStream};
use bytes::Bytes;
use futures_util::StreamExt as _;
use std::{
    io,
    sync::Arc,
    time::{Duration, Instant},
};

const PASSTHROUGH_STREAM_MIN_BYTES_PER_SEC: u64 = 16 * 1024;
const IDLE_TIMEOUT_MESSAGE: &str = "passthrough stream idle timeout";
const TOTAL_TIMEOUT_MESSAGE: &str = "passthrough stream total timeout";
const TRUNCATED_MESSAGE: &str = "passthrough stream ended before Content-Length";
const OVERRUN_MESSAGE: &str = "passthrough stream exceeded Content-Length";

#[derive(Clone)]
pub(in crate::server) struct DownloadStreamPolicy {
    idle_timeout: Duration,
    overruns: Option<Arc<ExternalMetrics>>,
}

struct ActiveDownloadStream {
    source: BodyDataStream,
    remaining: usize,
    allows_early_end: bool,
    idle_timeout: Duration,
    total_deadline: Option<Instant>,
    overruns: Option<Arc<ExternalMetrics>>,
}

type GuardedFrame = Option<(Result<Bytes, io::Error>, Option<ActiveDownloadStream>)>;

impl DownloadStreamPolicy {
    pub(in crate::server) fn for_passthrough(socket_io_timeout_ms: u64) -> Self {
        Self {
            idle_timeout: Duration::from_millis(socket_io_timeout_ms.max(1)),
            overruns: None,
        }
    }

    // Only the external route feeds the overrun counter: a stored object that outruns its own
    // Content-Length is a store bug, not the misbehaving-origin signal that series tracks.
    pub(in crate::server) fn for_external(
        socket_io_timeout_ms: u64,
        overruns: Arc<ExternalMetrics>,
    ) -> Self {
        let mut policy = Self::for_passthrough(socket_io_timeout_ms);
        policy.overruns = Some(overruns);
        policy
    }

    pub(in crate::server) fn guard(self, body: Body, expected_length: usize) -> Body {
        self.guarded(body, expected_length, false)
    }

    pub(in crate::server) fn guard_capped(self, body: Body, max_length: usize) -> Body {
        self.guarded(body, max_length, true)
    }

    fn guarded(self, body: Body, remaining: usize, allows_early_end: bool) -> Body {
        let transfer_seconds = u64::try_from(remaining)
            .unwrap_or(u64::MAX)
            .div_ceil(PASSTHROUGH_STREAM_MIN_BYTES_PER_SEC);
        let total_timeout = self
            .idle_timeout
            .saturating_add(Duration::from_secs(transfer_seconds));
        let stream = ActiveDownloadStream {
            source: body.into_data_stream(),
            remaining,
            allows_early_end,
            idle_timeout: self.idle_timeout,
            total_deadline: Instant::now().checked_add(total_timeout),
            overruns: self.overruns,
        };
        Body::from_stream(futures_util::stream::unfold(
            Some(stream),
            |stream: Option<ActiveDownloadStream>| async move { stream?.next().await },
        ))
    }
}

impl ActiveDownloadStream {
    async fn next(mut self) -> GuardedFrame {
        if self.remaining == 0 {
            return self.finish().await;
        }
        match self.next_frame().await {
            Ok(Some(Ok(chunk))) => {
                if chunk.len() > self.remaining {
                    return self.overrun();
                }
                self.remaining -= chunk.len();
                Some((Ok(chunk), Some(self)))
            }
            Ok(Some(Err(error))) => fail(io::Error::other(error)),
            Ok(None) if self.allows_early_end => None,
            Ok(None) => fail(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                TRUNCATED_MESSAGE,
            )),
            Err(error) => fail(error),
        }
    }

    async fn finish(mut self) -> GuardedFrame {
        match self.next_frame().await {
            Ok(None) => None,
            Ok(Some(Ok(_))) => self.overrun(),
            Ok(Some(Err(error))) => fail(io::Error::other(error)),
            Err(error) => fail(error),
        }
    }

    fn overrun(&self) -> GuardedFrame {
        if let Some(overruns) = &self.overruns {
            overruns.record_stream_overrun();
        }
        fail(io::Error::new(io::ErrorKind::InvalidData, OVERRUN_MESSAGE))
    }

    async fn next_frame(&mut self) -> Result<Option<Result<Bytes, axum::Error>>, io::Error> {
        let now = Instant::now();
        if let Some(total_deadline) = self.total_deadline
            && now >= total_deadline
        {
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                TOTAL_TIMEOUT_MESSAGE,
            ));
        }
        let (deadline, message) = self.next_deadline(now);
        match deadline {
            Some(deadline) => tokio::time::timeout_at(deadline.into(), self.source.next())
                .await
                .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, message)),
            None => Ok(self.source.next().await),
        }
    }

    fn next_deadline(&self, now: Instant) -> (Option<Instant>, &'static str) {
        let idle_deadline = now.checked_add(self.idle_timeout);
        match (idle_deadline, self.total_deadline) {
            (Some(idle), Some(total)) if idle < total => (Some(idle), IDLE_TIMEOUT_MESSAGE),
            (_, Some(total)) => (Some(total), TOTAL_TIMEOUT_MESSAGE),
            (idle, None) => (idle, IDLE_TIMEOUT_MESSAGE),
        }
    }
}

fn fail(error: io::Error) -> GuardedFrame {
    Some((Err(error), None))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metrics::Metrics;
    use http_body_util::BodyExt as _;

    fn body_from_chunks(chunks: Vec<&'static [u8]>) -> Body {
        Body::from_stream(futures_util::stream::iter(
            chunks
                .into_iter()
                .map(|chunk| Ok::<_, io::Error>(Bytes::from_static(chunk))),
        ))
    }

    async fn collect_error(body: Body) -> io::Error {
        let mut error = body
            .collect()
            .await
            .map(|collected| collected.to_bytes())
            .expect_err("the guarded stream fails")
            .into_inner();
        loop {
            error = match error.downcast::<io::Error>() {
                Ok(error) => return *error,
                Err(error) => error
                    .downcast::<axum::Error>()
                    .expect("the guarded stream fails with an io error")
                    .into_inner(),
            };
        }
    }

    #[tokio::test]
    async fn a_stream_matching_its_content_length_passes_through_unchanged() {
        let guarded = DownloadStreamPolicy::for_passthrough(30_000)
            .guard(body_from_chunks(vec![b"he", b"llo"]), 5);
        let collected = guarded
            .collect()
            .await
            .expect("the guarded stream ends")
            .to_bytes();
        assert_eq!(&b"hello"[..], &collected[..]);
    }

    #[tokio::test]
    async fn a_stream_that_ends_before_its_content_length_fails_with_unexpected_eof() {
        let guarded =
            DownloadStreamPolicy::for_passthrough(30_000).guard(body_from_chunks(vec![b"hel"]), 5);
        assert_eq!(
            io::ErrorKind::UnexpectedEof,
            collect_error(guarded).await.kind()
        );
    }

    #[tokio::test]
    async fn a_stream_that_overruns_its_content_length_fails_with_invalid_data() {
        let guarded = DownloadStreamPolicy::for_passthrough(30_000)
            .guard(body_from_chunks(vec![b"hello", b"world"]), 5);
        assert_eq!(
            io::ErrorKind::InvalidData,
            collect_error(guarded).await.kind()
        );
    }

    #[tokio::test]
    async fn a_chunk_larger_than_the_remaining_content_length_fails_with_invalid_data() {
        let guarded = DownloadStreamPolicy::for_passthrough(30_000)
            .guard(body_from_chunks(vec![b"hello world"]), 5);
        assert_eq!(
            io::ErrorKind::InvalidData,
            collect_error(guarded).await.kind()
        );
    }

    #[tokio::test]
    async fn a_stalled_stream_fails_at_its_idle_timeout() {
        let stalled =
            Body::from_stream(futures_util::stream::pending::<Result<Bytes, io::Error>>());
        let guarded = DownloadStreamPolicy::for_passthrough(1).guard(stalled, 5);
        assert_eq!(io::ErrorKind::TimedOut, collect_error(guarded).await.kind());
    }

    #[tokio::test]
    async fn an_external_stream_that_sends_past_its_content_length_is_counted() {
        let metrics = Metrics::new();
        let guarded = DownloadStreamPolicy::for_external(30_000, metrics.external())
            .guard(body_from_chunks(vec![b"hello", b"world"]), 5);
        assert_eq!(
            io::ErrorKind::InvalidData,
            collect_error(guarded).await.kind()
        );
        assert!(
            metrics
                .render()
                .contains("fluxer_media_proxy_external_stream_overruns_total 1\n")
        );
    }

    #[tokio::test]
    async fn an_external_chunk_past_the_remaining_content_length_is_counted() {
        let metrics = Metrics::new();
        let guarded = DownloadStreamPolicy::for_external(30_000, metrics.external())
            .guard(body_from_chunks(vec![b"hello world"]), 5);
        assert_eq!(
            io::ErrorKind::InvalidData,
            collect_error(guarded).await.kind()
        );
        assert!(
            metrics
                .render()
                .contains("fluxer_media_proxy_external_stream_overruns_total 1\n")
        );
    }

    #[tokio::test]
    async fn a_well_behaved_external_stream_leaves_the_overrun_counter_alone() {
        let metrics = Metrics::new();
        let guarded = DownloadStreamPolicy::for_external(30_000, metrics.external())
            .guard(body_from_chunks(vec![b"he", b"llo"]), 5);
        let collected = guarded
            .collect()
            .await
            .expect("the guarded stream ends")
            .to_bytes();
        assert_eq!(&b"hello"[..], &collected[..]);
        assert!(
            metrics
                .render()
                .contains("fluxer_media_proxy_external_stream_overruns_total 0\n")
        );
    }

    #[tokio::test]
    async fn a_capped_stream_that_ends_before_its_cap_passes_through_unchanged() {
        let guarded = DownloadStreamPolicy::for_passthrough(30_000)
            .guard_capped(body_from_chunks(vec![b"he", b"llo"]), 1024);
        let collected = guarded
            .collect()
            .await
            .expect("the guarded stream ends")
            .to_bytes();
        assert_eq!(&b"hello"[..], &collected[..]);
    }

    #[tokio::test]
    async fn a_capped_stream_that_exceeds_its_cap_fails_with_invalid_data() {
        let guarded = DownloadStreamPolicy::for_passthrough(30_000)
            .guard_capped(body_from_chunks(vec![b"hello", b"world"]), 5);
        assert_eq!(
            io::ErrorKind::InvalidData,
            collect_error(guarded).await.kind()
        );
    }

    #[tokio::test]
    async fn a_stalled_capped_stream_still_fails_at_its_idle_timeout() {
        let stalled =
            Body::from_stream(futures_util::stream::pending::<Result<Bytes, io::Error>>());
        let guarded = DownloadStreamPolicy::for_passthrough(1).guard_capped(stalled, 1024);
        assert_eq!(io::ErrorKind::TimedOut, collect_error(guarded).await.kind());
    }
}
