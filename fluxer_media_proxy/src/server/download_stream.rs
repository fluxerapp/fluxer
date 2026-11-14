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
        Body::from_stream(
            futures_util::stream::unfold(
                Some(stream),
                |stream: Option<ActiveDownloadStream>| async move { stream?.next().await },
            )
            .fuse(),
        )
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
            overruns.external_stream_overruns.inc();
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
