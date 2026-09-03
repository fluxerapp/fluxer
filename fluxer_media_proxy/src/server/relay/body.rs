// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{storage::RelayBodyChunks, upload_relay::RelayError};
use axum::{body::Body, http::HeaderValue};
use bytes::Bytes;
use http_body_util::BodyExt as _;
use parking_lot::Mutex;
use std::{
    io,
    sync::{Arc, OnceLock},
    time::Duration,
};

const RELAY_STREAM_FRAME_BYTES: usize = 64 * 1024;
const RELAY_SOURCE_FRAME_LIMIT: u32 = 131_072;

#[derive(Clone)]
pub(in crate::server) struct RelayBodyProgress {
    relayed_bytes: Arc<Mutex<u64>>,
}

impl RelayBodyProgress {
    fn new() -> Self {
        Self {
            relayed_bytes: Arc::new(Mutex::new(0)),
        }
    }

    fn update(&self, frame: &Bytes) {
        if frame.is_empty() {
            return;
        }
        let mut relayed_bytes = self.relayed_bytes.lock();
        let total = relayed_bytes.saturating_add(frame.len() as u64);
        *relayed_bytes = total;
    }
}

pub(in crate::server) fn validate_completed_relay_body(
    client_failure: &Arc<OnceLock<RelayError>>,
    progress: &RelayBodyProgress,
    expected_bytes: u64,
) -> Result<(), RelayError> {
    if let Some(failure) = client_failure.get().copied() {
        return Err(failure);
    }
    if *progress.relayed_bytes.lock() != expected_bytes {
        return Err(RelayError::ClientUploadFailed);
    }
    Ok(())
}

pub(in crate::server) fn relay_etag(value: &str) -> HeaderValue {
    HeaderValue::from_str(value).unwrap_or_else(|_| HeaderValue::from_static(""))
}

pub(in crate::server) struct RelayBodyStreamRequest {
    pub(in crate::server) body: Body,
    pub(in crate::server) declared_length: u64,
    pub(in crate::server) deadline: tokio::time::Instant,
    pub(in crate::server) failure: Arc<OnceLock<RelayError>>,
}

pub(in crate::server) struct RelayBodyStream {
    state: RelayBodyStreamState,
}

enum RelayBodyStreamState {
    Active(Box<RelayBodyActiveStream>),
    Terminal(RelayBodyProgress),
}

struct RelayBodyActiveStream {
    body: Body,
    declared_length: u64,
    written: u64,
    source_frames_read: u32,
    deadline: tokio::time::Instant,
    pending: Bytes,
    failure: Arc<OnceLock<RelayError>>,
    progress: RelayBodyProgress,
}

impl RelayBodyStream {
    pub(in crate::server) fn new(request: RelayBodyStreamRequest) -> Self {
        let RelayBodyStreamRequest {
            body,
            declared_length,
            deadline,
            failure,
        } = request;
        Self {
            state: RelayBodyStreamState::Active(Box::new(RelayBodyActiveStream {
                body,
                declared_length,
                written: 0,
                source_frames_read: 0,
                deadline,
                pending: Bytes::new(),
                failure,
                progress: RelayBodyProgress::new(),
            })),
        }
    }

    pub(in crate::server) fn progress(&self) -> RelayBodyProgress {
        match &self.state {
            RelayBodyStreamState::Active(stream) => stream.progress.clone(),
            RelayBodyStreamState::Terminal(progress) => progress.clone(),
        }
    }

    pub(in crate::server) async fn next(self) -> Option<(Result<Bytes, io::Error>, Self)> {
        match self.state {
            RelayBodyStreamState::Active(stream) => stream.next().await,
            RelayBodyStreamState::Terminal(_) => None,
        }
    }

    fn active(stream: Box<RelayBodyActiveStream>) -> Self {
        Self {
            state: RelayBodyStreamState::Active(stream),
        }
    }

    fn terminal(progress: RelayBodyProgress) -> Self {
        Self {
            state: RelayBodyStreamState::Terminal(progress),
        }
    }
}

impl RelayBodyActiveStream {
    async fn next(mut self: Box<Self>) -> Option<(Result<Bytes, io::Error>, RelayBodyStream)> {
        let now = tokio::time::Instant::now();
        if now >= self.deadline {
            return self.fail(
                RelayError::ClientUploadFailed,
                io::ErrorKind::TimedOut,
                "client upload total deadline elapsed",
            );
        }
        if !self.pending.is_empty() {
            return Some(self.emit_pending_frame());
        }
        let read_timeout = self
            .deadline
            .duration_since(now)
            .max(Duration::from_millis(1));
        let frame = match tokio::time::timeout_at(now + read_timeout, self.body.frame()).await {
            Ok(Some(Ok(frame))) => frame,
            Ok(Some(Err(_))) => {
                return self.fail(
                    RelayError::ClientUploadFailed,
                    io::ErrorKind::ConnectionAborted,
                    "client body read failed",
                );
            }
            Ok(None) if self.written == self.declared_length => return None,
            Ok(None) => {
                return self.fail(
                    RelayError::ClientUploadFailed,
                    io::ErrorKind::UnexpectedEof,
                    "payload shorter than declared length",
                );
            }
            Err(_) => {
                return self.fail(
                    RelayError::ClientUploadFailed,
                    io::ErrorKind::TimedOut,
                    "client upload body timed out",
                );
            }
        };
        let Some(source_frames_read) = self
            .source_frames_read
            .checked_add(1)
            .filter(|count| *count <= RELAY_SOURCE_FRAME_LIMIT)
        else {
            return self.fail(
                RelayError::PayloadTooLarge,
                io::ErrorKind::InvalidData,
                "client upload exceeded the source frame limit",
            );
        };
        self.source_frames_read = source_frames_read;
        let Ok(chunk) = frame.into_data() else {
            return Some((Ok(Bytes::new()), RelayBodyStream::active(self)));
        };
        let next = self.written.saturating_add(chunk.len() as u64);
        if next > self.declared_length {
            return self.fail(
                RelayError::PayloadTooLarge,
                io::ErrorKind::InvalidData,
                "payload exceeded declared length",
            );
        }
        self.written = next;
        self.pending = chunk;
        Some(self.emit_pending_frame())
    }

    fn emit_pending_frame(mut self: Box<Self>) -> (Result<Bytes, io::Error>, RelayBodyStream) {
        let frame_length = self.pending.len().min(RELAY_STREAM_FRAME_BYTES);
        let frame = self.pending.split_to(frame_length);
        self.progress.update(&frame);
        (Ok(frame), RelayBodyStream::active(self))
    }

    fn fail(
        self,
        failure: RelayError,
        kind: io::ErrorKind,
        message: &'static str,
    ) -> Option<(Result<Bytes, io::Error>, RelayBodyStream)> {
        let _ = self.failure.set(failure);
        Some((
            Err(io::Error::new(kind, message)),
            RelayBodyStream::terminal(self.progress),
        ))
    }
}

pub(in crate::server) fn relay_body_chunks(stream: RelayBodyStream) -> RelayBodyChunks {
    Box::pin(futures_util::stream::unfold(
        Some(stream),
        |state: Option<RelayBodyStream>| async move {
            let mut stream = state?;
            loop {
                match stream.next().await {
                    Some((Ok(chunk), next)) if chunk.is_empty() => {
                        stream = next;
                        tokio::task::yield_now().await;
                    }
                    Some((Ok(chunk), next)) => return Some((Ok(chunk), Some(next))),
                    Some((Err(err), _)) => return Some((Err(err), None)),
                    None => return None,
                }
            }
        },
    ))
}
