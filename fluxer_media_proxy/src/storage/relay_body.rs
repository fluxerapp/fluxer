// SPDX-License-Identifier: AGPL-3.0-or-later

use bytes::Bytes;
use futures_util::Stream;
use http_body::{Frame, SizeHint};
use std::{
    pin::Pin,
    task::{Context, Poll},
};
use tokio::io::{AsyncRead, ReadBuf};

const RELAY_BODY_CHUNK_BYTES: usize = 256 * 1024;

pub type RelayBodyChunks =
    Pin<Box<dyn Stream<Item = Result<Bytes, std::io::Error>> + Send + 'static>>;

pub enum RelayBody {
    Spooled(tokio::fs::File),
    Streamed(RelayBodyChunks),
}

pub struct RelayPutOptions {
    pub body: RelayBody,
    pub content_length: u64,
    pub content_type: Option<String>,
    pub upload_id: Option<String>,
    pub part_number: Option<u32>,
    pub timeout_ms: u64,
}

pub(super) struct SizedFileBody {
    file: tokio::fs::File,
    remaining: u64,
}

impl SizedFileBody {
    pub(super) fn new(file: tokio::fs::File, len: u64) -> Self {
        Self {
            file,
            remaining: len,
        }
    }
}

impl http_body::Body for SizedFileBody {
    type Data = Bytes;
    type Error = std::io::Error;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        if self.remaining == 0 {
            return Poll::Ready(None);
        }
        let chunk_len = self.remaining.min(RELAY_BODY_CHUNK_BYTES as u64) as usize;
        let mut buffer = vec![0u8; chunk_len];
        let read = {
            let mut read_buf = ReadBuf::new(&mut buffer);
            match Pin::new(&mut self.file).poll_read(cx, &mut read_buf) {
                Poll::Ready(Ok(())) => read_buf.filled().len(),
                Poll::Ready(Err(err)) => return Poll::Ready(Some(Err(err))),
                Poll::Pending => return Poll::Pending,
            }
        };
        if read == 0 {
            return Poll::Ready(Some(Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "upload relay body ended before declared content length",
            ))));
        }
        buffer.truncate(read);
        self.remaining = self.remaining.saturating_sub(read as u64);
        Poll::Ready(Some(Ok(Frame::data(Bytes::from(buffer)))))
    }

    fn size_hint(&self) -> SizeHint {
        SizeHint::with_exact(self.remaining)
    }
}
