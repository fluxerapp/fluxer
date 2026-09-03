// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget, ByteReservation},
    http_headers, range, response_body_limit,
    storage::StorageError,
};
use bytes::Bytes;
use futures_util::{Stream, StreamExt as _};
use http::{HeaderMap, StatusCode, header};
use parking_lot::Mutex;
use std::{
    io,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll},
};
use tokio::{
    io::{AsyncRead, AsyncReadExt as _, ReadBuf},
    sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError},
};

struct ReadBufferBudget {
    reservation: ByteReservation,
    reserved_bytes: usize,
}

impl ReadBufferBudget {
    fn reserve(budget: &ByteBudget, bytes: usize) -> Result<Self, StorageError> {
        let Some(reservation) = budget.try_reserve(bytes) else {
            return Err(StorageError::BufferBudgetExhausted);
        };
        Ok(Self {
            reservation,
            reserved_bytes: bytes,
        })
    }

    fn grow_to(&mut self, required_bytes: usize) -> Result<(), StorageError> {
        if required_bytes <= self.reserved_bytes {
            return Ok(());
        }
        let additional = required_bytes - self.reserved_bytes;
        if !self.reservation.try_grow(additional) {
            return Err(StorageError::BufferBudgetExhausted);
        }
        self.reserved_bytes = required_bytes;
        Ok(())
    }

    fn shrink_to(&mut self, bytes: usize) {
        self.reservation.shrink_to(bytes);
        self.reserved_bytes = bytes;
    }

    fn into_reservation(self) -> ByteReservation {
        self.reservation
    }
}

fn provider_status_error(status: StatusCode) -> StorageError {
    if status == StatusCode::NOT_FOUND {
        return StorageError::ObjectChanged;
    }
    StorageError::ObjectStorage(anyhow::anyhow!(
        "object storage provider rejected a read with status {status}"
    ))
}

pub(super) async fn read_response_bytes(
    mut response: reqwest::Response,
    expected_length: usize,
    budget: &ByteBudget,
) -> Result<BudgetedBytes, StorageError> {
    let _transport_chunk_reservation = budget
        .try_reserve(response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX)
        .ok_or(StorageError::BufferBudgetExhausted)?;
    let mut buffer_budget = ReadBufferBudget::reserve(budget, expected_length)?;
    let mut body = Vec::new();
    body.try_reserve_exact(expected_length)
        .map_err(|_| StorageError::BufferAllocationFailed)?;
    buffer_budget.grow_to(body.capacity())?;
    let mut chunks_read = 0_u64;
    let chunks_max = response_body_limit::response_body_chunk_limit(expected_length as u64);
    while let Some(chunk) = response.chunk().await? {
        if chunk.len() > response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX {
            return Err(StorageError::ObjectStorage(anyhow::anyhow!(
                "object storage response transport chunk exceeded its byte bound"
            )));
        }
        chunks_read = chunks_read
            .checked_add(1)
            .filter(|chunks| *chunks <= chunks_max)
            .ok_or_else(|| {
                StorageError::ObjectStorage(anyhow::anyhow!(
                    "object storage response exceeded its chunk limit"
                ))
            })?;
        let next_length = body
            .len()
            .checked_add(chunk.len())
            .filter(|length| *length <= expected_length)
            .ok_or(StorageError::ObjectChanged)?;
        body.extend_from_slice(&chunk);
        assert_eq!(body.len(), next_length);
    }
    if body.len() != expected_length {
        return Err(StorageError::ObjectChanged);
    }
    buffer_budget.shrink_to(body.capacity());
    Ok(BudgetedBytes::budgeted(
        Bytes::from(body),
        buffer_budget.into_reservation(),
    ))
}

pub(super) async fn read_exact_bytes(
    reader: impl AsyncRead + Unpin,
    expected_length: usize,
    budget: &ByteBudget,
) -> Result<BudgetedBytes, StorageError> {
    let mut buffer_budget = ReadBufferBudget::reserve(budget, expected_length)?;
    let mut body = Vec::new();
    body.try_reserve_exact(expected_length)
        .map_err(|_| StorageError::BufferAllocationFailed)?;
    buffer_budget.grow_to(body.capacity())?;
    let mut limited = reader.take(expected_length as u64);
    while body.len() < expected_length {
        if limited.read_buf(&mut body).await? == 0 {
            return Err(StorageError::ObjectChanged);
        }
        assert!(body.len() <= expected_length);
    }
    let mut reader = limited.into_inner();
    let mut extra = [0u8; 1];
    if reader.read(&mut extra).await? != 0 {
        return Err(StorageError::ObjectChanged);
    }
    buffer_budget.shrink_to(body.capacity());
    Ok(BudgetedBytes::budgeted(
        Bytes::from(body),
        buffer_budget.into_reservation(),
    ))
}

pub(super) struct StreamResponseValidation<'a> {
    pub(super) status: StatusCode,
    pub(super) headers: &'a HeaderMap,
    pub(super) total_length: u64,
    pub(super) expected_length: u64,
    pub(super) byte_range: Option<range::ByteRange>,
}

pub(super) fn validate_stream_response(
    validation: StreamResponseValidation<'_>,
) -> Result<(), StorageError> {
    let StreamResponseValidation {
        status,
        headers,
        total_length,
        expected_length,
        byte_range,
    } = validation;
    if !status.is_success() {
        return Err(provider_status_error(status));
    }
    let content_length = http_headers::parse_content_length(headers).ok_or_else(|| {
        StorageError::ObjectStorage(anyhow::anyhow!(
            "object storage provider omitted a single valid Content-Length"
        ))
    })?;
    if content_length != expected_length {
        return Err(StorageError::ObjectChanged);
    }
    match byte_range {
        None if status == StatusCode::OK && expected_length == total_length => Ok(()),
        Some(range) if status == StatusCode::PARTIAL_CONTENT => {
            let mut content_ranges = headers.get_all(header::CONTENT_RANGE).iter();
            let content_range = content_ranges.next().and_then(|value| value.to_str().ok());
            if content_ranges.next().is_some() {
                return Err(StorageError::ObjectStorage(anyhow::anyhow!(
                    "object storage provider returned multiple Content-Range values"
                )));
            }
            let actual = range::parse_content_range(content_range).ok_or_else(|| {
                StorageError::ObjectStorage(anyhow::anyhow!(
                    "object storage provider returned an invalid Content-Range"
                ))
            })?;
            let total_length =
                usize::try_from(total_length).map_err(|_| StorageError::StreamTooLong)?;
            if actual.start != range.start
                || actual.end != range.end
                || actual.size != Some(total_length)
            {
                return Err(StorageError::ObjectChanged);
            }
            Ok(())
        }
        Some(range)
            if status == StatusCode::OK
                && range.start == 0
                && range.end.checked_add(1) == usize::try_from(total_length).ok() =>
        {
            Ok(())
        }
        _ => Err(StorageError::ObjectStorage(anyhow::anyhow!(
            "object storage provider returned an unexpected successful status {status}"
        ))),
    }
}

pub(super) type ByteStream =
    Pin<Box<dyn Stream<Item = Result<Bytes, std::io::Error>> + Send + 'static>>;

struct LocalStreamBufferData {
    data: Vec<u8>,
    _reservation: Option<ByteReservation>,
    _slot: Option<OwnedSemaphorePermit>,
}

#[derive(Clone)]
pub(super) struct LocalStreamBufferPool {
    inner: Arc<LocalStreamBufferPoolInner>,
}

const LOCAL_STREAM_FALLBACK_BUFFER_BYTES: usize = 4 * 1024;

impl LocalStreamBufferData {
    fn unpooled(data: Vec<u8>) -> Self {
        Self {
            data,
            _reservation: None,
            _slot: None,
        }
    }
}

struct LocalStreamBufferPoolInner {
    available: Mutex<Vec<LocalStreamBufferData>>,
    budget: ByteBudget,
    slots: Arc<Semaphore>,
    max_buffer_bytes: usize,
    max_buffers: usize,
}

struct LocalStreamBuffer {
    data: Option<LocalStreamBufferData>,
    pool: LocalStreamBufferPool,
}

impl AsRef<[u8]> for LocalStreamBuffer {
    fn as_ref(&self) -> &[u8] {
        &self
            .data
            .as_ref()
            .expect("local stream buffer owner must retain its data")
            .data
    }
}

struct LocalReaderStream<R> {
    reader: R,
    buffer: Option<LocalStreamBuffer>,
    buffer_pool: LocalStreamBufferPool,
    capacity: usize,
    state: LocalReaderState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LocalReaderState {
    Active,
    Terminated,
}

impl<R> LocalReaderStream<R> {
    fn new(
        reader: R,
        buffer_pool: LocalStreamBufferPool,
        capacity: usize,
    ) -> Result<Self, StorageError> {
        assert!(capacity > 0, "local reader stream capacity must be nonzero");
        let buffer = buffer_pool.acquire(capacity).map_err(|error| match error {
            LocalStreamBufferError::AllocationFailed => StorageError::BufferAllocationFailed,
        })?;
        Ok(Self {
            reader,
            buffer: Some(buffer),
            buffer_pool,
            capacity,
            state: LocalReaderState::Active,
        })
    }

    fn replenish_buffer(&mut self) -> Result<(), io::Error> {
        assert!(self.buffer.is_none());
        let buffer = self
            .buffer_pool
            .acquire(self.capacity)
            .map_err(|error| match error {
                LocalStreamBufferError::AllocationFailed => {
                    io::Error::from(io::ErrorKind::OutOfMemory)
                }
            })?;
        self.buffer = Some(buffer);
        Ok(())
    }

    fn terminate(&mut self) {
        self.state = LocalReaderState::Terminated;
        self.buffer = None;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LocalStreamBufferError {
    AllocationFailed,
}

impl LocalStreamBufferPool {
    pub(super) fn new(max_buffer_bytes: usize, max_buffers: usize) -> anyhow::Result<Self> {
        anyhow::ensure!(
            max_buffer_bytes > 0,
            "local stream buffer size must be nonzero"
        );
        anyhow::ensure!(
            max_buffers > 0,
            "local stream buffer pool capacity must be nonzero"
        );
        let budget_bytes = max_buffer_bytes
            .checked_mul(max_buffers)
            .ok_or_else(|| anyhow::anyhow!("object storage stream buffer budget overflowed"))?;
        let mut available = Vec::new();
        available
            .try_reserve_exact(max_buffers)
            .map_err(|_| anyhow::anyhow!("object storage stream buffer pool allocation failed"))?;
        Ok(Self {
            inner: Arc::new(LocalStreamBufferPoolInner {
                available: Mutex::new(available),
                budget: ByteBudget::new(budget_bytes),
                slots: Arc::new(Semaphore::new(max_buffers)),
                max_buffer_bytes,
                max_buffers,
            }),
        })
    }

    fn acquire(&self, capacity: usize) -> Result<LocalStreamBuffer, LocalStreamBufferError> {
        assert!(capacity > 0);
        assert!(capacity <= self.inner.max_buffer_bytes);
        let available = self.inner.available.lock().pop();
        if let Some(data) = available {
            if data.data.capacity() >= capacity {
                return Ok(LocalStreamBuffer {
                    data: Some(data),
                    pool: self.clone(),
                });
            }
            drop(data);
        }
        let data = allocate_local_stream_buffer(&self.inner.slots, &self.inner.budget, capacity)?;
        Ok(LocalStreamBuffer {
            data: Some(data),
            pool: self.clone(),
        })
    }

    fn release(&self, mut data: LocalStreamBufferData) {
        if data._slot.is_none() {
            return;
        }
        data.data.clear();
        let mut available = self.inner.available.lock();
        if available.len() < self.inner.max_buffers {
            available.push(data);
        }
    }
}

impl LocalStreamBuffer {
    fn capacity(&self) -> usize {
        self.data
            .as_ref()
            .expect("local stream buffer owner must retain its data")
            .data
            .capacity()
    }

    fn read_buffer(&mut self, capacity: usize) -> ReadBuf<'_> {
        let data = &mut self
            .data
            .as_mut()
            .expect("local stream buffer owner must retain its data")
            .data;
        assert!(data.is_empty());
        assert!(capacity > 0);
        assert!(capacity <= data.capacity());
        let spare = data.spare_capacity_mut();
        ReadBuf::uninit(&mut spare[..capacity])
    }

    fn finish_read(&mut self, length: usize, capacity: usize) {
        let data = &mut self
            .data
            .as_mut()
            .expect("local stream buffer owner must retain its data")
            .data;
        assert!(data.is_empty());
        assert!(length > 0);
        assert!(length <= capacity);
        assert!(capacity <= data.capacity());
        unsafe { data.set_len(length) };
    }
}

impl Drop for LocalStreamBuffer {
    fn drop(&mut self) {
        if let Some(data) = self.data.take() {
            self.pool.release(data);
        }
    }
}

fn unpooled_local_stream_buffer(
    capacity: usize,
) -> Result<LocalStreamBufferData, LocalStreamBufferError> {
    let capacity = capacity.min(LOCAL_STREAM_FALLBACK_BUFFER_BYTES);
    let mut data = Vec::new();
    data.try_reserve_exact(capacity)
        .map_err(|_| LocalStreamBufferError::AllocationFailed)?;
    Ok(LocalStreamBufferData::unpooled(data))
}

fn allocate_local_stream_buffer(
    slots: &Arc<Semaphore>,
    budget: &ByteBudget,
    capacity: usize,
) -> Result<LocalStreamBufferData, LocalStreamBufferError> {
    let pooled = match Arc::clone(slots).try_acquire_owned() {
        Ok(slot) => budget
            .try_reserve(capacity)
            .map(|reservation| (slot, reservation)),
        Err(TryAcquireError::NoPermits) => None,
        Err(TryAcquireError::Closed) => {
            unreachable!("local stream buffer capacity semaphore is never closed")
        }
    };
    let Some((slot, mut reservation)) = pooled else {
        return unpooled_local_stream_buffer(capacity);
    };
    let mut data = Vec::new();
    data.try_reserve_exact(capacity)
        .map_err(|_| LocalStreamBufferError::AllocationFailed)?;
    if data.capacity() > reservation.amount()
        && !reservation.try_grow(data.capacity() - reservation.amount())
    {
        return unpooled_local_stream_buffer(capacity);
    }
    reservation.shrink_to(data.capacity());
    Ok(LocalStreamBufferData {
        data,
        _reservation: Some(reservation),
        _slot: Some(slot),
    })
}

impl<R> Stream for LocalReaderStream<R>
where
    R: AsyncRead + Unpin,
{
    type Item = Result<Bytes, io::Error>;

    fn poll_next(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if self.state == LocalReaderState::Terminated {
            return Poll::Ready(None);
        }
        if self.buffer.is_none()
            && let Err(error) = self.replenish_buffer()
        {
            self.terminate();
            return Poll::Ready(Some(Err(error)));
        }

        let this = self.as_mut().get_mut();
        let mut capacity = this.capacity;
        let (read_result, bytes_read) = {
            let buffer = this
                .buffer
                .as_mut()
                .expect("active local reader must retain its stream buffer");
            capacity = capacity.min(buffer.capacity());
            let mut read_buffer = buffer.read_buffer(capacity);
            let read_result = Pin::new(&mut this.reader).poll_read(context, &mut read_buffer);
            (read_result, read_buffer.filled().len())
        };
        match read_result {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Err(error)) => {
                this.terminate();
                Poll::Ready(Some(Err(error)))
            }
            Poll::Ready(Ok(())) if bytes_read == 0 => {
                this.terminate();
                Poll::Ready(None)
            }
            Poll::Ready(Ok(())) => {
                let mut buffer = this
                    .buffer
                    .take()
                    .expect("active local reader must retain its stream buffer");
                buffer.finish_read(bytes_read, capacity);
                Poll::Ready(Some(Ok(Bytes::from_owner(buffer))))
            }
        }
    }
}

pub(super) fn local_reader_stream(
    reader: impl AsyncRead + Unpin + Send + 'static,
    buffer_pool: LocalStreamBufferPool,
    capacity: usize,
) -> Result<ByteStream, StorageError> {
    Ok(Box::pin(LocalReaderStream::new(
        reader,
        buffer_pool,
        capacity,
    )?))
}

pub(super) fn exact_response_stream(
    response: reqwest::Response,
    expected_length: u64,
) -> impl Stream<Item = Result<Bytes, std::io::Error>> + Send + 'static {
    let stream = response
        .bytes_stream()
        .map(|result| result.map_err(|error| std::io::Error::other(error.without_url())));
    exact_stream(
        Box::pin(stream),
        expected_length,
        ExactStreamEnd::RequireSourceEnd,
    )
}

pub(super) fn exact_byte_stream(
    stream: ByteStream,
    expected_length: u64,
) -> impl Stream<Item = Result<Bytes, std::io::Error>> + Send + 'static {
    exact_stream(stream, expected_length, ExactStreamEnd::LengthBounded)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExactStreamEnd {
    RequireSourceEnd,
    LengthBounded,
}

fn exact_stream(
    stream: ByteStream,
    expected_length: u64,
    end: ExactStreamEnd,
) -> impl Stream<Item = Result<Bytes, std::io::Error>> + Send + 'static {
    // Only a chunk that carries no bytes needs a count bound. The byte accounting below already
    // bounds every other chunk, and counting them all aborts a legitimate transfer whenever the
    // transport hands over reads smaller than the assumed average chunk size.
    let empty_chunks_remaining = response_body_limit::response_body_chunk_limit(expected_length);
    futures_util::stream::try_unfold(
        (stream, expected_length, empty_chunks_remaining, end),
        |(mut stream, remaining, mut empty_chunks_remaining, end)| async move {
            if remaining == 0 && end == ExactStreamEnd::LengthBounded {
                return Ok(None);
            }
            loop {
                let next = stream.next().await;
                match next {
                    Some(Ok(chunk)) if chunk.is_empty() => {
                        empty_chunks_remaining =
                            empty_chunks_remaining.checked_sub(1).ok_or_else(|| {
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    "object storage stream exceeded its empty chunk limit",
                                )
                            })?;
                    }
                    Some(Ok(chunk)) => {
                        let chunk_length = chunk.len() as u64;
                        if remaining == 0 || chunk_length > remaining {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "object storage stream exceeded its mapped content length",
                            ));
                        }
                        return Ok(Some((
                            chunk,
                            (
                                stream,
                                remaining - chunk_length,
                                empty_chunks_remaining,
                                end,
                            ),
                        )));
                    }
                    Some(Err(error)) => return Err(error),
                    None if remaining == 0 => return Ok(None),
                    None => {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::UnexpectedEof,
                            "object storage stream ended before its mapped content length",
                        ));
                    }
                }
            }
        },
    )
}
