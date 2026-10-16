// SPDX-License-Identifier: AGPL-3.0-or-later

use super::native_runtime::clear_vips_error;
use super::{MediaError, ensure_deadline_pending, native_optional_deadline, native_status_error};
use crate::native;
use libc::{c_int, c_void, size_t};
use std::{ffi::CStr, slice};

const STREAMING_WRITE_INITIAL_CAPACITY: usize = 16 * 1024;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum StreamingWriteFailure {
    DeadlineExceeded,
    OutputLimitExceeded,
    LengthOverflow,
    InvalidBytesPointer,
    AllocationFailed,
}

struct StreamingWriteContext {
    out: Vec<u8>,
    cap: usize,
    deadline: Option<i64>,
    failure: Option<StreamingWriteFailure>,
}

impl StreamingWriteContext {
    fn reject(&mut self, failure: StreamingWriteFailure) -> c_int {
        if self.failure.is_none() {
            self.failure = Some(failure);
        }
        -1
    }

    fn media_error(&self, native_status: native::NativeStatus) -> Option<MediaError> {
        match self.failure {
            Some(StreamingWriteFailure::DeadlineExceeded) => Some(MediaError::RequestTimeout),
            Some(
                StreamingWriteFailure::OutputLimitExceeded | StreamingWriteFailure::LengthOverflow,
            ) => Some(MediaError::StreamTooLong),
            Some(StreamingWriteFailure::InvalidBytesPointer) => Some(MediaError::MediaEncodeFailed),
            Some(StreamingWriteFailure::AllocationFailed) => Some(MediaError::AllocationFailed),
            None => native_status_error(native_status, MediaError::MediaEncodeFailed)
                .or_else(|| self.out.is_empty().then_some(MediaError::MediaEncodeFailed)),
        }
    }
}

unsafe extern "C" fn streaming_write_callback(
    user_data: *mut c_void,
    bytes: *const c_void,
    len: size_t,
) -> c_int {
    if user_data.is_null() {
        return -1;
    }
    let context = unsafe { &mut *(user_data as *mut StreamingWriteContext) };
    if ensure_deadline_pending(context.deadline).is_err() {
        return context.reject(StreamingWriteFailure::DeadlineExceeded);
    }
    if context.failure.is_some() {
        return -1;
    }
    if len == 0 {
        return 0;
    }
    if bytes.is_null() {
        return context.reject(StreamingWriteFailure::InvalidBytesPointer);
    }
    if len > isize::MAX as usize {
        return context.reject(StreamingWriteFailure::LengthOverflow);
    }
    let Some(next_len) = context.out.len().checked_add(len) else {
        return context.reject(StreamingWriteFailure::LengthOverflow);
    };
    if next_len > isize::MAX as usize {
        return context.reject(StreamingWriteFailure::LengthOverflow);
    }
    if next_len > context.cap {
        return context.reject(StreamingWriteFailure::OutputLimitExceeded);
    }
    if next_len > context.out.capacity() {
        let next_capacity = context
            .out
            .capacity()
            .saturating_mul(2)
            .max(next_len)
            .min(context.cap);
        let additional = next_capacity - context.out.len();
        if context.out.try_reserve_exact(additional).is_err() {
            return context.reject(StreamingWriteFailure::AllocationFailed);
        }
    }
    let chunk = unsafe { slice::from_raw_parts(bytes.cast::<u8>(), len) };
    context.out.extend_from_slice(chunk);
    0
}

pub(super) fn write_vips_image_to_vec(
    image: &native::VipsImageHandle<'_>,
    suffix: &CStr,
    max_bytes: usize,
    deadline: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    assert!(
        max_bytes > 0,
        "streaming encode byte limit must be positive"
    );
    ensure_deadline_pending(deadline)?;
    let mut out = Vec::new();
    out.try_reserve_exact(max_bytes.min(STREAMING_WRITE_INITIAL_CAPACITY))
        .map_err(|_| MediaError::AllocationFailed)?;
    let mut context = StreamingWriteContext {
        out,
        cap: max_bytes,
        deadline,
        failure: None,
    };
    let status = native::NativeStatus::from_code(unsafe {
        native::fluxer_vips_image_write_to_callback(
            image.as_ptr(),
            suffix.as_ptr(),
            native_optional_deadline(deadline),
            Some(streaming_write_callback),
            (&mut context as *mut StreamingWriteContext).cast(),
        )
    });
    ensure_deadline_pending(deadline)?;
    if let Some(error) = context.media_error(status) {
        clear_vips_error();
        return Err(error);
    }
    Ok(context.out)
}
