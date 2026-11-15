// SPDX-License-Identifier: AGPL-3.0-or-later

const RESPONSE_BODY_CHUNK_BYTES_MIN: u64 = 4 * 1024;
const RESPONSE_BODY_CHUNK_OVERHEAD_MAX: u64 = 16;
pub(crate) const RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX: usize = 512 * 1024;

pub(crate) fn response_body_chunk_limit(maximum_bytes: u64) -> u64 {
    maximum_bytes
        .div_ceil(RESPONSE_BODY_CHUNK_BYTES_MIN)
        .checked_add(RESPONSE_BODY_CHUNK_OVERHEAD_MAX)
        .expect("u64 response length chunk limit must fit u64")
}
