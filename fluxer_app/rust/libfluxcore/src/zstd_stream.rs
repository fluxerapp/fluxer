// SPDX-License-Identifier: AGPL-3.0-or-later

use zstd::zstd_safe::zstd_sys::ZSTD_EndDirective;
use zstd::zstd_safe::{CCtx, CParameter, DCtx, InBuffer, OutBuffer};

const READ_BUFFER_BYTES: usize = 16 * 1024;
const MAX_DECOMPRESSED_BYTES: usize = 64 * 1024 * 1024;

#[derive(Debug)]
pub enum ZstdError {
    DecompressedSizeTooLarge,
    Decode,
    Encode,
    OutOfMemory,
}

impl ZstdError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::DecompressedSizeTooLarge => "zstd decompressed payload is too large",
            Self::Decode => "zstd decompression failed",
            Self::Encode => "zstd compression failed",
            Self::OutOfMemory => "out of memory",
        }
    }
}

pub struct ZstdStreamEncoder {
    context: CCtx<'static>,
}

impl ZstdStreamEncoder {
    pub fn new(level: i32) -> Result<Self, ZstdError> {
        let mut context = CCtx::try_create().ok_or(ZstdError::OutOfMemory)?;
        context
            .set_parameter(CParameter::CompressionLevel(level))
            .map_err(|_| ZstdError::Encode)?;
        Ok(Self { context })
    }

    pub fn compress_chunk(&mut self, input: &[u8]) -> Result<Vec<u8>, ZstdError> {
        let mut input = InBuffer::around(input);
        let mut output = Vec::new();
        let mut buffer = [0u8; READ_BUFFER_BYTES];

        loop {
            let previous_input_pos = input.pos();
            let (remaining, written) = {
                let mut out_buffer = OutBuffer::around(&mut buffer[..]);
                let remaining = self
                    .context
                    .compress_stream2(&mut out_buffer, &mut input, ZSTD_EndDirective::ZSTD_e_flush)
                    .map_err(|_| ZstdError::Encode)?;
                (remaining, out_buffer.pos())
            };

            append_output(&mut output, &buffer[..written])?;

            if remaining == 0 {
                return Ok(output);
            }
            let made_progress = input.pos() > previous_input_pos || written > 0;
            if !made_progress {
                return Err(ZstdError::Encode);
            }
        }
    }
}

pub struct ZstdStreamDecoder {
    context: DCtx<'static>,
}

impl ZstdStreamDecoder {
    pub fn new() -> Result<Self, ZstdError> {
        let mut context = DCtx::try_create().ok_or(ZstdError::OutOfMemory)?;
        context.init().map_err(|_| ZstdError::Decode)?;
        Ok(Self { context })
    }

    pub fn decompress_chunk(&mut self, input: &[u8]) -> Result<Vec<u8>, ZstdError> {
        if input.is_empty() {
            return Ok(Vec::new());
        }

        let mut input = InBuffer::around(input);
        let mut output = Vec::new();
        let mut buffer = [0u8; READ_BUFFER_BYTES];

        loop {
            let previous_input_pos = input.pos();
            let (remaining, written) = {
                let mut out_buffer = OutBuffer::around(&mut buffer[..]);
                let remaining = self
                    .context
                    .decompress_stream(&mut out_buffer, &mut input)
                    .map_err(|_| ZstdError::Decode)?;
                (remaining, out_buffer.pos())
            };

            append_output(&mut output, &buffer[..written])?;
            let consumed = input.pos() > previous_input_pos;
            if input.pos() == input.src.len() && written < buffer.len() {
                return Ok(output);
            }
            if !consumed && written == 0 {
                if remaining > 0 && input.pos() == input.src.len() {
                    return Ok(output);
                }
                return Err(ZstdError::Decode);
            }
        }
    }
}

fn append_output(output: &mut Vec<u8>, bytes: &[u8]) -> Result<(), ZstdError> {
    if bytes.is_empty() {
        return Ok(());
    }
    let next_len = output
        .len()
        .checked_add(bytes.len())
        .ok_or(ZstdError::DecompressedSizeTooLarge)?;
    if next_len > MAX_DECOMPRESSED_BYTES {
        return Err(ZstdError::DecompressedSizeTooLarge);
    }
    output
        .try_reserve(bytes.len())
        .map_err(|_| ZstdError::OutOfMemory)?;
    output.extend_from_slice(bytes);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keeps_partial_frame_state_between_chunks() {
        let compressed = zstd::stream::encode_all(&b"hello stream"[..], 1).unwrap();
        let split_at = compressed.len() / 2;
        let mut decoder = ZstdStreamDecoder::new().unwrap();

        let first = decoder.decompress_chunk(&compressed[..split_at]).unwrap();
        let second = decoder.decompress_chunk(&compressed[split_at..]).unwrap();

        assert_eq!([first, second].concat(), b"hello stream");
    }

    #[test]
    fn encoder_handles_a_payload_larger_than_the_buffer() {
        let mut encoder = ZstdStreamEncoder::new(3).unwrap();
        let mut decoder = ZstdStreamDecoder::new().unwrap();
        let payload = vec![b'a'; READ_BUFFER_BYTES * 8 + 123];

        let compressed = encoder.compress_chunk(&payload).unwrap();
        assert_eq!(decoder.decompress_chunk(&compressed).unwrap(), payload);
    }

    #[test]
    fn rejects_invalid_zstd() {
        let mut decoder = ZstdStreamDecoder::new().unwrap();
        assert!(matches!(
            decoder.decompress_chunk(b"not zstd"),
            Err(ZstdError::Decode)
        ));
    }

    #[test]
    fn accepts_the_decompressed_limit() {
        let input = vec![b'x'; MAX_DECOMPRESSED_BYTES];
        let compressed = zstd::stream::encode_all(input.as_slice(), 1).unwrap();
        let mut decoder = ZstdStreamDecoder::new().unwrap();
        assert_eq!(decoder.decompress_chunk(&compressed).unwrap(), input);
    }

    #[test]
    fn rejects_frames_over_the_decompressed_limit() {
        let input = vec![b'x'; MAX_DECOMPRESSED_BYTES + 1];
        let compressed = zstd::stream::encode_all(input.as_slice(), 1).unwrap();
        let mut decoder = ZstdStreamDecoder::new().unwrap();
        assert!(matches!(
            decoder.decompress_chunk(&compressed),
            Err(ZstdError::DecompressedSizeTooLarge)
        ));
    }
}
