// SPDX-License-Identifier: AGPL-3.0-or-later

use zstd::zstd_safe::zstd_sys::ZSTD_EndDirective;
use zstd::zstd_safe::{CCtx, CParameter, DCtx, InBuffer, OutBuffer};

const READ_BUFFER_BYTES: usize = 16 * 1024;
const MAX_DECOMPRESSED_BYTES: usize = 64 * 1024 * 1024;
const ENCODER_WINDOW_LOG: u32 = 17;
const ENCODER_TABLE_LOG: u32 = 15;

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
        for parameter in [
            CParameter::CompressionLevel(level),
            CParameter::WindowLog(ENCODER_WINDOW_LOG),
            CParameter::HashLog(ENCODER_TABLE_LOG),
            CParameter::ChainLog(ENCODER_TABLE_LOG),
        ] {
            context
                .set_parameter(parameter)
                .map_err(|_| ZstdError::Encode)?;
        }
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
