// SPDX-License-Identifier: AGPL-3.0-or-later

use guild_rt::{ResourceKind, Sink};
use zstd_safe::zstd_sys::ZSTD_EndDirective;
use zstd_safe::{CCtx, CParameter, DCtx, DParameter, InBuffer, OutBuffer, WriteBuf};

pub const MIN_WINDOW_LOG: u32 = 10;
pub const MAX_WINDOW_LOG: u32 = 31;
pub const MAX_TABLE_LOG: u32 = 15;
pub const MIN_BLOCK_SIZE: u32 = 1 << 10;
pub const MAX_BLOCK_SIZE: u32 = 1 << 17;
const OUTPUT_STEP: usize = 64 * 1024;
const FIRST_DECOMPRESS_CAPACITY: usize = 4 * 1024;

const SINK: Sink = Sink(ResourceKind::ZstdContext);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StreamError {
    BadParameter(&'static str),
    OutOfMemory,
    Zstd(&'static str),
    TooLarge,
    Stuck,
}

impl StreamError {
    pub fn reason(&self) -> &'static str {
        match self {
            StreamError::BadParameter(name) => name,
            StreamError::OutOfMemory => "out_of_memory",
            StreamError::Zstd(name) => name,
            StreamError::TooLarge => "too_large",
            StreamError::Stuck => "stuck",
        }
    }
}

fn zstd_error(code: usize) -> StreamError {
    StreamError::Zstd(zstd_safe::get_error_name(code))
}

pub trait Output {
    type Buffer: WriteBuf + ?Sized;

    fn buffer(&mut self) -> &mut Self::Buffer;

    fn capacity(&self) -> usize;

    fn reserve(&mut self, total: usize) -> Result<(), StreamError>;
}

impl Output for Vec<u8> {
    type Buffer = Vec<u8>;

    fn buffer(&mut self) -> &mut Vec<u8> {
        self
    }

    fn capacity(&self) -> usize {
        Vec::capacity(self)
    }

    fn reserve(&mut self, total: usize) -> Result<(), StreamError> {
        match total.checked_sub(self.len()) {
            Some(additional) if total > Vec::capacity(self) => self
                .try_reserve_exact(additional)
                .map_err(|_| StreamError::OutOfMemory),
            _ => Ok(()),
        }
    }
}

fn set(ctx: &mut CCtx<'static>, parameter: CParameter) -> Result<(), StreamError> {
    ctx.set_parameter(parameter).map(|_| ()).map_err(zstd_error)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CompressorConfig {
    pub level: i32,
    pub window_log: u32,
    pub max_block_size: u32,
}

impl CompressorConfig {
    pub fn validate(self) -> Result<CompressorConfig, StreamError> {
        if self.level < zstd_safe::min_c_level() || self.level > zstd_safe::max_c_level() {
            return Err(StreamError::BadParameter("level"));
        }
        if !(MIN_WINDOW_LOG..=MAX_WINDOW_LOG).contains(&self.window_log) {
            return Err(StreamError::BadParameter("window_log"));
        }
        if self.max_block_size != 0
            && !(MIN_BLOCK_SIZE..=MAX_BLOCK_SIZE).contains(&self.max_block_size)
        {
            return Err(StreamError::BadParameter("max_block_size"));
        }
        Ok(self)
    }

    fn table_log(self) -> u32 {
        MAX_TABLE_LOG.min(self.window_log.saturating_sub(1))
    }
}

pub fn output_bound(chunks: &[&[u8]]) -> usize {
    let total = chunks.iter().map(|chunk| chunk.len()).sum::<usize>();
    zstd_safe::compress_bound(total).saturating_add(64)
}

pub struct Compressor {
    ctx: CCtx<'static>,
    reported: u64,
}

impl Compressor {
    pub fn new(config: CompressorConfig) -> Result<Compressor, StreamError> {
        let config = config.validate()?;
        let mut ctx = CCtx::try_create().ok_or(StreamError::OutOfMemory)?;
        set(&mut ctx, CParameter::CompressionLevel(config.level))?;
        set(&mut ctx, CParameter::WindowLog(config.window_log))?;
        set(&mut ctx, CParameter::HashLog(config.table_log()))?;
        set(&mut ctx, CParameter::ChainLog(config.table_log()))?;
        if config.max_block_size != 0 {
            set(&mut ctx, CParameter::MaxBlockSize(config.max_block_size))?;
        }
        let mut compressor = Compressor { ctx, reported: 0 };
        SINK.opened();
        compressor.report();
        Ok(compressor)
    }

    fn report(&mut self) {
        let now = self.ctx.sizeof() as u64;
        if now != self.reported {
            SINK.bytes_moved(self.reported, now);
            self.reported = now;
        }
    }

    pub fn context_bytes(&self) -> usize {
        self.ctx.sizeof()
    }

    pub fn compress(&mut self, data: &[u8]) -> Result<Vec<u8>, StreamError> {
        let mut out = Vec::new();
        self.compress_into(&[data], &mut out)?;
        Ok(out)
    }

    pub fn compress_into<O: Output + ?Sized>(
        &mut self,
        chunks: &[&[u8]],
        out: &mut O,
    ) -> Result<usize, StreamError> {
        out.reserve(output_bound(chunks))?;
        let mut written = 0;
        let (last, rest) = match chunks.split_last() {
            Some((last, rest)) => (*last, rest),
            None => (&[][..], chunks),
        };
        let result = rest
            .iter()
            .try_for_each(|chunk| self.step(chunk, Directive::Continue, out, &mut written))
            .and_then(|()| self.step(last, Directive::Flush, out, &mut written));
        self.report();
        result.map(|()| written)
    }

    fn step<O: Output + ?Sized>(
        &mut self,
        data: &[u8],
        directive: Directive,
        out: &mut O,
        written: &mut usize,
    ) -> Result<(), StreamError> {
        let mut input = InBuffer::around(data);
        loop {
            if *written == out.capacity() {
                out.reserve(written.saturating_add(OUTPUT_STEP))?;
            }
            let read_before = input.pos();
            let written_before = *written;
            let remaining = {
                let mut output = OutBuffer::around_pos(out.buffer(), written_before);
                let remaining = self
                    .ctx
                    .compress_stream2(&mut output, &mut input, directive.end());
                *written = output.pos();
                remaining.map_err(zstd_error)?
            };
            let consumed = input.pos() == data.len();
            match directive {
                Directive::Continue if consumed => return Ok(()),
                Directive::Flush if consumed && remaining == 0 => return Ok(()),
                _ if input.pos() == read_before && *written == written_before => {
                    return Err(StreamError::Stuck);
                }
                _ => {}
            }
        }
    }
}

#[derive(Clone, Copy)]
enum Directive {
    Continue,
    Flush,
}

impl Directive {
    fn end(self) -> ZSTD_EndDirective {
        match self {
            Directive::Continue => ZSTD_EndDirective::ZSTD_e_continue,
            Directive::Flush => ZSTD_EndDirective::ZSTD_e_flush,
        }
    }
}

impl Drop for Compressor {
    fn drop(&mut self) {
        SINK.sub_bytes(self.reported);
        SINK.closed();
    }
}

pub struct Decompressor {
    ctx: DCtx<'static>,
    reported: u64,
}

impl Decompressor {
    pub fn new(window_log_max: u32) -> Result<Decompressor, StreamError> {
        if !(MIN_WINDOW_LOG..=MAX_WINDOW_LOG).contains(&window_log_max) {
            return Err(StreamError::BadParameter("window_log_max"));
        }
        let mut ctx = DCtx::try_create().ok_or(StreamError::OutOfMemory)?;
        ctx.set_parameter(DParameter::WindowLogMax(window_log_max))
            .map_err(zstd_error)?;
        let mut decompressor = Decompressor { ctx, reported: 0 };
        SINK.opened();
        decompressor.report();
        Ok(decompressor)
    }

    fn report(&mut self) {
        let now = self.ctx.sizeof() as u64;
        if now != self.reported {
            SINK.bytes_moved(self.reported, now);
            self.reported = now;
        }
    }

    pub fn decompress(&mut self, data: &[u8], max_size: usize) -> Result<Vec<u8>, StreamError> {
        let mut out = Vec::new();
        self.decompress_into(data, max_size, &mut out)?;
        Ok(out)
    }

    pub fn decompress_into<O: Output + ?Sized>(
        &mut self,
        data: &[u8],
        max_size: usize,
        out: &mut O,
    ) -> Result<usize, StreamError> {
        let limit = max_size.saturating_add(1);
        let first = data
            .len()
            .saturating_mul(4)
            .max(FIRST_DECOMPRESS_CAPACITY)
            .min(limit);
        out.reserve(first)?;
        let mut written = 0;
        let mut input = InBuffer::around(data);
        let result = loop {
            let capacity = out.capacity();
            if written == capacity {
                if capacity >= limit {
                    break Err(StreamError::TooLarge);
                }
                if let Err(error) = out.reserve(capacity + capacity.min(limit - capacity)) {
                    break Err(error);
                }
            }
            let read_before = input.pos();
            let written_before = written;
            let step = {
                let mut output = OutBuffer::around_pos(out.buffer(), written_before);
                let step = self.ctx.decompress_stream(&mut output, &mut input);
                written = output.pos();
                step
            };
            if let Err(code) = step {
                break Err(zstd_error(code));
            }
            if written > max_size {
                break Err(StreamError::TooLarge);
            }
            let consumed = input.pos() == data.len();
            let progressed = input.pos() != read_before || written != written_before;
            if consumed && (written < out.capacity() || !progressed) {
                break Ok(());
            }
            if !progressed {
                break Err(StreamError::Stuck);
            }
        };
        self.report();
        result.map(|()| written)
    }
}

impl Drop for Decompressor {
    fn drop(&mut self) {
        SINK.sub_bytes(self.reported);
        SINK.closed();
    }
}

#[cfg(test)]
mod tests {
    use super::{Compressor, CompressorConfig, Decompressor, StreamError};

    fn config(max_block_size: u32) -> CompressorConfig {
        CompressorConfig {
            level: 3,
            window_log: 17,
            max_block_size,
        }
    }

    #[test]
    fn round_trips_across_calls() {
        let mut compressor = Compressor::new(config(16_384)).unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        for n in [0usize, 1, 1_000, 200_000] {
            let payload: Vec<u8> = (0..n).map(|i| (i % 251) as u8).collect();
            let frame = compressor.compress(&payload).unwrap();
            let back = decompressor.decompress(&frame, 10 * 1024 * 1024).unwrap();
            assert_eq!(back, payload);
        }
    }

    #[test]
    fn refuses_output_above_the_limit() {
        let mut compressor = Compressor::new(config(0)).unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        let frame = compressor.compress(&vec![b'a'; 5_000]).unwrap();
        assert_eq!(
            decompressor.decompress(&frame, 4_999),
            Err(StreamError::TooLarge)
        );
    }

    #[test]
    fn exact_limit_passes() {
        let mut compressor = Compressor::new(config(0)).unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        let frame = compressor.compress(&vec![b'a'; 5_000]).unwrap();
        assert_eq!(decompressor.decompress(&frame, 5_000).unwrap().len(), 5_000);
    }

    #[test]
    fn small_window_stream_decodes_across_buffer_wraps() {
        let mut compressor = Compressor::new(CompressorConfig {
            level: 3,
            window_log: 14,
            max_block_size: 16_384,
        })
        .unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        for n in 0..200usize {
            let payload: Vec<u8> = (0..(n * 97) % 40_000)
                .map(|i| ((i * 31 + n) % 199) as u8)
                .collect();
            let frame = compressor.compress(&payload).unwrap();
            let back = decompressor.decompress(&frame, 10 * 1024 * 1024).unwrap();
            assert_eq!(back, payload);
        }
    }

    #[test]
    fn chunked_input_matches_flat_input() {
        let mut flat = Compressor::new(config(16_384)).unwrap();
        let mut chunked = Compressor::new(config(16_384)).unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        for n in [0usize, 3, 1_000, 70_000] {
            let payload: Vec<u8> = (0..n).map(|i| ((i * 7) % 251) as u8).collect();
            let third = n / 3;
            let chunks = [
                &payload[..third],
                &[][..],
                &payload[third..2 * third],
                &payload[2 * third..],
            ];
            let mut out = Vec::new();
            let size = chunked.compress_into(&chunks, &mut out).unwrap();
            assert_eq!(size, out.len());
            assert_eq!(out, flat.compress(&payload).unwrap());
            let back = decompressor.decompress(&out, 10 * 1024 * 1024).unwrap();
            assert_eq!(back, payload);
        }
    }

    #[test]
    fn output_grows_past_the_first_reservation() {
        let mut compressor = Compressor::new(config(0)).unwrap();
        let mut decompressor = Decompressor::new(21).unwrap();
        let payload = vec![b'z'; 300_000];
        let frame = compressor.compress(&payload).unwrap();
        let mut out = Vec::new();
        let size = decompressor
            .decompress_into(&frame, payload.len(), &mut out)
            .unwrap();
        assert_eq!(size, payload.len());
        assert_eq!(out, payload);
    }

    #[test]
    fn rejects_bad_parameters() {
        assert!(Compressor::new(config(10)).is_err());
        assert!(Decompressor::new(40).is_err());
    }
}
