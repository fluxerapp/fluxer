// SPDX-License-Identifier: AGPL-3.0-or-later

use core::mem::size_of;

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct AudioStreamBasicDescription {
    pub m_sample_rate: f64,
    pub m_format_id: u32,
    pub m_format_flags: u32,
    pub m_bytes_per_packet: u32,
    pub m_frames_per_packet: u32,
    pub m_bytes_per_frame: u32,
    pub m_channels_per_frame: u32,
    pub m_bits_per_channel: u32,
    pub m_reserved: u32,
}

pub const K_AUDIO_FORMAT_LINEAR_PCM: u32 =
    ((b'l' as u32) << 24) | ((b'p' as u32) << 16) | ((b'c' as u32) << 8) | (b'm' as u32);

pub const K_LINEAR_PCM_FLAG_IS_FLOAT: u32 = 1 << 0;
pub const K_LINEAR_PCM_FLAG_IS_BIG_ENDIAN: u32 = 1 << 1;
pub const K_LINEAR_PCM_FLAG_IS_SIGNED_INTEGER: u32 = 1 << 2;
pub const K_LINEAR_PCM_FLAG_IS_PACKED: u32 = 1 << 3;
pub const K_LINEAR_PCM_FLAG_IS_NON_INTERLEAVED: u32 = 1 << 5;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct AudioBuffer {
    pub m_number_channels: u32,
    pub m_data_byte_size: u32,
    pub m_data: *mut core::ffi::c_void,
}

#[repr(C)]
pub struct AudioBufferList {
    pub m_number_buffers: u32,

    pub buffers: [AudioBuffer; 1],
}

pub fn output_frame_capacity(input_frames: u32, in_rate: f64, out_rate: f64) -> u32 {
    if in_rate <= 0.0 {
        return input_frames;
    }
    let ratio = out_rate / in_rate;
    let f = (input_frames as f64) * ratio;
    (f.ceil() + 1.0) as u32
}

pub fn converted_frame_count(input_frames: u32, in_rate: f64, out_rate: f64) -> u32 {
    if input_frames == 0 {
        return 0;
    }
    if in_rate <= 0.0 || out_rate <= 0.0 || in_rate == out_rate {
        return input_frames;
    }
    let ratio = out_rate / in_rate;
    ((input_frames as f64) * ratio).ceil() as u32
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PcmConvertError {
    UnsupportedFormat,
    UnsupportedBitDepth,
    MissingData,
    OutputTooSmall,
}

pub fn is_linear_pcm(asbd: &AudioStreamBasicDescription) -> bool {
    asbd.m_format_id == K_AUDIO_FORMAT_LINEAR_PCM
}
pub fn is_float(asbd: &AudioStreamBasicDescription) -> bool {
    (asbd.m_format_flags & K_LINEAR_PCM_FLAG_IS_FLOAT) != 0
}
pub fn is_signed_integer(asbd: &AudioStreamBasicDescription) -> bool {
    (asbd.m_format_flags & K_LINEAR_PCM_FLAG_IS_SIGNED_INTEGER) != 0
}
pub fn is_big_endian(asbd: &AudioStreamBasicDescription) -> bool {
    (asbd.m_format_flags & K_LINEAR_PCM_FLAG_IS_BIG_ENDIAN) != 0
}
pub fn is_non_interleaved(asbd: &AudioStreamBasicDescription) -> bool {
    (asbd.m_format_flags & K_LINEAR_PCM_FLAG_IS_NON_INTERLEAVED) != 0
}
pub fn is_packed(asbd: &AudioStreamBasicDescription) -> bool {
    (asbd.m_format_flags & K_LINEAR_PCM_FLAG_IS_PACKED) != 0
}

pub fn is_native_f32_interleaved(asbd: &AudioStreamBasicDescription) -> bool {
    is_linear_pcm(asbd)
        && is_float(asbd)
        && is_packed(asbd)
        && asbd.m_bits_per_channel == 32
        && !is_big_endian(asbd)
        && !is_non_interleaved(asbd)
}

pub fn is_native_f32_planar(asbd: &AudioStreamBasicDescription) -> bool {
    is_linear_pcm(asbd)
        && is_float(asbd)
        && is_packed(asbd)
        && asbd.m_bits_per_channel == 32
        && !is_big_endian(asbd)
        && is_non_interleaved(asbd)
}

fn bytes_per_sample(asbd: &AudioStreamBasicDescription) -> Result<u32, PcmConvertError> {
    if !is_linear_pcm(asbd) {
        return Err(PcmConvertError::UnsupportedFormat);
    }
    if asbd.m_bits_per_channel == 0 || asbd.m_bits_per_channel % 8 != 0 {
        return Err(PcmConvertError::UnsupportedBitDepth);
    }
    let bytes = asbd.m_bits_per_channel / 8;
    if !matches!(bytes, 1 | 2 | 3 | 4 | 8) {
        return Err(PcmConvertError::UnsupportedBitDepth);
    }
    Ok(bytes)
}

pub unsafe fn buffer_at(list: *const AudioBufferList, index: usize) -> *const AudioBuffer {
    let buffers = unsafe { core::ptr::addr_of!((*list).buffers) } as *const AudioBuffer;
    unsafe { buffers.add(index) }
}

pub unsafe fn input_frame_count_for_buffer_list(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
) -> Result<u32, PcmConvertError> {
    if !is_linear_pcm(asbd) {
        return Err(PcmConvertError::UnsupportedFormat);
    }
    let nb = unsafe { (*list).m_number_buffers };
    if nb == 0 {
        return Err(PcmConvertError::MissingData);
    }
    let first = unsafe { &*buffer_at(list, 0) };
    if first.m_data.is_null() || first.m_data_byte_size == 0 {
        return Ok(0);
    }
    let channels = if asbd.m_channels_per_frame == 0 {
        1
    } else {
        asbd.m_channels_per_frame
    };
    let bps = bytes_per_sample(asbd)?;
    let frame_stride = if asbd.m_bytes_per_frame != 0 {
        asbd.m_bytes_per_frame
    } else if is_non_interleaved(asbd) {
        bps
    } else {
        bps * channels
    };
    if frame_stride == 0 {
        return Ok(0);
    }
    Ok(first.m_data_byte_size / frame_stride)
}

fn read_unsigned(bytes: &[u8], big_endian: bool) -> u64 {
    let mut out: u64 = 0;
    if big_endian {
        for &b in bytes {
            out = (out << 8) | (b as u64);
        }
    } else {
        let mut shift = 0u32;
        for &b in bytes {
            out |= (b as u64) << shift;
            shift += 8;
        }
    }
    out
}

fn sign_extend(value: u64, bits: u32) -> i64 {
    if bits == 64 {
        return value as i64;
    }
    let shift = (64 - bits) as u32;
    ((value << shift) as i64) >> shift
}

fn pow2_float(exponent: u32) -> f64 {
    debug_assert!(exponent < 64);
    (1u64 << exponent) as f64
}

fn read_scalar_sample(
    asbd: &AudioStreamBasicDescription,
    bytes: &[u8],
) -> Result<f32, PcmConvertError> {
    let bits = asbd.m_bits_per_channel;
    if bits == 0 || bits > 64 || bits % 8 != 0 {
        return Err(PcmConvertError::UnsupportedBitDepth);
    }
    let raw = read_unsigned(bytes, is_big_endian(asbd));

    if is_float(asbd) {
        return match bits {
            32 => Ok(f32::from_bits(raw as u32)),
            64 => Ok(f64::from_bits(raw) as f32),
            _ => Err(PcmConvertError::UnsupportedBitDepth),
        };
    }

    if is_signed_integer(asbd) {
        let signed = sign_extend(raw, bits);
        let denom = pow2_float(bits - 1);
        return Ok(((signed as f64) / denom) as f32);
    }

    let midpoint = pow2_float(bits - 1);
    Ok((((raw as f64) - midpoint) / midpoint) as f32)
}

fn source_channel_for(target_channel: u32, source_channels: u32) -> u32 {
    if source_channels <= 1 {
        return 0;
    }
    target_channel.min(source_channels - 1)
}

const MAX_FAST_PATH_CHANNELS: usize = 8;

#[derive(Copy, Clone)]
enum SampleKind {
    F32,
    F64,
    Signed { bits: u32, denom: f64 },
    Unsigned { midpoint: f64 },
}

#[derive(Copy, Clone)]
struct ChannelCursor {
    base: *const u8,
    stride: usize,
}

const NULL_CURSOR: ChannelCursor = ChannelCursor {
    base: core::ptr::null(),
    stride: 0,
};

fn sample_kind_for(asbd: &AudioStreamBasicDescription) -> Result<SampleKind, PcmConvertError> {
    let bits = asbd.m_bits_per_channel;
    assert!(bits >= 8);
    assert!(bits <= 64);
    if is_float(asbd) {
        return match bits {
            32 => Ok(SampleKind::F32),
            64 => Ok(SampleKind::F64),
            _ => Err(PcmConvertError::UnsupportedBitDepth),
        };
    }
    if is_signed_integer(asbd) {
        return Ok(SampleKind::Signed {
            bits,
            denom: pow2_float(bits - 1),
        });
    }
    Ok(SampleKind::Unsigned {
        midpoint: pow2_float(bits - 1),
    })
}

unsafe fn read_sample_raw(
    cursor: ChannelCursor,
    frame_index: usize,
    bps: usize,
    big_endian: bool,
) -> u64 {
    debug_assert!(!cursor.base.is_null());
    debug_assert!(bps >= 1);
    debug_assert!(bps <= 8);
    let p = unsafe { cursor.base.add(frame_index * cursor.stride) };
    let mut raw: u64 = 0;
    if big_endian {
        for i in 0..bps {
            raw = (raw << 8) | (unsafe { *p.add(i) } as u64);
        }
    } else {
        for i in 0..bps {
            raw |= (unsafe { *p.add(i) } as u64) << (8 * i as u32);
        }
    }
    raw
}

fn sample_from_raw(kind: SampleKind, raw: u64) -> f32 {
    match kind {
        SampleKind::F32 => f32::from_bits(raw as u32),
        SampleKind::F64 => f64::from_bits(raw) as f32,
        SampleKind::Signed { bits, denom } => ((sign_extend(raw, bits) as f64) / denom) as f32,
        SampleKind::Unsigned { midpoint } => (((raw as f64) - midpoint) / midpoint) as f32,
    }
}

unsafe fn build_channel_cursors(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
    input_frames: u32,
    output_channels: u32,
    cursors: &mut [ChannelCursor; MAX_FAST_PATH_CHANNELS],
) -> Result<(), PcmConvertError> {
    assert!(input_frames > 0);
    assert!(output_channels > 0);
    assert!(output_channels as usize <= MAX_FAST_PATH_CHANNELS);
    let channels = if asbd.m_channels_per_frame == 0 {
        1
    } else {
        asbd.m_channels_per_frame
    };
    let bps = bytes_per_sample(asbd)?;
    let non_interleaved = is_non_interleaved(asbd);
    let buffer_count = unsafe { (*list).m_number_buffers };
    if buffer_count == 0 {
        return Err(PcmConvertError::MissingData);
    }
    let frame_stride = if asbd.m_bytes_per_frame != 0 {
        asbd.m_bytes_per_frame
    } else if non_interleaved {
        bps
    } else {
        bps * channels
    };
    for ch in 0..output_channels {
        let source_channel = source_channel_for(ch, channels);
        let buffer_index = if non_interleaved && buffer_count > 1 {
            source_channel.min(buffer_count - 1) as usize
        } else {
            0
        };
        let buffer = unsafe { &*buffer_at(list, buffer_index) };
        if buffer.m_data.is_null() {
            return Err(PcmConvertError::MissingData);
        }
        let channel_offset = if non_interleaved && buffer_count > 1 {
            0
        } else {
            source_channel * bps
        };
        let last_end = ((input_frames - 1) as usize) * (frame_stride as usize)
            + (channel_offset as usize)
            + (bps as usize);
        if last_end > buffer.m_data_byte_size as usize {
            return Err(PcmConvertError::MissingData);
        }
        cursors[ch as usize] = ChannelCursor {
            base: unsafe { (buffer.m_data as *const u8).add(channel_offset as usize) },
            stride: frame_stride as usize,
        };
    }
    Ok(())
}

unsafe fn convert_with_cursors(
    cursors: &[ChannelCursor; MAX_FAST_PATH_CHANNELS],
    kind: SampleKind,
    bps: usize,
    big_endian: bool,
    input_frames: u32,
    out_frames: u32,
    step: f64,
    output_channels: u32,
    output: &mut [f32],
) -> u32 {
    assert!(input_frames > 0);
    assert!(out_frames > 0);
    assert!(output_channels > 0);
    let oc = output_channels as usize;
    assert!(oc <= MAX_FAST_PATH_CHANNELS);
    assert!(output.len() >= (out_frames as usize) * oc);
    for out_frame in 0..out_frames {
        let src_pos = (out_frame as f64) * step;
        let mut base = src_pos.floor() as u32;
        if base >= input_frames {
            base = input_frames - 1;
        }
        let next = if base + 1 < input_frames {
            base + 1
        } else {
            base
        };
        let frac = (src_pos - src_pos.floor()) as f32;
        for ch in 0..oc {
            let raw_a = unsafe { read_sample_raw(cursors[ch], base as usize, bps, big_endian) };
            let raw_b = unsafe { read_sample_raw(cursors[ch], next as usize, bps, big_endian) };
            let a = sample_from_raw(kind, raw_a);
            let b = sample_from_raw(kind, raw_b);
            output[(out_frame as usize) * oc + ch] = a + (b - a) * frac;
        }
    }
    out_frames
}

unsafe fn read_frame_channel(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
    frame_index: u32,
    target_channel: u32,
) -> Result<f32, PcmConvertError> {
    let channels = if asbd.m_channels_per_frame == 0 {
        1
    } else {
        asbd.m_channels_per_frame
    };
    let source_channel = source_channel_for(target_channel, channels);
    let bps = bytes_per_sample(asbd)?;
    let non_interleaved = is_non_interleaved(asbd);
    let buffer_count = unsafe { (*list).m_number_buffers };
    if buffer_count == 0 {
        return Err(PcmConvertError::MissingData);
    }
    let buffer_index = if non_interleaved && buffer_count > 1 {
        source_channel.min(buffer_count - 1) as usize
    } else {
        0
    };
    let buffer = unsafe { &*buffer_at(list, buffer_index) };
    if buffer.m_data.is_null() {
        return Err(PcmConvertError::MissingData);
    }
    let frame_stride = if asbd.m_bytes_per_frame != 0 {
        asbd.m_bytes_per_frame
    } else if non_interleaved {
        bps
    } else {
        bps * channels
    };
    let channel_offset = if non_interleaved && buffer_count > 1 {
        0
    } else {
        source_channel * bps
    };
    let offset = (frame_index as usize) * (frame_stride as usize) + (channel_offset as usize);
    let end = offset + bps as usize;
    if end > buffer.m_data_byte_size as usize {
        return Err(PcmConvertError::MissingData);
    }
    let data = buffer.m_data as *const u8;
    let slice = unsafe { core::slice::from_raw_parts(data.add(offset), bps as usize) };
    read_scalar_sample(asbd, slice)
}

unsafe fn copy_native_f32_interleaved(
    list: *const AudioBufferList,
    input_frames: u32,
    channels: u32,
    output: &mut [f32],
) -> Result<u32, PcmConvertError> {
    debug_assert!(input_frames > 0);
    debug_assert!(channels > 0);
    let sample_count = (input_frames as usize) * (channels as usize);
    debug_assert!(output.len() >= sample_count);
    let buffer_count = unsafe { (*list).m_number_buffers };
    if buffer_count == 0 {
        return Err(PcmConvertError::MissingData);
    }
    let buffer = unsafe { &*buffer_at(list, 0) };
    if buffer.m_data.is_null() {
        return Err(PcmConvertError::MissingData);
    }
    let byte_count = sample_count * size_of::<f32>();
    if (buffer.m_data_byte_size as usize) < byte_count {
        return Err(PcmConvertError::MissingData);
    }
    unsafe {
        core::ptr::copy_nonoverlapping(
            buffer.m_data as *const u8,
            output.as_mut_ptr() as *mut u8,
            byte_count,
        );
    }
    Ok(input_frames)
}

unsafe fn copy_native_f32_planar(
    list: *const AudioBufferList,
    input_frames: u32,
    channels: u32,
    output_channels: u32,
    output: &mut [f32],
) -> Result<u32, PcmConvertError> {
    assert!(input_frames > 0);
    assert!(channels > 0);
    assert!(output_channels > 0);
    assert!(output_channels as usize <= MAX_FAST_PATH_CHANNELS);
    let sample_count = (input_frames as usize) * (output_channels as usize);
    assert!(output.len() >= sample_count);
    let buffer_count = unsafe { (*list).m_number_buffers };
    assert!(buffer_count >= channels);
    let plane_byte_count = (input_frames as usize) * size_of::<f32>();
    let mut planes = [core::ptr::null::<f32>(); MAX_FAST_PATH_CHANNELS];
    for ch in 0..output_channels {
        let plane_index = source_channel_for(ch, channels) as usize;
        let buffer = unsafe { &*buffer_at(list, plane_index) };
        if buffer.m_data.is_null() {
            return Err(PcmConvertError::MissingData);
        }
        if (buffer.m_data_byte_size as usize) < plane_byte_count {
            return Err(PcmConvertError::MissingData);
        }
        planes[ch as usize] = buffer.m_data as *const f32;
    }
    let oc = output_channels as usize;
    for frame in 0..input_frames as usize {
        for ch in 0..oc {
            output[frame * oc + ch] = unsafe { *planes[ch].add(frame) };
        }
    }
    Ok(input_frames)
}

unsafe fn convert_per_sample(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
    input_frames: u32,
    out_frames: u32,
    step: f64,
    output_channels: u32,
    output: &mut [f32],
) -> Result<u32, PcmConvertError> {
    assert!(input_frames > 0);
    assert!(out_frames > 0);
    assert!(output.len() >= (out_frames as usize) * (output_channels as usize));
    for out_frame in 0..out_frames {
        let src_pos = (out_frame as f64) * step;
        let mut base = src_pos.floor() as u32;
        if base >= input_frames {
            base = input_frames - 1;
        }
        let next = if base + 1 < input_frames {
            base + 1
        } else {
            base
        };
        let frac = (src_pos - src_pos.floor()) as f32;
        for ch in 0..output_channels {
            let a = unsafe { read_frame_channel(asbd, list, base, ch)? };
            let b = unsafe { read_frame_channel(asbd, list, next, ch)? };
            output[(out_frame as usize) * (output_channels as usize) + (ch as usize)] =
                a + (b - a) * frac;
        }
    }
    Ok(out_frames)
}

unsafe fn try_same_rate_fast_path(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
    input_frames: u32,
    channels: u32,
    output_channels: u32,
    output: &mut [f32],
) -> Option<Result<u32, PcmConvertError>> {
    assert!(input_frames > 0);
    assert!(channels > 0);
    assert!(output_channels > 0);
    if is_native_f32_interleaved(asbd) {
        let frame_stride = if asbd.m_bytes_per_frame != 0 {
            asbd.m_bytes_per_frame
        } else {
            4 * channels
        };
        if channels == output_channels && frame_stride == 4 * channels {
            return Some(unsafe {
                copy_native_f32_interleaved(list, input_frames, channels, output)
            });
        }
    }
    if is_native_f32_planar(asbd) && (output_channels as usize) <= MAX_FAST_PATH_CHANNELS {
        let buffer_count = unsafe { (*list).m_number_buffers };
        let stride_ok = asbd.m_bytes_per_frame == 0 || asbd.m_bytes_per_frame == 4;
        if buffer_count >= channels && stride_ok {
            return Some(unsafe {
                copy_native_f32_planar(list, input_frames, channels, output_channels, output)
            });
        }
    }
    None
}

pub unsafe fn convert_buffer_list_to_interleaved_f32(
    asbd: &AudioStreamBasicDescription,
    list: *const AudioBufferList,
    input_frames: u32,
    output_sample_rate: f64,
    output_channels: u32,
    output: &mut [f32],
) -> Result<u32, PcmConvertError> {
    if !is_linear_pcm(asbd) {
        return Err(PcmConvertError::UnsupportedFormat);
    }
    if output_channels == 0 {
        return Ok(0);
    }
    let out_frames = converted_frame_count(input_frames, asbd.m_sample_rate, output_sample_rate);
    let needed = (out_frames as usize) * (output_channels as usize);
    if needed > output.len() {
        return Err(PcmConvertError::OutputTooSmall);
    }
    if input_frames == 0 || out_frames == 0 {
        return Ok(0);
    }
    let channels = if asbd.m_channels_per_frame == 0 {
        1
    } else {
        asbd.m_channels_per_frame
    };
    if asbd.m_sample_rate == output_sample_rate {
        let fast = unsafe {
            try_same_rate_fast_path(
                asbd,
                list,
                input_frames,
                channels,
                output_channels,
                &mut *output,
            )
        };
        if let Some(result) = fast {
            return result;
        }
    }
    let step = if asbd.m_sample_rate > 0.0 && output_sample_rate > 0.0 {
        asbd.m_sample_rate / output_sample_rate
    } else {
        1.0
    };
    if (output_channels as usize) <= MAX_FAST_PATH_CHANNELS {
        let bps = bytes_per_sample(asbd)?;
        let kind = sample_kind_for(asbd)?;
        let mut cursors = [NULL_CURSOR; MAX_FAST_PATH_CHANNELS];
        unsafe { build_channel_cursors(asbd, list, input_frames, output_channels, &mut cursors)? };
        let converted = unsafe {
            convert_with_cursors(
                &cursors,
                kind,
                bps as usize,
                is_big_endian(asbd),
                input_frames,
                out_frames,
                step,
                output_channels,
                output,
            )
        };
        return Ok(converted);
    }
    unsafe {
        convert_per_sample(
            asbd,
            list,
            input_frames,
            out_frames,
            step,
            output_channels,
            output,
        )
    }
}

const _: () = {
    assert!(size_of::<AudioStreamBasicDescription>() == 40);
    assert!(size_of::<AudioBuffer>() == 16);
};
