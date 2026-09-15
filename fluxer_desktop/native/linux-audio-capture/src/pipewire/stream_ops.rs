// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::mem;
use std::ops::Range;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef;
use pipewire as pw;
use pw::keys;
use pw::properties::{PropertiesBox, properties};
use pw::spa;
use spa::param::format::{MediaSubtype, MediaType};
use spa::param::format_utils;
use spa::pod::Pod;
use spa::sys as spa_sys;

use fluxer_audio_apm::{
    APM_MAX_FRAME_SAMPLES, ApmConfig, ApmError, AudioProcessor, StubAudioProcessor,
    expected_frame_samples,
};
use fluxer_rt_thread::MonotonicClock;

use crate::audio_contract::{self, DIRECT_CAPTURE_CHANNELS, DIRECT_CAPTURE_SAMPLE_RATE};
use crate::direct_buffer::DirectAudioBuffer;

use super::common::{LinkKey, MAX_FRAME_SAMPLES, OwnedLink};

pub(crate) type ScreenAudioSinkSlot = Arc<RwLock<Option<Arc<NativeScreenFrameSinkHandleRef>>>>;

pub const DIRECT_CAPTURE_APM_FRAME_SAMPLES: usize =
    (DIRECT_CAPTURE_SAMPLE_RATE as usize) / 100 * (DIRECT_CAPTURE_CHANNELS as usize);

const _: () = assert!(DIRECT_CAPTURE_APM_FRAME_SAMPLES <= APM_MAX_FRAME_SAMPLES * 2);
const _: () = assert!(MAX_FRAME_SAMPLES >= DIRECT_CAPTURE_APM_FRAME_SAMPLES);

pub struct DirectCaptureApm {
    processor: Box<dyn AudioProcessor + Send>,
    accum_f32: Box<[f32; DIRECT_CAPTURE_APM_FRAME_SAMPLES]>,
    accum_len: usize,
    scratch_i16: Box<[i16; DIRECT_CAPTURE_APM_FRAME_SAMPLES]>,
    expected_sample_rate_hz: u32,
    expected_channels: u16,
    processed_samples: u64,
    apm_frames_processed: u64,
}

impl DirectCaptureApm {
    pub fn new(sample_rate_hz: u32, channels: u16) -> Result<Self, ApmError> {
        assert!(sample_rate_hz >= 8_000);
        assert!(channels >= 1);
        let processor = StubAudioProcessor::new(ApmConfig::default(), sample_rate_hz, channels)?;
        let expected = expected_frame_samples(sample_rate_hz, channels);
        assert!(expected > 0);
        assert!(expected <= DIRECT_CAPTURE_APM_FRAME_SAMPLES);
        Ok(Self {
            processor: Box::new(processor),
            accum_f32: Box::new([0.0; DIRECT_CAPTURE_APM_FRAME_SAMPLES]),
            accum_len: 0,
            scratch_i16: Box::new([0i16; DIRECT_CAPTURE_APM_FRAME_SAMPLES]),
            expected_sample_rate_hz: sample_rate_hz,
            expected_channels: channels,
            processed_samples: 0,
            apm_frames_processed: 0,
        })
    }

    pub fn reconfigure(&mut self, sample_rate_hz: u32, channels: u16) -> Result<(), ApmError> {
        assert!(sample_rate_hz >= 8_000);
        assert!(channels >= 1);
        if self.expected_sample_rate_hz == sample_rate_hz && self.expected_channels == channels {
            return Ok(());
        }
        let processor = StubAudioProcessor::new(ApmConfig::default(), sample_rate_hz, channels)?;
        let expected = expected_frame_samples(sample_rate_hz, channels);
        if expected == 0 || expected > DIRECT_CAPTURE_APM_FRAME_SAMPLES {
            return Err(ApmError::ChannelsOutOfRange { channels });
        }
        self.processor = Box::new(processor);
        self.expected_sample_rate_hz = sample_rate_hz;
        self.expected_channels = channels;
        self.accum_len = 0;
        Ok(())
    }

    pub fn process_in_place(&mut self, samples: &mut [f32]) -> Result<usize, ApmError> {
        assert!(!samples.is_empty());
        assert!(self.expected_channels >= 1);
        let apm_frame_len =
            expected_frame_samples(self.expected_sample_rate_hz, self.expected_channels);
        assert!(apm_frame_len > 0);
        assert!(apm_frame_len <= self.scratch_i16.len());
        let mut processed_complete: usize = 0;
        let mut idx: usize = 0;
        let total = samples.len();
        while idx < total {
            let want = apm_frame_len - self.accum_len;
            let take = want.min(total - idx);
            for offset in 0..take {
                self.accum_f32[self.accum_len + offset] = samples[idx + offset];
            }
            self.accum_len += take;
            idx += take;
            if self.accum_len == apm_frame_len {
                self.run_apm_one_frame(apm_frame_len)?;
                if idx >= apm_frame_len {
                    let dst_lo = idx - apm_frame_len;
                    for offset in 0..apm_frame_len {
                        samples[dst_lo + offset] = self.accum_f32[offset];
                    }
                    processed_complete += apm_frame_len;
                } else {
                    processed_complete += take;
                }
                self.accum_len = 0;
                self.apm_frames_processed = self.apm_frames_processed.saturating_add(1);
            }
        }
        self.processed_samples = self.processed_samples.saturating_add(total as u64);
        Ok(processed_complete)
    }

    fn run_apm_one_frame(&mut self, apm_frame_len: usize) -> Result<(), ApmError> {
        assert!(apm_frame_len <= self.scratch_i16.len());
        assert!(apm_frame_len <= self.accum_f32.len());
        for offset in 0..apm_frame_len {
            self.scratch_i16[offset] = f32_sample_to_i16(self.accum_f32[offset]);
        }
        let result = self.processor.process_capture_frame(
            &mut self.scratch_i16[..apm_frame_len],
            self.expected_sample_rate_hz,
            self.expected_channels,
        );
        result?;
        for offset in 0..apm_frame_len {
            self.accum_f32[offset] = i16_sample_to_f32(self.scratch_i16[offset]);
        }
        Ok(())
    }

    pub fn apm_frames_processed(&self) -> u64 {
        self.apm_frames_processed
    }

    pub fn processed_samples(&self) -> u64 {
        self.processed_samples
    }

    pub fn pending_accumulator_len(&self) -> usize {
        self.accum_len
    }
}

pub(crate) fn f32_sample_to_i16(value: f32) -> i16 {
    let scaled = value * (i16::MAX as f32);
    if scaled >= (i16::MAX as f32) {
        return i16::MAX;
    }
    if scaled <= (i16::MIN as f32) {
        return i16::MIN;
    }
    scaled as i16
}

pub(crate) fn i16_sample_to_f32(value: i16) -> f32 {
    (value as f32) / (i16::MAX as f32)
}

pub(crate) struct DirectUserData {
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) format: spa::param::audio::AudioInfoRaw,
    pub(crate) apm: Mutex<DirectCaptureApm>,
    pub(crate) scratch: Mutex<Box<[f32; MAX_FRAME_SAMPLES]>>,
    pub(crate) last_push_ns: Arc<AtomicU64>,
    pub(crate) clock: Arc<dyn MonotonicClock>,
    pub(crate) screen_audio_sink: ScreenAudioSinkSlot,
}

pub(crate) struct DirectStreamRuntime {
    pub(crate) active_stream: std::rc::Rc<std::cell::RefCell<Option<pw::stream::StreamRc>>>,
    pub(crate) active_listener:
        std::rc::Rc<std::cell::RefCell<Option<pw::stream::StreamListener<DirectUserData>>>>,
    pub(crate) owned_links: std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    pub(crate) owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    pub(crate) sink_proxy: std::rc::Rc<std::cell::RefCell<Option<pw::node::Node>>>,
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) running: Arc<AtomicBool>,
    pub(crate) sink_node_name: String,
    pub(crate) stream_node_name: String,
    pub(crate) last_push_ns: Arc<AtomicU64>,
    pub(crate) clock: Arc<dyn MonotonicClock>,
    pub(crate) screen_audio_sink: ScreenAudioSinkSlot,
}

pub(crate) fn direct_chunk_payload_range(
    raw_len: usize,
    offset: usize,
    size: usize,
) -> Option<Range<usize>> {
    if size == 0 || offset >= raw_len {
        return None;
    }
    let end = offset.checked_add(size)?.min(raw_len);
    let available = end.checked_sub(offset)?;
    let aligned = available - (available % mem::size_of::<f32>());
    if aligned == 0 {
        return None;
    }
    Some(offset..offset + aligned)
}

pub(crate) fn build_direct_audio_info() -> spa::param::audio::AudioInfoRaw {
    let mut audio_info = spa::param::audio::AudioInfoRaw::new();
    audio_info.set_format(spa::param::audio::AudioFormat::F32LE);
    audio_info.set_rate(DIRECT_CAPTURE_SAMPLE_RATE);
    audio_info.set_channels(DIRECT_CAPTURE_CHANNELS);
    let mut position = [0; spa::param::audio::MAX_CHANNELS];
    position[0] = spa_sys::SPA_AUDIO_CHANNEL_FL;
    position[1] = spa_sys::SPA_AUDIO_CHANNEL_FR;
    audio_info.set_position(position);
    audio_info
}

pub(crate) fn build_direct_stream_props(
    target_sink_name: &str,
    stream_node_name: &str,
) -> PropertiesBox {
    properties! {
        *keys::NODE_NAME => stream_node_name,
        *keys::MEDIA_TYPE => "Audio",
        *keys::MEDIA_CATEGORY => "Capture",
        *keys::MEDIA_ROLE => "Music",
        "media.class" => "Stream/Input/Audio",
        *keys::STREAM_CAPTURE_SINK => "true",
        "node.latency" => audio_contract::direct_capture_latency_fraction(),
        "node.passive" => "true",
        "node.virtual" => "true",
        "node.hidden" => "true",
        "node.dont-fallback" => "true",
        "node.dont-move" => "true",
        "node.dont-reconnect" => "true",
        "stream.dont-remix" => "true",
        "audio.rate" => DIRECT_CAPTURE_SAMPLE_RATE.to_string(),
        "audio.channels" => DIRECT_CAPTURE_CHANNELS.to_string(),
        "audio.position" => "[FL,FR]",
        "target.object" => target_sink_name,
    }
}

fn handle_param_changed(user_data: &mut DirectUserData, id: u32, param: Option<&Pod>) {
    let Some(param) = param else { return };
    if id != spa::param::ParamType::Format.as_raw() {
        return;
    }
    let Ok((media_type, media_subtype)) = format_utils::parse_format(param) else {
        return;
    };
    if media_type != MediaType::Audio || media_subtype != MediaSubtype::Raw {
        return;
    }
    if user_data.format.parse(param).is_err() {
        return;
    }
    let rate = user_data.format.rate();
    let channels = user_data.format.channels();
    if let Ok(mut guard) = user_data.samples.lock() {
        guard.set_format(rate, channels);
    }
    if let Ok(mut apm_guard) = user_data.apm.lock() {
        let channels_u16 = channels.min(u16::MAX as u32) as u16;
        let _ = apm_guard.reconfigure(rate, channels_u16.max(1));
    }
}

fn decode_f32_into_scratch(raw_payload: &[u8], byte_offset: usize, scratch: &mut [f32]) -> usize {
    let available = raw_payload.len().saturating_sub(byte_offset);
    let sample_count = available / mem::size_of::<f32>();
    let take = sample_count.min(scratch.len());
    let mut written = 0usize;
    let mut iter = raw_payload[byte_offset..].chunks_exact(mem::size_of::<f32>());
    for slot in scratch.iter_mut().take(take) {
        let Some(chunk) = iter.next() else {
            break;
        };
        *slot = f32::from_ne_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        written += 1;
    }
    written
}

pub(crate) fn process_audio_chunk(user_data: &mut DirectUserData, payload: &[u8]) {
    let Ok(mut scratch_guard) = user_data.scratch.lock() else {
        return;
    };
    let channels = user_data.format.channels().max(1);
    if channels == 0 {
        return;
    }
    let now_ns = user_data.clock.now_ns();
    let mut byte_offset = 0usize;
    loop {
        let written = decode_f32_into_scratch(payload, byte_offset, &mut scratch_guard[..]);
        if written == 0 {
            break;
        }
        byte_offset += written * mem::size_of::<f32>();
        let aligned = audio_contract::whole_frame_sample_count(written, channels);
        if aligned == 0 {
            break;
        }
        if let Ok(mut apm_guard) = user_data.apm.lock() {
            let _ = apm_guard.process_in_place(&mut scratch_guard[..aligned]);
        }
        if now_ns > 0 {
            user_data.last_push_ns.store(now_ns, Ordering::Release);
        }
        if let Ok(guard) = user_data.screen_audio_sink.read()
            && let Some(sink) = guard.as_ref()
        {
            let frames = aligned as u32 / channels;
            if frames > 0 {
                sink.enqueue_screen_audio_f32(
                    &scratch_guard[..aligned],
                    frames,
                    channels,
                    user_data.format.rate(),
                    (now_ns / 1_000) as i64,
                );
            }
        } else if let Ok(mut samples_guard) = user_data.samples.lock() {
            let now_us = (now_ns / 1_000) as i64;
            samples_guard.push(&scratch_guard[..aligned], now_us);
        }
    }
}

fn process_stream_buffer(stream: &pw::stream::Stream, user_data: &mut DirectUserData) {
    let Some(mut buffer) = stream.dequeue_buffer() else {
        return;
    };
    let datas = buffer.datas_mut();
    if datas.is_empty() {
        return;
    }
    let data = &mut datas[0];
    let chunk = data.chunk();
    let n_bytes = chunk.size() as usize;
    let offset = chunk.offset() as usize;
    let Some(raw) = data.data() else { return };
    let Some(payload) = direct_chunk_payload_range(raw.len(), offset, n_bytes) else {
        return;
    };
    process_audio_chunk(user_data, &raw[payload]);
}

pub(crate) struct BuildDirectStreamArgs<'a> {
    pub(crate) core: &'a pw::core::CoreRc,
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) target_sink_name: &'a str,
    pub(crate) stream_node_name: &'a str,
    pub(crate) last_push_ns: Arc<AtomicU64>,
    pub(crate) clock: Arc<dyn MonotonicClock>,
    pub(crate) screen_audio_sink: ScreenAudioSinkSlot,
}

pub(crate) fn build_direct_stream(
    args: BuildDirectStreamArgs<'_>,
) -> Result<
    (
        pw::stream::StreamRc,
        pw::stream::StreamListener<DirectUserData>,
    ),
    pw::Error,
> {
    let props = build_direct_stream_props(args.target_sink_name, args.stream_node_name);
    let apm = DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
        .map_err(|_| pw::Error::CreationFailed)?;
    let data = DirectUserData {
        samples: args.samples,
        format: spa::param::audio::AudioInfoRaw::new(),
        apm: Mutex::new(apm),
        scratch: Mutex::new(Box::new([0.0_f32; MAX_FRAME_SAMPLES])),
        last_push_ns: args.last_push_ns,
        clock: args.clock,
        screen_audio_sink: args.screen_audio_sink,
    };
    let stream = pw::stream::StreamRc::new(args.core.clone(), "fluxer-direct-capture", props)?;
    let listener = stream
        .add_local_listener_with_user_data(data)
        .param_changed(|_, user_data, id, param| {
            handle_param_changed(user_data, id, param);
        })
        .process(|stream, user_data| {
            process_stream_buffer(stream, user_data);
        })
        .register()?;

    let audio_info = build_direct_audio_info();
    let obj = spa::pod::Object {
        type_: spa::utils::SpaTypes::ObjectParamFormat.as_raw(),
        id: spa::param::ParamType::EnumFormat.as_raw(),
        properties: audio_info.into(),
    };
    let values: Vec<u8> = spa::pod::serialize::PodSerializer::serialize(
        std::io::Cursor::new(Vec::new()),
        &spa::pod::Value::Object(obj),
    )
    .map_err(|_| pw::Error::CreationFailed)?
    .0
    .into_inner();
    let mut params = [Pod::from_bytes(&values).ok_or(pw::Error::CreationFailed)?];

    stream.connect(
        spa::utils::Direction::Input,
        None,
        pw::stream::StreamFlags::AUTOCONNECT
            | pw::stream::StreamFlags::MAP_BUFFERS
            | pw::stream::StreamFlags::RT_PROCESS,
        &mut params,
    )?;
    Ok((stream, listener))
}

#[cfg(test)]
pub(crate) fn build_test_user_data(
    last_push_ns: Arc<AtomicU64>,
    clock: Arc<dyn MonotonicClock>,
) -> DirectUserData {
    let apm = DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
        .expect("apm");
    let mut format = spa::param::audio::AudioInfoRaw::new();
    format.set_rate(DIRECT_CAPTURE_SAMPLE_RATE);
    format.set_channels(DIRECT_CAPTURE_CHANNELS);
    DirectUserData {
        samples: Arc::new(Mutex::new(DirectAudioBuffer::default_format())),
        format,
        apm: Mutex::new(apm),
        scratch: Mutex::new(Box::new([0.0_f32; MAX_FRAME_SAMPLES])),
        last_push_ns,
        clock,
        screen_audio_sink: Arc::new(RwLock::new(None)),
    }
}
