// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::mem;
use std::ops::Range;
use std::sync::{Arc, Mutex};

use pipewire as pw;
use pw::keys;
use pw::properties::{PropertiesBox, properties};
use pw::spa;
use spa::param::format::{MediaSubtype, MediaType};
use spa::param::format_utils;
use spa::pod::Pod;
use spa::sys as spa_sys;

use fluxer_rt_thread::MonotonicClock;

use crate::audio_contract::{self, DIRECT_CAPTURE_CHANNELS, DIRECT_CAPTURE_SAMPLE_RATE};
use crate::direct_buffer::DirectAudioBuffer;

use super::common::{LinkKey, MAX_FRAME_SAMPLES, OwnedLink};

pub(crate) struct DirectUserData {
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) format: spa::param::audio::AudioInfoRaw,
    pub(crate) scratch: Mutex<Box<[f32; MAX_FRAME_SAMPLES]>>,
    pub(crate) clock: Arc<dyn MonotonicClock>,
}

pub(crate) struct DirectStreamRuntime {
    pub(crate) active_stream: std::rc::Rc<std::cell::RefCell<Option<pw::stream::StreamRc>>>,
    pub(crate) active_listener:
        std::rc::Rc<std::cell::RefCell<Option<pw::stream::StreamListener<DirectUserData>>>>,
    pub(crate) owned_links: std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    pub(crate) owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    pub(crate) sink_proxy: std::rc::Rc<std::cell::RefCell<Option<pw::node::Node>>>,
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) sink_node_name: String,
    pub(crate) stream_node_name: String,
    pub(crate) clock: Arc<dyn MonotonicClock>,
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
}

fn decode_f32_into_scratch(raw_payload: &[u8], byte_offset: usize, scratch: &mut [f32]) -> usize {
    let available = raw_payload.len().saturating_sub(byte_offset);
    let sample_count = available / mem::size_of::<f32>();
    let take = sample_count.min(scratch.len());
    let mut written = 0usize;
    let mut iter = raw_payload[byte_offset..]
        .as_chunks::<{ mem::size_of::<f32>() }>()
        .0
        .iter();
    for slot in scratch.iter_mut().take(take) {
        let Some(chunk) = iter.next() else {
            break;
        };
        *slot = f32::from_ne_bytes(*chunk);
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
        if let Ok(mut samples_guard) = user_data.samples.lock() {
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
    pub(crate) clock: Arc<dyn MonotonicClock>,
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
    let data = DirectUserData {
        samples: args.samples,
        format: spa::param::audio::AudioInfoRaw::new(),
        scratch: Mutex::new(Box::new([0.0_f32; MAX_FRAME_SAMPLES])),
        clock: args.clock,
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
