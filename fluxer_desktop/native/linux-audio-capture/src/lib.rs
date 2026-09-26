#![allow(clippy::too_many_lines)]

// SPDX-License-Identifier: AGPL-3.0-or-later

mod audio_contract;
mod audio_mix_runtime;
mod backend;
mod direct_buffer;
mod ignore_audio_runtime;

#[doc(hidden)]
pub mod audio_mix_runtime_bench_helpers {
    pub use crate::audio_mix_runtime::{
        AudioMixRuntime, AudioMixRuntimeBuilder, CaptureSource, CapturedMixOutputSink,
        MIX_CHANNELS, MIX_SAMPLE_RATE_HZ, MIX_TICK_PERIOD_NS, MixOutputFrame, MixOutputSink,
        MixRuntimeError, NullMixOutputSink, SOURCE_RING_CAP_FRAMES,
    };
}

#[doc(hidden)]
pub mod ignore_audio_bench_helpers {
    pub use crate::ignore_audio_runtime::{
        AUDIO_BUFFERING_MAX_TICKS, IgnoreAudioDecision, IgnoreAudioEvaluation, IgnoreAudioMetrics,
        IgnoreAudioPolicy, IgnoreAudioResetReason, IgnoreAudioSourceResetEvent,
        IgnoreAudioSourceState, IgnoreAudioTick, SOURCE_RESET_AFTER_BUFFERED_TICKS,
        SOURCE_STALE_AFTER_NS,
    };
}
#[cfg(target_os = "linux")]
mod pipewire;
#[cfg(target_os = "linux")]
mod pipewire_bridge;
mod routing;
#[cfg(target_os = "linux")]
mod self_identity;

use std::ptr;
use std::sync::Arc;
use std::sync::Mutex;

use fluxer_screen_frame_bus::{NativeScreenFrameSinkHandle, NativeScreenFrameSinkHandleRef};
use napi::Env;
use napi::JsValue;
use napi::Status;
use napi::bindgen_prelude::{
    Array, ArrayBuffer, Error, Function, Object, Result, Unknown, ValueType,
};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi_derive::napi;

use crate::audio_contract::{
    MAX_INVENTORY_FIELD_LENGTH, MAX_INVENTORY_FIELDS, MAX_ROUTING_RULE_KEY_LENGTH,
    MAX_ROUTING_RULE_KEYS_PER_PATTERN, MAX_ROUTING_RULE_PATTERNS, MAX_ROUTING_RULE_VALUE_LENGTH,
};
use crate::backend::{
    CaptureBridge as CaptureBridgeTrait, DirectCapture as DirectCaptureTrait, RoutingGraphSnapshot,
};
use crate::routing::{PropMap, PropPattern, RoutingRule, SelfIdentity};

type LifecycleTsfn =
    Arc<ThreadsafeFunction<(String, String), (), (String, String), Status, false, false, 8>>;

#[cfg(target_os = "linux")]
fn make_self_identity() -> SelfIdentity {
    let mut id = SelfIdentity::default();
    self_identity::populate_self_identity(&mut id);
    id
}

#[cfg(not(target_os = "linux"))]
#[allow(dead_code)]
fn make_self_identity() -> SelfIdentity {
    SelfIdentity::default()
}

#[cfg(target_os = "linux")]
fn open_capture_backend() -> Option<(Box<dyn CaptureBridgeTrait>, &'static str)> {
    if let Some(bridge) = pipewire_bridge::PipeWireBridge::open() {
        bridge.populate_self_identity(make_self_identity());
        return Some((Box::new(bridge), "pipewire"));
    }
    None
}

#[cfg(not(target_os = "linux"))]
fn open_capture_backend() -> Option<(Box<dyn CaptureBridgeTrait>, &'static str)> {
    None
}

#[cfg(target_os = "linux")]
fn open_direct_backend() -> Option<Box<dyn DirectCaptureTrait>> {
    if let Some(direct) = pipewire_bridge::PipeWireDirectCapture::open() {
        direct.populate_self_identity(make_self_identity());
        return Some(Box::new(direct));
    }
    None
}

#[cfg(not(target_os = "linux"))]
fn open_direct_backend() -> Option<Box<dyn DirectCaptureTrait>> {
    None
}

#[cfg(target_os = "linux")]
fn pipewire_reachable() -> bool {
    pipewire_bridge::daemon_reachable()
}

#[cfg(not(target_os = "linux"))]
fn pipewire_reachable() -> bool {
    false
}

#[napi(js_name = "pipeWireAvailable")]
pub fn pipe_wire_available() -> bool {
    pipewire_reachable()
}

#[napi(js_name = "audioBackend")]
pub fn audio_backend() -> &'static str {
    if pipewire_reachable() {
        "pipewire"
    } else {
        "none"
    }
}

#[napi]
pub struct AudioBridge {
    backend: Mutex<Option<Box<dyn CaptureBridgeTrait>>>,
    name: &'static str,
}

#[napi]
impl AudioBridge {
    #[napi(constructor)]
    pub fn new() -> Self {
        match open_capture_backend() {
            Some((backend, name)) => Self {
                backend: Mutex::new(Some(backend)),
                name,
            },
            None => Self {
                backend: Mutex::new(None),
                name: "none",
            },
        }
    }

    #[napi]
    pub fn inventory(&self, fields: Option<Vec<String>>) -> Result<Vec<PropMapWire>> {
        let fields = match fields {
            Some(values) => validate_inventory_fields(values)?,
            None => Vec::new(),
        };
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("AudioBridge backend poisoned"))?;
        let snapshot = guard.as_ref().map(|b| b.inventory()).unwrap_or_default();
        Ok(snapshot
            .into_iter()
            .map(|entry| project_inventory_entry(entry, &fields))
            .collect())
    }

    #[napi]
    pub fn apply(&self, rule: Object) -> Result<bool> {
        let parsed = parse_routing_rule(&rule)?;
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("AudioBridge backend poisoned"))?;
        Ok(guard.as_ref().is_some_and(|b| b.apply(parsed)))
    }

    #[napi]
    pub fn release(&self) -> Result<()> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("AudioBridge backend poisoned"))?;
        if let Some(b) = guard.as_ref() {
            b.release();
        }
        Ok(())
    }

    #[napi(js_name = "routingGraph")]
    pub fn routing_graph(&self) -> Result<RoutingGraphWire> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("AudioBridge backend poisoned"))?;
        let graph = guard
            .as_ref()
            .map(|b| b.routing_graph())
            .unwrap_or_default();
        Ok(RoutingGraphWire(graph))
    }

    #[napi]
    pub fn backend(&self) -> &'static str {
        self.name
    }
}

impl Default for AudioBridge {
    fn default() -> Self {
        Self::new()
    }
}

fn retain_screen_audio_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(generic_error(
            "DirectAudioCapture.setScreenAudioSink expects a native external sink handle",
        ));
    }
    let raw_value = value.value();
    let mut data: *mut std::ffi::c_void = ptr::null_mut();
    let status =
        unsafe { napi::sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != napi::sys::Status::napi_ok || data.is_null() {
        return Err(generic_error(
            "DirectAudioCapture.setScreenAudioSink received an empty native external sink handle",
        ));
    }
    let handle = unsafe { data.cast::<NativeScreenFrameSinkHandle>().as_ref() }
        .and_then(NativeScreenFrameSinkHandle::retain_ref)
        .ok_or_else(|| {
            generic_error("DirectAudioCapture.setScreenAudioSink received an invalid handle")
        })?;
    Ok(Arc::new(handle))
}

#[napi]
pub struct DirectAudioCapture {
    backend: Mutex<Option<Box<dyn DirectCaptureTrait>>>,
    lifecycle_tsfn: Mutex<Option<LifecycleTsfn>>,
}

#[napi]
impl DirectAudioCapture {
    #[napi(constructor)]
    pub fn new() -> Self {
        Self {
            backend: Mutex::new(open_direct_backend()),
            lifecycle_tsfn: Mutex::new(None),
        }
    }

    #[napi(js_name = "setLifecycleCallback")]
    pub fn set_lifecycle_callback(&self, callback: Function<(String, String), ()>) -> Result<()> {
        let tsfn: LifecycleTsfn = Arc::new(
            callback
                .build_threadsafe_function::<(String, String)>()
                .max_queue_size::<8>()
                .build_callback(|ctx| Ok(ctx.value))?,
        );
        let mut guard = self
            .lifecycle_tsfn
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture lifecycle poisoned"))?;
        *guard = Some(tsfn);
        Ok(())
    }

    #[napi]
    pub fn start(&self, rule: Object) -> Result<bool> {
        let parsed = parse_routing_rule(&rule)?;
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        Ok(guard.as_ref().is_some_and(|b| b.start(parsed)))
    }

    #[napi]
    pub fn set_rule(&self, rule: Object) -> Result<bool> {
        let parsed = parse_routing_rule(&rule)?;
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        Ok(guard.as_ref().is_some_and(|b| b.set_rule(parsed)))
    }

    #[napi]
    pub fn read<'env>(&self, env: &'env Env) -> Result<Option<NativeAudioFrame<'env>>> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        let Some(backend) = guard.as_ref() else {
            return Ok(None);
        };
        let Some(frame) = backend.read() else {
            return Ok(None);
        };
        let arraybuffer = audio_samples_to_arraybuffer(env, &frame.samples)?;
        Ok(Some(NativeAudioFrame {
            samples: arraybuffer,
            sample_rate: frame.sample_rate,
            channels: frame.channels,
            timestamp_us: frame.timestamp_us.max(0) as f64,
        }))
    }

    #[napi(js_name = "setScreenAudioSink")]
    pub fn set_screen_audio_sink(&self, sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_screen_audio_sink_handle(sink_handle)?;
        if !sink.supports_screen_audio() {
            return Err(generic_error(
                "DirectAudioCapture.setScreenAudioSink handle does not support screen audio",
            ));
        }
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        if let Some(b) = guard.as_ref() {
            b.set_screen_audio_sink(sink);
        }
        Ok(())
    }

    #[napi(js_name = "clearScreenAudioSink")]
    pub fn clear_screen_audio_sink(&self) -> Result<()> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        if let Some(b) = guard.as_ref() {
            b.clear_screen_audio_sink();
        }
        Ok(())
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        if let Some(b) = guard.as_ref() {
            b.stop();
        }
        drop(guard);
        self.emit_lifecycle("closed-clean", "direct audio capture stopped");
        Ok(())
    }

    #[napi(js_name = "routingGraph")]
    pub fn routing_graph(&self) -> Result<RoutingGraphWire> {
        let guard = self
            .backend
            .lock()
            .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
        let graph = guard
            .as_ref()
            .map(|b| b.routing_graph())
            .unwrap_or_default();
        Ok(RoutingGraphWire(graph))
    }
}

impl Default for DirectAudioCapture {
    fn default() -> Self {
        Self::new()
    }
}

impl DirectAudioCapture {
    fn emit_lifecycle(&self, kind: &str, message: &str) {
        let tsfn = self
            .lifecycle_tsfn
            .lock()
            .ok()
            .and_then(|guard| guard.as_ref().cloned());
        let Some(tsfn) = tsfn else {
            return;
        };
        let _: Status = tsfn.call(
            (kind.to_string(), message.to_string()),
            ThreadsafeFunctionCallMode::NonBlocking,
        );
    }
}

#[napi]
pub struct AudioMixRuntimeHandle {
    inner: Mutex<Option<crate::audio_mix_runtime::AudioMixRuntime>>,
    source_count: u32,
    mark_pushed_total: Arc<std::sync::atomic::AtomicU64>,
}

#[napi]
impl AudioMixRuntimeHandle {
    #[napi(constructor)]
    pub fn new(source_count: u32) -> Result<Self> {
        Self::build(source_count, None)
    }

    #[napi(factory, js_name = "boundToDirectCapture")]
    pub fn bound_to_direct_capture(direct: &DirectAudioCapture) -> Result<Self> {
        let arc = direct_capture_freshness(direct)?;
        Self::build(1, Some(arc))
    }

    fn build(
        source_count: u32,
        bound_freshness: Option<Arc<std::sync::atomic::AtomicU64>>,
    ) -> Result<Self> {
        if source_count == 0 {
            return Err(invalid_arg(
                "AudioMixRuntimeHandle requires at least 1 source",
            ));
        }
        if source_count as usize > fluxer_audio_mix::MAX_MIX_SOURCES {
            return Err(invalid_arg("AudioMixRuntimeHandle exceeds MAX_MIX_SOURCES"));
        }
        let clock: Arc<dyn fluxer_rt_thread::MonotonicClock> =
            Arc::new(fluxer_rt_thread::SystemMonotonicClock::new());
        let mut builder =
            crate::audio_mix_runtime::AudioMixRuntimeBuilder::new().with_clock(Arc::clone(&clock));
        for index in 0..source_count {
            let source_id = (index as u64) + 1;
            let (_source, consumer) = crate::audio_mix_runtime::CaptureSource::create(
                source_id,
                crate::audio_mix_runtime::MIX_SAMPLE_RATE_HZ,
                crate::audio_mix_runtime::MIX_CHANNELS,
            )
            .map_err(|_| generic_error("CaptureSource::create failed"))?;
            let freshness = if index == 0 {
                match &bound_freshness {
                    Some(arc) => Arc::clone(arc),
                    None => Arc::new(std::sync::atomic::AtomicU64::new(u64::MAX)),
                }
            } else {
                Arc::new(std::sync::atomic::AtomicU64::new(u64::MAX))
            };
            builder = builder.add_source_with_freshness(source_id, consumer, freshness);
        }
        let runtime = builder
            .build(crate::audio_mix_runtime::NullMixOutputSink)
            .map_err(|_| generic_error("AudioMixRuntimeBuilder::build failed"))?;
        let mark_pushed_total = runtime.mark_pushed_total_arc();
        Ok(Self {
            inner: Mutex::new(Some(runtime)),
            source_count,
            mark_pushed_total,
        })
    }

    #[napi(js_name = "sourceCount")]
    pub fn source_count_js(&self) -> u32 {
        assert!(self.source_count > 0);
        assert!(self.source_count as usize <= fluxer_audio_mix::MAX_MIX_SOURCES);
        self.source_count
    }

    #[napi]
    pub fn tick(&self, tick_at_ns: Option<i64>) -> Result<u32> {
        use fluxer_rt_thread::MonotonicClock as _;
        assert!(self.source_count > 0);
        let mut guard = self
            .inner
            .lock()
            .map_err(|_| generic_error("AudioMixRuntimeHandle poisoned"))?;
        let runtime = guard
            .as_mut()
            .ok_or_else(|| generic_error("AudioMixRuntimeHandle disposed"))?;
        let at_ns: u64 = match tick_at_ns {
            Some(v) if v > 0 => v as u64,
            _ => fluxer_rt_thread::SystemMonotonicClock::new().now_ns(),
        };
        assert!(at_ns > 0);
        let marked = runtime
            .observe_source_pushes_without_mix(at_ns)
            .map_err(|_| generic_error("AudioMixRuntime tick failed"))?;
        Ok(marked.min(u32::MAX as u64) as u32)
    }

    #[napi(js_name = "markPushedTotal")]
    pub fn mark_pushed_total_js(&self) -> u32 {
        assert!(self.source_count > 0);
        let value = self
            .mark_pushed_total
            .load(std::sync::atomic::Ordering::Acquire);
        let clamped = value.min(u32::MAX as u64);
        assert!(clamped <= u32::MAX as u64);
        clamped as u32
    }

    #[napi]
    pub fn dispose(&self) -> Result<()> {
        assert!(self.source_count > 0);
        assert!(self.source_count as usize <= fluxer_audio_mix::MAX_MIX_SOURCES);
        let mut guard = self
            .inner
            .lock()
            .map_err(|_| generic_error("AudioMixRuntimeHandle poisoned"))?;
        guard.take();
        Ok(())
    }
}

fn direct_capture_freshness(
    direct: &DirectAudioCapture,
) -> Result<Arc<std::sync::atomic::AtomicU64>> {
    let guard = direct
        .backend
        .lock()
        .map_err(|_| generic_error("DirectAudioCapture backend poisoned"))?;
    let backend = guard
        .as_ref()
        .ok_or_else(|| generic_error("DirectAudioCapture backend unavailable"))?;
    backend
        .last_push_ns_arc()
        .ok_or_else(|| generic_error("DirectAudioCapture backend lacks freshness atomic"))
}

#[napi(object)]
pub struct NativeAudioFrame<'env> {
    pub samples: ArrayBuffer<'env>,
    #[napi(js_name = "sampleRate")]
    pub sample_rate: u32,
    pub channels: u32,
    #[napi(js_name = "timestampUs")]
    pub timestamp_us: f64,
}

pub struct PropMapWire(pub PropMap);

impl napi::bindgen_prelude::ToNapiValue for PropMapWire {
    unsafe fn to_napi_value(
        raw_env: napi::sys::napi_env,
        value: Self,
    ) -> Result<napi::sys::napi_value> {
        let env = napi::Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        for (key, val) in value.0 {
            object.set(&key, val)?;
        }
        unsafe {
            <Object<'_> as napi::bindgen_prelude::ToNapiValue>::to_napi_value(raw_env, object)
        }
    }
}

pub struct RoutingGraphWire(pub RoutingGraphSnapshot);

impl napi::bindgen_prelude::ToNapiValue for RoutingGraphWire {
    unsafe fn to_napi_value(
        raw_env: napi::sys::napi_env,
        value: Self,
    ) -> Result<napi::sys::napi_value> {
        let env = napi::Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        object.set("backend", value.0.backend)?;
        object.set("nodes", routing_graph_nodes_to_array(&env, value.0.nodes)?)?;
        object.set("ports", routing_graph_ports_to_array(&env, value.0.ports)?)?;
        object.set(
            "ownedLinks",
            routing_graph_links_to_array(&env, value.0.owned_links)?,
        )?;
        unsafe {
            <Object<'_> as napi::bindgen_prelude::ToNapiValue>::to_napi_value(raw_env, object)
        }
    }
}

fn prop_map_to_object<'env>(env: &'env Env, props: PropMap) -> Result<Object<'env>> {
    let mut object = Object::new(env)?;
    for (key, value) in props {
        object.set(&key, value)?;
    }
    Ok(object)
}

fn routing_graph_nodes_to_array<'env>(
    env: &'env Env,
    nodes: Vec<crate::backend::RoutingGraphNode>,
) -> Result<Array<'env>> {
    let mut array = env.create_array(nodes.len() as u32)?;
    for (index, node) in nodes.into_iter().enumerate() {
        let mut object = Object::new(env)?;
        object.set("id", node.id)?;
        object.set("props", prop_map_to_object(env, node.props)?)?;
        array.set(index as u32, object)?;
    }
    Ok(array)
}

fn routing_graph_ports_to_array<'env>(
    env: &'env Env,
    ports: Vec<crate::backend::RoutingGraphPort>,
) -> Result<Array<'env>> {
    let mut array = env.create_array(ports.len() as u32)?;
    for (index, port) in ports.into_iter().enumerate() {
        let mut object = Object::new(env)?;
        object.set("id", port.id)?;
        object.set("nodeId", port.node_id)?;
        object.set("direction", port.direction)?;
        object.set("channel", port.channel)?;
        object.set("props", prop_map_to_object(env, port.props)?)?;
        array.set(index as u32, object)?;
    }
    Ok(array)
}

fn routing_graph_links_to_array<'env>(
    env: &'env Env,
    links: Vec<crate::backend::RoutingGraphLink>,
) -> Result<Array<'env>> {
    let mut array = env.create_array(links.len() as u32)?;
    for (index, link) in links.into_iter().enumerate() {
        let mut object = Object::new(env)?;
        object.set("outputNodeId", link.output_node_id)?;
        object.set("outputPortId", link.output_port_id)?;
        object.set("inputNodeId", link.input_node_id)?;
        object.set("inputPortId", link.input_port_id)?;
        object.set("owned", true)?;
        object.set("passive", true)?;
        array.set(index as u32, object)?;
    }
    Ok(array)
}

fn project_inventory_entry(mut entry: PropMap, fields: &[String]) -> PropMapWire {
    if fields.is_empty() {
        return PropMapWire(entry);
    }
    let mut filtered = PropMap::with_capacity(fields.len());
    for field in fields {
        if let Some(value) = entry.remove(field) {
            filtered.insert(field.clone(), value);
        }
    }
    PropMapWire(filtered)
}

fn audio_samples_to_arraybuffer<'env>(
    env: &'env Env,
    samples: &[f32],
) -> Result<ArrayBuffer<'env>> {
    let bytes: Vec<u8> = samples
        .iter()
        .flat_map(|sample| sample.to_le_bytes())
        .collect();
    ArrayBuffer::from_data(env, bytes)
}

fn validate_inventory_fields(values: Vec<String>) -> Result<Vec<String>> {
    if values.len() as u32 > MAX_INVENTORY_FIELDS {
        return Err(invalid_arg("too many inventory fields"));
    }
    for value in &values {
        if value.len() > MAX_INVENTORY_FIELD_LENGTH {
            return Err(invalid_arg("inventory field exceeds length cap"));
        }
    }
    Ok(values)
}

fn parse_routing_rule(value: &Object) -> Result<RoutingRule> {
    Ok(RoutingRule {
        include_when: parse_pattern_list(value, "include")?,
        never_when: parse_pattern_list(value, "exclude")?,
        pin_target_for: parse_pattern_list(value, "workaround")?,
        skip_hardware_devices: read_optional_bool(value, "ignoreDevices")?
            .or(read_optional_bool(value, "ignore_devices")?)
            .unwrap_or(false),
        only_audio_sinks: read_optional_bool(value, "onlySpeakers")?
            .or(read_optional_bool(value, "only_speakers")?)
            .unwrap_or(false),
        only_default_audio_sink: read_optional_bool(value, "onlyDefaultSpeakers")?
            .or(read_optional_bool(value, "only_default_speakers")?)
            .unwrap_or(false),
    })
}

fn parse_pattern_list(value: &Object, name: &str) -> Result<Vec<PropPattern>> {
    let Some(raw) = read_optional_unknown(value, name)? else {
        return Ok(Vec::new());
    };
    if matches!(
        raw.get_type()?,
        napi::ValueType::Null | napi::ValueType::Undefined
    ) {
        return Ok(Vec::new());
    }
    let array = unsafe { raw.cast::<napi::bindgen_prelude::Array>() }
        .map_err(|_| invalid_arg(format!("{name} must be an array of objects")))?;
    let len = array.len();
    if len > MAX_ROUTING_RULE_PATTERNS {
        return Err(invalid_arg(format!("{name} exceeds pattern cap")));
    }
    let mut out = Vec::with_capacity(len as usize);
    for index in 0..len {
        let entry = array
            .get::<Object>(index)
            .map_err(|_| invalid_arg(format!("{name}[{index}] must be an object")))?
            .ok_or_else(|| invalid_arg(format!("{name}[{index}] must be an object")))?;
        out.push(object_to_prop_map(&entry)?);
    }
    Ok(out)
}

fn object_to_prop_map(object: &Object) -> Result<PropMap> {
    let keys = Object::keys(object)?;
    if keys.len() as u32 > MAX_ROUTING_RULE_KEYS_PER_PATTERN {
        return Err(invalid_arg("routing pattern has too many keys"));
    }
    let mut out = PropMap::with_capacity(keys.len());
    for key in keys {
        if key.is_empty() || key.len() > MAX_ROUTING_RULE_KEY_LENGTH {
            return Err(invalid_arg("routing pattern key is empty or too long"));
        }
        let raw = read_optional_unknown(object, &key)?
            .ok_or_else(|| invalid_arg("routing pattern value missing"))?;
        if raw.get_type()? != napi::ValueType::String {
            return Err(invalid_arg("routing pattern value must be a string"));
        }
        let value: String = unsafe { raw.cast() }?;
        if value.len() > MAX_ROUTING_RULE_VALUE_LENGTH {
            return Err(invalid_arg("routing pattern value too long"));
        }
        out.insert(key, value);
    }
    Ok(out)
}

fn read_optional_unknown<'a>(object: &Object<'a>, name: &str) -> Result<Option<Unknown<'a>>> {
    object.get::<Unknown>(name)
}

fn read_optional_bool(object: &Object, name: &str) -> Result<Option<bool>> {
    let Some(raw) = read_optional_unknown(object, name)? else {
        return Ok(None);
    };
    match raw.get_type()? {
        napi::ValueType::Null | napi::ValueType::Undefined => Ok(None),
        napi::ValueType::Boolean => Ok(Some(unsafe { raw.cast() }?)),
        _ => Err(invalid_arg(format!("{name} must be a boolean"))),
    }
}

fn generic_error(reason: impl Into<String>) -> Error {
    Error::new(Status::GenericFailure, reason.into())
}

fn invalid_arg(reason: impl Into<String>) -> Error {
    Error::new(Status::InvalidArg, reason.into())
}

#[allow(dead_code)]
fn _keep_arc_in_scope(_: Arc<()>) {}

#[cfg(all(test, target_os = "linux"))]
mod js_path_tests {
    use super::AudioMixRuntimeHandle;
    use crate::pipewire::stream_ops::{
        DIRECT_CAPTURE_APM_FRAME_SAMPLES, build_test_user_data, process_audio_chunk,
    };
    use fluxer_rt_thread::MonotonicClock;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};

    #[derive(Debug)]
    struct FakeClock {
        value_ns: AtomicU64,
    }

    impl FakeClock {
        fn new(initial_ns: u64) -> Self {
            assert!(initial_ns > 0);
            Self {
                value_ns: AtomicU64::new(initial_ns),
            }
        }
    }

    impl MonotonicClock for FakeClock {
        fn now_ns(&self) -> u64 {
            self.value_ns.load(Ordering::Acquire)
        }
    }

    fn make_f32_payload(samples: &[f32]) -> Vec<u8> {
        assert!(!samples.is_empty());
        let mut out = Vec::with_capacity(samples.len() * 4);
        for sample in samples {
            out.extend_from_slice(&sample.to_ne_bytes());
        }
        assert_eq!(out.len(), samples.len() * 4);
        out
    }

    fn build_handle_with_shared_freshness(last_push_ns: Arc<AtomicU64>) -> AudioMixRuntimeHandle {
        assert!(Arc::strong_count(&last_push_ns) >= 1);
        let handle = AudioMixRuntimeHandle::build(1, Some(last_push_ns))
            .expect("AudioMixRuntimeHandle build via JS path");
        assert_eq!(handle.source_count_js(), 1);
        handle
    }

    #[test]
    fn js_runtime_tick_consumes_freshness_pushed_by_production_callback() {
        let clock: Arc<dyn MonotonicClock> = Arc::new(FakeClock::new(11_000_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut user_data = build_test_user_data(Arc::clone(&last_push_ns), Arc::clone(&clock));
        let handle = build_handle_with_shared_freshness(Arc::clone(&last_push_ns));
        assert_eq!(handle.mark_pushed_total_js(), 0);
        assert_eq!(last_push_ns.load(Ordering::Acquire), u64::MAX);
        let frame: Vec<f32> = (0..DIRECT_CAPTURE_APM_FRAME_SAMPLES)
            .map(|n| (n as f32) * 0.0001)
            .collect();
        let payload = make_f32_payload(&frame);
        process_audio_chunk(&mut user_data, &payload);
        let pushed_after_callback = last_push_ns.load(Ordering::Acquire);
        assert_ne!(pushed_after_callback, u64::MAX);
        assert_eq!(pushed_after_callback, 11_000_000);
        let marked = handle
            .tick(Some(pushed_after_callback as i64))
            .expect("AudioMixRuntimeHandle::tick observes freshness");
        assert_eq!(marked, 1);
        let total = handle.mark_pushed_total_js();
        assert!(
            total >= 1,
            "AudioMixRuntimeHandle::tick did not advance mark_pushed_total ({total})",
        );
        handle.dispose().expect("dispose");
    }

    #[test]
    fn js_runtime_tick_idempotent_for_unchanged_freshness_atomic() {
        let clock: Arc<dyn MonotonicClock> = Arc::new(FakeClock::new(22_000_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut user_data = build_test_user_data(Arc::clone(&last_push_ns), Arc::clone(&clock));
        let handle = build_handle_with_shared_freshness(Arc::clone(&last_push_ns));
        let frame: Vec<f32> = (0..DIRECT_CAPTURE_APM_FRAME_SAMPLES)
            .map(|n| (n as f32) * 0.0002)
            .collect();
        let payload = make_f32_payload(&frame);
        process_audio_chunk(&mut user_data, &payload);
        let observed = last_push_ns.load(Ordering::Acquire);
        let _ = handle.tick(Some(observed as i64)).expect("first tick");
        let after_first = handle.mark_pushed_total_js();
        assert!(after_first >= 1);
        let _ = handle.tick(Some(observed as i64 + 1)).expect("second tick");
        let after_second = handle.mark_pushed_total_js();
        assert_eq!(
            after_first, after_second,
            "second tick must not advance mark_pushed_total when freshness atomic is unchanged",
        );
        handle.dispose().expect("dispose");
    }
}
