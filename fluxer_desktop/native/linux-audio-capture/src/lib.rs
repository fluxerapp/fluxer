#![cfg(target_os = "linux")]
#![allow(clippy::too_many_lines)]

// SPDX-License-Identifier: AGPL-3.0-or-later

mod audio_contract;
mod direct_buffer;

#[cfg(target_os = "linux")]
mod pipewire;
#[cfg(target_os = "linux")]
mod pipewire_bridge;
mod routing;
#[cfg(target_os = "linux")]
mod self_identity;

use std::sync::Arc;
use std::sync::Mutex;

use napi::Env;
use napi::Status;
use napi::bindgen_prelude::{ArrayBuffer, Error, Function, Object, Result, Unknown};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi_derive::napi;

use crate::audio_contract::{
    MAX_INVENTORY_FIELD_LENGTH, MAX_INVENTORY_FIELDS, MAX_ROUTING_RULE_KEY_LENGTH,
    MAX_ROUTING_RULE_KEYS_PER_PATTERN, MAX_ROUTING_RULE_PATTERNS, MAX_ROUTING_RULE_VALUE_LENGTH,
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

#[cfg(target_os = "linux")]
fn open_capture_backend() -> Option<(pipewire_bridge::PipeWireBridge, &'static str)> {
    if let Some(bridge) = pipewire_bridge::PipeWireBridge::open() {
        bridge.populate_self_identity(make_self_identity());
        return Some((bridge, "pipewire"));
    }
    None
}

#[cfg(target_os = "linux")]
fn open_direct_backend() -> Option<pipewire_bridge::PipeWireDirectCapture> {
    if let Some(direct) = pipewire_bridge::PipeWireDirectCapture::open() {
        direct.populate_self_identity(make_self_identity());
        return Some(direct);
    }
    None
}

#[cfg(target_os = "linux")]
fn pipewire_reachable() -> bool {
    pipewire_bridge::daemon_reachable()
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
    backend: Mutex<Option<pipewire_bridge::PipeWireBridge>>,
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

#[napi]
pub struct DirectAudioCapture {
    backend: Mutex<Option<pipewire_bridge::PipeWireDirectCapture>>,
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
