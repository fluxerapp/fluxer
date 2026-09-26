#![allow(non_snake_case)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use napi::{
    Env, Status,
    bindgen_prelude::{Function, Object, Result, ToNapiValue},
    sys,
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;
use x11rb::connection::{Connection, RequestConnection};
use x11rb::errors::ReplyError;
use x11rb::protocol::record::{
    self, ConnectionExt as RecordConnectionExt, ExtRange, Range, Range8, Range16,
};
use x11rb::protocol::xproto::{
    ConnectionExt as XprotoConnectionExt, GetKeyboardMappingReply, Keycode,
};
use x11rb::rust_connection::RustConnection;
use x11rb::wrapper::ConnectionExt as WrapperConnectionExt;
use x11rb::x11_utils::TryParse;

use crate::env::{DisplayServer, detect_display_server};
use crate::keymap;
use crate::modifiers::{self, Modifiers};
use crate::mouse::{self, MouseClassification};
use crate::x11;

const RECORD_FROM_SERVER: u8 = 0;
const RECORD_START_OF_DATA: u8 = 4;

const KEY_PRESS: u8 = 2;
const KEY_RELEASE: u8 = 3;
const BUTTON_PRESS: u8 = 4;
const BUTTON_RELEASE: u8 = 5;
const MOTION_NOTIFY: u8 = 6;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct XKeyButtonProto {
    type_: u8,
    detail: u8,
    seq_l: u8,
    seq_h: u8,
    time: u32,
    root: u32,
    event: u32,
    child: u32,
    root_x: i16,
    root_y: i16,
    event_x: i16,
    event_y: i16,
    state: u16,
    same_screen: u8,
    pad0: u8,
}

#[derive(Debug, Clone)]
pub enum EventKind {
    KeyDown,
    KeyUp,
    MouseDown,
    MouseUp,
    MouseMove,
    Wheel,
}

#[derive(Debug, Clone)]
pub struct DecodedEvent {
    pub kind: EventKind,
    pub keycode: u32,
    pub key_name: String,
    pub button: u8,
    pub delta_x: i32,
    pub delta_y: i32,
    pub x: i32,
    pub y: i32,
    pub has_xy: bool,
    pub mods: Modifiers,
}

impl DecodedEvent {
    fn new(kind: EventKind, mods: Modifiers) -> Self {
        Self {
            kind,
            keycode: 0,
            key_name: String::new(),
            button: 0,
            delta_x: 0,
            delta_y: 0,
            x: 0,
            y: 0,
            has_xy: false,
            mods,
        }
    }
}

impl ToNapiValue for DecodedEvent {
    unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        let kind = match event.kind {
            EventKind::KeyDown => "keydown",
            EventKind::KeyUp => "keyup",
            EventKind::MouseDown => "mousedown",
            EventKind::MouseUp => "mouseup",
            EventKind::MouseMove => "mousemove",
            EventKind::Wheel => "wheel",
        };
        object.set("type", kind)?;
        object.set("ctrlKey", event.mods.ctrl)?;
        object.set("altKey", event.mods.alt)?;
        object.set("shiftKey", event.mods.shift)?;
        object.set("metaKey", event.mods.meta)?;
        match event.kind {
            EventKind::KeyDown | EventKind::KeyUp => {
                object.set("keycode", event.keycode)?;
                object.set("keyName", event.key_name.as_str())?;
            }
            EventKind::MouseDown | EventKind::MouseUp => {
                object.set("button", u32::from(event.button))?;
                if event.has_xy {
                    object.set("x", event.x)?;
                    object.set("y", event.y)?;
                }
            }
            EventKind::MouseMove => {
                object.set("x", event.x)?;
                object.set("y", event.y)?;
            }
            EventKind::Wheel => {
                object.set("deltaX", event.delta_x)?;
                object.set("deltaY", event.delta_y)?;
                if event.has_xy {
                    object.set("x", event.x)?;
                    object.set("y", event.y)?;
                }
            }
        }
        unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, object) }
    }
}

const EVENT_QUEUE_LIMIT: usize = 1024;

type EventTsfn = Arc<
    ThreadsafeFunction<
        DecodedEvent,
        UnknownReturnValue,
        DecodedEvent,
        Status,
        false,
        true,
        EVENT_QUEUE_LIMIT,
    >,
>;

#[derive(Clone)]
struct KeysymCache {
    min_keycode: Keycode,
    syms: Vec<u32>,
}

impl KeysymCache {
    fn build(reply: &GetKeyboardMappingReply, min_keycode: Keycode) -> Self {
        let per = reply.keysyms_per_keycode as usize;
        let count = reply.keysyms.len().checked_div(per).unwrap_or(0);
        let mut syms = Vec::with_capacity(count);
        if per > 0 {
            for i in 0..count {
                syms.push(reply.keysyms[i * per]);
            }
        }
        Self { min_keycode, syms }
    }

    fn lookup(&self, keycode: u8) -> u32 {
        if keycode < self.min_keycode {
            return 0;
        }
        let idx = (keycode - self.min_keycode) as usize;
        self.syms.get(idx).copied().unwrap_or(0)
    }
}

struct Active {
    ctrl_conn: Arc<RustConnection>,
    record_ctx: record::Context,
    worker: Option<JoinHandle<()>>,
    stop: Arc<AtomicBool>,
}

struct Inner {
    callback: EventTsfn,
    active: Mutex<Option<Active>>,
}

#[napi]
pub struct InputHook {
    inner: Arc<Inner>,
}

#[napi]
impl InputHook {
    #[napi(constructor)]
    pub fn new(callback: Function<DecodedEvent, UnknownReturnValue>) -> Result<Self> {
        let tsfn = callback
            .build_threadsafe_function::<DecodedEvent>()
            .weak::<true>()
            .callee_handled::<false>()
            .max_queue_size::<EVENT_QUEUE_LIMIT>()
            .build()
            .map_err(|err| generic_error(format!("failed to create TSFN: {}", err.reason)))?;
        Ok(Self {
            inner: Arc::new(Inner {
                callback: Arc::new(tsfn),
                active: Mutex::new(None),
            }),
        })
    }

    #[napi]
    pub fn start(&self) -> Result<()> {
        let mut guard = self
            .inner
            .active
            .lock()
            .map_err(|_| generic_error("InputHook lock poisoned"))?;
        if guard.is_some() {
            return Ok(());
        }
        let active = start_record(self.inner.callback.clone())?;
        *guard = Some(active);
        Ok(())
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        let active = {
            let mut guard = self
                .inner
                .active
                .lock()
                .map_err(|_| generic_error("InputHook lock poisoned"))?;
            guard.take()
        };
        if let Some(active) = active {
            tear_down(active);
        }
        Ok(())
    }
}

impl Drop for InputHook {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.inner.active.lock()
            && let Some(active) = guard.take()
        {
            tear_down(active);
        }
    }
}

fn start_record(callback: EventTsfn) -> Result<Active> {
    if !detect_display_server().supports_global_xrecord() {
        return Err(generic_error(
            "InputHook.start failed: WaylandUnsupported — global input \
             capture is blocked by the Wayland security model. Use \
             @fluxer/linux-evdev for kernel-level capture when the user has \
             input device access."
                .to_string(),
        ));
    }

    let (ctrl_conn, _) = x11rb::connect(None)
        .map_err(|err| generic_error(format!("InputHook.start failed: NoXDisplay: {err}")))?;
    let (data_conn, _) = x11rb::connect(None)
        .map_err(|err| generic_error(format!("InputHook.start failed: NoXDisplay: {err}")))?;
    let ctrl_conn = Arc::new(ctrl_conn);

    let has_record = ctrl_conn
        .extension_information(record::X11_EXTENSION_NAME)
        .map_err(|err| generic_error(format!("InputHook.start failed: {err}")))?
        .is_some();
    if !has_record {
        return Err(generic_error(
            "InputHook.start failed: RecordExtensionUnavailable",
        ));
    }
    ctrl_conn
        .record_query_version(
            record::X11_XML_VERSION.0 as _,
            record::X11_XML_VERSION.1 as _,
        )
        .map_err(|err| generic_error(format!("InputHook.start failed: RecordQueryVersion: {err}")))?
        .reply()
        .map_err(|err| {
            generic_error(format!("InputHook.start failed: RecordQueryVersion: {err}"))
        })?;

    let setup = ctrl_conn.setup();
    let min_keycode = setup.min_keycode;
    let max_keycode = setup.max_keycode;
    let count = max_keycode.saturating_sub(min_keycode).saturating_add(1);
    let mapping = ctrl_conn
        .get_keyboard_mapping(min_keycode, count)
        .map_err(|err| generic_error(format!("InputHook.start failed: GetKeyboardMapping: {err}")))?
        .reply()
        .map_err(|err| {
            generic_error(format!("InputHook.start failed: GetKeyboardMapping: {err}"))
        })?;
    let keysyms = KeysymCache::build(&mapping, min_keycode);

    let record_ctx = ctrl_conn
        .generate_id()
        .map_err(|err| generic_error(format!("InputHook.start failed: GenerateId: {err}")))?;
    let empty = Range8 { first: 0, last: 0 };
    let empty_ext = ExtRange {
        major: empty,
        minor: Range16 { first: 0, last: 0 },
    };
    let range = Range {
        core_requests: empty,
        core_replies: empty,
        ext_requests: empty_ext,
        ext_replies: empty_ext,
        delivered_events: empty,
        device_events: Range8 {
            first: KEY_PRESS,
            last: MOTION_NOTIFY,
        },
        errors: empty,
        client_started: false,
        client_died: false,
    };
    ctrl_conn
        .record_create_context(record_ctx, 0, &[record::CS::ALL_CLIENTS.into()], &[range])
        .map_err(|err| {
            generic_error(format!(
                "InputHook.start failed: RecordCreateContext: {err}"
            ))
        })?
        .check()
        .map_err(|err| {
            generic_error(format!(
                "InputHook.start failed: RecordCreateContext: {err}"
            ))
        })?;

    let stop = Arc::new(AtomicBool::new(false));

    let worker_callback = callback.clone();
    let worker_stop = stop.clone();
    let worker = thread::Builder::new()
        .name("fluxer-linux-input-hook".to_string())
        .spawn(move || {
            worker_main(data_conn, record_ctx, keysyms, worker_callback, worker_stop);
        })
        .map_err(|err| generic_error(format!("InputHook.start failed: thread spawn: {err}")))?;

    Ok(Active {
        ctrl_conn,
        record_ctx,
        worker: Some(worker),
        stop,
    })
}

fn worker_main(
    data_conn: RustConnection,
    record_ctx: record::Context,
    keysyms: KeysymCache,
    callback: EventTsfn,
    stop: Arc<AtomicBool>,
) {
    let cookie = match data_conn.record_enable_context(record_ctx) {
        Ok(c) => c,
        Err(_) => {
            stop.store(true, Ordering::Release);
            return;
        }
    };

    for reply in cookie {
        if stop.load(Ordering::Acquire) {
            break;
        }
        let reply = match reply {
            Ok(r) => r,
            Err(ReplyError::ConnectionError(_)) => break,
            Err(_) => continue,
        };
        if reply.client_swapped {
            continue;
        }
        match reply.category {
            RECORD_START_OF_DATA => continue,
            RECORD_FROM_SERVER => {}
            _ => continue,
        }
        let mut data: &[u8] = &reply.data;
        while !data.is_empty() {
            let consumed = decode_one(data, &keysyms, &callback, &stop);
            if consumed == 0 || consumed > data.len() {
                break;
            }
            data = &data[consumed..];
        }
    }
}

fn decode_one(
    data: &[u8],
    keysyms: &KeysymCache,
    callback: &EventTsfn,
    stop: &Arc<AtomicBool>,
) -> usize {
    if data.is_empty() {
        return 0;
    }
    let type_ = data[0];
    match type_ {
        KEY_PRESS | KEY_RELEASE | BUTTON_PRESS | BUTTON_RELEASE | MOTION_NOTIFY => {
            if data.len() < std::mem::size_of::<XKeyButtonProto>() {
                return 0;
            }

            let evt: XKeyButtonProto =
                unsafe { std::ptr::read_unaligned(data.as_ptr() as *const XKeyButtonProto) };
            handle_event(&evt, keysyms, callback, stop);
            32
        }
        0 => {
            if data.len() < 8 {
                return 0;
            }
            let (length, _) = match u32::try_parse(&data[4..]) {
                Ok(v) => v,
                Err(_) => return 0,
            };
            32 + (length as usize) * 4
        }
        _ => 32,
    }
}

fn handle_event(
    evt: &XKeyButtonProto,
    keysyms: &KeysymCache,
    callback: &EventTsfn,
    stop: &Arc<AtomicBool>,
) {
    let mods = modifiers::from_state(u32::from(evt.state));
    match evt.type_ {
        KEY_PRESS | KEY_RELEASE => {
            let _lookup = x11::xkb_lookup_for_base();
            let keysym = keysyms.lookup(evt.detail);
            let mut event = DecodedEvent::new(
                if evt.type_ == KEY_PRESS {
                    EventKind::KeyDown
                } else {
                    EventKind::KeyUp
                },
                mods,
            );
            event.keycode = keysym;
            event.key_name = match keymap::keysym_to_name(keysym) {
                Some(name) => name.to_string(),
                None => keymap::fallback_name(keysym),
            };
            dispatch(callback, stop, event);
        }
        BUTTON_PRESS | BUTTON_RELEASE => {
            let cls = mouse::classify(u32::from(evt.detail));
            match cls {
                MouseClassification::Button(b) => {
                    let mut event = DecodedEvent::new(
                        if evt.type_ == BUTTON_PRESS {
                            EventKind::MouseDown
                        } else {
                            EventKind::MouseUp
                        },
                        mods,
                    );
                    event.button = b;
                    event.x = i32::from(evt.root_x);
                    event.y = i32::from(evt.root_y);
                    event.has_xy = true;
                    dispatch(callback, stop, event);
                }
                MouseClassification::Wheel(dir) => {
                    if evt.type_ == BUTTON_PRESS {
                        let mut event = DecodedEvent::new(EventKind::Wheel, mods);
                        event.delta_x = dir.delta_x();
                        event.delta_y = dir.delta_y();
                        event.x = i32::from(evt.root_x);
                        event.y = i32::from(evt.root_y);
                        event.has_xy = true;
                        dispatch(callback, stop, event);
                    }
                }
                MouseClassification::Ignored => {}
            }
        }
        MOTION_NOTIFY => {
            let mut event = DecodedEvent::new(EventKind::MouseMove, mods);
            event.x = i32::from(evt.root_x);
            event.y = i32::from(evt.root_y);
            event.has_xy = true;
            dispatch(callback, stop, event);
        }
        _ => {}
    }
}

fn dispatch(callback: &EventTsfn, stop: &Arc<AtomicBool>, event: DecodedEvent) {
    let status = callback.call(event, ThreadsafeFunctionCallMode::NonBlocking);
    if status == Status::Closing {
        stop.store(true, Ordering::Release);
    }
}

fn tear_down(mut active: Active) {
    active.stop.store(true, Ordering::Release);

    if active.record_ctx != 0 {
        let _ = active.ctrl_conn.record_disable_context(active.record_ctx);
        let _ = active.ctrl_conn.sync();
    }
    if let Some(worker) = active.worker.take() {
        let _ = worker.join();
    }
    if active.record_ctx != 0 {
        let _ = active.ctrl_conn.record_free_context(active.record_ctx);
        let _ = active.ctrl_conn.sync();
    }
}

#[napi(js_name = "isAvailable")]
pub fn is_available() -> bool {
    if !detect_display_server().supports_global_xrecord() {
        return false;
    }
    x11rb::connect(None).is_ok()
}

#[allow(dead_code)]
pub(crate) fn detected_display_server() -> DisplayServer {
    detect_display_server()
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

#[allow(dead_code)]
const _ASSERT_PROTO_LAYOUT: fn() = || {
    use std::mem::offset_of;
    let _ = offset_of!(XKeyButtonProto, seq_l);
    let _ = offset_of!(XKeyButtonProto, seq_h);
    let _ = offset_of!(XKeyButtonProto, event);
    let _ = offset_of!(XKeyButtonProto, child);
    let _ = offset_of!(XKeyButtonProto, event_x);
    let _ = offset_of!(XKeyButtonProto, event_y);
    let _ = offset_of!(XKeyButtonProto, same_screen);
    let _ = offset_of!(XKeyButtonProto, pad0);
    let _ = offset_of!(XKeyButtonProto, root);
    let _ = offset_of!(XKeyButtonProto, time);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decoded_keydown_carries_keysym_and_name() {
        let mut event = DecodedEvent::new(EventKind::KeyDown, modifiers::from_state(0));
        event.keycode = 0x0061;
        event.key_name = keymap::keysym_to_name(0x0061).unwrap().to_string();
        assert!(matches!(event.kind, EventKind::KeyDown));
        assert_eq!(event.keycode, 0x0061);
        assert_eq!(event.key_name, "A");
        assert!(!event.has_xy);
    }

    #[test]
    fn decoded_wheel_uses_120_step_deltas() {
        let dir = mouse::WheelDirection::Down;
        let mut event = DecodedEvent::new(EventKind::Wheel, modifiers::from_state(0));
        event.delta_x = dir.delta_x();
        event.delta_y = dir.delta_y();
        event.has_xy = true;
        assert_eq!(event.delta_x, 0);
        assert_eq!(event.delta_y, 120);
    }

    #[test]
    fn x_key_button_proto_layout_offsets_match_x11_wire_format() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(XKeyButtonProto, type_), 0);
        assert_eq!(offset_of!(XKeyButtonProto, detail), 1);

        assert_eq!(offset_of!(XKeyButtonProto, time), 4);
        assert_eq!(offset_of!(XKeyButtonProto, root_x), 20);
        assert_eq!(offset_of!(XKeyButtonProto, root_y), 22);
        assert_eq!(offset_of!(XKeyButtonProto, state), 28);
    }

    #[test]
    fn event_kind_to_string_matches_js_contract() {
        let cases: &[(EventKind, &str)] = &[
            (EventKind::KeyDown, "keydown"),
            (EventKind::KeyUp, "keyup"),
            (EventKind::MouseDown, "mousedown"),
            (EventKind::MouseUp, "mouseup"),
            (EventKind::MouseMove, "mousemove"),
            (EventKind::Wheel, "wheel"),
        ];
        for (kind, expected) in cases {
            let label = match kind {
                EventKind::KeyDown => "keydown",
                EventKind::KeyUp => "keyup",
                EventKind::MouseDown => "mousedown",
                EventKind::MouseUp => "mouseup",
                EventKind::MouseMove => "mousemove",
                EventKind::Wheel => "wheel",
            };
            assert_eq!(label, *expected);
        }
    }

    #[test]
    fn keysym_cache_returns_zero_below_min_keycode() {
        let cache = KeysymCache {
            min_keycode: 8,
            syms: vec![0x61, 0x62, 0x63],
        };
        assert_eq!(cache.lookup(7), 0);
        assert_eq!(cache.lookup(8), 0x61);
        assert_eq!(cache.lookup(10), 0x63);
        assert_eq!(cache.lookup(255), 0);
    }
}
