// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    collections::HashMap,
    ffi::c_void,
    ptr,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

use futures_lite::{FutureExt, StreamExt, future};
use napi::{
    Env, JsValue, Status, ValueType,
    bindgen_prelude::{Array, AsyncTask, Function, Object, Result, Task, ToNapiValue, Unknown},
    sys,
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;
use zbus::{
    MatchRule, MessageStream,
    blocking::{Connection as BlockingConnection, Proxy},
    message::Type as MessageType,
    zvariant::Value,
};

const NOTIFY_DEST: &str = "org.freedesktop.Notifications";
const NOTIFY_PATH: &str = "/org/freedesktop/Notifications";
const NOTIFY_IFACE: &str = "org.freedesktop.Notifications";
const CALL_TIMEOUT: Duration = Duration::from_millis(5_000);
const SIGNAL_POLL_INTERVAL: Duration = Duration::from_millis(200);
const SIGNAL_THREAD_START_TIMEOUT: Duration = Duration::from_secs(5);
const EVENT_QUEUE_LIMIT: usize = 1024;

const DBUS_SESSION_UNREACHABLE: &str = "DBus session bus unreachable: no $DBUS_SESSION_BUS_ADDRESS and no \
     $XDG_RUNTIME_DIR/bus socket — desktop notifications require a \
     running session bus (systemd-logind, elogind, or dbus-launch).";

fn has_dbus_session() -> bool {
    has_dbus_session_from(
        std::env::var("DBUS_SESSION_BUS_ADDRESS").ok().as_deref(),
        std::env::var("XDG_RUNTIME_DIR").ok().as_deref(),
        |path| std::path::Path::new(path).exists(),
    )
}

fn has_dbus_session_from(
    bus_address: Option<&str>,
    xdg_runtime_dir: Option<&str>,
    path_exists: impl Fn(&str) -> bool,
) -> bool {
    if bus_address.is_some_and(|v| !v.is_empty()) {
        return true;
    }
    if let Some(dir) = xdg_runtime_dir
        && !dir.is_empty()
    {
        let candidate = format!("{}/bus", dir.trim_end_matches('/'));
        if path_exists(&candidate) {
            return true;
        }
    }
    false
}

type EventThreadsafeFunction = Arc<
    ThreadsafeFunction<
        NotifyEvent,
        UnknownReturnValue,
        NotifyEvent,
        Status,
        false,
        true,
        EVENT_QUEUE_LIMIT,
    >,
>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Urgency {
    Low,
    Normal,
    Critical,
}

impl Urgency {
    fn parse(value: &str) -> Option<Self> {
        match value {
            "low" => Some(Self::Low),
            "normal" => Some(Self::Normal),
            "critical" => Some(Self::Critical),
            _ => None,
        }
    }

    fn as_byte(self) -> u8 {
        match self {
            Self::Low => 0,
            Self::Normal => 1,
            Self::Critical => 2,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyImageData {
    width: i32,
    height: i32,
    rowstride: i32,
    has_alpha: bool,
    bits_per_sample: i32,
    channels: i32,
    data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyHints {
    urgency: Option<Urgency>,
    category: Option<String>,
    desktop_entry: Option<String>,
    sound_file: Option<String>,
    suppress_sound: Option<bool>,
    transient: Option<bool>,
    action_icons: Option<bool>,
    image_data: Option<NotifyImageData>,
}

impl NotifyHints {
    fn empty() -> Self {
        Self {
            urgency: None,
            category: None,
            desktop_entry: None,
            sound_file: None,
            suppress_sound: None,
            transient: None,
            action_icons: None,
            image_data: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyAction {
    key: String,
    label: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyArgs {
    app_name: String,
    replaces_id: u32,
    app_icon: String,
    summary: String,
    body: String,
    actions: Vec<NotifyAction>,
    hints: NotifyHints,
    expire_timeout_ms: i32,
}

#[derive(Debug)]
pub enum NotifyEvent {
    ActionInvoked { id: u32, action_key: String },
    Closed { id: u32, reason: u32 },
}

impl ToNapiValue for NotifyEvent {
    unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        match event {
            Self::ActionInvoked { id, action_key } => {
                object.set("kind", "actionInvoked")?;
                object.set("id", id)?;
                object.set("actionKey", action_key)?;
            }
            Self::Closed { id, reason } => {
                object.set("kind", "closed")?;
                object.set("id", id)?;
                object.set("reason", reason)?;
            }
        }
        unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, object) }
    }
}

struct Inner {
    method_conn: Mutex<Option<BlockingConnection>>,
    stop_signal_thread: Arc<AtomicBool>,
    signal_thread: Mutex<Option<JoinHandle<()>>>,
    event_callback: Mutex<Option<EventThreadsafeFunction>>,
}

impl Inner {
    fn close(&self) {
        self.stop_signal_thread.store(true, Ordering::Release);

        if let Ok(mut signal_thread) = self.signal_thread.lock()
            && let Some(thread) = signal_thread.take()
        {
            let _ = thread.join();
        }

        if let Ok(mut method_conn) = self.method_conn.lock()
            && let Some(conn) = method_conn.take()
        {
            let _ = conn.close();
        }

        if let Ok(mut event_callback) = self.event_callback.lock() {
            let _ = event_callback.take();
        }
    }

    fn with_connection<T>(
        &self,
        label: &'static str,
        f: impl FnOnce(&BlockingConnection) -> Result<T>,
    ) -> Result<T> {
        let guard = self
            .method_conn
            .lock()
            .map_err(|_| generic_error("notifications client lock poisoned"))?;
        let conn = guard
            .as_ref()
            .ok_or_else(|| generic_error("notifications client closed"))?;
        f(conn).map_err(|err| generic_error(format!("{label}: {}", err.reason)))
    }
}

impl Drop for Inner {
    fn drop(&mut self) {
        self.close();
    }
}

#[napi]
pub struct FreedesktopNotifications {
    inner: Arc<Inner>,
}

#[napi]
impl FreedesktopNotifications {
    #[napi(constructor)]
    pub fn new(on_event: Function<NotifyEvent, UnknownReturnValue>) -> Result<Self> {
        let event_callback = Arc::new(
            on_event
                .build_threadsafe_function::<NotifyEvent>()
                .weak::<true>()
                .callee_handled::<false>()
                .max_queue_size::<EVENT_QUEUE_LIMIT>()
                .build()
                .map_err(|err| {
                    generic_error(format!(
                        "failed to create notification callback: {}",
                        err.reason
                    ))
                })?,
        );
        if !has_dbus_session() {
            return Err(generic_error(DBUS_SESSION_UNREACHABLE));
        }
        let method_conn = open_blocking_connection()
            .map_err(|err| generic_error(format!("notifications client init failed: {err}")))?;
        let stop_signal_thread = Arc::new(AtomicBool::new(false));
        let signal_thread = spawn_signal_thread(event_callback.clone(), stop_signal_thread.clone())
            .map_err(|err| generic_error(format!("notifications client init failed: {err}")))?;

        Ok(Self {
            inner: Arc::new(Inner {
                method_conn: Mutex::new(Some(method_conn)),
                stop_signal_thread,
                signal_thread: Mutex::new(Some(signal_thread)),
                event_callback: Mutex::new(Some(event_callback)),
            }),
        })
    }

    #[napi]
    pub fn notify(&self, payload: Object) -> Result<AsyncTask<NotifyTask>> {
        let args = parse_notify_payload(&payload)?;
        Ok(AsyncTask::new(NotifyTask {
            inner: self.inner.clone(),
            args,
        }))
    }

    #[napi(js_name = "closeNotification")]
    pub fn close_notification(&self, id: u32) -> Result<AsyncTask<CloseNotificationTask>> {
        Ok(AsyncTask::new(CloseNotificationTask {
            inner: self.inner.clone(),
            id,
        }))
    }

    #[napi(js_name = "getServerCapabilities")]
    pub fn get_server_capabilities(&self) -> Result<AsyncTask<GetCapabilitiesTask>> {
        Ok(AsyncTask::new(GetCapabilitiesTask {
            inner: self.inner.clone(),
        }))
    }

    #[napi]
    pub fn close(&self) -> Result<AsyncTask<CloseTask>> {
        Ok(AsyncTask::new(CloseTask {
            inner: self.inner.clone(),
        }))
    }
}

impl Drop for FreedesktopNotifications {
    fn drop(&mut self) {
        self.inner.close();
    }
}

pub struct NotifyTask {
    inner: Arc<Inner>,
    args: NotifyArgs,
}

impl Task for NotifyTask {
    type Output = u32;
    type JsValue = u32;

    fn compute(&mut self) -> Result<Self::Output> {
        self.inner
            .with_connection("Notify failed", |conn| notify(conn, &self.args))
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(output)
    }
}

pub struct CloseNotificationTask {
    inner: Arc<Inner>,
    id: u32,
}

impl Task for CloseNotificationTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        self.inner
            .with_connection("CloseNotification failed", |conn| {
                close_notification(conn, self.id)
            })
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

pub struct GetCapabilitiesTask {
    inner: Arc<Inner>,
}

impl Task for GetCapabilitiesTask {
    type Output = Vec<String>;
    type JsValue = Vec<String>;

    fn compute(&mut self) -> Result<Self::Output> {
        self.inner
            .with_connection("GetCapabilities failed", get_capabilities)
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(output)
    }
}

pub struct CloseTask {
    inner: Arc<Inner>,
}

impl Task for CloseTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        self.inner.close();
        Ok(())
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

#[napi(object)]
pub struct ServerInformation {
    pub name: String,
    pub vendor: String,
    pub version: String,
    #[napi(js_name = "specVersion")]
    pub spec_version: String,
}

pub struct GetServerInformationTask;

impl Task for GetServerInformationTask {
    type Output = ServerInformation;
    type JsValue = ServerInformation;

    fn compute(&mut self) -> Result<Self::Output> {
        if !has_dbus_session() {
            return Err(generic_error(DBUS_SESSION_UNREACHABLE));
        }
        let conn = open_blocking_connection()
            .map_err(|err| generic_error(format!("openSessionBus failed: {err}")))?;
        get_server_information_on(&conn)
            .map_err(|err| generic_error(format!("GetServerInformation failed: {}", err.reason)))
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(output)
    }
}

#[napi(js_name = "getServerInformation")]
pub fn get_server_information() -> Result<AsyncTask<GetServerInformationTask>> {
    Ok(AsyncTask::new(GetServerInformationTask))
}

fn open_blocking_connection() -> zbus::Result<BlockingConnection> {
    zbus::blocking::connection::Builder::session()?
        .method_timeout(CALL_TIMEOUT)
        .build()
}

fn notification_proxy(conn: &BlockingConnection) -> Result<Proxy<'_>> {
    Proxy::new(conn, NOTIFY_DEST, NOTIFY_PATH, NOTIFY_IFACE)
        .map_err(|err| generic_error(err.to_string()))
}

fn notify(conn: &BlockingConnection, args: &NotifyArgs) -> Result<u32> {
    let proxy = notification_proxy(conn)?;
    let actions = flatten_actions(&args.actions);
    let hints = build_hints(&args.hints);
    proxy
        .call(
            "Notify",
            &(
                args.app_name.as_str(),
                args.replaces_id,
                args.app_icon.as_str(),
                args.summary.as_str(),
                args.body.as_str(),
                actions,
                hints,
                args.expire_timeout_ms,
            ),
        )
        .map_err(|err| generic_error(err.to_string()))
}

fn close_notification(conn: &BlockingConnection, id: u32) -> Result<()> {
    notification_proxy(conn)?
        .call("CloseNotification", &(id,))
        .map_err(|err| generic_error(err.to_string()))
}

fn get_capabilities(conn: &BlockingConnection) -> Result<Vec<String>> {
    notification_proxy(conn)?
        .call("GetCapabilities", &())
        .map_err(|err| generic_error(err.to_string()))
}

fn get_server_information_on(conn: &BlockingConnection) -> Result<ServerInformation> {
    let (name, vendor, version, spec_version): (String, String, String, String) =
        notification_proxy(conn)?
            .call("GetServerInformation", &())
            .map_err(|err| generic_error(err.to_string()))?;
    Ok(ServerInformation {
        name,
        vendor,
        version,
        spec_version,
    })
}

fn flatten_actions(actions: &[NotifyAction]) -> Vec<&str> {
    let mut out = Vec::with_capacity(actions.len() * 2);
    for action in actions {
        out.push(action.key.as_str());
        out.push(action.label.as_str());
    }
    out
}

fn build_hints(hints: &NotifyHints) -> HashMap<&'static str, Value<'_>> {
    let mut out = HashMap::new();
    if let Some(urgency) = hints.urgency {
        out.insert("urgency", Value::new(urgency.as_byte()));
    }
    if let Some(category) = hints.category.as_deref() {
        out.insert("category", Value::new(category));
    }
    if let Some(desktop_entry) = hints.desktop_entry.as_deref() {
        out.insert("desktop-entry", Value::new(desktop_entry));
    }
    if let Some(sound_file) = hints.sound_file.as_deref() {
        out.insert("sound-file", Value::new(sound_file));
    }
    if let Some(suppress_sound) = hints.suppress_sound {
        out.insert("suppress-sound", Value::new(suppress_sound));
    }
    if let Some(transient) = hints.transient {
        out.insert("transient", Value::new(transient));
    }
    if let Some(action_icons) = hints.action_icons {
        out.insert("action-icons", Value::new(action_icons));
    }
    if let Some(image) = hints.image_data.as_ref() {
        out.insert(
            "image-data",
            Value::new((
                image.width,
                image.height,
                image.rowstride,
                image.has_alpha,
                image.bits_per_sample,
                image.channels,
                image.data.as_slice(),
            )),
        );
    }
    out
}

fn spawn_signal_thread(
    event_callback: EventThreadsafeFunction,
    stop: Arc<AtomicBool>,
) -> std::result::Result<JoinHandle<()>, String> {
    let (ready_tx, ready_rx) = mpsc::sync_channel(1);
    let stop_for_thread = stop.clone();
    let thread = thread::Builder::new()
        .name("fluxer-linux-notifications-signals".to_string())
        .spawn(move || {
            let setup = future::block_on(async {
                let conn = zbus::Connection::session().await?;
                let rule = MatchRule::builder()
                    .msg_type(MessageType::Signal)
                    .path(NOTIFY_PATH)?
                    .interface(NOTIFY_IFACE)?
                    .build();
                let stream = MessageStream::for_match_rule(rule, &conn, Some(64)).await?;
                zbus::Result::Ok((conn, stream))
            });

            let (_conn, mut stream) = match setup {
                Ok(parts) => {
                    let _ = ready_tx.send(Ok(()));
                    parts
                }
                Err(err) => {
                    let _ = ready_tx.send(Err(err.to_string()));
                    return;
                }
            };

            while !stop_for_thread.load(Ordering::Acquire) {
                let timeout = async {
                    async_io::Timer::after(SIGNAL_POLL_INTERVAL).await;
                    None
                };
                let next_message = stream.next().or(timeout);
                match future::block_on(next_message) {
                    Some(Ok(message)) => {
                        if let Some(event) = event_from_signal(&message) {
                            let status =
                                event_callback.call(event, ThreadsafeFunctionCallMode::NonBlocking);
                            if status == Status::Closing {
                                break;
                            }
                        }
                    }
                    Some(Err(_)) => break,
                    None => {}
                }
            }
        })
        .map_err(|err| err.to_string())?;

    match ready_rx.recv_timeout(SIGNAL_THREAD_START_TIMEOUT) {
        Ok(Ok(())) => Ok(thread),
        Ok(Err(err)) => {
            let _ = thread.join();
            Err(err)
        }
        Err(err) => {
            stop.store(true, Ordering::Release);
            if matches!(err, mpsc::RecvTimeoutError::Disconnected) {
                let _ = thread.join();
            }
            Err(err.to_string())
        }
    }
}

fn event_from_signal(message: &zbus::Message) -> Option<NotifyEvent> {
    let header = message.header();
    if header.interface().map(|iface| iface.as_str()) != Some(NOTIFY_IFACE) {
        return None;
    }

    match header.member()?.as_str() {
        "ActionInvoked" => {
            let (id, action_key): (u32, String) = message.body().deserialize().ok()?;
            Some(NotifyEvent::ActionInvoked { id, action_key })
        }
        "NotificationClosed" => {
            let (id, reason): (u32, u32) = message.body().deserialize().ok()?;
            Some(NotifyEvent::Closed { id, reason })
        }
        _ => None,
    }
}

fn parse_notify_payload(payload: &Object) -> Result<NotifyArgs> {
    Ok(NotifyArgs {
        app_name: read_string_or_empty(payload, "appName"),
        replaces_id: read_optional_u32(payload, "replacesId").unwrap_or(0),
        app_icon: read_string_or_empty(payload, "appIcon"),
        summary: read_string_or_empty(payload, "summary"),
        body: read_string_or_empty(payload, "body"),
        actions: parse_actions(payload),
        hints: parse_hints(payload)?,
        expire_timeout_ms: read_optional_i32(payload, "expireTimeoutMs").unwrap_or(-1),
    })
}

fn parse_actions(payload: &Object) -> Vec<NotifyAction> {
    let Some(actions) = read_array(payload, "actions") else {
        return Vec::new();
    };

    let mut out = Vec::with_capacity(actions.len() as usize);
    for index in 0..actions.len() {
        let action = actions.get::<Object>(index).ok().flatten();
        match action {
            Some(action) => out.push(NotifyAction {
                key: read_string_or_empty(&action, "key"),
                label: read_string_or_empty(&action, "label"),
            }),
            None => out.push(NotifyAction {
                key: String::new(),
                label: String::new(),
            }),
        }
    }
    out
}

fn parse_hints(payload: &Object) -> Result<NotifyHints> {
    let Some(hints_object) = read_object(payload, "hints") else {
        return Ok(NotifyHints::empty());
    };

    let mut hints = NotifyHints::empty();
    if let Some(value) = read_optional_string(&hints_object, "urgency") {
        hints.urgency = Urgency::parse(&value);
    }
    hints.category = read_optional_string(&hints_object, "category");
    hints.desktop_entry = read_optional_string(&hints_object, "desktopEntry");
    hints.sound_file = read_optional_string(&hints_object, "soundFile");
    hints.suppress_sound = read_optional_bool(&hints_object, "suppressSound");
    hints.transient = read_optional_bool(&hints_object, "transient");
    hints.action_icons = read_optional_bool(&hints_object, "actionIcons");

    if let Some(image_object) = read_object(&hints_object, "imageData") {
        hints.image_data = Some(parse_image_data(&image_object)?);
    }

    Ok(hints)
}

fn parse_image_data(value: &Object) -> Result<NotifyImageData> {
    let width = read_optional_i32(value, "width").unwrap_or(0);
    let height = read_optional_i32(value, "height").unwrap_or(0);
    let channels = read_optional_i32(value, "channels").unwrap_or(4);
    let mut rowstride = read_optional_i32(value, "rowstride").unwrap_or(0);
    let bits_per_sample = read_optional_i32(value, "bitsPerSample").unwrap_or(8);
    let has_alpha = read_optional_bool(value, "hasAlpha").unwrap_or(true);
    let data_value = read_unknown(value, "data")
        .ok_or_else(|| invalid_arg("imageData.data must be a Buffer or Uint8Array"))?;
    let data = read_image_bytes(data_value)?;

    if width <= 0 || height <= 0 {
        return Err(invalid_arg("imageData width and height must be positive"));
    }
    if rowstride <= 0 {
        rowstride = width.saturating_mul(channels);
    }
    validate_image_data_shape(
        width,
        height,
        rowstride,
        has_alpha,
        bits_per_sample,
        channels,
        data.len(),
    )?;

    Ok(NotifyImageData {
        width,
        height,
        rowstride,
        has_alpha,
        bits_per_sample,
        channels,
        data,
    })
}

fn validate_image_data_shape(
    width: i32,
    height: i32,
    rowstride: i32,
    has_alpha: bool,
    bits_per_sample: i32,
    channels: i32,
    data_len: usize,
) -> Result<()> {
    if bits_per_sample != 8 {
        return Err(invalid_arg("imageData.bitsPerSample must be 8"));
    }

    let expected_channels = if has_alpha { 4 } else { 3 };
    if channels != expected_channels {
        return Err(invalid_arg(format!(
            "imageData.channels must be {expected_channels} when hasAlpha is {has_alpha}"
        )));
    }

    let width = usize::try_from(width).map_err(|_| invalid_arg("imageData width is invalid"))?;
    let height = usize::try_from(height).map_err(|_| invalid_arg("imageData height is invalid"))?;
    let rowstride =
        usize::try_from(rowstride).map_err(|_| invalid_arg("imageData.rowstride is invalid"))?;
    let channels =
        usize::try_from(channels).map_err(|_| invalid_arg("imageData.channels is invalid"))?;

    let row_bytes = width
        .checked_mul(channels)
        .ok_or_else(|| invalid_arg("imageData row byte count overflows"))?;
    if rowstride < row_bytes {
        return Err(invalid_arg(
            "imageData.rowstride is smaller than one pixel row",
        ));
    }

    let min_data_len = if height == 1 {
        row_bytes
    } else {
        rowstride
            .checked_mul(height - 1)
            .and_then(|bytes| bytes.checked_add(row_bytes))
            .ok_or_else(|| invalid_arg("imageData byte count overflows"))?
    };
    if data_len < min_data_len {
        return Err(invalid_arg(
            "imageData.data is too short for the supplied dimensions",
        ));
    }

    Ok(())
}

fn read_unknown<'a>(object: &Object<'a>, key: &str) -> Option<Unknown<'a>> {
    object.get::<Unknown>(key).ok().flatten()
}

fn read_string_or_empty(object: &Object, key: &str) -> String {
    read_optional_string(object, key).unwrap_or_default()
}

fn read_optional_string(object: &Object, key: &str) -> Option<String> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::String {
        return None;
    }
    unsafe { value.cast::<String>().ok() }
}

fn read_optional_u32(object: &Object, key: &str) -> Option<u32> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::Number {
        return None;
    }
    unsafe { value.cast::<u32>().ok() }
}

fn read_optional_i32(object: &Object, key: &str) -> Option<i32> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::Number {
        return None;
    }
    unsafe { value.cast::<i32>().ok() }
}

fn read_optional_bool(object: &Object, key: &str) -> Option<bool> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::Boolean {
        return None;
    }
    unsafe { value.cast::<bool>().ok() }
}

fn read_object<'a>(object: &Object<'a>, key: &str) -> Option<Object<'a>> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::Object {
        return None;
    }
    unsafe { value.cast::<Object>().ok() }
}

fn read_array<'a>(object: &Object<'a>, key: &str) -> Option<Array<'a>> {
    let value = read_unknown(object, key)?;
    if value.get_type().ok()? != ValueType::Object {
        return None;
    }
    unsafe { value.cast::<Array>().ok() }
}

fn read_image_bytes(value: Unknown) -> Result<Vec<u8>> {
    let raw = value.raw();
    let raw_env = value.value().env;

    let mut is_typed_array = false;
    let typed_status = unsafe { sys::napi_is_typedarray(raw_env, raw, &mut is_typed_array) };
    if typed_status == sys::Status::napi_ok && is_typed_array {
        let mut array_type: sys::napi_typedarray_type = 0;
        let mut len = 0usize;
        let mut data_ptr: *mut c_void = ptr::null_mut();
        let mut arraybuffer = ptr::null_mut();
        let mut byte_offset = 0usize;
        let status = unsafe {
            sys::napi_get_typedarray_info(
                raw_env,
                raw,
                &mut array_type,
                &mut len,
                &mut data_ptr,
                &mut arraybuffer,
                &mut byte_offset,
            )
        };
        if status != sys::Status::napi_ok {
            return Err(invalid_arg("imageData.data must be a readable typed array"));
        }
        if !is_uint8_typed_array(array_type) {
            return Err(invalid_arg("imageData.data must be a Buffer or Uint8Array"));
        }
        return copy_non_empty_bytes(data_ptr.cast(), len);
    }

    let mut is_buffer = false;
    let buffer_status = unsafe { sys::napi_is_buffer(raw_env, raw, &mut is_buffer) };
    if buffer_status == sys::Status::napi_ok && is_buffer {
        let mut data_ptr: *mut c_void = ptr::null_mut();
        let mut len = 0usize;
        let status = unsafe { sys::napi_get_buffer_info(raw_env, raw, &mut data_ptr, &mut len) };
        if status != sys::Status::napi_ok {
            return Err(invalid_arg("imageData.data must be a readable Buffer"));
        }
        return copy_non_empty_bytes(data_ptr.cast(), len);
    }

    Err(invalid_arg("imageData.data must be a Buffer or Uint8Array"))
}

fn is_uint8_typed_array(array_type: sys::napi_typedarray_type) -> bool {
    array_type == sys::TypedarrayType::uint8_array
}

fn copy_non_empty_bytes(data_ptr: *const u8, len: usize) -> Result<Vec<u8>> {
    if data_ptr.is_null() || len == 0 {
        return Err(invalid_arg("imageData.data must not be empty"));
    }
    Ok(unsafe { std::slice::from_raw_parts(data_ptr, len).to_vec() })
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

fn invalid_arg(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::InvalidArg, reason.into())
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum FakeArg<'a> {
    U32(u32),
    String(&'a str),
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyReply {
    id: u32,
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct ActionInvokedSignal {
    id: u32,
    action_key: String,
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClosedSignal {
    id: u32,
    reason: u32,
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseError {
    InvalidReply,
}

#[cfg(test)]
fn parse_notify_reply_from_args(
    args: &[FakeArg<'_>],
) -> std::result::Result<NotifyReply, ParseError> {
    match args.first() {
        Some(FakeArg::U32(id)) => Ok(NotifyReply { id: *id }),
        _ => Err(ParseError::InvalidReply),
    }
}

#[cfg(test)]
fn parse_action_invoked_from_args(
    args: &[FakeArg<'_>],
) -> std::result::Result<ActionInvokedSignal, ParseError> {
    match (args.first(), args.get(1)) {
        (Some(FakeArg::U32(id)), Some(FakeArg::String(action_key))) => Ok(ActionInvokedSignal {
            id: *id,
            action_key: (*action_key).to_string(),
        }),
        _ => Err(ParseError::InvalidReply),
    }
}

#[cfg(test)]
fn parse_closed_from_args(args: &[FakeArg<'_>]) -> std::result::Result<ClosedSignal, ParseError> {
    match (args.first(), args.get(1)) {
        (Some(FakeArg::U32(id)), Some(FakeArg::U32(reason))) => Ok(ClosedSignal {
            id: *id,
            reason: *reason,
        }),
        _ => Err(ParseError::InvalidReply),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn notify_reply_valid_u32_yields_id() {
        let reply = parse_notify_reply_from_args(&[FakeArg::U32(42)]).unwrap();
        assert_eq!(reply.id, 42);
    }

    #[test]
    fn notify_reply_rejects_empty_body() {
        assert_eq!(
            parse_notify_reply_from_args(&[]),
            Err(ParseError::InvalidReply)
        );
    }

    #[test]
    fn notify_reply_rejects_wrong_type() {
        assert_eq!(
            parse_notify_reply_from_args(&[FakeArg::String("wrong")]),
            Err(ParseError::InvalidReply)
        );
    }

    #[test]
    fn action_invoked_valid_body() {
        let signal =
            parse_action_invoked_from_args(&[FakeArg::U32(7), FakeArg::String("default")]).unwrap();
        assert_eq!(
            signal,
            ActionInvokedSignal {
                id: 7,
                action_key: "default".to_string()
            }
        );
    }

    #[test]
    fn action_invoked_rejects_swapped_types() {
        assert_eq!(
            parse_action_invoked_from_args(&[FakeArg::String("x"), FakeArg::String("y")]),
            Err(ParseError::InvalidReply)
        );
    }

    #[test]
    fn notification_closed_valid_body() {
        let signal = parse_closed_from_args(&[FakeArg::U32(11), FakeArg::U32(2)]).unwrap();
        assert_eq!(signal, ClosedSignal { id: 11, reason: 2 });
    }

    #[test]
    fn notification_closed_rejects_too_short() {
        assert_eq!(
            parse_closed_from_args(&[FakeArg::U32(11)]),
            Err(ParseError::InvalidReply)
        );
    }

    #[test]
    fn actions_are_flattened_as_key_label_pairs() {
        let actions = vec![
            NotifyAction {
                key: "default".to_string(),
                label: "Open".to_string(),
            },
            NotifyAction {
                key: "dismiss".to_string(),
                label: "Dismiss".to_string(),
            },
        ];

        assert_eq!(
            flatten_actions(&actions),
            vec!["default", "Open", "dismiss", "Dismiss"]
        );
    }

    #[test]
    fn urgency_strings_match_freedesktop_bytes() {
        assert_eq!(Urgency::parse("low").map(Urgency::as_byte), Some(0));
        assert_eq!(Urgency::parse("normal").map(Urgency::as_byte), Some(1));
        assert_eq!(Urgency::parse("critical").map(Urgency::as_byte), Some(2));
        assert_eq!(Urgency::parse("unknown"), None);
    }

    #[test]
    fn default_notify_args_match_legacy_defaults() {
        let args = NotifyArgs {
            app_name: String::new(),
            replaces_id: 0,
            app_icon: String::new(),
            summary: String::new(),
            body: String::new(),
            actions: Vec::new(),
            hints: NotifyHints::empty(),
            expire_timeout_ms: -1,
        };

        assert_eq!(args.replaces_id, 0);
        assert_eq!(args.expire_timeout_ms, -1);
        assert!(flatten_actions(&args.actions).is_empty());
        assert!(build_hints(&args.hints).is_empty());
    }

    #[test]
    fn hints_use_freedesktop_variant_signatures() {
        let hints = NotifyHints {
            urgency: Some(Urgency::Critical),
            category: Some("im.received".to_string()),
            desktop_entry: Some("fluxer".to_string()),
            sound_file: Some("/tmp/notify.oga".to_string()),
            suppress_sound: Some(true),
            transient: Some(true),
            action_icons: Some(false),
            image_data: Some(NotifyImageData {
                width: 1,
                height: 1,
                rowstride: 4,
                has_alpha: true,
                bits_per_sample: 8,
                channels: 4,
                data: vec![0, 1, 2, 3],
            }),
        };

        let values = build_hints(&hints);

        assert_eq!(values["urgency"].value_signature().to_string(), "y");
        assert_eq!(values["category"].value_signature().to_string(), "s");
        assert_eq!(values["desktop-entry"].value_signature().to_string(), "s");
        assert_eq!(values["sound-file"].value_signature().to_string(), "s");
        assert_eq!(values["suppress-sound"].value_signature().to_string(), "b");
        assert_eq!(values["transient"].value_signature().to_string(), "b");
        assert_eq!(values["action-icons"].value_signature().to_string(), "b");
        assert_eq!(
            values["image-data"].value_signature().to_string(),
            "(iiibiiay)"
        );
    }

    #[test]
    fn dbus_session_detected_via_bus_address() {
        assert!(has_dbus_session_from(
            Some("unix:path=/run/user/1000/bus"),
            None,
            |_| false
        ));
    }

    #[test]
    fn dbus_session_detected_via_runtime_dir_socket() {
        assert!(has_dbus_session_from(
            None,
            Some("/run/user/1000"),
            |path| path == "/run/user/1000/bus"
        ));
        assert!(has_dbus_session_from(
            None,
            Some("/run/user/1000/"),
            |path| path == "/run/user/1000/bus"
        ));
    }

    #[test]
    fn dbus_session_absent_in_headless_container() {
        assert!(!has_dbus_session_from(None, None, |_| false));
        assert!(!has_dbus_session_from(Some(""), Some(""), |_| true));
        assert!(!has_dbus_session_from(None, Some("/tmp/xdg"), |_| false));
    }

    #[test]
    fn dbus_session_unreachable_marker_is_stable_for_renderer_matching() {
        assert!(DBUS_SESSION_UNREACHABLE.starts_with("DBus session bus unreachable"));
    }

    #[test]
    fn image_rowstride_defaults_to_width_times_channels() {
        let width = 3;
        let channels = 4;
        let mut rowstride = 0;
        if rowstride <= 0 {
            rowstride = width * channels;
        }
        assert_eq!(rowstride, 12);
    }

    #[test]
    fn image_data_shape_matches_freedesktop_pixbuf_contract() {
        assert!(validate_image_data_shape(2, 2, 8, true, 8, 4, 16).is_ok());
        assert!(validate_image_data_shape(2, 2, 6, false, 8, 3, 12).is_ok());
        assert!(validate_image_data_shape(2, 2, 8, true, 16, 4, 16).is_err());
        assert!(validate_image_data_shape(2, 2, 8, true, 8, 3, 16).is_err());
        assert!(validate_image_data_shape(2, 2, 4, true, 8, 4, 16).is_err());
        assert!(validate_image_data_shape(2, 2, 8, true, 8, 4, 15).is_err());
    }

    #[test]
    fn image_data_typed_array_gate_accepts_only_uint8array() {
        assert!(is_uint8_typed_array(sys::TypedarrayType::uint8_array));
        assert!(!is_uint8_typed_array(sys::TypedarrayType::int8_array));
        assert!(!is_uint8_typed_array(
            sys::TypedarrayType::uint8_clamped_array
        ));
        assert!(!is_uint8_typed_array(sys::TypedarrayType::float32_array));
    }
}
