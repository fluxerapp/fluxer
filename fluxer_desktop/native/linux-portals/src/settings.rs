// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(target_os = "linux")]
use std::{
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

#[cfg(target_os = "linux")]
use futures_lite::{FutureExt, StreamExt, future};
#[cfg(target_os = "linux")]
use zbus::{
    MatchRule, MessageStream,
    blocking::{Connection as BlockingConnection, Proxy as BlockingProxy},
    message::Type as MessageType,
    zvariant::{OwnedValue, Value},
};

pub const PORTAL_DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PORTAL_PATH: &str = "/org/freedesktop/portal/desktop";
pub const SETTINGS_INTERFACE: &str = "org.freedesktop.portal.Settings";
pub const APPEARANCE_NAMESPACE: &str = "org.freedesktop.appearance";
#[cfg(target_os = "linux")]
pub const READ_TIMEOUT: Duration = Duration::from_millis(1_500);
#[cfg(target_os = "linux")]
pub const SIGNAL_POLL_INTERVAL: Duration = Duration::from_millis(200);
#[cfg(target_os = "linux")]
pub const SIGNAL_THREAD_START_TIMEOUT: Duration = Duration::from_secs(5);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorScheme {
    NoPreference,
    PreferDark,
    PreferLight,
}

impl ColorScheme {
    pub fn from_u32(value: u32) -> Self {
        match value {
            1 => Self::PreferDark,
            2 => Self::PreferLight,
            _ => Self::NoPreference,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::NoPreference => "no-preference",
            Self::PreferDark => "prefer-dark",
            Self::PreferLight => "prefer-light",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Contrast {
    NoPreference,
    High,
}

impl Contrast {
    pub fn from_u32(value: u32) -> Self {
        match value {
            1 => Self::High,
            _ => Self::NoPreference,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::NoPreference => "no-preference",
            Self::High => "high",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AccentColor {
    pub r: f64,
    pub g: f64,
    pub b: f64,
}

pub fn classify_accent_color(r: f64, g: f64, b: f64) -> Option<AccentColor> {
    if r < 0.0 || g < 0.0 || b < 0.0 {
        return None;
    }
    Some(AccentColor { r, g, b })
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChangeEvent {
    pub namespace: String,
    pub key: String,
    pub payload: ChangePayload,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChangePayload {
    Uint32(u32),
    Accent(AccentColor),
    Unknown,
}

#[cfg(target_os = "linux")]
fn open_blocking_connection() -> zbus::Result<BlockingConnection> {
    zbus::blocking::connection::Builder::session()?
        .method_timeout(READ_TIMEOUT)
        .build()
}

#[cfg(target_os = "linux")]
fn read_uint32_setting(key: &str) -> Option<u32> {
    let conn = open_blocking_connection().ok()?;
    let proxy =
        BlockingProxy::new(&conn, PORTAL_DESTINATION, PORTAL_PATH, SETTINGS_INTERFACE).ok()?;
    let value: OwnedValue = proxy.call("Read", &(APPEARANCE_NAMESPACE, key)).ok()?;
    crate::kwin::integer_from_variant(crate::kwin::value_of_owned(&value))
}

#[cfg(target_os = "linux")]
pub fn read_color_scheme() -> ColorScheme {
    read_uint32_setting("color-scheme")
        .map(ColorScheme::from_u32)
        .unwrap_or(ColorScheme::NoPreference)
}

#[cfg(target_os = "linux")]
pub fn read_contrast() -> Contrast {
    read_uint32_setting("contrast")
        .map(Contrast::from_u32)
        .unwrap_or(Contrast::NoPreference)
}

#[cfg(target_os = "linux")]
pub fn read_accent_color() -> Option<AccentColor> {
    let conn = open_blocking_connection().ok()?;
    let proxy =
        BlockingProxy::new(&conn, PORTAL_DESTINATION, PORTAL_PATH, SETTINGS_INTERFACE).ok()?;
    let value: OwnedValue = proxy
        .call("Read", &(APPEARANCE_NAMESPACE, "accent-color"))
        .ok()?;
    extract_accent_from_variant(crate::kwin::value_of_owned(&value))
}

#[cfg(target_os = "linux")]
fn extract_accent_from_variant(value: &Value<'_>) -> Option<AccentColor> {
    let inner: &Value<'_> = match value {
        Value::Value(b) => b.as_ref(),
        other => other,
    };
    let Value::Structure(structure) = inner else {
        return None;
    };
    let fields = structure.fields();
    if fields.len() < 3 {
        return None;
    }
    let r = double_from_value(&fields[0])?;
    let g = double_from_value(&fields[1])?;
    let b = double_from_value(&fields[2])?;
    classify_accent_color(r, g, b)
}

#[cfg(target_os = "linux")]
fn double_from_value(value: &Value<'_>) -> Option<f64> {
    match value {
        Value::F64(v) => Some(*v),
        Value::Value(b) => double_from_value(b.as_ref()),
        _ => None,
    }
}

#[cfg(not(target_os = "linux"))]
pub fn read_color_scheme() -> ColorScheme {
    ColorScheme::NoPreference
}

#[cfg(not(target_os = "linux"))]
pub fn read_contrast() -> Contrast {
    Contrast::NoPreference
}

#[cfg(not(target_os = "linux"))]
pub fn read_accent_color() -> Option<AccentColor> {
    None
}

#[cfg(target_os = "linux")]
type ChangeCallback = Arc<dyn Fn(ChangeEvent) + Send + Sync + 'static>;

#[cfg(target_os = "linux")]
pub struct Subscription {
    stop_flag: Arc<AtomicBool>,
    thread: Mutex<Option<JoinHandle<()>>>,
}

#[cfg(target_os = "linux")]
impl Subscription {
    pub fn new(callback: ChangeCallback) -> Result<Self, String> {
        let stop_flag = Arc::new(AtomicBool::new(false));
        let (ready_tx, ready_rx) = mpsc::sync_channel(1);
        let stop_for_thread = stop_flag.clone();
        let thread = thread::Builder::new()
            .name("fluxer-linux-portals-settings".to_string())
            .spawn(move || {
                let setup = future::block_on(async {
                    let conn = zbus::Connection::session().await?;
                    let rule = MatchRule::builder()
                        .msg_type(MessageType::Signal)
                        .interface(SETTINGS_INTERFACE)?
                        .member("SettingChanged")?
                        .build();
                    let stream = MessageStream::for_match_rule(rule, &conn, Some(32)).await?;
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
                        None::<zbus::Result<zbus::Message>>
                    };
                    match future::block_on(stream.next().or(timeout)) {
                        Some(Ok(message)) => {
                            if let Some(event) = parse_setting_changed(&message)
                                && event.namespace == APPEARANCE_NAMESPACE
                            {
                                callback(event);
                            }
                        }
                        Some(Err(_)) => break,
                        None => {}
                    }
                }
            })
            .map_err(|err| err.to_string())?;
        match ready_rx.recv_timeout(SIGNAL_THREAD_START_TIMEOUT) {
            Ok(Ok(())) => Ok(Self {
                stop_flag,
                thread: Mutex::new(Some(thread)),
            }),
            Ok(Err(err)) => {
                let _ = thread.join();
                Err(err)
            }
            Err(err) => {
                stop_flag.store(true, Ordering::Release);
                if matches!(err, mpsc::RecvTimeoutError::Disconnected) {
                    let _ = thread.join();
                }
                Err(err.to_string())
            }
        }
    }

    pub fn close(&self) {
        self.stop_flag.store(true, Ordering::Release);
        if let Ok(mut thread) = self.thread.lock()
            && let Some(t) = thread.take()
        {
            let _ = t.join();
        }
    }
}

#[cfg(target_os = "linux")]
impl Drop for Subscription {
    fn drop(&mut self) {
        self.close();
    }
}

#[cfg(target_os = "linux")]
fn parse_setting_changed(message: &zbus::Message) -> Option<ChangeEvent> {
    let body = message.body();
    let (namespace, key, value): (String, String, OwnedValue) = body.deserialize().ok()?;
    let value_ref = crate::kwin::value_of_owned(&value);
    let payload = classify_payload(&key, value_ref);
    Some(ChangeEvent {
        namespace,
        key,
        payload,
    })
}

#[cfg(target_os = "linux")]
fn classify_payload(_key: &str, value: &Value<'_>) -> ChangePayload {
    if let Some(n) = crate::kwin::integer_from_variant(value) {
        return ChangePayload::Uint32(n);
    }
    if let Some(accent) = extract_accent_from_variant(value) {
        return ChangePayload::Accent(accent);
    }
    ChangePayload::Unknown
}
