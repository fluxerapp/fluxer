// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::env;
use std::os::fd::OwnedFd;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, mpsc};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use futures_lite::{FutureExt, StreamExt, future};
use zbus::names::OwnedUniqueName;
use zbus::{
    MatchRule, MessageStream,
    blocking::{Connection as BlockingConnection, Proxy as BlockingProxy},
    message::Type as MessageType,
    zvariant::{OwnedObjectPath, OwnedValue, Value},
};

pub const PORTAL_DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PORTAL_PATH: &str = "/org/freedesktop/portal/desktop";
pub const SCREEN_CAST_INTERFACE: &str = "org.freedesktop.portal.ScreenCast";
pub const REQUEST_INTERFACE: &str = "org.freedesktop.portal.Request";
pub const SESSION_INTERFACE: &str = "org.freedesktop.portal.Session";
pub const PROPERTIES_INTERFACE: &str = "org.freedesktop.DBus.Properties";
pub const REGISTRY_INTERFACE: &str = "org.freedesktop.host.portal.Registry";

pub const REQUEST_TIMEOUT: Duration = Duration::from_secs(5 * 60);
const SIGNAL_POLL_INTERVAL: Duration = Duration::from_millis(200);
const MIN_PORTAL_VERSION: u32 = 4;
const PORTAL_APP_ID_ENV: &str = "FLUXER_LINUX_PORTAL_APP_ID";
const FLATPAK_INFO_PATH: &str = "/.flatpak-info";

pub const CURSOR_MODE_HIDDEN: u32 = 1;
pub const CURSOR_MODE_EMBEDDED: u32 = 2;
pub const CURSOR_MODE_METADATA: u32 = 4;
pub const SOURCE_TYPE_MONITOR: u32 = 1;
pub const SOURCE_TYPE_WINDOW: u32 = 2;
pub const SOURCE_TYPES_ALL: u32 = SOURCE_TYPE_MONITOR | SOURCE_TYPE_WINDOW;

static TOKEN_SEQ: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortalError {
    DbusError,
    PortalTimeout,
    InvalidReply,
    SendFailed,
    Cancelled,

    CursorModeUnavailable,
    PortalTooOld(u32),
    NoStreams,
}

impl std::fmt::Display for PortalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DbusError => f.write_str("DbusError"),
            Self::PortalTimeout => f.write_str("PortalTimeout"),
            Self::InvalidReply => f.write_str("InvalidReply"),
            Self::SendFailed => f.write_str("SendFailed"),
            Self::Cancelled => f.write_str("Cancelled"),
            Self::CursorModeUnavailable => f.write_str("CursorModeUnavailable"),
            Self::PortalTooOld(v) => write!(f, "PortalTooOld(version={v})"),
            Self::NoStreams => f.write_str("NoStreams"),
        }
    }
}

pub fn mint_token(prefix: &str) -> String {
    let seq = TOKEN_SEQ.fetch_add(1, Ordering::Relaxed);
    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0);
    format!("{prefix}_{ms:x}_{seq:x}")
}

pub fn request_path(unique_bus_name: &str, handle_token: &str) -> String {
    let trimmed = unique_bus_name.strip_prefix(':').unwrap_or(unique_bus_name);
    let mut out = String::with_capacity(40 + trimmed.len() + handle_token.len());
    out.push_str("/org/freedesktop/portal/desktop/request/");
    for ch in trimmed.chars() {
        out.push(if ch == '.' { '_' } else { ch });
    }
    out.push('/');
    out.push_str(handle_token);
    out
}

pub fn session_path(unique_bus_name: &str, session_token: &str) -> String {
    let trimmed = unique_bus_name.strip_prefix(':').unwrap_or(unique_bus_name);
    let mut out = String::with_capacity(40 + trimmed.len() + session_token.len());
    out.push_str("/org/freedesktop/portal/desktop/session/");
    for ch in trimmed.chars() {
        out.push(if ch == '.' { '_' } else { ch });
    }
    out.push('/');
    out.push_str(session_token);
    out
}

pub fn read_portal_version() -> Result<u32, PortalError> {
    let conn = blocking_session_conn()?;
    register_portal_app_id(&conn);
    let proxy = BlockingProxy::new(&conn, PORTAL_DESTINATION, PORTAL_PATH, PROPERTIES_INTERFACE)
        .map_err(|_| PortalError::DbusError)?;
    let value: OwnedValue = proxy
        .call("Get", &(SCREEN_CAST_INTERFACE, "version"))
        .map_err(|_| PortalError::DbusError)?;
    let v: &Value<'_> = &value;
    match v {
        Value::U32(n) => Ok(*n),
        Value::Value(inner) => match inner.as_ref() {
            Value::U32(n) => Ok(*n),
            _ => Err(PortalError::InvalidReply),
        },
        _ => Err(PortalError::InvalidReply),
    }
}

pub fn read_available_cursor_modes() -> Result<u32, PortalError> {
    let conn = blocking_session_conn()?;
    register_portal_app_id(&conn);
    let proxy = BlockingProxy::new(&conn, PORTAL_DESTINATION, PORTAL_PATH, PROPERTIES_INTERFACE)
        .map_err(|_| PortalError::DbusError)?;
    let value: OwnedValue = proxy
        .call("Get", &(SCREEN_CAST_INTERFACE, "AvailableCursorModes"))
        .map_err(|_| PortalError::DbusError)?;
    let v: &Value<'_> = &value;
    match v {
        Value::U32(n) => Ok(*n),
        Value::Value(inner) => match inner.as_ref() {
            Value::U32(n) => Ok(*n),
            _ => Err(PortalError::InvalidReply),
        },
        _ => Err(PortalError::InvalidReply),
    }
}

#[derive(Debug, Clone)]
pub struct StreamInfo {
    pub node_id: u32,
    pub source_type: u32,
    pub mapping_id: Option<String>,
    pub width: u32,
    pub height: u32,
    pub position_x: i32,
    pub position_y: i32,
}

#[derive(Debug, Clone)]
pub struct StartedSession {
    pub session_handle: String,
    pub streams: Vec<StreamInfo>,
}

pub struct LiveSession {
    pub handle: String,
    pub conn: BlockingConnection,
}

impl LiveSession {
    pub fn close(self) {
        let path = OwnedObjectPath::try_from(self.handle.as_str())
            .ok()
            .and_then(|p| {
                BlockingProxy::new(&self.conn, PORTAL_DESTINATION, p, SESSION_INTERFACE).ok()
            });
        if let Some(proxy) = path {
            let _ = proxy.call::<_, _, ()>("Close", &());
        }
    }
}

fn blocking_session_conn() -> Result<BlockingConnection, PortalError> {
    zbus::blocking::connection::Builder::session()
        .map_err(|_| PortalError::DbusError)?
        .method_timeout(Duration::from_secs(30))
        .build()
        .map_err(|_| PortalError::DbusError)
}

fn unique_name(conn: &BlockingConnection) -> Result<OwnedUniqueName, PortalError> {
    conn.unique_name()
        .ok_or(PortalError::DbusError)
        .map(|n| n.to_owned())
}

fn normalize_desktop_entry_app_id(value: &str) -> Option<String> {
    let trimmed = value.trim();
    let app_id = trimmed.strip_suffix(".desktop").unwrap_or(trimmed);
    if app_id.is_empty() || app_id.len() > 255 || app_id.starts_with('.') {
        return None;
    }
    let valid = app_id
        .bytes()
        .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'.' | b'-' | b'_'));
    valid.then(|| app_id.to_string())
}

fn configured_portal_app_id() -> Option<String> {
    if Path::new(FLATPAK_INFO_PATH).exists() {
        return None;
    }
    env::var(PORTAL_APP_ID_ENV)
        .ok()
        .and_then(|value| normalize_desktop_entry_app_id(&value))
}

fn register_portal_app_id(conn: &BlockingConnection) {
    let Some(app_id) = configured_portal_app_id() else {
        return;
    };
    let Ok(proxy) = BlockingProxy::new(conn, PORTAL_DESTINATION, PORTAL_PATH, REGISTRY_INTERFACE)
    else {
        return;
    };
    let options: HashMap<&str, Value<'_>> = HashMap::new();
    let _ = proxy.call::<_, _, ()>("Register", &(app_id.as_str(), options));
}

struct ResponseEnvelope {
    code: u32,
    results: HashMap<String, OwnedValue>,
}

struct PendingRequest {
    rx: mpsc::Receiver<ResponseEnvelope>,
    stop_flag: Arc<std::sync::atomic::AtomicBool>,
    listener: std::thread::JoinHandle<()>,
}

impl PendingRequest {
    fn wait(self) -> Result<ResponseEnvelope, PortalError> {
        let result = self
            .rx
            .recv_timeout(REQUEST_TIMEOUT)
            .map_err(|_| PortalError::PortalTimeout);
        self.stop_flag
            .store(true, std::sync::atomic::Ordering::Release);
        let _ = self.listener.join();
        result
    }
}

fn watch_request(
    conn: &BlockingConnection,
    expected_path: &str,
) -> Result<PendingRequest, PortalError> {
    let (tx, rx) = mpsc::sync_channel::<ResponseEnvelope>(1);
    let (ready_tx, ready_rx) = mpsc::sync_channel::<Result<(), ()>>(1);
    let stop_flag = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let stop_for_thread = stop_flag.clone();
    let expected_for_thread = expected_path.to_string();
    let conn_for_thread = conn.clone();
    let listener = std::thread::Builder::new()
        .name("fluxer-linux-screen-capture-req".to_string())
        .spawn(move || {
            response_listener(
                conn_for_thread,
                &expected_for_thread,
                tx,
                ready_tx,
                stop_for_thread,
            );
        })
        .map_err(|_| PortalError::DbusError)?;

    match ready_rx.recv_timeout(Duration::from_secs(5)) {
        Ok(Ok(())) => {}
        _ => {
            stop_flag.store(true, std::sync::atomic::Ordering::Release);
            let _ = listener.join();
            return Err(PortalError::DbusError);
        }
    }

    Ok(PendingRequest {
        rx,
        stop_flag,
        listener,
    })
}

fn response_listener(
    conn: BlockingConnection,
    expected_path: &str,
    tx: mpsc::SyncSender<ResponseEnvelope>,
    ready: mpsc::SyncSender<Result<(), ()>>,
    stop: Arc<std::sync::atomic::AtomicBool>,
) {
    let setup = future::block_on(async {
        let conn: zbus::Connection = conn.into();
        let rule = MatchRule::builder()
            .msg_type(MessageType::Signal)
            .interface(REQUEST_INTERFACE)?
            .member("Response")?
            .path(expected_path.to_string())?
            .build();
        let stream = MessageStream::for_match_rule(rule, &conn, Some(8)).await?;
        zbus::Result::Ok((conn, stream))
    });
    let (_conn, mut stream) = match setup {
        Ok(parts) => parts,
        Err(_) => {
            let _ = ready.send(Err(()));
            return;
        }
    };
    let _ = ready.send(Ok(()));
    while !stop.load(std::sync::atomic::Ordering::Acquire) {
        let timeout = async {
            async_io::Timer::after(SIGNAL_POLL_INTERVAL).await;
            None::<zbus::Result<zbus::Message>>
        };
        match future::block_on(stream.next().or(timeout)) {
            Some(Ok(message)) => {
                if let Some(env) = parse_response(&message) {
                    let _ = tx.send(env);
                    return;
                }
            }
            Some(Err(_)) => return,
            None => {}
        }
    }
}

fn parse_response(message: &zbus::Message) -> Option<ResponseEnvelope> {
    let body = message.body();
    let (code, results): (u32, HashMap<String, OwnedValue>) = body.deserialize().ok()?;
    Some(ResponseEnvelope { code, results })
}

fn value_of(v: &OwnedValue) -> &Value<'_> {
    use std::ops::Deref as _;
    v.deref()
}

fn unwrap_variant<'a>(v: &'a Value<'a>) -> &'a Value<'a> {
    match v {
        Value::Value(boxed) => boxed.as_ref(),
        other => other,
    }
}

fn read_u32(value: &Value<'_>) -> Option<u32> {
    match unwrap_variant(value) {
        Value::U32(n) => Some(*n),
        Value::U64(n) => Some(*n as u32),
        Value::I32(n) => Some(*n as u32),
        _ => None,
    }
}

fn read_i32(value: &Value<'_>) -> Option<i32> {
    match unwrap_variant(value) {
        Value::I32(n) => Some(*n),
        Value::U32(n) => Some(*n as i32),
        _ => None,
    }
}

fn read_str(value: &Value<'_>) -> Option<String> {
    match unwrap_variant(value) {
        Value::Str(s) => Some(s.as_str().to_string()),
        _ => None,
    }
}

fn read_pair_i32(value: &Value<'_>) -> Option<(i32, i32)> {
    let Value::Structure(s) = unwrap_variant(value) else {
        return None;
    };
    let fields = s.fields();
    if fields.len() < 2 {
        return None;
    }
    Some((read_i32(&fields[0])?, read_i32(&fields[1])?))
}

fn read_pair_u32(value: &Value<'_>) -> Option<(u32, u32)> {
    let Value::Structure(s) = unwrap_variant(value) else {
        return None;
    };
    let fields = s.fields();
    if fields.len() < 2 {
        return None;
    }
    Some((read_u32(&fields[0])?, read_u32(&fields[1])?))
}

fn parse_streams(results: &HashMap<String, OwnedValue>) -> Vec<StreamInfo> {
    let Some(raw) = results.get("streams") else {
        return Vec::new();
    };
    let val = value_of(raw);
    let inner = unwrap_variant(val);
    let Value::Array(arr) = inner else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for entry in arr.iter() {
        let entry_inner = unwrap_variant(entry);
        let Value::Structure(s) = entry_inner else {
            continue;
        };
        let fields = s.fields();
        if fields.len() < 2 {
            continue;
        }
        let Some(node_id) = read_u32(&fields[0]) else {
            continue;
        };
        let Value::Dict(dict) = unwrap_variant(&fields[1]) else {
            out.push(StreamInfo {
                node_id,
                source_type: 0,
                mapping_id: None,
                width: 0,
                height: 0,
                position_x: 0,
                position_y: 0,
            });
            continue;
        };
        let mut source_type = 0u32;
        let mut mapping_id: Option<String> = None;
        let mut size = (0u32, 0u32);
        let mut position = (0i32, 0i32);
        for (k, v) in dict.iter() {
            let Value::Str(key) = k else { continue };
            match key.as_str() {
                "source_type" => {
                    source_type = read_u32(v).unwrap_or(0);
                }
                "mapping_id" => {
                    mapping_id = read_str(v);
                }
                "size" => {
                    if let Some(p) = read_pair_u32(v) {
                        size = p;
                    }
                }
                "position" => {
                    if let Some(p) = read_pair_i32(v) {
                        position = p;
                    }
                }
                _ => {}
            }
        }
        out.push(StreamInfo {
            node_id,
            source_type,
            mapping_id,
            width: size.0,
            height: size.1,
            position_x: position.0,
            position_y: position.1,
        });
    }
    out
}

fn cursor_mode_matches(results: &HashMap<String, OwnedValue>, expected: u32) -> bool {
    let Some(raw) = results.get("cursor_mode") else {
        return true;
    };
    matches!(read_u32(value_of(raw)), Some(mode) if mode == expected)
}

pub fn open_session_and_pick() -> Result<(LiveSession, Vec<StreamInfo>), PortalError> {
    let version = read_portal_version()?;
    if version < MIN_PORTAL_VERSION {
        return Err(PortalError::PortalTooOld(version));
    }
    let available_cursor_modes = read_available_cursor_modes().unwrap_or(0);
    if available_cursor_modes & CURSOR_MODE_HIDDEN == 0 {
        return Err(PortalError::CursorModeUnavailable);
    }

    let conn = blocking_session_conn()?;
    register_portal_app_id(&conn);
    let unique = unique_name(&conn)?;
    let unique_str = unique.as_str().to_string();

    let proxy = BlockingProxy::new(
        &conn,
        PORTAL_DESTINATION,
        PORTAL_PATH,
        SCREEN_CAST_INTERFACE,
    )
    .map_err(|_| PortalError::DbusError)?;

    let create_token = mint_token("fluxer_sc_create");
    let session_token = mint_token("fluxer_sc_session");
    let create_request_path = request_path(&unique_str, &create_token);
    let expected_session_path = session_path(&unique_str, &session_token);
    let create_pending = watch_request(&conn, &create_request_path)?;

    let mut create_opts: HashMap<&str, Value<'_>> = HashMap::new();
    create_opts.insert("handle_token", Value::new(create_token.as_str()));
    create_opts.insert("session_handle_token", Value::new(session_token.as_str()));
    let reply_path: OwnedObjectPath = proxy
        .call("CreateSession", &(create_opts,))
        .map_err(|_| PortalError::SendFailed)?;
    if !reply_path.as_str().is_empty() && reply_path.as_str() != create_request_path {
        return Err(PortalError::InvalidReply);
    }
    let envelope = create_pending.wait()?;
    if envelope.code != 0 {
        return Err(PortalError::Cancelled);
    }
    let returned_session_handle = envelope
        .results
        .get("session_handle")
        .and_then(|v| read_str(value_of(v)))
        .ok_or(PortalError::InvalidReply)?;
    if returned_session_handle != expected_session_path {
        return Err(PortalError::InvalidReply);
    }
    let session_handle = returned_session_handle;

    let select_token = mint_token("fluxer_sc_select");
    let select_request_path = request_path(&unique_str, &select_token);
    let select_pending = watch_request(&conn, &select_request_path)?;
    let mut select_opts: HashMap<&str, Value<'_>> = HashMap::new();
    select_opts.insert("handle_token", Value::new(select_token.as_str()));
    select_opts.insert("types", Value::new(SOURCE_TYPES_ALL));
    select_opts.insert("multiple", Value::new(false));
    select_opts.insert("cursor_mode", Value::new(CURSOR_MODE_HIDDEN));
    let session_obj = OwnedObjectPath::try_from(session_handle.as_str())
        .map_err(|_| PortalError::InvalidReply)?;
    let select_reply: OwnedObjectPath = proxy
        .call("SelectSources", &(&session_obj, select_opts))
        .map_err(|_| PortalError::SendFailed)?;
    if !select_reply.as_str().is_empty() && select_reply.as_str() != select_request_path {
        return Err(PortalError::InvalidReply);
    }
    let select_envelope = select_pending.wait()?;
    if select_envelope.code != 0 {
        return Err(PortalError::Cancelled);
    }
    if !cursor_mode_matches(&select_envelope.results, CURSOR_MODE_HIDDEN) {
        return Err(PortalError::CursorModeUnavailable);
    }

    let start_token = mint_token("fluxer_sc_start");
    let start_request_path = request_path(&unique_str, &start_token);
    let start_pending = watch_request(&conn, &start_request_path)?;
    let mut start_opts: HashMap<&str, Value<'_>> = HashMap::new();
    start_opts.insert("handle_token", Value::new(start_token.as_str()));
    let start_reply: OwnedObjectPath = proxy
        .call("Start", &(&session_obj, "", start_opts))
        .map_err(|_| PortalError::SendFailed)?;
    if !start_reply.as_str().is_empty() && start_reply.as_str() != start_request_path {
        return Err(PortalError::InvalidReply);
    }
    let start_envelope = start_pending.wait()?;
    if start_envelope.code != 0 {
        return Err(PortalError::Cancelled);
    }
    if !cursor_mode_matches(&start_envelope.results, CURSOR_MODE_HIDDEN) {
        return Err(PortalError::CursorModeUnavailable);
    }
    let streams = parse_streams(&start_envelope.results);
    if streams.is_empty() {
        return Err(PortalError::NoStreams);
    }
    Ok((
        LiveSession {
            handle: session_handle,
            conn,
        },
        streams,
    ))
}

pub fn open_pipewire_remote(session: &LiveSession) -> Result<OwnedFd, PortalError> {
    let proxy = BlockingProxy::new(
        &session.conn,
        PORTAL_DESTINATION,
        PORTAL_PATH,
        SCREEN_CAST_INTERFACE,
    )
    .map_err(|_| PortalError::DbusError)?;
    let session_obj = OwnedObjectPath::try_from(session.handle.as_str())
        .map_err(|_| PortalError::InvalidReply)?;
    let opts: HashMap<&str, Value<'_>> = HashMap::new();
    let fd: zbus::zvariant::OwnedFd = proxy
        .call("OpenPipeWireRemote", &(&session_obj, opts))
        .map_err(|_| PortalError::SendFailed)?;
    Ok(OwnedFd::from(fd))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn request_path_sanitizes_unique_bus_name() {
        assert_eq!(
            request_path(":1.42", "fluxer_sc_create_1"),
            "/org/freedesktop/portal/desktop/request/1_42/fluxer_sc_create_1"
        );
    }

    #[test]
    fn session_path_sanitizes_unique_bus_name() {
        assert_eq!(
            session_path(":1.42", "fluxer_sc_session_1"),
            "/org/freedesktop/portal/desktop/session/1_42/fluxer_sc_session_1"
        );
    }

    #[test]
    fn mint_token_is_distinct_and_prefixed() {
        let a = mint_token("fluxer_sc_create");
        let b = mint_token("fluxer_sc_create");
        assert_ne!(a, b);
        assert!(a.starts_with("fluxer_sc_create_"));
    }

    #[test]
    fn cursor_mode_constants_match_portal_spec() {
        assert_eq!(CURSOR_MODE_HIDDEN, 1);
        assert_eq!(CURSOR_MODE_EMBEDDED, 2);
        assert_eq!(CURSOR_MODE_METADATA, 4);
    }

    #[test]
    fn source_type_mask_combines_monitor_and_window() {
        assert_eq!(SOURCE_TYPES_ALL, 3);
        assert_eq!(SOURCE_TYPE_MONITOR | SOURCE_TYPE_WINDOW, SOURCE_TYPES_ALL);
    }

    #[test]
    fn normalize_desktop_entry_app_id_accepts_fluxer_ids() {
        assert_eq!(
            normalize_desktop_entry_app_id("fluxer-canary"),
            Some("fluxer-canary".to_string())
        );
        assert_eq!(
            normalize_desktop_entry_app_id("fluxer-canary.desktop"),
            Some("fluxer-canary".to_string())
        );
        assert_eq!(
            normalize_desktop_entry_app_id("app.fluxer.canary"),
            Some("app.fluxer.canary".to_string())
        );
    }

    #[test]
    fn normalize_desktop_entry_app_id_rejects_paths_and_empty_values() {
        assert_eq!(normalize_desktop_entry_app_id(""), None);
        assert_eq!(normalize_desktop_entry_app_id(".hidden"), None);
        assert_eq!(normalize_desktop_entry_app_id("../fluxer-canary"), None);
        assert_eq!(normalize_desktop_entry_app_id("fluxer canary"), None);
    }

    #[test]
    fn cursor_mode_matches_treats_missing_key_as_honoured() {
        let map: HashMap<String, OwnedValue> = HashMap::new();
        assert!(cursor_mode_matches(&map, CURSOR_MODE_HIDDEN));
    }
}
