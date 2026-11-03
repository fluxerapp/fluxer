// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(target_os = "linux")]
use std::time::Duration;

#[cfg(target_os = "linux")]
use zbus::{
    blocking::{Connection, Proxy},
    zvariant::{OwnedValue, Value},
};

#[cfg(target_os = "linux")]
pub(crate) fn value_of_owned(value: &OwnedValue) -> &Value<'_> {
    use std::ops::Deref as _;
    value.deref()
}

pub const KWIN_DESTINATION: &str = "org.kde.KWin";
pub const KWIN_WINDOW_INTERFACE: &str = "org.kde.KWin.Window";
pub const PROPERTIES_INTERFACE: &str = "org.freedesktop.DBus.Properties";
#[cfg(target_os = "linux")]
pub const REQUEST_TIMEOUT: Duration = Duration::from_millis(1_500);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveError {
    InvalidToken,
    DbusOpenFailed,
    DbusCallFailed,
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::InvalidToken => "InvalidToken",
            Self::DbusOpenFailed => "DbusOpenFailed",
            Self::DbusCallFailed => "DbusCallFailed",
        };
        f.write_str(s)
    }
}

pub fn is_safe_kwin_path_segment(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    token
        .bytes()
        .all(|ch| ch.is_ascii_alphanumeric() || ch == b'_')
}

pub fn build_kwin_window_path(token: &str) -> String {
    let mut out = String::with_capacity("/org/kde/KWin/Window/".len() + token.len());
    out.push_str("/org/kde/KWin/Window/");
    out.push_str(token);
    out
}

#[cfg(target_os = "linux")]
pub fn resolve_kwin_window_pid(token: &str) -> Result<Option<u32>, ResolveError> {
    if !is_safe_kwin_path_segment(token) {
        return Err(ResolveError::InvalidToken);
    }
    let conn = zbus::blocking::connection::Builder::session()
        .map_err(|_| ResolveError::DbusOpenFailed)?
        .method_timeout(REQUEST_TIMEOUT)
        .build()
        .map_err(|_| ResolveError::DbusOpenFailed)?;
    Ok(resolve_kwin_window_pid_on(&conn, token))
}

#[cfg(target_os = "linux")]
fn resolve_kwin_window_pid_on(conn: &Connection, token: &str) -> Option<u32> {
    let path = build_kwin_window_path(token);
    let proxy = Proxy::new(conn, KWIN_DESTINATION, path.as_str(), PROPERTIES_INTERFACE).ok()?;
    let reply: OwnedValue = proxy.call("Get", &(KWIN_WINDOW_INTERFACE, "pid")).ok()?;
    integer_from_variant(value_of_owned(&reply))
}

#[cfg(target_os = "linux")]
pub(crate) fn integer_from_variant(value: &Value<'_>) -> Option<u32> {
    let raw: i64 = match value {
        Value::U8(v) => *v as i64,
        Value::U16(v) => *v as i64,
        Value::U32(v) => *v as i64,
        Value::U64(v) => *v as i64,
        Value::I16(v) => *v as i64,
        Value::I32(v) => *v as i64,
        Value::I64(v) => *v,
        Value::Value(inner) => return integer_from_variant(inner),
        _ => return None,
    };
    if raw <= 0 || raw > u32::MAX as i64 {
        return None;
    }
    Some(raw as u32)
}

#[cfg(not(target_os = "linux"))]
pub fn resolve_kwin_window_pid(_token: &str) -> Result<Option<u32>, ResolveError> {
    Err(ResolveError::DbusOpenFailed)
}
