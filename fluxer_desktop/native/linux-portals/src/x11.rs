// SPDX-License-Identifier: AGPL-3.0-or-later

use std::os::raw::c_long;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveError {
    InvalidToken,
    LibX11Unavailable,
    MissingSymbol,
    DisplayUnavailable,
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::InvalidToken => "InvalidToken",
            Self::LibX11Unavailable => "LibX11Unavailable",
            Self::MissingSymbol => "MissingSymbol",
            Self::DisplayUnavailable => "DisplayUnavailable",
        };
        f.write_str(s)
    }
}

pub fn parse_window_token(token: &str) -> Option<u32> {
    if token.is_empty() {
        return None;
    }
    let parsed: u64 = if let Some(stripped) = token
        .strip_prefix("0x")
        .or_else(|| token.strip_prefix("0X"))
    {
        u64::from_str_radix(stripped, 16).ok()?
    } else {
        token.parse::<u64>().ok()?
    };
    if parsed == 0 {
        return None;
    }
    u32::try_from(parsed).ok()
}

pub fn pid_from_long(value: c_long) -> Option<u32> {
    if value <= 0 {
        return None;
    }
    if (value as u64) > u32::MAX as u64 {
        return None;
    }
    Some(value as u32)
}

#[cfg(target_os = "linux")]
pub fn resolve_x11_window_pid(token: &str) -> Result<Option<u32>, ResolveError> {
    use x11rb::protocol::xproto::{AtomEnum, ConnectionExt};
    use x11rb::rust_connection::RustConnection;

    let window = parse_window_token(token).ok_or(ResolveError::InvalidToken)?;
    let (conn, _screen) =
        RustConnection::connect(None).map_err(|_| ResolveError::DisplayUnavailable)?;

    let atom_cookie = conn
        .intern_atom(true, b"_NET_WM_PID")
        .map_err(|_| ResolveError::DisplayUnavailable)?;
    let atom = atom_cookie
        .reply()
        .map_err(|_| ResolveError::DisplayUnavailable)?
        .atom;
    if atom == 0 {
        return Ok(None);
    }

    let reply = conn
        .get_property(false, window, atom, AtomEnum::CARDINAL, 0, 1)
        .map_err(|_| ResolveError::DisplayUnavailable)?
        .reply()
        .map_err(|_| ResolveError::DisplayUnavailable)?;

    if reply.type_ != u32::from(AtomEnum::CARDINAL) || reply.format != 32 || reply.value_len < 1 {
        return Ok(None);
    }

    let Some(values) = reply.value32() else {
        return Ok(None);
    };
    let pid = values.collect::<Vec<u32>>();
    let Some(&first) = pid.first() else {
        return Ok(None);
    };
    Ok(pid_from_long(first as c_long))
}

#[cfg(not(target_os = "linux"))]
pub fn resolve_x11_window_pid(_token: &str) -> Result<Option<u32>, ResolveError> {
    Err(ResolveError::LibX11Unavailable)
}
