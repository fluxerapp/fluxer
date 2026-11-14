// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::OsStr;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayServer {
    X11,

    Wayland,

    WaylandWithXwayland,

    Unknown,
}

impl DisplayServer {
    pub fn supports_global_xrecord(self) -> bool {
        matches!(self, Self::X11)
    }
}

pub fn detect_display_server() -> DisplayServer {
    let wayland_socket = wayland_socket_path(
        std::env::var_os("WAYLAND_DISPLAY").as_deref(),
        std::env::var_os("XDG_RUNTIME_DIR").as_deref(),
    );
    detect_from(
        std::env::var("XDG_SESSION_TYPE").ok().as_deref(),
        std::env::var("DISPLAY").ok().as_deref(),
        wayland_socket.is_some_and(|path| path.exists()),
    )
}

fn wayland_socket_path(
    wayland_display: Option<&OsStr>,
    xdg_runtime_dir: Option<&OsStr>,
) -> Option<PathBuf> {
    let display = Path::new(wayland_display.filter(|value| !value.is_empty())?);
    if display.is_absolute() {
        return Some(display.to_path_buf());
    }
    let runtime_dir = xdg_runtime_dir.filter(|value| !value.is_empty())?;
    Some(Path::new(runtime_dir).join(display))
}

fn detect_from(
    xdg_session_type: Option<&str>,
    display: Option<&str>,
    wayland_socket_exists: bool,
) -> DisplayServer {
    let has_x11 = display.is_some_and(|v| !v.is_empty());
    let is_wayland = match xdg_session_type {
        Some("x11") => return DisplayServer::X11,
        Some("wayland") => true,
        _ => wayland_socket_exists,
    };
    match (is_wayland, has_x11) {
        (true, true) => DisplayServer::WaylandWithXwayland,
        (true, false) => DisplayServer::Wayland,
        (false, true) => DisplayServer::X11,
        (false, false) => DisplayServer::Unknown,
    }
}
