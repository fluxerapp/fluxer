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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_pure_x11_from_display_only() {
        assert_eq!(
            detect_from(Some("x11"), Some(":0"), false),
            DisplayServer::X11
        );
        assert_eq!(detect_from(None, Some(":0"), false), DisplayServer::X11);
    }

    #[test]
    fn x11_session_type_wins_over_leaked_wayland_display() {
        assert_eq!(
            detect_from(Some("x11"), Some(":0"), true),
            DisplayServer::X11
        );
    }

    #[test]
    fn detect_pure_wayland_from_socket_only() {
        assert_eq!(
            detect_from(Some("wayland"), None, true),
            DisplayServer::Wayland
        );
        assert_eq!(detect_from(None, None, true), DisplayServer::Wayland);
    }

    #[test]
    fn detect_xwayland_when_socket_and_display_present() {
        let ds = detect_from(None, Some(":0"), true);
        assert_eq!(ds, DisplayServer::WaylandWithXwayland);
        assert!(!ds.supports_global_xrecord());
    }

    #[test]
    fn detect_xwayland_from_wayland_session_with_display_only() {
        let ds = detect_from(Some("wayland"), Some(":0"), false);
        assert_eq!(ds, DisplayServer::WaylandWithXwayland);
        assert!(!ds.supports_global_xrecord());
    }

    #[test]
    fn missing_wayland_socket_without_session_type_is_x11() {
        assert_eq!(
            detect_from(Some("tty"), Some(":0"), false),
            DisplayServer::X11
        );
    }

    #[test]
    fn detect_unknown_when_nothing_set() {
        assert_eq!(detect_from(None, None, false), DisplayServer::Unknown);
        assert_eq!(
            detect_from(Some("tty"), None, false),
            DisplayServer::Unknown
        );
    }

    #[test]
    fn detect_empty_display_var_is_ignored() {
        assert_eq!(
            detect_from(Some("wayland"), Some(""), true),
            DisplayServer::Wayland
        );
    }

    #[test]
    fn xdg_session_type_without_sockets() {
        assert_eq!(detect_from(Some("x11"), None, false), DisplayServer::X11);
        assert_eq!(
            detect_from(Some("wayland"), None, false),
            DisplayServer::Wayland
        );
    }

    #[test]
    fn wayland_socket_path_resolves_relative_names_under_runtime_dir() {
        assert_eq!(
            wayland_socket_path(
                Some(OsStr::new("wayland-0")),
                Some(OsStr::new("/run/user/1000"))
            ),
            Some(PathBuf::from("/run/user/1000/wayland-0"))
        );
        assert_eq!(
            wayland_socket_path(Some(OsStr::new("/tmp/wl")), None),
            Some(PathBuf::from("/tmp/wl"))
        );
        assert_eq!(
            wayland_socket_path(Some(OsStr::new("wayland-0")), None),
            None
        );
        assert_eq!(
            wayland_socket_path(Some(OsStr::new("")), Some(OsStr::new("/run"))),
            None
        );
        assert_eq!(wayland_socket_path(None, Some(OsStr::new("/run"))), None);
    }

    #[test]
    fn global_xrecord_support_is_only_for_pure_x11() {
        assert!(DisplayServer::X11.supports_global_xrecord());
        assert!(!DisplayServer::WaylandWithXwayland.supports_global_xrecord());
        assert!(!DisplayServer::Wayland.supports_global_xrecord());
        assert!(!DisplayServer::Unknown.supports_global_xrecord());
    }
}
