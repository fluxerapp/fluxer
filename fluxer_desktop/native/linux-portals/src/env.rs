// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DesktopSession {
    Kde,
    Gnome,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayServer {
    X11,
    Wayland,
    WaylandWithXwayland,
    Unknown,
}

pub fn has_dbus_session() -> bool {
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

impl DisplayServer {
    pub fn x11_reachable(self) -> bool {
        matches!(self, Self::X11 | Self::WaylandWithXwayland)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WindowPidBackend {
    Kwin,
    GnomeShellEval,
    X11,
}

pub fn detect_desktop_session() -> DesktopSession {
    detect_desktop_session_from(
        std::env::var("XDG_CURRENT_DESKTOP").ok().as_deref(),
        std::env::var("XDG_SESSION_DESKTOP").ok().as_deref(),
        std::env::var("DESKTOP_SESSION").ok().as_deref(),
    )
}

fn detect_desktop_session_from(
    xdg_current_desktop: Option<&str>,
    xdg_session_desktop: Option<&str>,
    desktop_session: Option<&str>,
) -> DesktopSession {
    let candidates = [xdg_current_desktop, xdg_session_desktop, desktop_session];
    for raw in candidates.into_iter().flatten() {
        for token in raw.split(':') {
            let token = token.trim().to_ascii_lowercase();
            match token.as_str() {
                "kde" | "plasma" | "kde-plasma" => return DesktopSession::Kde,
                "gnome" | "gnome-classic" | "gnome-xorg" | "ubuntu" | "pop" => {
                    return DesktopSession::Gnome;
                }
                "sway" | "hyprland" | "wlroots" | "cosmic" | "wayfire" | "river" | "niri" => {
                    return DesktopSession::Other;
                }
                _ => {}
            }
        }
    }
    DesktopSession::Other
}

pub fn detect_display_server() -> DisplayServer {
    detect_display_server_from(
        std::env::var("XDG_SESSION_TYPE").ok().as_deref(),
        std::env::var("DISPLAY").ok().as_deref(),
        std::env::var("WAYLAND_DISPLAY").ok().as_deref(),
    )
}

fn detect_display_server_from(
    xdg_session_type: Option<&str>,
    display: Option<&str>,
    wayland_display: Option<&str>,
) -> DisplayServer {
    let has_x11 = display.is_some_and(|v| !v.is_empty());
    let has_wayland = wayland_display.is_some_and(|v| !v.is_empty());
    match (has_x11, has_wayland) {
        (true, true) => DisplayServer::WaylandWithXwayland,
        (true, false) => DisplayServer::X11,
        (false, true) => DisplayServer::Wayland,
        (false, false) => match xdg_session_type {
            Some("x11") => DisplayServer::X11,
            Some("wayland") => DisplayServer::Wayland,
            _ => DisplayServer::Unknown,
        },
    }
}

pub fn window_pid_backend_precedence() -> Vec<WindowPidBackend> {
    backend_precedence_for(
        detect_desktop_session(),
        detect_display_server(),
        has_dbus_session(),
    )
}

fn backend_precedence_for(
    session: DesktopSession,
    display: DisplayServer,
    dbus_available: bool,
) -> Vec<WindowPidBackend> {
    let mut out = Vec::with_capacity(3);
    match session {
        DesktopSession::Kde => {
            if dbus_available {
                out.push(WindowPidBackend::Kwin);
            }
            if display.x11_reachable() {
                out.push(WindowPidBackend::X11);
            }
        }
        DesktopSession::Gnome => {
            if dbus_available {
                out.push(WindowPidBackend::GnomeShellEval);
            }
            if display.x11_reachable() {
                out.push(WindowPidBackend::X11);
            }
        }
        DesktopSession::Other => {
            if dbus_available {
                out.push(WindowPidBackend::Kwin);
                out.push(WindowPidBackend::GnomeShellEval);
            }
            if display.x11_reachable() {
                out.push(WindowPidBackend::X11);
            }
        }
    }
    out
}
