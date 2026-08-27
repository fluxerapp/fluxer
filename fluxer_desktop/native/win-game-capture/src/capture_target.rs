// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ptr::{null, null_mut};

use windows_sys::Win32::{
    Foundation::{HWND, LPARAM, RECT},
    Graphics::Gdi::{
        EnumDisplayMonitors, GetMonitorInfoW, HMONITOR, MONITOR_DEFAULTTONEAREST, MONITORINFO,
        MonitorFromRect,
    },
    System::Threading::GetCurrentProcessId,
    UI::WindowsAndMessaging::{
        EnumWindows, GW_OWNER, GWL_STYLE, GetForegroundWindow, GetWindow, GetWindowLongPtrW,
        GetWindowRect, GetWindowThreadProcessId, IsWindow, IsWindowVisible, WS_BORDER, WS_MAXIMIZE,
    },
};

const MONITOR_LIMIT: usize = 16;

fn parse_hwnd_source_id(source_id: &str) -> Option<HWND> {
    let token = source_id.strip_prefix("window:")?.split(':').next()?;
    let value = if let Some(hex) = token
        .strip_prefix("0x")
        .or_else(|| token.strip_prefix("0X"))
    {
        isize::from_str_radix(hex, 16).ok()?
    } else {
        token.parse::<isize>().ok()?
    };
    let hwnd = value as HWND;
    (unsafe { IsWindow(hwnd) } != 0).then_some(hwnd)
}

fn parse_screen_ordinal(source_id: &str) -> Option<usize> {
    let token = source_id.strip_prefix("screen:")?.split(':').next()?;
    token.parse::<usize>().ok()
}

pub(crate) fn resolve_game_capture_target(
    source_id: &str,
    source_kind: &str,
) -> Result<HWND, String> {
    if let Some(hwnd) = parse_hwnd_source_id(source_id) {
        return Ok(hwnd);
    }
    if source_kind != "game" && source_kind != "screen" {
        return Err(format!(
            "invalid game capture source: {source_kind}:{source_id}"
        ));
    }
    let monitor = monitor_for_screen_source(source_id)?;
    find_fullscreen_window_on_monitor(monitor)
        .or_else(find_foreground_fullscreen_window)
        .or_else(find_fullscreen_window_on_any_monitor)
        .ok_or_else(|| "no fullscreen game window found on selected display".to_string())
}

fn monitor_for_screen_source(source_id: &str) -> Result<HMONITOR, String> {
    let ordinal = parse_screen_ordinal(source_id).unwrap_or(0);
    let monitors = enumerate_monitors();
    monitors
        .get(ordinal)
        .copied()
        .or_else(|| monitors.first().copied())
        .ok_or_else(|| "no monitors available for game capture".to_string())
}

fn enumerate_monitors() -> Vec<HMONITOR> {
    unsafe extern "system" fn enum_monitor(
        monitor: HMONITOR,
        _hdc: windows_sys::Win32::Graphics::Gdi::HDC,
        _rect: *mut RECT,
        param: LPARAM,
    ) -> i32 {
        let monitors = &mut *(param as *mut Vec<HMONITOR>);
        if monitors.len() >= MONITOR_LIMIT {
            return 0;
        }
        monitors.push(monitor);
        1
    }
    let mut monitors = Vec::with_capacity(MONITOR_LIMIT);
    unsafe {
        EnumDisplayMonitors(
            null_mut(),
            null(),
            Some(enum_monitor),
            &mut monitors as *mut _ as LPARAM,
        );
    }
    assert!(monitors.len() <= MONITOR_LIMIT, "monitor targets bounded");
    monitors
}

pub(crate) fn monitor_rect(monitor: HMONITOR) -> Option<RECT> {
    let mut info = MONITORINFO {
        cbSize: std::mem::size_of::<MONITORINFO>() as u32,
        rcMonitor: RECT::default(),
        rcWork: RECT::default(),
        dwFlags: 0,
    };
    if unsafe { GetMonitorInfoW(monitor, &mut info) } == 0 {
        return None;
    }
    Some(info.rcMonitor)
}

fn rect_matches_monitor(window_rect: &RECT, monitor_rect: &RECT) -> bool {
    const TOLERANCE: i32 = 2;
    (window_rect.left - monitor_rect.left).abs() <= TOLERANCE
        && (window_rect.top - monitor_rect.top).abs() <= TOLERANCE
        && (window_rect.right - monitor_rect.right).abs() <= TOLERANCE
        && (window_rect.bottom - monitor_rect.bottom).abs() <= TOLERANCE
}

fn is_regular_maximized_window(hwnd: HWND) -> bool {
    let style = unsafe { GetWindowLongPtrW(hwnd, GWL_STYLE) } as u32;
    (style & WS_MAXIMIZE) != 0 && (style & WS_BORDER) != 0
}

fn is_fullscreen_window_on_monitor(hwnd: HWND, monitor: HMONITOR, monitor_rect: &RECT) -> bool {
    if unsafe { IsWindowVisible(hwnd) } == 0 {
        return false;
    }
    if !unsafe { GetWindow(hwnd, GW_OWNER) }.is_null() {
        return false;
    }
    let mut pid = 0u32;
    unsafe {
        GetWindowThreadProcessId(hwnd, &mut pid);
    }
    if pid == 0 || pid == unsafe { GetCurrentProcessId() } {
        return false;
    }
    if is_regular_maximized_window(hwnd) {
        return false;
    }
    let mut rect = RECT::default();
    if unsafe { GetWindowRect(hwnd, &mut rect) } == 0 {
        return false;
    }
    let window_monitor = unsafe { MonitorFromRect(&rect, MONITOR_DEFAULTTONEAREST) };
    window_monitor == monitor && rect_matches_monitor(&rect, monitor_rect)
}

fn find_foreground_fullscreen_window() -> Option<HWND> {
    let hwnd = unsafe { GetForegroundWindow() };
    if hwnd.is_null() {
        return None;
    }
    let mut rect = RECT::default();
    if unsafe { GetWindowRect(hwnd, &mut rect) } == 0 {
        return None;
    }
    let monitor = unsafe { MonitorFromRect(&rect, MONITOR_DEFAULTTONEAREST) };
    let monitor_rect = monitor_rect(monitor)?;
    is_fullscreen_window_on_monitor(hwnd, monitor, &monitor_rect).then_some(hwnd)
}

fn find_fullscreen_window_on_any_monitor() -> Option<HWND> {
    for monitor in enumerate_monitors() {
        if let Some(hwnd) = find_fullscreen_window_on_monitor(monitor) {
            return Some(hwnd);
        }
    }
    None
}

fn find_fullscreen_window_on_monitor(monitor: HMONITOR) -> Option<HWND> {
    struct Search {
        monitor: HMONITOR,
        monitor_rect: RECT,
        result: HWND,
        own_pid: u32,
    }
    unsafe extern "system" fn enum_window(hwnd: HWND, param: LPARAM) -> i32 {
        let search = &mut *(param as *mut Search);
        let mut pid = 0u32;
        GetWindowThreadProcessId(hwnd, &mut pid);
        if pid == 0 || pid == search.own_pid {
            return 1;
        }
        if !is_fullscreen_window_on_monitor(hwnd, search.monitor, &search.monitor_rect) {
            return 1;
        }
        search.result = hwnd;
        0
    }

    let monitor_rect = monitor_rect(monitor)?;
    let mut search = Search {
        monitor,
        monitor_rect,
        result: null_mut(),
        own_pid: unsafe { GetCurrentProcessId() },
    };
    unsafe {
        let _ = EnumWindows(Some(enum_window), &mut search as *mut _ as LPARAM);
    }
    (!search.result.is_null()).then_some(search.result)
}
