// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(target_os = "linux")]
use std::time::Duration;

#[cfg(target_os = "linux")]
pub const GNOME_SHELL_DESTINATION: &str = "org.gnome.Shell";
#[cfg(target_os = "linux")]
pub const GNOME_SHELL_PATH: &str = "/org/gnome/Shell";
#[cfg(target_os = "linux")]
pub const GNOME_SHELL_INTERFACE: &str = "org.gnome.Shell";
#[cfg(target_os = "linux")]
pub const GNOME_SHELL_EVAL_TIMEOUT: Duration = Duration::from_millis(1_500);

pub fn is_safe_shell_eval_token(token: &str) -> bool {
    if token.is_empty() || token.len() > 128 {
        return false;
    }
    token
        .bytes()
        .all(|ch| ch.is_ascii_alphanumeric() || ch == b'_')
}

pub fn is_gnome_eval_disabled_via_env() -> bool {
    match std::env::var("FLUXER_PORTALS_GNOME_EVAL") {
        Ok(v) => {
            let lower = v.to_ascii_lowercase();
            lower == "0" || lower == "false" || lower == "no"
        }
        Err(_) => false,
    }
}

pub fn parse_shell_eval_pid_payload(payload: &str) -> Option<u32> {
    let trimmed = payload.trim();
    if trimmed.is_empty() {
        return None;
    }
    let stripped = trimmed
        .trim_start_matches(['[', ' ', '\t'])
        .trim_end_matches([']', ' ', '\t', ','])
        .trim();
    if stripped.is_empty() {
        return None;
    }
    let mut end = 0;
    for (i, ch) in stripped.char_indices() {
        if ch.is_ascii_digit() {
            end = i + ch.len_utf8();
        } else {
            break;
        }
    }
    if end == 0 {
        return None;
    }
    let number: u64 = stripped[..end].parse().ok()?;
    if number == 0 || number > u32::MAX as u64 {
        return None;
    }
    Some(number as u32)
}

pub fn build_window_pid_script(token: &str) -> String {
    format!(
        "global.get_window_actors().map(a=>a.meta_window).filter(w=>w.get_id&&w.get_id().toString()===\"{token}\").map(w=>w.get_pid())[0]"
    )
}

#[cfg(target_os = "linux")]
pub fn resolve_gnome_shell_window_pid(token: &str) -> Result<Option<u32>, String> {
    if is_gnome_eval_disabled_via_env() {
        return Ok(None);
    }
    if !is_safe_shell_eval_token(token) {
        return Err("resolveWindowPid: token failed validation".into());
    }
    let conn = zbus::blocking::connection::Builder::session()
        .map_err(|err| format!("openSessionBus failed: {err}"))?
        .method_timeout(GNOME_SHELL_EVAL_TIMEOUT)
        .build()
        .map_err(|err| format!("openSessionBus failed: {err}"))?;
    let proxy = zbus::blocking::Proxy::new(
        &conn,
        GNOME_SHELL_DESTINATION,
        GNOME_SHELL_PATH,
        GNOME_SHELL_INTERFACE,
    )
    .map_err(|err| format!("shell Eval failed: {err}"))?;
    let script = build_window_pid_script(token);
    let (success, payload): (bool, String) = proxy
        .call("Eval", &(script.as_str(),))
        .map_err(|err| format!("shell Eval failed: {err}"))?;
    if !success {
        return Ok(None);
    }
    Ok(parse_shell_eval_pid_payload(&payload))
}

#[cfg(not(target_os = "linux"))]
pub fn resolve_gnome_shell_window_pid(_token: &str) -> Result<Option<u32>, String> {
    Err("not supported on this platform".into())
}
