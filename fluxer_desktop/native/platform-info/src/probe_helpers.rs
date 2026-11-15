// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn vendor_name(vendor_id: u32) -> Option<&'static str> {
    match vendor_id {
        0x1002 | 0x1022 => Some("AMD"),
        0x106b => Some("Apple"),
        0x10de => Some("NVIDIA"),
        0x1234 => Some("QEMU"),
        0x1414 => Some("Microsoft"),
        0x15ad => Some("VMware"),
        0x1af4 => Some("Virtio"),
        0x1b36 => Some("QEMU"),
        0x5143 => Some("Qualcomm"),
        0x8086 => Some("Intel"),
        _ => None,
    }
}

#[cfg(target_os = "macos")]
pub fn vendor_id_from_name(name: &str) -> u32 {
    let lower = name.to_ascii_lowercase();
    if lower.contains("apple") {
        return 0x106b;
    }
    if lower.contains("amd") || lower.contains("radeon") {
        return 0x1002;
    }
    if lower.contains("intel") {
        return 0x8086;
    }
    if lower.contains("nvidia") {
        return 0x10de;
    }
    if lower.contains("microsoft") {
        return 0x1414;
    }
    0
}

#[cfg(target_os = "macos")]
pub fn write_hex_u64(value: u64) -> String {
    format!("0x{value:x}")
}

#[cfg(target_os = "linux")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseHexIdError {
    InvalidHexId,
    InvalidDigit,
}

#[cfg(target_os = "linux")]
pub fn parse_hex_id(raw: &str) -> Result<u32, ParseHexIdError> {
    let mut trimmed = raw.trim();
    if let Some(rest) = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
    {
        trimmed = rest;
    }
    if trimmed.is_empty() || trimmed.len() > 8 {
        return Err(ParseHexIdError::InvalidHexId);
    }
    u32::from_str_radix(trimmed, 16).map_err(|_| ParseHexIdError::InvalidDigit)
}

#[cfg(target_os = "linux")]
pub fn is_drm_card_name(name: &str) -> bool {
    let Some(rest) = name.strip_prefix("card") else {
        return false;
    };
    !rest.is_empty() && rest.bytes().all(|ch| ch.is_ascii_digit())
}

#[cfg(target_os = "linux")]
pub fn basename(path: &str) -> &str {
    path.rsplit_once('/').map_or(path, |(_, base)| base)
}
