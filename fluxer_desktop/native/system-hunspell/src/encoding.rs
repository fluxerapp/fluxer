// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn is_utf8_encoding(encoding: &str) -> bool {
    let bytes = encoding.as_bytes();
    if bytes.is_empty() || bytes.len() > 16 {
        return false;
    }
    bytes.eq_ignore_ascii_case(b"utf-8") || bytes.eq_ignore_ascii_case(b"utf8")
}
