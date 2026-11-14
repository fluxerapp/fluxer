// SPDX-License-Identifier: AGPL-3.0-or-later

use encoding_rs::{Encoding, UTF_8, WINDOWS_1252};

const META_SCAN_LIMIT: usize = 4096;

pub fn decode_body(bytes: &[u8], content_type: Option<&str>) -> String {
    let encoding = Encoding::for_bom(bytes)
        .map(|(encoding, _)| encoding)
        .or_else(|| content_type.and_then(encoding_from_content_type))
        .or_else(|| encoding_from_meta(bytes))
        .unwrap_or_else(|| sniff_encoding(bytes));
    encoding.decode(bytes).0.into_owned()
}

fn encoding_from_content_type(content_type: &str) -> Option<&'static Encoding> {
    let label = charset_label(content_type)?;
    Encoding::for_label(label.as_bytes())
}

fn encoding_from_meta(bytes: &[u8]) -> Option<&'static Encoding> {
    let window = &bytes[..bytes.len().min(META_SCAN_LIMIT)];
    let text: String = window.iter().map(|&byte| byte as char).collect();
    let text = text.to_ascii_lowercase();
    let mut cursor = 0;
    while let Some(offset) = text[cursor..].find("<meta") {
        let start = cursor + offset;
        let end = text[start..]
            .find('>')
            .map_or(text.len(), |index| start + index);
        if let Some(label) = charset_label(&text[start..end])
            && let Some(encoding) = Encoding::for_label(label.as_bytes())
        {
            return Some(encoding);
        }
        cursor = end.max(start + 1);
    }
    None
}

fn charset_label(text: &str) -> Option<String> {
    let index = text.to_ascii_lowercase().find("charset")?;
    let rest = text[index + "charset".len()..].trim_start();
    let rest = rest.strip_prefix('=')?.trim_start();
    let value = match rest.as_bytes().first()? {
        b'"' => rest[1..].split('"').next()?,
        b'\'' => rest[1..].split('\'').next()?,
        _ => rest
            .split(|c: char| c.is_ascii_whitespace() || matches!(c, ';' | '/' | '>' | '"' | '\''))
            .next()?,
    };
    let value = value.trim();
    (!value.is_empty()).then(|| value.to_owned())
}

fn sniff_encoding(bytes: &[u8]) -> &'static Encoding {
    if std::str::from_utf8(bytes).is_ok() {
        UTF_8
    } else {
        WINDOWS_1252
    }
}
