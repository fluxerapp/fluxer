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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_charset_wins_over_meta() {
        let bytes = b"<html><head><meta charset=\"iso-8859-1\"><title>caf\xc3\xa9</title>";
        let decoded = decode_body(bytes, Some("text/html; charset=utf-8"));
        assert!(decoded.contains("café"));
    }

    #[test]
    fn decodes_shift_jis_from_header() {
        let bytes = [0x83, 0x5c, 0x83, 0x6a, 0x81, 0x5b];
        let decoded = decode_body(&bytes, Some("text/html; charset=Shift_JIS"));
        assert_eq!(decoded, "ソニー");
    }

    #[test]
    fn decodes_shift_jis_from_meta_when_header_has_no_charset() {
        let mut bytes = b"<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=Shift_JIS\"><title>".to_vec();
        bytes.extend_from_slice(&[0x83, 0x5c, 0x83, 0x6a, 0x81, 0x5b]);
        let decoded = decode_body(&bytes, Some("text/html"));
        assert!(decoded.contains("ソニー"));
    }

    #[test]
    fn decodes_meta_charset_shorthand() {
        let mut bytes = b"<html><head><meta charset=windows-1252><title>".to_vec();
        bytes.push(0xe9);
        let decoded = decode_body(&bytes, None);
        assert!(decoded.contains('é'));
    }

    #[test]
    fn strips_utf8_bom() {
        let decoded = decode_body(b"\xef\xbb\xbfhello", None);
        assert_eq!(decoded, "hello");
    }

    #[test]
    fn bom_overrides_declared_charset() {
        let decoded = decode_body(
            b"\xef\xbb\xbfcaf\xc3\xa9",
            Some("text/html; charset=Shift_JIS"),
        );
        assert_eq!(decoded, "café");
    }

    #[test]
    fn decodes_utf16_from_header() {
        let bytes = b"\x68\x00\x69\x00";
        assert_eq!(decode_body(bytes, Some("text/html; charset=UTF-16")), "hi");
    }

    #[test]
    fn sniffs_utf8_without_declaration() {
        assert_eq!(decode_body("café".as_bytes(), None), "café");
    }

    #[test]
    fn sniffs_latin1_without_declaration() {
        assert_eq!(decode_body(b"caf\xe9", None), "café");
    }

    #[test]
    fn declared_utf8_with_invalid_bytes_is_lossy_not_fatal() {
        let decoded = decode_body(b"ok\xff", Some("text/html; charset=utf-8"));
        assert!(decoded.starts_with("ok"));
        assert!(decoded.contains('\u{fffd}'));
    }

    #[test]
    fn unknown_charset_label_falls_back_to_sniffing() {
        assert_eq!(
            decode_body(b"caf\xe9", Some("text/html; charset=bogus")),
            "café"
        );
    }

    #[test]
    fn ignores_charset_declared_past_the_scan_window() {
        let mut early = b"<meta charset=\"utf-8\">".to_vec();
        early.push(0xe9);
        assert!(decode_body(&early, None).contains('\u{fffd}'));

        let mut late = vec![b' '; META_SCAN_LIMIT];
        late.extend_from_slice(b"<meta charset=\"utf-8\">");
        late.push(0xe9);
        assert!(decode_body(&late, None).contains('é'));
    }
}
