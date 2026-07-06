// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Decision {
    Inline,
    Attachment,
}

impl Decision {
    pub fn is_attachment(self) -> bool {
        self == Self::Attachment
    }
}

pub fn decide(content_type: &str, requested_download: bool) -> Decision {
    if requested_download {
        Decision::Attachment
    } else if is_inline_viewable(content_type) {
        Decision::Inline
    } else {
        Decision::Attachment
    }
}

fn normalize_mime(content_type: &str) -> &str {
    let semi = content_type.find(';').unwrap_or(content_type.len());
    content_type[..semi].trim_matches([' ', '\t'])
}

pub fn is_inline_viewable(content_type: &str) -> bool {
    let mime = normalize_mime(content_type);
    if mime.eq_ignore_ascii_case("image/svg+xml") {
        return false;
    }
    if mime.len() >= 6 && mime[..6].eq_ignore_ascii_case("image/") {
        return true;
    }
    if mime.len() >= 6 && mime[..6].eq_ignore_ascii_case("video/") {
        return true;
    }
    mime.eq_ignore_ascii_case("application/pdf")
}

fn is_safe_quoted_filename(s: &str) -> bool {
    !s.is_empty()
        && s.bytes()
            .all(|b| (0x20..0x7f).contains(&b) && !matches!(b, b'"' | b'\\' | b'/' | 0x80))
}

fn is_attr_char(b: u8) -> bool {
    b.is_ascii_alphanumeric()
        || matches!(
            b,
            b'!' | b'#' | b'$' | b'&' | b'+' | b'-' | b'.' | b'^' | b'_' | b'`' | b'|' | b'~'
        )
}

fn percent_encoded(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    let mut out = String::new();
    for &b in bytes {
        if is_attr_char(b) {
            out.push(char::from(b));
        } else {
            out.push('%');
            out.push(char::from(HEX[(b >> 4) as usize]));
            out.push(char::from(HEX[(b & 0x0f) as usize]));
        }
    }
    out
}

pub fn format_header(decision: Decision, filename: Option<&str>) -> String {
    let directive = match decision {
        Decision::Inline => "inline",
        Decision::Attachment => "attachment",
    };
    let Some(name) = filename else {
        return directive.to_owned();
    };
    if name.is_empty() {
        return directive.to_owned();
    }
    if is_safe_quoted_filename(name) {
        return format!("{directive}; filename=\"{name}\"");
    }

    let fallback: String = name
        .bytes()
        .map(|b| {
            if (0x20..0x7f).contains(&b) && !matches!(b, b'"' | b'\\') {
                char::from(b)
            } else {
                '_'
            }
        })
        .collect();
    format!(
        "{directive}; filename=\"{fallback}\"; filename*=UTF-8''{}",
        percent_encoded(name.as_bytes())
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inline_vs_attachment_based_on_mime() {
        assert_eq!(Decision::Inline, decide("image/png", false));
        assert_eq!(
            Decision::Inline,
            decide("image/jpeg; charset=binary", false)
        );
        assert_eq!(Decision::Inline, decide("video/mp4", false));
        assert_eq!(Decision::Inline, decide("application/pdf", false));
        assert_eq!(Decision::Attachment, decide("image/svg+xml", false));
        assert_eq!(
            Decision::Attachment,
            decide("application/octet-stream", false)
        );
        assert_eq!(Decision::Attachment, decide("text/html", false));
        assert_eq!(
            Decision::Attachment,
            decide("application/x-msdownload", false)
        );
    }

    #[test]
    fn explicit_download_forces_attachment() {
        assert_eq!(Decision::Attachment, decide("image/png", true));
        assert_eq!(Decision::Attachment, decide("video/mp4", true));
    }

    #[test]
    fn case_insensitive_mime_matching() {
        assert_eq!(Decision::Inline, decide("IMAGE/PNG", false));
        assert_eq!(Decision::Attachment, decide("Image/Svg+Xml", false));
    }

    #[test]
    fn format_header_ascii_filename_quoted() {
        assert_eq!(
            "attachment; filename=\"report.pdf\"",
            format_header(Decision::Attachment, Some("report.pdf"))
        );
    }

    #[test]
    fn format_header_inline_no_filename() {
        assert_eq!("inline", format_header(Decision::Inline, None));
    }

    #[test]
    fn format_header_non_ascii_filename_uses_rfc5987_ext_form() {
        let out = format_header(Decision::Attachment, Some("naïve résumé.pdf"));
        assert!(out.contains("filename*=UTF-8''"));
        assert!(out.contains("%C3%A9"));
        assert!(out.contains("filename=\""));
    }

    #[test]
    fn format_header_strips_embedded_quote_and_backslash() {
        let out = format_header(Decision::Attachment, Some("evil\"name\\.txt"));
        assert!(!out.contains("evil\""));
        assert!(!out.contains('\\'));
        assert!(out.contains("filename*=UTF-8''"));
        assert!(out.contains("%22"));
        assert!(out.contains("%5C"));
    }
}
