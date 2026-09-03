// SPDX-License-Identifier: AGPL-3.0-or-later

use http::HeaderValue;
use thiserror::Error;

pub const PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES: usize = 8 * 1024;
const CONTENT_DISPOSITION_DIRECTIVE_BYTES_MAX: usize = "attachment".len();
const CONTENT_DISPOSITION_FIXED_BYTES_MAX: usize = CONTENT_DISPOSITION_DIRECTIVE_BYTES_MAX + 40;
pub const CONTENT_DISPOSITION_FILENAME_BYTES_MAX: usize =
    (PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES - CONTENT_DISPOSITION_FIXED_BYTES_MAX) / 4;
const _: () = assert!(
    CONTENT_DISPOSITION_FILENAME_BYTES_MAX * 4 + CONTENT_DISPOSITION_FIXED_BYTES_MAX
        <= PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES
);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Decision {
    Inline,
    Attachment,
}

impl Decision {
    pub fn is_attachment(self) -> bool {
        self == Self::Attachment
    }

    pub fn header_value(self) -> HeaderValue {
        HeaderValue::from_static(self.directive())
    }

    fn directive(self) -> &'static str {
        match self {
            Self::Inline => "inline",
            Self::Attachment => "attachment",
        }
    }
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum ContentDispositionError {
    #[error("content disposition allocation failed")]
    AllocationFailed,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContentDisposition(HeaderValue);

impl ContentDisposition {
    pub fn into_header_value(self) -> HeaderValue {
        self.0
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
    if is_scriptable_document(mime) {
        return false;
    }
    let bytes = mime.as_bytes();
    if bytes.len() >= 6 {
        let prefix = &bytes[..6];
        if prefix.eq_ignore_ascii_case(b"image/") || prefix.eq_ignore_ascii_case(b"video/") {
            return true;
        }
    }
    false
}

fn is_scriptable_document(mime: &str) -> bool {
    mime.eq_ignore_ascii_case("image/svg+xml") || mime.eq_ignore_ascii_case("application/pdf")
}

fn is_safe_quoted_filename(s: &str) -> bool {
    !s.is_empty() && s.bytes().all(is_safe_quoted_byte)
}

fn is_safe_quoted_byte(b: u8) -> bool {
    (0x20..0x7f).contains(&b) && !matches!(b, b'"' | b'\\' | b'/')
}

fn is_attr_char(b: u8) -> bool {
    b.is_ascii_alphanumeric()
        || matches!(
            b,
            b'!' | b'#' | b'$' | b'&' | b'+' | b'-' | b'.' | b'^' | b'_' | b'`' | b'|' | b'~'
        )
}

fn append_percent_encoded(out: &mut String, bytes: &[u8]) {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    for &b in bytes {
        if is_attr_char(b) {
            out.push(char::from(b));
        } else {
            out.push('%');
            out.push(char::from(HEX[(b >> 4) as usize]));
            out.push(char::from(HEX[(b & 0x0f) as usize]));
        }
    }
}

pub(crate) fn truncate_on_char_boundary(name: &str, max_bytes: usize) -> &str {
    if name.len() <= max_bytes {
        return name;
    }
    let mut end = max_bytes;
    while end > 0 && !name.is_char_boundary(end) {
        end -= 1;
    }
    &name[..end]
}

pub fn header(
    decision: Decision,
    filename: Option<&str>,
) -> Result<ContentDisposition, ContentDispositionError> {
    let directive = decision.directive();
    let Some(name) = filename
        .map(|name| truncate_on_char_boundary(name, CONTENT_DISPOSITION_FILENAME_BYTES_MAX))
        .filter(|name| !name.is_empty())
    else {
        return Ok(ContentDisposition(decision.header_value()));
    };
    let safe_quoted_filename = is_safe_quoted_filename(name);
    let filename_capacity = if safe_quoted_filename {
        name.len()
    } else {
        name.len() * 4
    };
    let value_capacity = filename_capacity + directive.len() + 40;
    let mut value = String::new();
    value
        .try_reserve_exact(value_capacity)
        .map_err(|_| ContentDispositionError::AllocationFailed)?;
    value.push_str(directive);
    value.push_str("; filename=\"");
    if safe_quoted_filename {
        value.push_str(name);
        value.push('"');
    } else {
        for byte in name.bytes() {
            value.push(if is_safe_quoted_byte(byte) {
                char::from(byte)
            } else {
                '_'
            });
        }
        value.push_str("\"; filename*=UTF-8''");
        append_percent_encoded(&mut value, name.as_bytes());
    }
    Ok(ContentDisposition(
        HeaderValue::from_bytes(value.as_bytes())
            .expect("sanitised content disposition must be a valid header value"),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn header_string(decision: Decision, filename: Option<&str>) -> String {
        header(decision, filename)
            .expect("disposition header")
            .into_header_value()
            .to_str()
            .expect("disposition header is ascii")
            .to_owned()
    }

    #[test]
    fn inline_vs_attachment_based_on_mime() {
        assert_eq!(Decision::Inline, decide("image/png", false));
        assert_eq!(
            Decision::Inline,
            decide("image/jpeg; charset=binary", false)
        );
        assert_eq!(Decision::Inline, decide("video/mp4", false));
        assert_eq!(Decision::Attachment, decide("application/pdf", false));
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
    fn a_multibyte_boundary_inside_the_mime_prefix_is_not_a_panic() {
        assert_eq!(Decision::Attachment, decide("XX#-X\u{605}", false));
        assert!(!is_inline_viewable("XX#-X\u{605}"));
        assert!(!is_inline_viewable("\u{1f600}\u{1f600}"));
        assert!(!is_inline_viewable("imag\u{e9}/png"));
        for input in crate::test_fixtures::ADVERSARIAL_TEXT_INPUTS {
            let _ = decide(input, false);
            let _ = decide(input, true);
        }
    }

    #[test]
    fn format_header_ascii_filename_quoted() {
        assert_eq!(
            "attachment; filename=\"report.pdf\"",
            header_string(Decision::Attachment, Some("report.pdf"))
        );
    }

    #[test]
    fn format_header_inline_no_filename() {
        assert_eq!("inline", header_string(Decision::Inline, None));
        assert_eq!("inline", header_string(Decision::Inline, Some("")));
    }

    #[test]
    fn format_header_non_ascii_filename_uses_rfc5987_ext_form() {
        let out = header_string(Decision::Attachment, Some("naïve résumé.pdf"));
        assert!(out.contains("filename*=UTF-8''"));
        assert!(out.contains("%C3%A9"));
        assert!(out.contains("filename=\""));
    }

    #[test]
    fn format_header_strips_embedded_quote_and_backslash() {
        let out = header_string(Decision::Attachment, Some("evil\"name\\.txt"));
        assert!(!out.contains("evil\""));
        assert!(!out.contains('\\'));
        assert!(out.contains("filename*=UTF-8''"));
        assert!(out.contains("%22"));
        assert!(out.contains("%5C"));
    }

    #[test]
    fn sanitized_fallback_filename_replaces_path_separators() {
        assert_eq!(
            "attachment; filename=\".._.._x.png\"; filename*=UTF-8''..%2F..%2Fx.png",
            header_string(Decision::Attachment, Some("../../x.png"))
        );
    }

    #[test]
    fn filename_bound_leaves_room_for_the_worst_case_expansion() {
        assert_eq!(2035, CONTENT_DISPOSITION_FILENAME_BYTES_MAX);
        let widest = "\u{7f}".repeat(CONTENT_DISPOSITION_FILENAME_BYTES_MAX);
        assert!(header(Decision::Attachment, Some(&widest)).is_ok());
    }

    #[test]
    fn oversized_filenames_are_truncated_instead_of_dropped() {
        let oversized = "\u{7f}".repeat(PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
        let value = header_string(Decision::Attachment, Some(&oversized));
        assert!(value.starts_with("attachment; filename=\""));
        assert!(value.len() <= PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
    }

    #[test]
    fn long_multi_byte_filenames_keep_a_bounded_header_for_both_dispositions() {
        let filename = "\u{e9}".repeat(1100);
        for (decision, expected_prefix) in [
            (Decision::Inline, "inline; filename=\""),
            (Decision::Attachment, "attachment; filename=\""),
        ] {
            let value = header_string(decision, Some(&filename));
            assert!(value.starts_with(expected_prefix), "{value}");
            assert!(value.len() <= PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
            let encoded = value
                .split_once("filename*=UTF-8''")
                .expect("percent encoded filename")
                .1;
            assert_eq!(0, encoded.len() % 3);
            assert!(encoded.ends_with("%A9"));
            assert!(encoded.len() < filename.len() * 3);
        }
    }
}
