// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    disposition::{self, Decision},
    http_headers,
    range::ByteRange,
};
use http::{HeaderMap, header};

fn disposition_string(decision: Decision, filename: Option<&str>) -> String {
    disposition::header(decision, filename)
        .expect("disposition header")
        .into_header_value()
        .to_str()
        .expect("disposition header is ascii")
        .to_owned()
}

#[test]
fn disposition_policy_blocks_scriptable_documents() {
    assert_eq!(disposition::decide("image/png", false), Decision::Inline);
    assert_eq!(
        disposition::decide("video/webm; codecs=vp9", false),
        Decision::Inline
    );
    assert_eq!(
        disposition::decide("image/svg+xml", false),
        Decision::Attachment
    );
    assert_eq!(
        disposition::decide("application/pdf; charset=binary", false),
        Decision::Attachment
    );
    assert_eq!(disposition::decide("image/png", true), Decision::Attachment);
}

#[test]
fn disposition_filename_uses_ascii_and_rfc5987_forms() {
    assert_eq!(
        disposition_string(Decision::Attachment, Some("photo.png")),
        "attachment; filename=\"photo.png\""
    );
    assert_eq!(
        disposition_string(Decision::Attachment, Some("résumé/\".png")),
        "attachment; filename=\"r__sum____.png\"; filename*=UTF-8''r%C3%A9sum%C3%A9%2F%22.png"
    );
    assert_eq!(disposition_string(Decision::Inline, Some("")), "inline");
}

#[test]
fn public_media_headers_never_negotiate_a_conditional_validator() {
    let mut responses = Vec::new();
    for content_type in ["image/png", "video/mp4", "audio/mpeg", "image/svg+xml"] {
        for byte_range in [None, Some(ByteRange { start: 10, end: 19 })] {
            let mut headers = HeaderMap::new();
            http_headers::add_media_headers(&mut headers, 100, content_type, byte_range);
            responses.push(headers);
        }
    }
    let mut unsatisfiable = HeaderMap::new();
    http_headers::add_unsatisfiable_headers(&mut unsatisfiable, 4096);
    responses.push(unsatisfiable);
    let mut security_only = HeaderMap::new();
    http_headers::add_security_headers(&mut security_only);
    responses.push(security_only);

    for headers in &responses {
        for negotiated in ["etag", "if-none-match", "if-modified-since", "age"] {
            assert!(
                headers.get(negotiated).is_none(),
                "{negotiated} was negotiated on a public media response"
            );
        }
        if let Some(vary) = headers.get(header::VARY) {
            assert_eq!(vary, "Accept-Encoding");
        }
    }
}
