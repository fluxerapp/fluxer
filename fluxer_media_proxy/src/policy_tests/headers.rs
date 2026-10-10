// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{http_headers, range::ByteRange};
use http::{HeaderMap, header};

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
