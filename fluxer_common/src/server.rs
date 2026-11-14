// SPDX-License-Identifier: AGPL-3.0-or-later

use http::{HeaderMap, HeaderName, HeaderValue, header};

pub const STRICT_TRANSPORT_SECURITY: &str = "max-age=31536000; includeSubDomains; preload";
pub const REFERRER_POLICY: &str = "strict-origin-when-cross-origin";

pub fn set_baseline_security_headers(headers: &mut HeaderMap, permissions_policy: &'static str) {
    for (name, value) in [
        (
            HeaderName::from_static("strict-transport-security"),
            STRICT_TRANSPORT_SECURITY,
        ),
        (header::X_CONTENT_TYPE_OPTIONS, "nosniff"),
        (HeaderName::from_static("referrer-policy"), REFERRER_POLICY),
        (HeaderName::from_static("x-frame-options"), "DENY"),
        (
            HeaderName::from_static("permissions-policy"),
            permissions_policy,
        ),
    ] {
        headers
            .entry(name)
            .or_insert(HeaderValue::from_static(value));
    }
}
