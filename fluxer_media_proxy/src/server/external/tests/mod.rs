// SPDX-License-Identifier: AGPL-3.0-or-later

mod cache_policy;

use super::{
    ExternalBody, ExternalFetchError, ExternalSuccessStatus, FetchedExternal,
    PreparedExternalRequest,
    byte_range::{
        ExternalPartial, ExternalRangeSelection, parse_external_requested_range,
        validate_external_partial,
    },
    external_fetch_error_response, external_head_response,
    fetch::{
        ExternalBufferRequest, ExternalHead, buffer_external_response,
        external_streamed_source_is_svg,
    },
    is_redirect_status, map_internal_metadata_upstream_status, map_upstream_status,
    response::{
        ExternalPartialResponse, ExternalStreamDecision, ExternalStreamingResponse,
        external_partial_response, external_stream_decision, external_streaming_response,
    },
    serve_fetched_external,
};
use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    config::Config,
    constants,
    external_path::build_external_media_proxy_path,
    metrics::external::ExternalMetrics,
    server::{download_stream::DownloadStreamPolicy, state::AppState},
    signing,
};
use axum::{
    body::to_bytes,
    http::{HeaderMap, HeaderValue, Method, StatusCode, header},
};
use bytes::Bytes;
use std::{collections::HashMap, sync::Arc};

fn test_stream_policy() -> DownloadStreamPolicy {
    DownloadStreamPolicy::for_passthrough(30_000)
}

fn validated_partial(requested: &str, content_range: &str, content_length: u64) -> ExternalPartial {
    validate_external_partial(
        parse_external_requested_range(requested),
        Some(content_range),
        Some(content_length),
        constants::MAX_MEDIA_PROXY_BYTES,
    )
    .expect("the upstream partial response is valid")
}

#[test]
fn external_stream_decision_streams_an_unknown_length_media_body() {
    assert_eq!(
        ExternalStreamDecision::Stream(Some(1024)),
        external_stream_decision(true, Some(1024), "video/mp4")
    );
    assert_eq!(
        ExternalStreamDecision::Stream(None),
        external_stream_decision(true, None, "video/mp4")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(false, Some(1024), "video/mp4")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(true, Some(1024), "application/octet-stream")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(true, None, "")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(true, Some(1024), "image/svg+xml")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(true, Some(1024), "image/svg+xml; charset=utf-8")
    );
    assert_eq!(
        ExternalStreamDecision::Buffer,
        external_stream_decision(
            true,
            Some(constants::MAX_MEDIA_PROXY_BYTES as u64 + 1),
            "video/mp4"
        )
    );
    assert_eq!(
        ExternalStreamDecision::Stream(Some(constants::MAX_MEDIA_PROXY_BYTES as u64)),
        external_stream_decision(
            true,
            Some(constants::MAX_MEDIA_PROXY_BYTES as u64),
            "video/mp4"
        )
    );
}

#[tokio::test]
async fn external_streaming_response_passes_body_through() {
    let upstream = reqwest::Response::from(
        http::Response::builder()
            .status(StatusCode::OK)
            .body("streamed bytes")
            .unwrap(),
    );
    let response = external_streaming_response(ExternalStreamingResponse {
        method: Method::GET,
        response: upstream,
        prefix: Bytes::new(),
        content_length: Some(14),
        content_type: "video/mp4",
        disposition: Some(HeaderValue::from_static("inline; filename=\"clip.mp4\"")),
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        "14",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "video/mp4",
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"streamed bytes", body.as_ref());
}

#[tokio::test]
async fn external_partial_response_streams_partial_body() {
    let upstream = reqwest::Response::from(
        http::Response::builder()
            .status(StatusCode::PARTIAL_CONTENT)
            .body("abcd")
            .unwrap(),
    );
    let fetched = FetchedExternal {
        url: "https://media.example.test/clip.webm".to_owned(),
        status: ExternalSuccessStatus::Partial(validated_partial("bytes=0-3", "bytes 0-3/10", 4)),
        body: ExternalBody::Streaming {
            response: upstream,
            prefix: Bytes::new(),
            content_length: Some(4),
        },
        content_type: "video/webm".to_owned(),
    };
    let response = external_partial_response(ExternalPartialResponse {
        method: Method::GET,
        partial: fetched
            .status
            .partial()
            .expect("the fetched response is partial"),
        fetched,
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
    assert_eq!(
        "bytes 0-3/10",
        response
            .headers()
            .get(header::CONTENT_RANGE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "4",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"abcd", body.as_ref());
}

#[test]
fn external_partial_response_uses_media_headers() {
    let fetched = FetchedExternal {
        url: "https://media.example.test/clip.webm".to_owned(),
        status: ExternalSuccessStatus::Partial(validated_partial("bytes=0-3", "bytes 0-3/10", 4)),
        body: ExternalBody::Buffered(BudgetedBytes::unbudgeted(Bytes::from_static(b"abcd"))),
        content_type: "video/webm".to_owned(),
    };
    let response = external_partial_response(ExternalPartialResponse {
        method: Method::GET,
        partial: fetched
            .status
            .partial()
            .expect("the fetched response is partial"),
        fetched,
        disposition: Some(HeaderValue::from_static("inline; filename=\"clip.webm\"")),
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
    assert_eq!(
        "*",
        response
            .headers()
            .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "video/webm",
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "bytes 0-3/10",
        response
            .headers()
            .get(header::CONTENT_RANGE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "inline; filename=\"clip.webm\"",
        response
            .headers()
            .get(header::CONTENT_DISPOSITION)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert!(
        response
            .headers()
            .contains_key(header::CONTENT_SECURITY_POLICY)
    );
    assert!(response.headers().contains_key("strict-transport-security"));
    assert!(response.headers().contains_key("permissions-policy"));
    assert!(
        response
            .headers()
            .contains_key(header::X_CONTENT_TYPE_OPTIONS)
    );
    assert!(response.headers().contains_key("CDN-Cache-Control"));
}

#[test]
fn external_range_parser_accepts_one_canonical_range() {
    assert!(matches!(
        parse_external_requested_range("bytes=10-20"),
        Some(ExternalRangeSelection::Bounded { start: 10, end: 20 })
    ));
    assert!(matches!(
        parse_external_requested_range(" bytes=10- \t"),
        Some(ExternalRangeSelection::From { start: 10 })
    ));
    assert!(matches!(
        parse_external_requested_range("BYTES=-20"),
        Some(ExternalRangeSelection::Suffix { length: 20 })
    ));
    for invalid in ["items=1-2", "bytes=", "bytes=1-2, 3-4"] {
        assert!(
            parse_external_requested_range(invalid).is_none(),
            "accepted {invalid}"
        );
    }
}

#[test]
fn external_partial_validation_matches_requested_range_and_lengths() {
    let bounded = parse_external_requested_range("bytes=10-99");
    let partial = validate_external_partial(bounded, Some("bytes 10-49/50"), Some(40), 100)
        .expect("bounded partial");
    assert_eq!(Some(40), partial.content_length());
    assert_eq!(
        Some(HeaderValue::from_static("bytes 10-49/50")),
        partial.header_value()
    );

    let suffix = parse_external_requested_range("bytes=-10");
    assert!(validate_external_partial(suffix, Some("bytes 40-49/50"), Some(10), 10).is_some());
    let oversized_suffix = parse_external_requested_range("bytes=-100");
    assert!(
        validate_external_partial(oversized_suffix, Some("bytes 0-49/50"), Some(50), 50).is_some()
    );

    let from = parse_external_requested_range("bytes=10-");
    assert!(validate_external_partial(from, Some("bytes 10-49/50"), Some(40), 40).is_some());
    assert!(validate_external_partial(from, Some("bytes 11-49/50"), Some(39), 40).is_none());
    assert!(validate_external_partial(from, Some("bytes 10-49/*"), Some(40), 40).is_some());
    assert!(validate_external_partial(from, Some("bytes 10-49/50"), Some(39), 40).is_none());
    assert!(validate_external_partial(from, Some("bytes 10-49/50"), Some(40), 39).is_none());
}

#[test]
fn upstream_status_mapping_and_redirect_policy_are_closed() {
    assert_eq!(
        map_upstream_status(StatusCode::NOT_FOUND),
        StatusCode::NOT_FOUND
    );
    assert_eq!(
        map_upstream_status(StatusCode::INTERNAL_SERVER_ERROR),
        StatusCode::BAD_GATEWAY
    );
    assert!(is_redirect_status(StatusCode::MOVED_PERMANENTLY));
    assert!(is_redirect_status(StatusCode::PERMANENT_REDIRECT));
    assert!(!is_redirect_status(StatusCode::NOT_MODIFIED));
    assert!(!is_redirect_status(StatusCode::OK));
}

#[test]
fn upstream_status_is_clamped_to_known_whitelist() {
    assert_eq!(
        StatusCode::NOT_FOUND,
        map_upstream_status(StatusCode::NOT_FOUND)
    );
    assert_eq!(
        StatusCode::TOO_MANY_REQUESTS,
        map_upstream_status(StatusCode::TOO_MANY_REQUESTS)
    );
    assert_eq!(
        StatusCode::from_u16(428).unwrap(),
        map_upstream_status(StatusCode::from_u16(428).unwrap())
    );
    assert_eq!(
        StatusCode::BAD_GATEWAY,
        map_upstream_status(StatusCode::from_u16(451).unwrap())
    );
    assert_eq!(
        StatusCode::BAD_GATEWAY,
        map_upstream_status(StatusCode::INTERNAL_SERVER_ERROR)
    );
    assert_eq!(
        StatusCode::BAD_GATEWAY,
        map_upstream_status(StatusCode::SERVICE_UNAVAILABLE)
    );
}

#[test]
fn internal_metadata_does_not_surface_origin_429() {
    assert_eq!(
        StatusCode::SERVICE_UNAVAILABLE,
        map_internal_metadata_upstream_status(StatusCode::TOO_MANY_REQUESTS)
    );
    assert_eq!(
        StatusCode::NOT_FOUND,
        map_internal_metadata_upstream_status(StatusCode::NOT_FOUND)
    );
}

#[test]
fn external_partial_accepts_an_unknown_complete_length() {
    let bounded = parse_external_requested_range("bytes=0-9");
    let partial = validate_external_partial(bounded, Some("bytes 0-9/*"), Some(10), 100)
        .expect("an unknown complete length is a legal Content-Range");
    assert_eq!(Some(10), partial.content_length());
    assert_eq!(
        Some(HeaderValue::from_static("bytes 0-9/*")),
        partial.header_value()
    );
    assert!(validate_external_partial(bounded, Some("bytes 1-9/*"), Some(9), 100).is_none());
    assert!(validate_external_partial(bounded, Some("bytes 0-8/*"), Some(9), 100).is_some());

    let from = parse_external_requested_range("bytes=10-");
    assert!(validate_external_partial(from, Some("bytes 10-49/*"), Some(40), 100).is_some());
    assert!(validate_external_partial(from, Some("bytes 11-49/*"), Some(39), 100).is_none());

    let suffix = parse_external_requested_range("bytes=-10");
    assert!(validate_external_partial(suffix, Some("bytes 40-49/*"), Some(10), 100).is_some());
    assert!(validate_external_partial(suffix, Some("bytes 0-4/*"), Some(5), 100).is_some());
    assert!(validate_external_partial(suffix, Some("bytes 40-48/*"), Some(9), 100).is_none());
}

#[test]
fn external_partial_response_serves_an_unknown_complete_length() {
    let fetched = FetchedExternal {
        url: "https://media.example.test/clip.webm".to_owned(),
        status: ExternalSuccessStatus::Partial(validated_partial("bytes=0-3", "bytes 0-3/*", 4)),
        body: ExternalBody::Buffered(BudgetedBytes::unbudgeted(Bytes::from_static(b"abcd"))),
        content_type: "video/webm".to_owned(),
    };
    let response = external_partial_response(ExternalPartialResponse {
        method: Method::GET,
        partial: fetched
            .status
            .partial()
            .expect("the fetched response is partial"),
        fetched,
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
    assert_eq!(
        "bytes 0-3/*",
        response
            .headers()
            .get(header::CONTENT_RANGE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    assert_eq!(
        "4",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
}

#[test]
fn external_multi_range_reaches_the_origin_verbatim() {
    for spec in [
        "bytes=0-9,20-29",
        "bytes=1-2,3-4",
        "bytes=-0",
        "bytes=2-1",
        "bytes=1--2",
        "bytes=+5-",
        "bytes=cheese",
    ] {
        let forwarded =
            parse_external_requested_range(spec).expect("an unparsed spec reaches the origin");
        assert!(matches!(forwarded, ExternalRangeSelection::Verbatim { .. }));
        assert_eq!(forwarded.header_value(), spec);
    }
}

#[test]
fn external_verbatim_range_forwards_the_upstream_partial_unchanged() {
    let multi = parse_external_requested_range("bytes=0-9,20-29");
    let partial = validate_external_partial(multi, Some("bytes 0-9/50"), Some(10), 100)
        .expect("a verbatim range trusts the upstream partial");
    assert_eq!(None, partial.content_length());
    assert_eq!(
        Some(HeaderValue::from_static("bytes 0-9/50")),
        partial.header_value()
    );

    let multipart = validate_external_partial(multi, None, Some(4096), 100)
        .expect("a multipart partial carries no Content-Range");
    assert_eq!(None, multipart.content_length());
    assert_eq!(None, multipart.header_value());
}

#[tokio::test]
async fn external_multipart_partial_keeps_the_upstream_body_length() {
    let fetched = FetchedExternal {
        url: "https://media.example.test/clip.webm".to_owned(),
        status: ExternalSuccessStatus::Partial(
            validate_external_partial(
                parse_external_requested_range("bytes=0-1,3-4"),
                None,
                None,
                constants::MAX_MEDIA_PROXY_BYTES,
            )
            .expect("a verbatim range trusts the upstream partial"),
        ),
        body: ExternalBody::Buffered(BudgetedBytes::unbudgeted(Bytes::from_static(b"abcde"))),
        content_type: "multipart/byteranges; boundary=xyz".to_owned(),
    };
    let response = external_partial_response(ExternalPartialResponse {
        method: Method::GET,
        partial: fetched
            .status
            .partial()
            .expect("the fetched response is partial"),
        fetched,
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
    assert!(!response.headers().contains_key(header::CONTENT_RANGE));
    assert_eq!(
        "5",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"abcde", body.as_ref());
}

#[test]
fn external_partial_validation_rejects_a_partial_the_client_did_not_request() {
    let bounded = parse_external_requested_range("bytes=10-19");
    assert!(validate_external_partial(bounded, Some("bytes 10-19/50"), Some(10), 100).is_some());
    assert!(validate_external_partial(bounded, Some("bytes 0-19/50"), Some(20), 100).is_none());
    assert!(validate_external_partial(bounded, Some("bytes 10-29/50"), Some(20), 100).is_none());
    assert!(validate_external_partial(bounded, Some("bytes 11-19/50"), Some(9), 100).is_none());
    assert!(validate_external_partial(bounded, None, Some(10), 100).is_none());
    assert!(
        validate_external_partial(None, Some("bytes 10-19/50"), Some(10), 100).is_none(),
        "an unrequested partial is never validated"
    );
}

#[tokio::test]
async fn external_buffering_rejects_a_body_shorter_than_its_content_length() {
    let budget = ByteBudget::new(constants::MAX_MEDIA_PROXY_BYTES * 4);
    let metrics = ExternalMetrics::new();
    let truncated = buffer_external_response(ExternalBufferRequest {
        response: reqwest::Response::from(
            http::Response::builder()
                .status(StatusCode::OK)
                .header(header::CONTENT_LENGTH, "10")
                .body("abcd")
                .unwrap(),
        ),
        prefix: Bytes::new(),
        url: "https://media.example.test/clip.webm",
        budget: &budget,
        metrics: &metrics,
        content_length: Some(10),
        limit: constants::MAX_MEDIA_PROXY_BYTES,
    })
    .await;
    assert!(matches!(truncated, Err(ExternalFetchError::FetchFailed)));

    let complete = buffer_external_response(ExternalBufferRequest {
        response: reqwest::Response::from(
            http::Response::builder()
                .status(StatusCode::OK)
                .header(header::CONTENT_LENGTH, "4")
                .body("abcd")
                .unwrap(),
        ),
        prefix: Bytes::new(),
        url: "https://media.example.test/clip.webm",
        budget: &budget,
        metrics: &metrics,
        content_length: Some(4),
        limit: constants::MAX_MEDIA_PROXY_BYTES,
    })
    .await
    .expect("a complete body is buffered");
    assert_eq!(b"abcd", complete.as_bytes().as_ref());
}

#[test]
fn a_narrowed_upstream_partial_is_forwarded_instead_of_failing_the_fetch() {
    assert_eq!(
        StatusCode::BAD_GATEWAY,
        external_fetch_error_response(
            "https://media.example.test/clip.webm",
            ExternalFetchError::FetchFailed
        )
        .status()
    );

    let bounded = parse_external_requested_range("bytes=0-999");
    let narrowed = validate_external_partial(bounded, Some("bytes 0-255/4096"), Some(256), 4096)
        .expect("an origin may answer a range request with a narrower range");
    assert_eq!(Some(256), narrowed.content_length());
    assert_eq!(
        Some(HeaderValue::from_static("bytes 0-255/4096")),
        narrowed.header_value()
    );
    assert!(validate_external_partial(bounded, Some("bytes 0-255/*"), Some(256), 4096).is_some());

    let from = parse_external_requested_range("bytes=100-");
    assert!(validate_external_partial(from, Some("bytes 100-355/4096"), Some(256), 4096).is_some());
    assert!(validate_external_partial(from, Some("bytes 100-355/*"), Some(256), 4096).is_some());

    assert!(
        validate_external_partial(bounded, Some("bytes 1-255/4096"), Some(255), 4096).is_none()
    );
    assert!(
        validate_external_partial(bounded, Some("bytes 0-1000/4096"), Some(1001), 4096).is_none()
    );
    assert!(validate_external_partial(from, Some("bytes 99-355/4096"), Some(257), 4096).is_none());
    assert!(validate_external_partial(None, Some("bytes 0-255/4096"), Some(256), 4096).is_none());
}

#[tokio::test]
async fn an_unknown_length_upstream_streams_without_a_content_length() {
    let upstream = reqwest::Response::from(
        http::Response::builder()
            .status(StatusCode::OK)
            .body("streamed bytes")
            .unwrap(),
    );
    let response = external_streaming_response(ExternalStreamingResponse {
        method: Method::GET,
        response: upstream,
        prefix: Bytes::new(),
        content_length: None,
        content_type: "video/mp4",
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::OK, response.status());
    assert!(!response.headers().contains_key(header::CONTENT_LENGTH));
    assert_eq!(
        "video/mp4",
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"streamed bytes", body.as_ref());
}

#[test]
fn a_blocked_url_answers_bad_request_rather_than_bad_gateway() {
    assert_eq!(
        StatusCode::BAD_REQUEST,
        external_fetch_error_response(
            "https://metadata.internal.example/latest/",
            ExternalFetchError::BlockedUrl
        )
        .status()
    );
}

#[test]
fn a_mislabeled_svg_body_never_takes_the_external_streaming_path() {
    let svg = b"<svg xmlns=\"http://www.w3.org/2000/svg\"><script/></svg>";
    assert_eq!(
        ExternalStreamDecision::Stream(Some(svg.len() as u64)),
        external_stream_decision(true, Some(svg.len() as u64), "image/png"),
        "the declared headers alone still look streamable"
    );
    assert!(
        external_streamed_source_is_svg(svg, "logo.png"),
        "an SVG body behind an image/png label must be rasterized, not proxied through"
    );
    assert!(
        external_streamed_source_is_svg(b"\x89PNG\r\n\x1a\n", "logo.svg"),
        "a .svg final url is refused even when the bytes are not SVG"
    );
    assert!(!external_streamed_source_is_svg(
        b"\x89PNG\r\n\x1a\n",
        "logo.png"
    ));
    assert!(!external_streamed_source_is_svg(b"", "clip.mp4"));
}

#[tokio::test]
async fn external_streaming_response_emits_the_sniff_prefix_then_the_rest() {
    let upstream = reqwest::Response::from(
        http::Response::builder()
            .status(StatusCode::OK)
            .body("rest of the body")
            .unwrap(),
    );
    let response = external_streaming_response(ExternalStreamingResponse {
        method: Method::GET,
        response: upstream,
        prefix: Bytes::from_static(b"prefix "),
        content_length: Some(23),
        content_type: "video/mp4",
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        "23",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"prefix rest of the body", body.as_ref());
}

#[tokio::test]
async fn external_buffering_prepends_the_sniff_prefix() {
    let budget = ByteBudget::new(constants::MAX_MEDIA_PROXY_BYTES * 4);
    let metrics = ExternalMetrics::new();
    let buffered = buffer_external_response(ExternalBufferRequest {
        response: reqwest::Response::from(
            http::Response::builder()
                .status(StatusCode::OK)
                .header(header::CONTENT_LENGTH, "9")
                .body("ffix rest")
                .unwrap(),
        ),
        prefix: Bytes::from_static(b"<svg "),
        url: "https://media.example.test/logo.png",
        budget: &budget,
        metrics: &metrics,
        content_length: Some(14),
        limit: constants::MAX_MEDIA_PROXY_BYTES,
    })
    .await
    .expect("the sniffed prefix and the rest are one body");
    assert_eq!(b"<svg ffix rest", buffered.as_bytes().as_ref());
}

#[tokio::test]
async fn a_cached_external_transform_is_served_without_refetching_the_origin() {
    use super::{external_hint, serve_external};
    use crate::{
        config::Config,
        external_path::build_external_media_proxy_path,
        server::{
            params::url_filename,
            state::AppState,
            transform::{ServeBytesRequest, parameters::TransformRoute, serve_bytes_or_transform},
        },
        signing,
        test_fixtures::fixture_jpeg,
    };
    use axum::http::HeaderMap;
    use std::{collections::HashMap, sync::Arc};

    let cfg = Config::load_from_iter([(
        "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
        "secret".to_owned(),
    )])
    .expect("config loads");
    let app = Arc::new(AppState::for_tests(cfg));
    // A loopback origin is refused before any socket work, so a 200 can only be cached bytes.
    let url = "http://127.0.0.1/photo.jpg";
    let proxy_path = build_external_media_proxy_path(url).expect("the external path builds");
    let rest = format!(
        "{}/{proxy_path}",
        signing::create_signature(&proxy_path, app.cfg.secret_key.as_bytes())
    );
    let filename = url_filename(url);
    let data = Bytes::from(fixture_jpeg());
    let headers = HeaderMap::new();

    for pairs in [
        vec![("width", "32")],
        vec![("width", "32"), ("height", "24")],
        vec![("format", "png"), ("width", "32")],
        vec![("width", "32"), ("quality", "low")],
    ] {
        let params: HashMap<String, String> = pairs
            .iter()
            .map(|(key, value)| ((*key).to_owned(), (*value).to_owned()))
            .collect();
        assert_eq!(
            StatusCode::BAD_REQUEST,
            serve_external(&app, Method::GET, &rest, &params, &headers)
                .await
                .status(),
            "{pairs:?} must reach the fetch path while nothing is cached"
        );

        let served = serve_bytes_or_transform(
            app.media.transforms(),
            ServeBytesRequest {
                method: Method::GET,
                data: data.clone(),
                content_type: "image/jpeg".to_owned(),
                cache_identity: url,
                filename: &filename,
                route: TransformRoute::External,
                params: &params,
                headers: &headers,
            },
        )
        .await;
        assert_eq!(StatusCode::OK, served.status(), "{pairs:?}");
        let served_content_type = served
            .headers()
            .get(header::CONTENT_TYPE)
            .expect("a transform response declares its content type")
            .clone();
        let transformed = to_bytes(served.into_body(), usize::MAX).await.unwrap();
        app.media.external_hints().insert(
            url.to_owned(),
            external_hint(url, &filename, "image/jpeg", &data),
        );

        let cached = serve_external(&app, Method::GET, &rest, &params, &headers).await;
        assert_eq!(
            StatusCode::OK,
            cached.status(),
            "{pairs:?} must be answered from the transform cache"
        );
        assert_eq!(
            Some(&served_content_type),
            cached.headers().get(header::CONTENT_TYPE),
            "{pairs:?}"
        );
        assert_eq!(
            transformed,
            to_bytes(cached.into_body(), usize::MAX).await.unwrap(),
            "{pairs:?}"
        );
    }
}

fn probed_head(url: &str, content_type: &str, content_length: Option<u64>) -> ExternalHead {
    ExternalHead {
        url: url.to_owned(),
        status: StatusCode::OK,
        content_type: content_type.to_owned(),
        content_length,
    }
}

#[test]
fn a_plain_head_is_answered_from_the_upstream_head_metadata() {
    let response = external_head_response(
        probed_head("https://cdn.example.test/clip.mp4", "video/mp4", Some(4096)),
        false,
    )
    .expect("an upstream HEAD with a usable length answers the probe");
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        Some(&HeaderValue::from_static("4096")),
        response.headers().get(header::CONTENT_LENGTH)
    );
    assert_eq!(
        Some(&HeaderValue::from_static("video/mp4")),
        response.headers().get(header::CONTENT_TYPE)
    );

    assert_eq!(
        Some(&HeaderValue::from_static("video/mp4")),
        external_head_response(
            probed_head(
                "https://cdn.example.test/clip.mp4",
                "application/octet-stream",
                Some(4096)
            ),
            false,
        )
        .expect("an untrusted upstream type falls back to the filename")
        .headers()
        .get(header::CONTENT_TYPE)
    );

    let mut partial = probed_head("https://cdn.example.test/clip.mp4", "video/mp4", Some(4096));
    partial.status = StatusCode::PARTIAL_CONTENT;
    for unusable in [
        probed_head(
            "https://cdn.example.test/logo.svg",
            "image/svg+xml",
            Some(16),
        ),
        probed_head("https://cdn.example.test/clip.mp4", "video/mp4", None),
        probed_head(
            "https://cdn.example.test/clip.mp4",
            "video/mp4",
            Some(constants::MAX_MEDIA_PROXY_BYTES as u64 + 1),
        ),
        partial,
    ] {
        assert!(
            external_head_response(unusable, false).is_none(),
            "unusable upstream metadata must fall back to the body fetch"
        );
    }
}

#[tokio::test]
async fn an_upstream_that_ignores_the_client_range_still_streams_a_complete_body() {
    let cfg =
        Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")]).expect("test config");
    let app = Arc::new(AppState::for_tests(cfg));
    let url = "https://media.example.test/clip.mp4";
    let proxy_path = build_external_media_proxy_path(url).expect("the external path builds");
    let signature = signing::create_signature(&proxy_path, app.cfg.secret_key.as_bytes());
    let rest = format!("{signature}/{proxy_path}");
    let params = HashMap::new();
    let mut headers = HeaderMap::new();
    headers.insert(header::RANGE, HeaderValue::from_static("bytes=0-3"));
    let prepared = PreparedExternalRequest::new(&app, Method::GET, &rest, &params, &headers)
        .expect("the signed external request is accepted");
    let fetched = FetchedExternal {
        url: url.to_owned(),
        status: ExternalSuccessStatus::Complete,
        body: ExternalBody::Streaming {
            response: reqwest::Response::from(
                http::Response::builder()
                    .status(StatusCode::OK)
                    .body("streamed bytes")
                    .unwrap(),
            ),
            prefix: bytes::Bytes::new(),
            content_length: Some(14),
        },
        content_type: "video/mp4".to_owned(),
    };

    let response = serve_fetched_external(prepared, fetched).await;

    assert_eq!(StatusCode::OK, response.status());
    assert!(!response.headers().contains_key(header::CONTENT_RANGE));
    assert_eq!(
        "14",
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .unwrap()
            .to_str()
            .unwrap()
    );
    let body = to_bytes(response.into_body(), 64).await.unwrap();
    assert_eq!(b"streamed bytes", body.as_ref());
}
