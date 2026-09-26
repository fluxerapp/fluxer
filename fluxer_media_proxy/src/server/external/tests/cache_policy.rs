// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{test_stream_policy, validated_partial};
use crate::{
    byte_budget::BudgetedBytes,
    server::{
        external::{
            ExternalBody, ExternalSuccessStatus, FetchedExternal,
            response::{
                ExternalPartialResponse, ExternalStreamingResponse, external_partial_response,
                external_streaming_response,
            },
        },
        response::{MediaResponse, error::text, media_response},
        stored::response::passthrough_head_response,
    },
};
use axum::{
    http::{Method, StatusCode},
    response::Response,
};
use bytes::Bytes;
use http::header;

fn cache_control_of(response: &Response) -> &str {
    response
        .headers()
        .get(header::CACHE_CONTROL)
        .expect("cache-control is always set")
        .to_str()
        .expect("cache-control is ASCII")
}

#[tokio::test]
async fn external_streaming_response_caches_forever() {
    for (content_type, expected) in [
        ("video/mp4", "public, max-age=31536000, no-transform"),
        ("image/webp", "public, max-age=31536000"),
    ] {
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
            content_type,
            disposition: None,
            stream_policy: test_stream_policy(),
        });

        assert_eq!(
            expected,
            cache_control_of(&response),
            "content_type={content_type}"
        );
    }
}

#[test]
fn external_partial_response_caches_forever() {
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
        disposition: None,
        stream_policy: test_stream_policy(),
    });

    assert_eq!(
        "public, max-age=31536000, no-transform",
        cache_control_of(&response)
    );
}

#[test]
fn stored_media_responses_cache_forever() {
    let response = media_response(MediaResponse {
        method: Method::GET,
        data: BudgetedBytes::from(Bytes::from_static(b"stored bytes")),
        content_type: "image/webp",
        range_header: None,
        disposition: None,
    });
    assert_eq!("public, max-age=31536000", cache_control_of(&response));

    let streamable = media_response(MediaResponse {
        method: Method::GET,
        data: BudgetedBytes::from(Bytes::from_static(b"stored bytes")),
        content_type: "video/mp4",
        range_header: None,
        disposition: None,
    });
    assert_eq!(
        "public, max-age=31536000, no-transform",
        cache_control_of(&streamable)
    );

    let head = passthrough_head_response("image/webp", 12, None, None);
    assert_eq!("public, max-age=31536000", cache_control_of(&head));
}

#[test]
fn error_responses_declare_an_explicit_no_store_policy() {
    for status in [
        StatusCode::NOT_FOUND,
        StatusCode::BAD_GATEWAY,
        StatusCode::INTERNAL_SERVER_ERROR,
    ] {
        let response = text(status, "nope");
        assert_eq!(
            response.headers().get(header::CACHE_CONTROL).unwrap(),
            "no-store",
            "status {status} must not be cacheable"
        );
    }
}

#[test]
fn successful_text_responses_are_left_to_the_media_cache_policy() {
    let response = text(StatusCode::OK, "fine");
    assert!(response.headers().get(header::CACHE_CONTROL).is_none());
}
