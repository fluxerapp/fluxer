// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;

const WHATWG_JAVASCRIPT_ESSENCES: [&str; 16] = [
    "application/ecmascript",
    "application/javascript",
    "application/x-ecmascript",
    "application/x-javascript",
    "text/ecmascript",
    "text/javascript",
    "text/javascript1.0",
    "text/javascript1.1",
    "text/javascript1.2",
    "text/javascript1.3",
    "text/javascript1.4",
    "text/javascript1.5",
    "text/jscript",
    "text/livescript",
    "text/x-ecmascript",
    "text/x-javascript",
];

#[test]
fn javascript_content_type_matches_every_whatwg_essence() {
    for essence in WHATWG_JAVASCRIPT_ESSENCES {
        assert!(is_javascript_content_type(essence), "{essence}");
        assert!(
            is_javascript_content_type(&essence.to_ascii_uppercase()),
            "{essence} upper case"
        );
    }
}

#[test]
fn javascript_content_type_checks_every_comma_separated_member() {
    assert!(is_javascript_content_type("image/png, text/javascript"));
    assert!(is_javascript_content_type("text/javascript, image/png"));
    assert!(!is_javascript_content_type(
        "video/mp4; codecs=\"avc1.42E01E, mp4a.40.2\""
    ));
}

#[test]
fn javascript_content_type_ends_the_essence_where_browsers_do() {
    for content_type in [
        "text/javascript x",
        "text/javascript\tx",
        "text/javascript(x)",
        "TEXT/JAVASCRIPT (x)",
        " text/javascript ;charset=x",
        "\ttext/javascript\t;goal=module",
        "application/x-javascript(",
        "text/jscript x;charset=utf-8",
    ] {
        assert!(is_javascript_content_type(content_type), "{content_type:?}");
    }
}

fn webp_chunk(kind: &[u8; 4], payload: &[u8]) -> Vec<u8> {
    let mut chunk = kind.to_vec();
    chunk.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    chunk.extend_from_slice(payload);
    if payload.len() % 2 == 1 {
        chunk.push(0);
    }
    chunk
}

fn animated_webp(frame_count: usize) -> Vec<u8> {
    let mut body = b"WEBP".to_vec();
    body.extend(webp_chunk(b"VP8X", &[0x12, 0, 0, 0, 63, 0, 0, 63, 0, 0]));
    body.extend(webp_chunk(b"ANIM", &[0; 6]));
    for _ in 0..frame_count {
        body.extend(webp_chunk(b"ANMF", &[0; 21]));
    }
    let mut data = b"RIFF".to_vec();
    data.extend_from_slice(&(body.len() as u32).to_le_bytes());
    data.extend(body);
    data
}

#[test]
fn webp_with_one_animation_frame_is_static() {
    let info = sniff(&animated_webp(1));
    assert_eq!(info.mime, "image/webp");
    assert!(!info.animated);
    assert_eq!(info.frames, 1);
}

#[test]
fn webp_with_several_animation_frames_is_animated() {
    let info = sniff(&animated_webp(3));
    assert!(info.animated);
    assert_eq!(info.frames, 3);
}

#[test]
fn truncated_animated_webp_stays_animated() {
    let data = animated_webp(1);
    let info = sniff(&data[..data.len() - 4]);
    assert!(info.animated);
}

#[test]
fn webp_frames_ignore_bytes_after_the_riff_container() {
    let mut data = animated_webp(1);
    data.extend_from_slice(b"trailing");
    assert!(!sniff(&data).animated);
}
