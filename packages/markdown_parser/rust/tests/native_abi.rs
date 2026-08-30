// SPDX-License-Identifier: AGPL-3.0-or-later
#![cfg(not(target_arch = "wasm32"))]

use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::native::{FluxerMdBuffer, fluxer_md_buffer_free, fluxer_md_parse};

fn empty_buffer() -> FluxerMdBuffer {
    FluxerMdBuffer {
        data: std::ptr::null_mut(),
        data_len: 0,
        error: std::ptr::null_mut(),
        error_len: 0,
    }
}

fn parse(input: &str, flags: u32, tsv: &str) -> Result<String, String> {
    let mut out = empty_buffer();
    let status = unsafe {
        fluxer_md_parse(
            input.as_ptr(),
            input.len(),
            flags,
            tsv.as_ptr(),
            tsv.len(),
            &raw mut out,
        )
    };
    let result = if status == 0 {
        assert!(!out.data.is_null());
        assert!(out.error.is_null());
        let payload = unsafe { std::slice::from_raw_parts(out.data, out.data_len) };
        Ok(String::from_utf8(payload.to_vec()).expect("payload should be UTF-8"))
    } else {
        assert!(out.data.is_null());
        assert!(!out.error.is_null());
        let message = unsafe { std::slice::from_raw_parts(out.error, out.error_len) };
        Err(String::from_utf8(message.to_vec()).expect("error should be UTF-8"))
    };
    unsafe { fluxer_md_buffer_free(&raw mut out) };
    assert!(out.data.is_null());
    assert_eq!(out.data_len, 0);
    assert!(out.error.is_null());
    assert_eq!(out.error_len, 0);
    result
}

#[test]
fn parses_formatting_to_json_envelope() {
    let json = parse("**bold**", ParserFlags::ALL, "").expect("parse should succeed");
    assert_eq!(
        json,
        r#"{"nodes":[{"type":"Strong","children":[{"type":"Text","content":"bold"}]}]}"#
    );
}

#[test]
fn matches_wasm_abi_output() {
    let input = "# heading\n||spoiler|| <t:1234567890:R>";
    let native = parse(input, ParserFlags::ALL, "").expect("native parse should succeed");
    let json = fluxer_markdown_parser::parse_markdown_json(input, ParserFlags::ALL, "")
        .expect("json parse should succeed");
    assert_eq!(native, json);
}

#[test]
fn respects_flags() {
    let json = parse("# not a heading", 0, "").expect("parse should succeed");
    assert_eq!(
        json,
        r##"{"nodes":[{"type":"Text","content":"# not a heading"}]}"##
    );
}

#[test]
fn uses_emoji_context_records() {
    let json = parse(
        "hi \u{1F600}",
        ParserFlags::ALL,
        "S\t3\t4\t\u{1F600}\tgrinning face\t1f600",
    )
    .expect("parse should succeed");
    assert!(json.contains(r#""name":"grinning face""#), "got: {json}");
}

#[test]
fn empty_input_yields_empty_envelope() {
    let json = parse("", ParserFlags::ALL, "").expect("parse should succeed");
    assert_eq!(json, r#"{"nodes":[]}"#);
}

#[test]
fn rejects_invalid_utf8_input() {
    let mut out = empty_buffer();
    let invalid = [0xff_u8, 0xfe];
    let status = unsafe {
        fluxer_md_parse(
            invalid.as_ptr(),
            invalid.len(),
            ParserFlags::ALL,
            std::ptr::null(),
            0,
            &raw mut out,
        )
    };
    assert_eq!(status, 1);
    let message = unsafe { std::slice::from_raw_parts(out.error, out.error_len) };
    assert_eq!(message, b"invalid markdown input");
    unsafe { fluxer_md_buffer_free(&raw mut out) };
}

#[test]
fn double_free_is_a_no_op() {
    let mut out = empty_buffer();
    let input = "text";
    let status = unsafe {
        fluxer_md_parse(
            input.as_ptr(),
            input.len(),
            ParserFlags::ALL,
            std::ptr::null(),
            0,
            &raw mut out,
        )
    };
    assert_eq!(status, 0);
    unsafe { fluxer_md_buffer_free(&raw mut out) };
    unsafe { fluxer_md_buffer_free(&raw mut out) };
    unsafe { fluxer_md_buffer_free(std::ptr::null_mut()) };
}
