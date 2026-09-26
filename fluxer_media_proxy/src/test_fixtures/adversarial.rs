// SPDX-License-Identifier: AGPL-3.0-or-later

use super::images::{apng_header, minimal_gif};

pub fn adversarial_media_bytes() -> Vec<Vec<u8>> {
    let mut truncated_apng = apng_header(2);
    truncated_apng.truncate(20);

    let mut oversized_bmff = Vec::new();
    oversized_bmff.extend_from_slice(&u32::MAX.to_be_bytes());
    oversized_bmff.extend_from_slice(b"ftyp");
    oversized_bmff.extend_from_slice(b"isom");

    let mut webp_without_payload = vec![0_u8; 20];
    webp_without_payload[0..4].copy_from_slice(b"RIFF");
    webp_without_payload[4..8].copy_from_slice(&u32::MAX.to_le_bytes());
    webp_without_payload[8..12].copy_from_slice(b"WEBP");
    webp_without_payload[12..16].copy_from_slice(b"VP8X");
    webp_without_payload[16..20].copy_from_slice(&u32::MAX.to_le_bytes());

    vec![
        Vec::new(),
        vec![0x00],
        vec![0x00; 1024],
        vec![0xff; 1024],
        b"GIF".to_vec(),
        b"GIF89a".to_vec(),
        b"GIF89a\x01\x00".to_vec(),
        minimal_gif(),
        b"\x89PNG\r\n\x1a\n".to_vec(),
        apng_header(0),
        apng_header(u32::MAX),
        truncated_apng,
        b"RIFF".to_vec(),
        webp_without_payload,
        oversized_bmff,
        b"ftypM4A ".to_vec(),
        b"\xff\xd8\xff\xe0".to_vec(),
    ]
}

pub const ADVERSARIAL_RANGE_HEADERS: &[&str] = &[
    "",
    " ",
    "bytes",
    "bytes=",
    "bytes=-",
    "bytes=--1",
    "bytes=0-",
    "bytes=-0",
    "bytes=0-0",
    "bytes=0-1",
    "bytes=1-0",
    "bytes=18446744073709551615-18446744073709551615",
    "bytes=-18446744073709551615",
    "bytes=18446744073709551616-",
    "bytes=1-2,3-4",
    "bytes = 0-1",
    "BYTES=0-1",
    "items=0-1",
    "bytes 0-0/0",
    "bytes 0-1/1",
    "bytes 1-0/2",
    "bytes 0-1/*",
    "bytes */2",
    "bytes 0-1/18446744073709551616",
    "bytes\t0-1/2",
    "bytes 0-1/2 ",
];

pub const ADVERSARIAL_TEXT_INPUTS: &[&str] = &[
    "",
    " ",
    "%",
    "%0",
    "%00",
    "%2F",
    "%2f",
    "%5C",
    "%FF",
    "%C3",
    "%C3%A9",
    "%%%%",
    "%20%2e%2e%2f",
    "a_deadbeef",
    "A_DEADBEEF",
    "deadbeef",
    "deadbee",
    "deadbeef0",
    "0123456789",
    "a_",
    "\"quoted\"",
    "photo name.png",
    "r\u{e9}sum\u{e9}.png",
    "token=",
    "token=abc&token=def",
    "\u{7f}",
    "~!@#$^&*()_+",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adversarial_corpora_hold_every_documented_input() {
        assert_eq!(17, adversarial_media_bytes().len());
        assert_eq!(26, ADVERSARIAL_RANGE_HEADERS.len());
        assert_eq!(27, ADVERSARIAL_TEXT_INPUTS.len());
    }
}
