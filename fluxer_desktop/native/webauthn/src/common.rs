// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Result;
use napi::bindgen_prelude::{Error, Status};

pub fn ceremony_error(prefix: &str, message: &str) -> Error {
    Error::new(Status::GenericFailure, format!("{prefix}: {message}"))
}

pub const CREATE_PREFIX: &str = "WebAuthn registration failed";
pub const GET_PREFIX: &str = "WebAuthn authentication failed";

pub const TRANSPORT_USB: u32 = 0x0000_0001;
#[allow(dead_code)]
pub const TRANSPORT_NFC: u32 = 0x0000_0002;
#[allow(dead_code)]
pub const TRANSPORT_BLE: u32 = 0x0000_0004;
pub const TRANSPORT_INTERNAL: u32 = 0x0000_0010;

#[allow(dead_code)]
pub const ATTACHMENT_ANY: u32 = 0;
#[allow(dead_code)]
pub const ATTACHMENT_PLATFORM: u32 = 1;
#[allow(dead_code)]
pub const ATTACHMENT_CROSS_PLATFORM: u32 = 2;

#[allow(dead_code)]
pub const USER_VERIFICATION_REQUIRED: u32 = 1;
#[allow(dead_code)]
pub const USER_VERIFICATION_PREFERRED: u32 = 2;

#[allow(dead_code)]
pub const ATTESTATION_NONE: u32 = 1;

pub const ENTERPRISE_NONE: u32 = 0;

#[derive(Clone)]
#[allow(dead_code)]
pub struct DescriptorInput {
    pub id: Vec<u8>,
    pub transports: u32,
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct CreateInput {
    pub rp_id: String,
    pub rp_name: String,
    pub challenge: Vec<u8>,
    pub user_id: Vec<u8>,
    pub user_name: String,
    pub user_display_name: String,
    pub client_data_json: Vec<u8>,
    pub client_data_hash: Vec<u8>,
    pub pub_key_algs: Vec<i32>,
    pub exclude_credentials: Vec<DescriptorInput>,
    pub timeout_ms: u32,
    pub authenticator_attachment: u32,
    pub user_verification: u32,
    pub attestation: u32,
    pub enterprise_attestation: u32,
    pub require_resident_key: bool,
    pub prefer_resident_key: bool,
    pub window_handle: u64,
    pub pin: Option<String>,
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct GetInput {
    pub rp_id: String,
    pub challenge: Vec<u8>,
    pub client_data_json: Vec<u8>,
    pub client_data_hash: Vec<u8>,
    pub allow_credentials: Vec<DescriptorInput>,
    pub timeout_ms: u32,
    pub authenticator_attachment: u32,
    pub user_verification: u32,
    pub window_handle: u64,
    pub pin: Option<String>,
}

pub struct CreateResult {
    pub raw_id: Vec<u8>,
    pub attestation_object: Vec<u8>,
    pub client_data_json: Vec<u8>,
    pub used_transport: u32,
}

pub struct GetResult {
    pub raw_id: Vec<u8>,
    pub authenticator_data: Vec<u8>,
    pub signature: Vec<u8>,
    pub user_handle: Option<Vec<u8>>,
    pub client_data_json: Vec<u8>,
    pub used_transport: u32,
}

pub fn attachment_from_transport(transport: u32) -> Option<&'static str> {
    if (transport & TRANSPORT_INTERNAL) != 0 {
        Some("platform")
    } else if transport != 0 {
        Some("cross-platform")
    } else {
        None
    }
}

#[allow(dead_code)]
fn append_cbor_len(out: &mut Vec<u8>, major: u8, len: usize) {
    if len < 24 {
        out.push((major << 5) | (len as u8));
    } else if len <= u8::MAX as usize {
        out.push((major << 5) | 24);
        out.push(len as u8);
    } else if len <= u16::MAX as usize {
        out.push((major << 5) | 25);
        out.push(((len >> 8) & 0xff) as u8);
        out.push((len & 0xff) as u8);
    } else if len <= u32::MAX as usize {
        out.push((major << 5) | 26);
        out.push(((len >> 24) & 0xff) as u8);
        out.push(((len >> 16) & 0xff) as u8);
        out.push(((len >> 8) & 0xff) as u8);
        out.push((len & 0xff) as u8);
    } else {
        out.push((major << 5) | 27);
        out.push(((len >> 56) & 0xff) as u8);
        out.push(((len >> 48) & 0xff) as u8);
        out.push(((len >> 40) & 0xff) as u8);
        out.push(((len >> 32) & 0xff) as u8);
        out.push(((len >> 24) & 0xff) as u8);
        out.push(((len >> 16) & 0xff) as u8);
        out.push(((len >> 8) & 0xff) as u8);
        out.push((len & 0xff) as u8);
    }
}

#[allow(dead_code)]
fn append_cbor_text(out: &mut Vec<u8>, text: &str) {
    let bytes = text.as_bytes();
    append_cbor_len(out, 3, bytes.len());
    out.extend_from_slice(bytes);
}

#[allow(dead_code)]
fn append_cbor_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    append_cbor_len(out, 2, bytes.len());
    out.extend_from_slice(bytes);
}

#[allow(dead_code)]
pub fn build_attestation_object(
    fmt: &str,
    auth_data: &[u8],
    att_stmt_cbor: &[u8],
) -> Result<Vec<u8>> {
    let mut out = Vec::with_capacity(auth_data.len() + att_stmt_cbor.len() + 32);

    append_cbor_len(&mut out, 5, 3);
    append_cbor_text(&mut out, "fmt");
    append_cbor_text(&mut out, fmt);
    append_cbor_text(&mut out, "attStmt");
    if att_stmt_cbor.is_empty() {
        out.push(0xa0);
    } else {
        out.extend_from_slice(att_stmt_cbor);
    }
    append_cbor_text(&mut out, "authData");
    append_cbor_bytes(&mut out, auth_data);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attachment_from_transport_matches_legacy() {
        assert_eq!(
            Some("platform"),
            attachment_from_transport(TRANSPORT_INTERNAL)
        );
        assert_eq!(
            Some("platform"),
            attachment_from_transport(TRANSPORT_INTERNAL | TRANSPORT_USB)
        );
        assert_eq!(
            Some("cross-platform"),
            attachment_from_transport(TRANSPORT_USB)
        );
        assert_eq!(
            Some("cross-platform"),
            attachment_from_transport(TRANSPORT_NFC)
        );
        assert_eq!(
            Some("cross-platform"),
            attachment_from_transport(TRANSPORT_BLE)
        );
        assert_eq!(None, attachment_from_transport(0));
    }

    #[test]
    fn cbor_len_small() {
        let mut out = Vec::new();
        append_cbor_len(&mut out, 5, 3);
        assert_eq!(out, vec![(5 << 5) | 3]);
    }

    #[test]
    fn cbor_text_encodes_short_string() {
        let mut out = Vec::new();
        append_cbor_text(&mut out, "fmt");

        assert_eq!(out, vec![0x63, b'f', b'm', b't']);
    }

    #[test]
    fn cbor_bytes_one_byte_length() {
        let payload = vec![0xab; 24];
        let mut out = Vec::new();
        append_cbor_bytes(&mut out, &payload);
        assert_eq!(out[0], (2 << 5) | 24);
        assert_eq!(out[1], 24);
        assert_eq!(&out[2..], &payload[..]);
    }

    #[test]
    fn cbor_len_two_byte_form() {
        let mut out = Vec::new();
        append_cbor_len(&mut out, 2, 0x0123);
        assert_eq!(out, vec![(2 << 5) | 25, 0x01, 0x23]);
    }

    #[test]
    fn cbor_len_four_byte_form() {
        let mut out = Vec::new();
        append_cbor_len(&mut out, 2, 0x0001_0203);
        assert_eq!(out, vec![(2 << 5) | 26, 0x00, 0x01, 0x02, 0x03]);
    }

    #[test]
    fn build_attestation_object_empty_att_stmt() {
        let auth_data = [0x01u8, 0x02, 0x03];
        let bytes = build_attestation_object("none", &auth_data, &[]).unwrap();

        let expected: Vec<u8> = vec![
            0xa3, 0x63, b'f', b'm', b't', 0x64, b'n', b'o', b'n', b'e', 0x67, b'a', b't', b't',
            b'S', b't', b'm', b't', 0xa0, 0x68, b'a', b'u', b't', b'h', b'D', b'a', b't', b'a',
            0x43, 0x01, 0x02, 0x03,
        ];
        assert_eq!(bytes, expected);
    }

    #[test]
    fn build_attestation_object_passes_through_packed_att_stmt() {
        let att_stmt = [
            0xa2u8, 0x63, b'a', b'l', b'g', 0x26, 0x63, b's', b'i', b'g', 0x41, 0x00,
        ];
        let auth_data = [0xaau8; 4];
        let bytes = build_attestation_object("packed", &auth_data, &att_stmt).unwrap();

        let needle = att_stmt.as_slice();
        let position = bytes
            .windows(needle.len())
            .position(|w| w == needle)
            .expect("att_stmt must be spliced unmodified");

        assert!(position >= 8);
        let header = &bytes[position - 8..position];
        assert_eq!(header, b"\x67attStmt");
    }
}
