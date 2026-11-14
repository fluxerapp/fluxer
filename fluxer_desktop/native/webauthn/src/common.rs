// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Result;
use napi::bindgen_prelude::{Error, Status};

pub fn ceremony_error(prefix: &str, message: &str) -> Error {
    Error::new(Status::GenericFailure, format!("{prefix}: {message}"))
}

pub const CREATE_PREFIX: &str = "WebAuthn registration failed";
pub const GET_PREFIX: &str = "WebAuthn authentication failed";

pub const TRANSPORT_USB: u32 = 0x0000_0001;
pub const TRANSPORT_INTERNAL: u32 = 0x0000_0010;

#[allow(dead_code)]
pub const ATTACHMENT_ANY: u32 = 0;

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
