// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::upload_relay::{RelayError, token::TokenMethod, token::TokenPayload};
use http::{HeaderValue, Method};

const CONTENT_TYPE_MAX_BYTES: usize = 255;
const CONTENT_TYPE_PARAMETER_MAX_COUNT: usize = 32;
const CONTENT_TYPE_TOKEN_MAX_BYTES: usize = 127;

#[derive(Clone, Copy, Debug)]
pub struct RelayRequest<'a> {
    pub uploads_bucket: &'a str,
    pub request_key: &'a str,
    pub request_method: &'a Method,
    pub query_upload_id: Option<&'a str>,
    pub query_part_number: Option<u32>,
    pub content_length: Option<u64>,
    pub max_body_bytes: u64,
}

pub fn validate_relay_request(
    token: &TokenPayload,
    request: RelayRequest<'_>,
) -> Result<(), RelayError> {
    if token.b != request.uploads_bucket {
        return Err(RelayError::WrongBucket);
    }
    if token.k != request.request_key {
        return Err(RelayError::KeyMismatch);
    }
    if token.m != TokenMethod::Put || request.request_method != Method::PUT {
        return Err(RelayError::MethodMismatch);
    }
    match_upload_id(token.u.as_deref(), request.query_upload_id)?;
    match_part_number(token.p, request.query_part_number)?;
    if let Some(declared) = request.content_length
        && (declared > token.mb || declared > request.max_body_bytes)
    {
        return Err(RelayError::PayloadTooLarge);
    }
    Ok(())
}

fn match_upload_id(
    token_upload_id: Option<&str>,
    request_upload_id: Option<&str>,
) -> Result<(), RelayError> {
    match (token_upload_id, request_upload_id) {
        (Some(expected), Some(actual)) if expected == actual => Ok(()),
        (Some(_), _) => Err(RelayError::UploadIdMismatch),
        (None, Some(actual)) if !actual.is_empty() => Err(RelayError::UploadIdMismatch),
        _ => Ok(()),
    }
}

fn match_part_number(
    token_part_number: Option<u32>,
    request_part_number: Option<u32>,
) -> Result<(), RelayError> {
    match (token_part_number, request_part_number) {
        (None, None) => Ok(()),
        (Some(a), Some(b)) if a == b => Ok(()),
        _ => Err(RelayError::PartNumberMismatch),
    }
}

pub fn query_part_number(raw: Option<&str>) -> Result<Option<u32>, RelayError> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    if raw.is_empty() {
        return Err(RelayError::BadQuery);
    }
    raw.parse().map(Some).map_err(|_| RelayError::BadQuery)
}

pub fn valid_content_type(value: &str) -> bool {
    if value.is_empty()
        || value.len() > CONTENT_TYPE_MAX_BYTES
        || HeaderValue::from_bytes(value.as_bytes()).is_err()
    {
        return false;
    }
    let bytes = value.as_bytes();
    let parameter_start = bytes.iter().position(|byte| *byte == b';');
    let media_end = parameter_start.unwrap_or(bytes.len());
    let media_start = skip_optional_whitespace(bytes, 0, media_end);
    let media_end = trim_optional_whitespace_end(bytes, media_start, media_end);
    let media_type = &bytes[media_start..media_end];
    let Some(slash) = media_type.iter().position(|byte| *byte == b'/') else {
        return false;
    };
    let kind = &media_type[..slash];
    let subtype = &media_type[slash + 1..];
    if !valid_media_type_token(kind) || !valid_media_type_token(subtype) {
        return false;
    }
    let Some(mut index) = parameter_start else {
        return true;
    };
    let mut parameter_names = [(0usize, 0usize); CONTENT_TYPE_PARAMETER_MAX_COUNT];
    let mut parameter_count = 0usize;
    while index < bytes.len() {
        if bytes[index] != b';' {
            return false;
        }
        index = skip_optional_whitespace(bytes, index + 1, bytes.len());
        let name_start = index;
        while index < bytes.len() && is_content_type_token_byte(bytes[index]) {
            index += 1;
        }
        if index == name_start || index - name_start > CONTENT_TYPE_TOKEN_MAX_BYTES {
            return false;
        }
        if parameter_count == CONTENT_TYPE_PARAMETER_MAX_COUNT {
            return false;
        }
        if parameter_names[..parameter_count]
            .iter()
            .any(|(start, end)| bytes[*start..*end].eq_ignore_ascii_case(&bytes[name_start..index]))
        {
            return false;
        }
        parameter_names[parameter_count] = (name_start, index);
        parameter_count += 1;
        index = skip_optional_whitespace(bytes, index, bytes.len());
        if bytes.get(index) != Some(&b'=') {
            return false;
        }
        index = skip_optional_whitespace(bytes, index + 1, bytes.len());
        let Some(value_end) = content_type_parameter_value_end(bytes, index) else {
            return false;
        };
        index = skip_optional_whitespace(bytes, value_end, bytes.len());
        if index < bytes.len() && bytes[index] != b';' {
            return false;
        }
    }
    true
}

fn valid_media_type_token(value: &[u8]) -> bool {
    !value.is_empty()
        && value != b"*"
        && value.len() <= CONTENT_TYPE_TOKEN_MAX_BYTES
        && value.iter().copied().all(is_content_type_token_byte)
}

fn content_type_parameter_value_end(bytes: &[u8], start: usize) -> Option<usize> {
    if bytes.get(start) == Some(&b'"') {
        return quoted_content_type_parameter_value_end(bytes, start + 1);
    }
    let mut index = start;
    while index < bytes.len() && is_content_type_token_byte(bytes[index]) {
        index += 1;
    }
    (index > start && index - start <= CONTENT_TYPE_TOKEN_MAX_BYTES).then_some(index)
}

fn quoted_content_type_parameter_value_end(bytes: &[u8], mut index: usize) -> Option<usize> {
    loop {
        let byte = *bytes.get(index)?;
        match byte {
            b'"' => return Some(index + 1),
            b'\\' => {
                index += 1;
                if !is_quoted_pair_byte(*bytes.get(index)?) {
                    return None;
                }
            }
            _ if !is_quoted_text_byte(byte) => return None,
            _ => {}
        }
        index += 1;
    }
}

fn skip_optional_whitespace(bytes: &[u8], mut index: usize, end: usize) -> usize {
    while index < end && matches!(bytes[index], b' ' | b'\t') {
        index += 1;
    }
    index
}

fn trim_optional_whitespace_end(bytes: &[u8], start: usize, mut end: usize) -> usize {
    while end > start && matches!(bytes[end - 1], b' ' | b'\t') {
        end -= 1;
    }
    end
}

fn is_content_type_token_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric()
        || matches!(
            byte,
            b'!' | b'#'
                | b'$'
                | b'%'
                | b'&'
                | b'\''
                | b'*'
                | b'+'
                | b'-'
                | b'.'
                | b'^'
                | b'_'
                | b'`'
                | b'|'
                | b'~'
        )
}

fn is_quoted_text_byte(byte: u8) -> bool {
    matches!(byte, b'\t' | b' ' | b'!' | b'#'..=b'[' | b']'..=b'~' | 0x80..=0xff)
}

fn is_quoted_pair_byte(byte: u8) -> bool {
    matches!(byte, b'\t' | b' '..=b'~' | 0x80..=0xff)
}
