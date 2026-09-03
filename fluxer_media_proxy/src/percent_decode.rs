// SPDX-License-Identifier: AGPL-3.0-or-later

use thiserror::Error;

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum PercentDecodeError {
    #[error("percent-encoded value is invalid")]
    InvalidEncoding,
    #[error("percent-decoded value length overflowed")]
    LengthOverflow,
    #[error("percent-decoded value allocation failed")]
    AllocationFailed,
}

fn hex_value(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

fn decode_bytes(input: &str) -> Result<Vec<u8>, PercentDecodeError> {
    let bytes = input.as_bytes();
    let mut decoded = Vec::new();
    decoded
        .try_reserve_exact(bytes.len())
        .map_err(|_| PercentDecodeError::AllocationFailed)?;
    let mut index = 0;
    while index < bytes.len() {
        let value = bytes[index];
        if value == b'%' {
            let escape_start = index
                .checked_add(1)
                .ok_or(PercentDecodeError::LengthOverflow)?;
            let escape_end = index
                .checked_add(2)
                .ok_or(PercentDecodeError::LengthOverflow)?;
            if escape_end >= bytes.len() {
                return Err(PercentDecodeError::InvalidEncoding);
            }
            let (Some(high), Some(low)) =
                (hex_value(bytes[escape_start]), hex_value(bytes[escape_end]))
            else {
                return Err(PercentDecodeError::InvalidEncoding);
            };
            let decoded_value = (high << 4) | low;
            if decoded_value == b'/' {
                return Err(PercentDecodeError::InvalidEncoding);
            }
            decoded.push(decoded_value);
            index = index
                .checked_add(3)
                .ok_or(PercentDecodeError::LengthOverflow)?;
            continue;
        }
        decoded.push(value);
        index = index
            .checked_add(1)
            .ok_or(PercentDecodeError::LengthOverflow)?;
    }
    Ok(decoded)
}

pub fn decode_utf8(input: &str) -> Result<String, PercentDecodeError> {
    String::from_utf8(decode_bytes(input)?).map_err(|_| PercentDecodeError::InvalidEncoding)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{disposition, external_path, test_fixtures::ADVERSARIAL_TEXT_INPUTS};

    #[test]
    fn percent_decode_accepts_utf8_and_rejects_bad_or_path_changing_escapes() {
        assert_eq!(decode_utf8("hello%20world"), Ok("hello world".to_owned()));
        assert_eq!(decode_utf8("%C3%A9"), Ok("é".to_owned()));
        for invalid in ["%", "%0", "%GG", "%2F", "%2f", "%FF"] {
            assert_eq!(
                decode_utf8(invalid),
                Err(PercentDecodeError::InvalidEncoding),
                "accepted {invalid}"
            );
        }
    }

    #[test]
    fn strictness_does_not_leak_into_the_lossy_external_path_decoder() {
        assert_eq!("a/b", external_path::percent_decode_string("a%2Fb", false));
        assert_eq!("a b", external_path::percent_decode_string("a+b", true));
        assert_eq!("a%", external_path::percent_decode_string("a%", false));
        assert_eq!(
            "\u{fffd}",
            external_path::percent_decode_string("%FF", false)
        );
    }

    #[test]
    fn text_parsers_reject_adversarial_input_without_panicking() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            if let Ok(decoded) = decode_utf8(text) {
                assert_eq!(
                    decoded.contains('/'),
                    text.contains('/'),
                    "decoding changed the path structure of {text}"
                );
            }
            for decision in [
                disposition::Decision::Inline,
                disposition::Decision::Attachment,
            ] {
                assert!(
                    disposition::header(decision, Some(text)).is_ok(),
                    "disposition rejected {text}"
                );
            }
        }
    }
}
