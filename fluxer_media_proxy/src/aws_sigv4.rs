// SPDX-License-Identifier: AGPL-3.0-or-later

use hmac::{Hmac, KeyInit, Mac};
use sha2::{Digest, Sha256};
use std::time::{SystemTime, UNIX_EPOCH};
use thiserror::Error;

type HmacSha256 = Hmac<Sha256>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Header<'a> {
    pub name: &'a str,
    pub value: &'a str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Timestamp {
    pub date_scope: String,
    pub amz_date: String,
}

#[derive(Clone, Debug)]
pub struct Options<'a> {
    pub method: &'a str,
    pub url: &'a str,
    pub region: &'a str,
    pub access_key_id: &'a str,
    pub secret_access_key: &'a str,
    pub session_token: &'a str,
    pub payload: &'a [u8],
    pub payload_hash_override: Option<&'a str>,
    pub extra_signed_headers: &'a [Header<'a>],
    pub timestamp: Option<Timestamp>,
}

impl<'a> Options<'a> {
    pub fn new(
        method: &'a str,
        url: &'a str,
        region: &'a str,
        access_key_id: &'a str,
        secret_access_key: &'a str,
    ) -> Self {
        Self {
            method,
            url,
            region,
            access_key_id,
            secret_access_key,
            session_token: "",
            payload: &[],
            payload_hash_override: None,
            extra_signed_headers: &[],
            timestamp: None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedRequest {
    pub host: String,
    pub signed_headers: String,
    pub authorization: String,
    pub payload_hash: String,
    pub amz_date: String,
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum Error {
    #[error("invalid URL")]
    InvalidUrl,
    #[error("invalid signed header")]
    InvalidHeader,
    #[error("missing AWS credentials")]
    MissingAwsCredentials,
    #[error("too many signed headers")]
    TooManySignedHeaders,
    #[error("system clock unavailable")]
    ClockUnavailable,
}

#[derive(Clone, Debug)]
struct ParsedUrl<'a> {
    host: String,
    path: &'a str,
    query: &'a str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct CanonicalHeader {
    name: String,
    value: String,
}

pub fn format_timestamp(
    year: u32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    second: u32,
) -> Timestamp {
    Timestamp {
        date_scope: format!("{year:04}{month:02}{day:02}"),
        amz_date: format!("{year:04}{month:02}{day:02}T{hour:02}{minute:02}{second:02}Z"),
    }
}

fn current_timestamp() -> Result<Timestamp, Error> {
    let seconds = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| Error::ClockUnavailable)?
        .as_secs() as i64;
    let days = seconds.div_euclid(86_400);
    let sod = seconds.rem_euclid(86_400);
    let (year, month, day) = civil_from_days(days);
    Ok(format_timestamp(
        year as u32,
        month,
        day,
        (sod / 3600) as u32,
        ((sod % 3600) / 60) as u32,
        (sod % 60) as u32,
    ))
}

fn civil_from_days(days_since_epoch: i64) -> (i32, u32, u32) {
    let z = days_since_epoch + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = mp + if mp < 10 { 3 } else { -9 };
    let year = y + if m <= 2 { 1 } else { 0 };
    (year as i32, m as u32, d as u32)
}

fn parse_url(url: &str) -> Result<ParsedUrl<'_>, Error> {
    let scheme_end = url.find("://").ok_or(Error::InvalidUrl)?;
    let mut rest = &url[scheme_end + 3..];
    let authority_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
    if authority_end == 0 {
        return Err(Error::InvalidUrl);
    }
    let authority = &rest[..authority_end];
    rest = &rest[authority_end..];
    if authority.contains('@') {
        return Err(Error::InvalidUrl);
    }
    let host_raw = if authority.starts_with('[') {
        authority
    } else if let Some(colon) = authority.find(':') {
        &authority[..colon]
    } else {
        authority
    };
    if host_raw.is_empty() {
        return Err(Error::InvalidUrl);
    }
    let host = authority.to_ascii_lowercase();
    let mut path = "/";
    let mut query = "";
    if let Some(after_slash) = rest.strip_prefix('/') {
        let path_end = after_slash
            .find(['?', '#'])
            .map(|idx| idx + 1)
            .unwrap_or(rest.len());
        path = &rest[..path_end];
        rest = &rest[path_end..];
    }
    if let Some(after_q) = rest.strip_prefix('?') {
        let query_end = after_q.find('#').unwrap_or(after_q.len());
        query = &after_q[..query_end];
        rest = &after_q[query_end..];
    }
    if rest.starts_with('#') {
        return Err(Error::InvalidUrl);
    }
    Ok(ParsedUrl { host, path, query })
}

fn sha256_hex(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

fn hmac_sha256(key: &[u8], data: &str) -> [u8; 32] {
    let mut mac = HmacSha256::new_from_slice(key).expect("HMAC accepts any key length");
    mac.update(data.as_bytes());
    mac.finalize().into_bytes().into()
}

fn lower_name(name: &str) -> Result<String, Error> {
    let trimmed = name.trim_matches([' ', '\t']);
    if trimmed.is_empty() {
        return Err(Error::InvalidHeader);
    }
    Ok(trimmed.to_ascii_lowercase())
}

fn normalize_value(value: &str) -> String {
    let mut out = String::new();
    let mut in_space = false;
    for ch in value.trim_matches([' ', '\t', '\r', '\n']).chars() {
        if ch == ' ' || ch == '\t' {
            if !in_space {
                out.push(' ');
            }
            in_space = true;
        } else {
            out.push(ch);
            in_space = false;
        }
    }
    out
}

fn add_header(headers: &mut Vec<CanonicalHeader>, name: &str, value: &str) -> Result<(), Error> {
    if headers.len() >= 32 {
        return Err(Error::TooManySignedHeaders);
    }
    headers.push(CanonicalHeader {
        name: lower_name(name)?,
        value: normalize_value(value),
    });
    Ok(())
}

pub fn sign(options: Options<'_>) -> Result<SignedRequest, Error> {
    if options.region.is_empty()
        || options.access_key_id.is_empty()
        || options.secret_access_key.is_empty()
    {
        return Err(Error::MissingAwsCredentials);
    }
    let parsed = parse_url(options.url)?;
    let timestamp = match options.timestamp {
        Some(ts) => ts,
        None => current_timestamp()?,
    };
    let payload_hash = options
        .payload_hash_override
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| sha256_hex(options.payload));

    let mut headers = Vec::with_capacity(4 + options.extra_signed_headers.len());
    add_header(&mut headers, "host", &parsed.host)?;
    add_header(&mut headers, "x-amz-content-sha256", &payload_hash)?;
    add_header(&mut headers, "x-amz-date", &timestamp.amz_date)?;
    if !options.session_token.is_empty() {
        add_header(&mut headers, "x-amz-security-token", options.session_token)?;
    }
    for header in options.extra_signed_headers {
        add_header(&mut headers, header.name, header.value)?;
    }
    headers.sort_by(|a, b| a.name.cmp(&b.name));

    let signed_headers = headers
        .iter()
        .map(|h| h.name.as_str())
        .collect::<Vec<_>>()
        .join(";");
    let mut canonical_request = format!("{}\n{}\n{}\n", options.method, parsed.path, parsed.query);
    for header in &headers {
        canonical_request.push_str(&header.name);
        canonical_request.push(':');
        canonical_request.push_str(&header.value);
        canonical_request.push('\n');
    }
    canonical_request.push('\n');
    canonical_request.push_str(&signed_headers);
    canonical_request.push('\n');
    canonical_request.push_str(&payload_hash);
    let canonical_request_hash = sha256_hex(canonical_request.as_bytes());
    let credential_scope = format!(
        "{}/{}/s3/aws4_request",
        timestamp.date_scope, options.region
    );
    let string_to_sign = format!(
        "AWS4-HMAC-SHA256\n{}\n{}\n{}",
        timestamp.amz_date, credential_scope, canonical_request_hash
    );
    let date_key = hmac_sha256(
        format!("AWS4{}", options.secret_access_key).as_bytes(),
        &timestamp.date_scope,
    );
    let date_region_key = hmac_sha256(&date_key, options.region);
    let date_region_service_key = hmac_sha256(&date_region_key, "s3");
    let signing_key = hmac_sha256(&date_region_service_key, "aws4_request");
    let signature = hex::encode(hmac_sha256(&signing_key, &string_to_sign));
    let authorization = format!(
        "AWS4-HMAC-SHA256 Credential={}/{},SignedHeaders={},Signature={}",
        options.access_key_id, credential_scope, signed_headers, signature
    );
    Ok(SignedRequest {
        host: parsed.host,
        signed_headers,
        authorization,
        payload_hash,
        amz_date: timestamp.amz_date,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aws_s3_sigv4_get_example() {
        let mut opts = Options::new(
            "GET",
            "https://examplebucket.s3.amazonaws.com/test.txt",
            "us-east-1",
            "AKIAIOSFODNN7EXAMPLE",
            "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        );
        opts.extra_signed_headers = &[Header {
            name: "Range",
            value: "bytes=0-9",
        }];
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        let signed = sign(opts).unwrap();
        assert_eq!(
            "host;range;x-amz-content-sha256;x-amz-date",
            signed.signed_headers
        );
        assert_eq!(
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            signed.payload_hash
        );
        assert_eq!(
            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41",
            signed.authorization
        );
    }

    #[test]
    fn aws_s3_sigv4_supports_unsigned_payload_hash_override() {
        let mut opts = Options::new(
            "PUT",
            "https://examplebucket.s3.amazonaws.com/upload.bin",
            "us-east-1",
            "AKIAIOSFODNN7EXAMPLE",
            "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        );
        opts.payload_hash_override = Some("UNSIGNED-PAYLOAD");
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        let signed = sign(opts).unwrap();
        assert_eq!("UNSIGNED-PAYLOAD", signed.payload_hash);
        assert!(
            signed
                .authorization
                .contains("SignedHeaders=host;x-amz-content-sha256;x-amz-date")
        );
    }

    #[test]
    fn current_time_conversion_has_epoch_baseline() {
        assert_eq!(format_timestamp(1970, 1, 1, 0, 0, 0), {
            let (y, m, d) = civil_from_days(0);
            format_timestamp(y as u32, m, d, 0, 0, 0)
        });
    }

    const ACCESS_KEY_ID: &str = "AKIAIOSFODNN7EXAMPLE";
    const SECRET_ACCESS_KEY: &str = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY";
    const OBJECT_URL: &str = "https://examplebucket.s3.amazonaws.com/test.txt";

    fn sign_get(session_token: &str, extra_signed_headers: &[Header<'_>]) -> SignedRequest {
        let mut opts = Options::new(
            "GET",
            OBJECT_URL,
            "us-east-1",
            ACCESS_KEY_ID,
            SECRET_ACCESS_KEY,
        );
        opts.session_token = session_token;
        opts.extra_signed_headers = extra_signed_headers;
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        sign(opts).unwrap()
    }

    #[test]
    fn aws_s3_sigv4_put_object_example() {
        let mut opts = Options::new(
            "PUT",
            "https://examplebucket.s3.amazonaws.com/test%24file.text",
            "us-east-1",
            ACCESS_KEY_ID,
            SECRET_ACCESS_KEY,
        );
        opts.payload = b"Welcome to Amazon S3.";
        opts.extra_signed_headers = &[
            Header {
                name: "Date",
                value: "Fri, 24 May 2013 00:00:00 GMT",
            },
            Header {
                name: "x-amz-storage-class",
                value: "REDUCED_REDUNDANCY",
            },
        ];
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        let signed = sign(opts).unwrap();
        assert_eq!(
            "44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072",
            signed.payload_hash
        );
        assert_eq!(
            "date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class",
            signed.signed_headers
        );
        assert_eq!("examplebucket.s3.amazonaws.com", signed.host);
        assert_eq!("20130524T000000Z", signed.amz_date);
        assert_eq!(
            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature=98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd",
            signed.authorization
        );
    }

    #[test]
    fn extra_signed_headers_are_canonical_whatever_order_they_arrive_in() {
        let ascending = sign_get(
            "",
            &[
                Header {
                    name: "Accept-Encoding",
                    value: "identity",
                },
                Header {
                    name: "Range",
                    value: "bytes=0-9",
                },
                Header {
                    name: "X-Amz-Meta-Album",
                    value: "covers",
                },
            ],
        );
        let descending = sign_get(
            "",
            &[
                Header {
                    name: "\tX-AMZ-META-ALBUM ",
                    value: "covers",
                },
                Header {
                    name: "  range",
                    value: "bytes=0-9",
                },
                Header {
                    name: "accept-encoding",
                    value: "identity",
                },
            ],
        );
        assert_eq!(
            "accept-encoding;host;range;x-amz-content-sha256;x-amz-date;x-amz-meta-album",
            ascending.signed_headers
        );
        assert_eq!(ascending.signed_headers, descending.signed_headers);
        assert_eq!(ascending.authorization, descending.authorization);
    }

    #[test]
    fn signed_header_values_are_trimmed_and_their_inner_runs_collapsed() {
        let exact = sign_get(
            "",
            &[Header {
                name: "X-Amz-Meta-Note",
                value: "one two",
            }],
        );
        for padded in ["  one two  ", "\tone two\r\n", "one  \t  two"] {
            assert_eq!(
                exact.authorization,
                sign_get(
                    "",
                    &[Header {
                        name: "X-Amz-Meta-Note",
                        value: padded,
                    }],
                )
                .authorization,
                "{padded:?}"
            );
        }
        assert_ne!(
            exact.authorization,
            sign_get(
                "",
                &[Header {
                    name: "X-Amz-Meta-Note",
                    value: "onetwo",
                }],
            )
            .authorization
        );
    }

    #[test]
    fn a_configured_session_token_is_signed_as_x_amz_security_token() {
        let anonymous = sign_get("", &[]);
        assert_eq!(
            "host;x-amz-content-sha256;x-amz-date",
            anonymous.signed_headers
        );
        let session = sign_get("FQoGZXIvYXdzEXAMPLESESSIONTOKEN", &[]);
        assert_eq!(
            "host;x-amz-content-sha256;x-amz-date;x-amz-security-token",
            session.signed_headers
        );
        assert!(
            session.authorization.contains(
                "SignedHeaders=host;x-amz-content-sha256;x-amz-date;x-amz-security-token"
            )
        );
        assert_ne!(anonymous.authorization, session.authorization);
        assert_ne!(
            session.authorization,
            sign_get("FQoGZXIvYXdzANOTHERSESSIONTOKEN", &[]).authorization
        );
    }

    #[test]
    fn empty_credentials_and_broken_urls_are_refused_before_signing() {
        let mut opts = Options::new("GET", OBJECT_URL, "", ACCESS_KEY_ID, SECRET_ACCESS_KEY);
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        assert_eq!(Err(Error::MissingAwsCredentials), sign(opts));
        let mut opts = Options::new(
            "GET",
            "https://",
            "us-east-1",
            ACCESS_KEY_ID,
            SECRET_ACCESS_KEY,
        );
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        assert_eq!(Err(Error::InvalidUrl), sign(opts));
        let mut opts = Options::new(
            "GET",
            OBJECT_URL,
            "us-east-1",
            ACCESS_KEY_ID,
            SECRET_ACCESS_KEY,
        );
        opts.extra_signed_headers = &[Header {
            name: "   ",
            value: "x",
        }];
        opts.timestamp = Some(format_timestamp(2013, 5, 24, 0, 0, 0));
        assert_eq!(Err(Error::InvalidHeader), sign(opts));
    }
}
