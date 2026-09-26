// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{StorageError, Store, s3::signed_headers};
use crate::{
    aws_sigv4,
    config::{BucketStyle, Config},
};
use http::StatusCode;
use percent_encoding::{AsciiSet, percent_encode};
use reqwest::Method;

const PATH_ENCODE_SET: &AsciiSet = &percent_encoding::NON_ALPHANUMERIC
    .remove(b'-')
    .remove(b'.')
    .remove(b'_')
    .remove(b'~')
    .remove(b'/');

impl Store {
    pub(super) fn s3_url(&self, bucket: &str, key: &str) -> Result<String, StorageError> {
        super::keys::safe_bucket(bucket)?;
        super::keys::safe_key(key)?;
        object_url(
            &self.cfg.storage.s3_endpoint,
            write_bucket_style(&self.cfg),
            bucket,
            key,
        )
    }

    pub(super) fn s3_read_url(&self, bucket: &str, key: &str) -> Result<String, StorageError> {
        let Some(endpoint) = self.read_endpoint_for(bucket) else {
            return self.s3_url(bucket, key);
        };
        super::keys::safe_bucket(bucket)?;
        super::keys::safe_key(key)?;
        object_url(endpoint, self.cfg.storage.s3_read_bucket_style, bucket, key)
    }

    pub(super) fn read_endpoint_for(&self, bucket: &str) -> Option<&str> {
        self.cfg
            .storage
            .s3_read_endpoint
            .as_deref()
            .filter(|_| bucket == self.cfg.storage.s3_read_bucket)
    }

    pub(super) fn read_is_unsigned(&self, bucket: &str) -> bool {
        self.read_endpoint_for(bucket).is_some() && !self.cfg.storage.s3_read_signed
    }

    pub(super) fn read_headers(
        &self,
        bucket: &str,
        method: Method,
        url: &str,
        extra_signed_headers: &[aws_sigv4::Header<'_>],
    ) -> Result<reqwest::header::HeaderMap, StorageError> {
        if self.read_is_unsigned(bucket) {
            return Ok(reqwest::header::HeaderMap::new());
        }
        let signed = self.sign(method, url, &[], None, extra_signed_headers)?;
        Ok(signed_headers(&signed, &self.cfg))
    }

    pub(super) fn read_status_error(
        &self,
        bucket: &str,
        status: StatusCode,
        versioned: bool,
    ) -> Option<StorageError> {
        if status == StatusCode::NOT_FOUND
            || (status == StatusCode::FORBIDDEN && self.read_is_unsigned(bucket))
        {
            return Some(if versioned {
                StorageError::ObjectChanged
            } else {
                StorageError::NotFound
            });
        }
        if status == StatusCode::PRECONDITION_FAILED {
            return Some(StorageError::ObjectChanged);
        }
        None
    }

    pub(super) fn s3_bucket_url(&self, bucket: &str) -> Result<String, StorageError> {
        super::keys::safe_bucket(bucket)?;
        let endpoint = &self.cfg.storage.s3_endpoint;
        if endpoint.is_empty() {
            return Err(StorageError::InvalidS3Endpoint);
        }
        let endpoint = endpoint.trim_end_matches('/');
        if self.cfg.storage.s3_force_path_style {
            return Ok(format!("{endpoint}/{bucket}"));
        }
        let (scheme, host, port, base_path) = virtual_hosted_endpoint(endpoint, bucket)?;
        Ok(format!("{scheme}://{bucket}.{host}{port}{base_path}"))
    }
}

fn write_bucket_style(cfg: &Config) -> BucketStyle {
    if cfg.storage.s3_force_path_style {
        BucketStyle::Path
    } else {
        BucketStyle::VirtualHosted
    }
}

fn object_url(
    endpoint: &str,
    style: BucketStyle,
    bucket: &str,
    key: &str,
) -> Result<String, StorageError> {
    if endpoint.is_empty() {
        return Err(StorageError::InvalidS3Endpoint);
    }
    let endpoint = endpoint.trim_end_matches('/');
    let encoded_key = percent_encode(key.as_bytes(), PATH_ENCODE_SET).to_string();
    match style {
        BucketStyle::Path => Ok(format!("{endpoint}/{bucket}/{encoded_key}")),
        BucketStyle::Rooted => Ok(format!("{endpoint}/{encoded_key}")),
        BucketStyle::VirtualHosted => {
            let (scheme, host, port, base_path) = virtual_hosted_endpoint(endpoint, bucket)?;
            Ok(format!(
                "{scheme}://{bucket}.{host}{port}{base_path}/{encoded_key}"
            ))
        }
    }
}

fn virtual_hosted_endpoint(
    endpoint: &str,
    bucket: &str,
) -> Result<(String, String, String, String), StorageError> {
    super::keys::validate_virtual_hosted_bucket(bucket)?;
    let parsed = url::Url::parse(endpoint).map_err(|_| StorageError::InvalidS3Endpoint)?;
    let host = parsed
        .host_str()
        .ok_or(StorageError::InvalidS3Endpoint)?
        .to_owned();
    let port = parsed.port().map(|p| format!(":{p}")).unwrap_or_default();
    Ok((
        parsed.scheme().to_owned(),
        host,
        port,
        parsed.path().trim_end_matches('/').to_owned(),
    ))
}
