// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    HeadResult, ObjectStreamPlan, ObjectStreamRequest, StorageError, Store, StreamObject,
    StreamRange, record_storage_outcome,
};
use crate::{config::StorageBackend, constants};

impl Store {
    pub async fn head_object(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        self.head_object_limited(bucket, key, constants::MAX_MEDIA_PROXY_BYTES)
            .await
    }

    pub async fn head_object_limited(
        &self,
        bucket: &str,
        key: &str,
        max_bytes: usize,
    ) -> Result<HeadResult, StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.head_local(bucket, key, max_bytes).await,
            StorageBackend::S3 => self.head_s3(bucket, key, max_bytes).await,
        }
    }

    pub async fn stream_object(
        &self,
        bucket: &str,
        key: &str,
        range_header: Option<&str>,
    ) -> Result<StreamObject, StorageError> {
        self.stream_object_inner(ObjectStreamPlan {
            bucket,
            key,
            max_bytes: constants::MAX_MEDIA_PROXY_BYTES,
            range: match range_header {
                Some(header) => StreamRange::Header(header),
                None => StreamRange::Full,
            },
            expected_identity: None,
        })
        .await
    }

    pub async fn stream_object_limited(
        &self,
        request: ObjectStreamRequest<'_>,
    ) -> Result<StreamObject, StorageError> {
        self.stream_object_inner(ObjectStreamPlan {
            bucket: request.bucket,
            key: request.key,
            max_bytes: request.max_bytes,
            range: match request.byte_range {
                Some(byte_range) => StreamRange::Bytes(byte_range),
                None => StreamRange::Full,
            },
            expected_identity: Some(request.expected_identity),
        })
        .await
    }

    async fn stream_object_inner(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        let result = match self.cfg.storage.backend {
            StorageBackend::Local => self.stream_local(plan).await,
            StorageBackend::S3 => self.stream_s3(plan).await,
        };
        record_storage_outcome(&self.metrics, &result);
        result
    }
}
