// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, Object, StorageError,
    Store, record_storage_outcome, unversioned_read_budget,
};
use crate::{byte_budget::ByteBudget, config::StorageBackend, constants};
use bytes::Bytes;

impl Store {
    pub async fn read_object(&self, bucket: &str, key: &str) -> Result<Object, StorageError> {
        let budget = unversioned_read_budget(constants::MAX_MEDIA_PROXY_BYTES);
        self.read_object_within(bucket, key, &budget).await
    }

    pub async fn read_object_within(
        &self,
        bucket: &str,
        key: &str,
        budget: &ByteBudget,
    ) -> Result<Object, StorageError> {
        let object = self
            .read_object_limited(bucket, key, constants::MAX_MEDIA_PROXY_BYTES, budget)
            .await?;
        Ok(Object {
            data: Bytes::from_owner(object.data),
            content_type: object.content_type,
        })
    }

    pub async fn read_object_limited(
        &self,
        bucket: &str,
        key: &str,
        limit: usize,
        budget: &ByteBudget,
    ) -> Result<BufferedStorageObject, StorageError> {
        self.read_object_inner(BufferedObjectReadRequest {
            bucket,
            key,
            limit,
            budget,
            content_digest: ContentDigestRequest::Omit,
        })
        .await
    }

    pub async fn read_object_limited_with_digest(
        &self,
        bucket: &str,
        key: &str,
        limit: usize,
        budget: &ByteBudget,
    ) -> Result<BufferedStorageObject, StorageError> {
        self.read_object_inner(BufferedObjectReadRequest {
            bucket,
            key,
            limit,
            budget,
            content_digest: ContentDigestRequest::Include,
        })
        .await
    }

    async fn read_object_inner(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let result = match self.cfg.storage.backend {
            StorageBackend::Local => self.read_local(request).await,
            StorageBackend::S3 => self.read_s3(request).await,
        };
        record_storage_outcome(&self.metrics, &result);
        result
    }
}
