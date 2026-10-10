// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, Object, StorageError,
    Store, record_storage_outcome, unversioned_read_budget,
};
use crate::{byte_budget::ByteBudget, config::StorageBackend, constants};

impl Store {
    pub async fn read_object(&self, bucket: &str, key: &str) -> Result<Object, StorageError> {
        let budget = unversioned_read_budget(constants::MAX_MEDIA_PROXY_BYTES);
        let object = self
            .read_object_limited(bucket, key, constants::MAX_MEDIA_PROXY_BYTES, &budget)
            .await?;
        Ok(Object {
            data: object.data.as_bytes().clone(),
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
        let result = self.read_object_direct(request).await;
        record_storage_outcome(&self.metrics, &result);
        result
    }

    async fn read_object_direct(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.read_local(request).await,
            StorageBackend::S3 => self.read_s3(request).await,
        }
    }
}
