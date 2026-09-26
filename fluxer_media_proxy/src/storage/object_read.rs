// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, Object,
    ObjectReadRequest, StorageError, Store, record_storage_outcome, unversioned_read_budget,
};
use crate::{
    byte_budget::ByteBudget,
    config::StorageBackend,
    constants,
    storage::source_read::{SourceReadClaim, SourceReadWaitOutcome},
};

const SOURCE_READ_LEADER_RETRY_LIMIT: usize = 1;

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
            expected_identity: None,
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
            expected_identity: None,
            content_digest: ContentDigestRequest::Include,
        })
        .await
    }

    pub async fn read_object_versioned(
        &self,
        request: ObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        self.read_object_inner(BufferedObjectReadRequest {
            bucket: request.bucket,
            key: request.key,
            limit: request.max_bytes,
            budget: request.budget,
            expected_identity: Some(request.expected_identity),
            content_digest: ContentDigestRequest::Omit,
        })
        .await
    }

    async fn read_object_inner(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let Some(expected_identity) = request.expected_identity else {
            let result = self.read_object_direct(request).await;
            record_storage_outcome(&self.metrics, &result);
            return result;
        };
        let key = format!(
            "{}\u{0}{}\u{0}{}\u{0}{}\u{0}{}",
            request.bucket,
            request.key,
            expected_identity.cache_identity(),
            request.limit,
            request.content_digest.cache_key()
        );
        let mut leader_retries = 0;
        loop {
            let claim = match self.source_reads.claim(key.clone()) {
                Ok(claim) => claim,
                Err(error) => {
                    let result = Err(error);
                    record_storage_outcome::<BufferedStorageObject>(&self.metrics, &result);
                    return result;
                }
            };
            match claim {
                SourceReadClaim::Leader(leader) => {
                    let result = self.read_object_direct(request).await;
                    leader.publish(&result);
                    record_storage_outcome(&self.metrics, &result);
                    return result;
                }
                SourceReadClaim::Waiter(waiter) => match waiter.wait().await {
                    SourceReadWaitOutcome::Retry
                        if leader_retries < SOURCE_READ_LEADER_RETRY_LIMIT =>
                    {
                        leader_retries += 1;
                    }
                    SourceReadWaitOutcome::Retry => {
                        let result = Err(StorageError::SourceReadLeaderEnded);
                        record_storage_outcome::<BufferedStorageObject>(&self.metrics, &result);
                        return result;
                    }
                    SourceReadWaitOutcome::Completed(result) => {
                        record_storage_outcome::<BufferedStorageObject>(&self.metrics, &result);
                        return result;
                    }
                },
            }
        }
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
