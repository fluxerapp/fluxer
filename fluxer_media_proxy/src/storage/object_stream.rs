// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    HeadResult, ObjectStreamPlan, StorageError, Store, StreamObject, StreamRange, merge_fallback,
    record_storage_outcome, wants_fallback,
};
use crate::config::StorageBackend;

impl Store {
    pub async fn head_object(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        let result = self.head_backend(bucket, key).await;
        if wants_fallback(&result)
            && let Some(fallback) = self.reads_fallback()
        {
            let fallback_result = fallback
                .head_backend(&self.fallback_bucket(bucket), key)
                .await;
            self.record_fallback(&result, &fallback_result);
            return merge_fallback(result, fallback_result);
        }
        result
    }

    pub async fn stream_object(
        &self,
        bucket: &str,
        key: &str,
        range_header: Option<&str>,
    ) -> Result<StreamObject, StorageError> {
        let plan = ObjectStreamPlan {
            bucket,
            key,
            range: match range_header {
                Some(header) => StreamRange::Header(header),
                None => StreamRange::Full,
            },
        };
        let mut result = self.stream_backend(plan).await;
        if wants_fallback(&result)
            && let Some(fallback) = self.reads_fallback()
        {
            let bucket = self.fallback_bucket(bucket);
            let fallback_result = fallback
                .stream_backend(ObjectStreamPlan {
                    bucket: &bucket,
                    ..plan
                })
                .await;
            self.record_fallback(&result, &fallback_result);
            result = merge_fallback(result, fallback_result);
        }
        record_storage_outcome(&self.metrics, &result);
        result
    }

    async fn head_backend(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.head_local(bucket, key).await,
            StorageBackend::S3 => self.head_s3(bucket, key).await,
        }
    }

    async fn stream_backend(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.stream_local(plan).await,
            StorageBackend::S3 => self.stream_s3(plan).await,
        }
    }
}
