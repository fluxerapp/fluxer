// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    HeadResult, ObjectStreamPlan, StorageError, Store, StreamObject, StreamRange,
    record_storage_outcome,
};
use crate::config::StorageBackend;

impl Store {
    pub async fn head_object(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.head_local(bucket, key).await,
            StorageBackend::S3 => self.head_s3(bucket, key).await,
        }
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
        let result = match self.cfg.storage.backend {
            StorageBackend::Local => self.stream_local(plan).await,
            StorageBackend::S3 => self.stream_s3(plan).await,
        };
        record_storage_outcome(&self.metrics, &result);
        result
    }
}
