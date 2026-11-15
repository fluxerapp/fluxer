// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::generator::{MAX_WORKER_ID, SnowflakeGenerator};
use crate::types::{SERVICE_NAME, SnowflakeRequest, SnowflakeResponse};
use fluxer_svc::shard::ShardService;
use tokio::sync::Mutex;

pub const MAX_BATCH_SIZE: u32 = 512;

pub struct SnowflakesShard {
    generator: Mutex<SnowflakeGenerator>,
}

impl SnowflakesShard {
    pub fn new(worker_id: u32) -> anyhow::Result<Self> {
        if worker_id > MAX_WORKER_ID {
            anyhow::bail!("snowflake shard id {worker_id} exceeds max worker id {MAX_WORKER_ID}");
        }
        Ok(Self {
            generator: Mutex::new(SnowflakeGenerator::new(worker_id)?),
        })
    }

    async fn generate_batch(&self, count: u32) -> anyhow::Result<SnowflakeResponse> {
        if count == 0 || count > MAX_BATCH_SIZE {
            anyhow::bail!("batch count must be between 1 and {MAX_BATCH_SIZE}");
        }
        let mut generator = self.generator.lock().await;
        let ids = generator
            .generate_many(count)
            .await?
            .into_iter()
            .map(|id| id.to_string())
            .collect();
        Ok(SnowflakeResponse { ids })
    }
}

impl ShardService for SnowflakesShard {
    type Request = SnowflakeRequest;
    type Response = SnowflakeResponse;

    fn service_name(&self) -> &str {
        SERVICE_NAME
    }

    async fn handle(&self, request: SnowflakeRequest) -> anyhow::Result<SnowflakeResponse> {
        match request {
            SnowflakeRequest::GenerateBatch { count, .. } => self.generate_batch(count).await,
        }
    }
}
