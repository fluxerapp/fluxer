// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Deserialize;
use serde_json::Value;

use fluxer_svc::postgres::{KvClient, PostgresConfig, connect as connect_pool, decode_row};

use super::{
    LoadFuture, RowError, TopologyError, TopologyStore, VOICE_SERVERS_TABLE, VoiceServer,
    VoiceServerRow,
};

pub const BACKEND: &str = "postgres";
const WHOLE_TABLE_PREFIX: &str = "";

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default)]
struct VoiceServerKvRow {
    region_id: Option<String>,
    server_id: Option<String>,
    endpoint: Option<String>,
    api_key: Option<String>,
    api_secret: Option<String>,
    is_active: Option<bool>,
}

fn column(value: Option<String>, name: &'static str) -> Result<String, RowError> {
    value.ok_or(RowError::MissingColumn(name))
}

pub fn row_from_json(value: Value) -> Result<VoiceServerRow, RowError> {
    let decoded = decode_row(value).map_err(|_| RowError::Undecodable)?;
    if !decoded.is_object() {
        return Err(RowError::Undecodable);
    }
    let row: VoiceServerKvRow =
        serde_json::from_value(decoded).map_err(|_| RowError::Undecodable)?;

    Ok(VoiceServerRow {
        region_id: column(row.region_id, "region_id")?,
        server_id: column(row.server_id, "server_id")?,
        endpoint: column(row.endpoint, "endpoint")?,
        api_key: column(row.api_key, "api_key")?,
        api_secret: column(row.api_secret, "api_secret")?,
        is_active: row.is_active.unwrap_or(false),
    })
}

#[derive(Clone)]
pub struct PostgresTopology {
    kv: KvClient,
}

impl PostgresTopology {
    pub fn new(kv: KvClient) -> Self {
        Self { kv }
    }

    pub async fn connect(config: &PostgresConfig) -> anyhow::Result<Self> {
        let pool = connect_pool(config).await?;
        Ok(Self::new(KvClient::new(pool, config)?))
    }

    async fn read(&self) -> Result<Vec<VoiceServer>, TopologyError> {
        let rows = self
            .kv
            .get_row_key_prefix_rows(VOICE_SERVERS_TABLE, WHOLE_TABLE_PREFIX)
            .await
            .map_err(|error| TopologyError::Query(error.to_string()))?;

        let mut servers = Vec::with_capacity(rows.len());
        for (_, value) in rows {
            servers.push(row_from_json(value)?.into_server()?);
        }
        Ok(servers)
    }
}

impl std::fmt::Debug for PostgresTopology {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("PostgresTopology")
            .field("table", &VOICE_SERVERS_TABLE)
            .finish()
    }
}

impl TopologyStore for PostgresTopology {
    fn load(&self) -> LoadFuture<'_> {
        Box::pin(async move { self.read().await })
    }

    fn backend(&self) -> &'static str {
        BACKEND
    }
}
