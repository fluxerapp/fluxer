// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use scylla::DeserializeRow;
use scylla::client::session::Session;
use scylla::statement::prepared::PreparedStatement;

use fluxer_svc::scylla::{ScyllaConfig, connect};

use super::{LoadFuture, RowError, TopologyError, TopologyStore, VoiceServer, VoiceServerRow};

pub const BACKEND: &str = "scylla";
pub const VOICE_SERVERS_CQL: &str =
    "SELECT region_id, server_id, endpoint, api_key, api_secret, is_active FROM voice_servers";

#[derive(Debug, DeserializeRow)]
struct VoiceServerCqlRow {
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

fn row_from_cql(row: VoiceServerCqlRow) -> Result<VoiceServerRow, RowError> {
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
pub struct ScyllaTopology {
    session: Arc<Session>,
    statement: PreparedStatement,
}

impl ScyllaTopology {
    pub async fn new(session: Arc<Session>) -> anyhow::Result<Self> {
        let statement = session.prepare(VOICE_SERVERS_CQL).await?;
        Ok(Self { session, statement })
    }

    pub async fn connect(config: &ScyllaConfig) -> anyhow::Result<Self> {
        Self::new(connect(config).await?).await
    }

    async fn read(&self) -> Result<Vec<VoiceServer>, TopologyError> {
        let result = self
            .session
            .execute_unpaged(&self.statement, ())
            .await
            .map_err(|error| TopologyError::Unavailable(error.to_string()))?;
        let rows = result
            .into_rows_result()
            .map_err(|error| TopologyError::Query(error.to_string()))?;

        let mut servers = Vec::new();
        for row in rows
            .rows::<VoiceServerCqlRow>()
            .map_err(|error| TopologyError::Query(error.to_string()))?
        {
            let row = row.map_err(|error| TopologyError::Query(error.to_string()))?;
            servers.push(row_from_cql(row)?.into_server()?);
        }
        Ok(servers)
    }
}

impl std::fmt::Debug for ScyllaTopology {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ScyllaTopology")
            .field("statement", &VOICE_SERVERS_CQL)
            .finish()
    }
}

impl TopologyStore for ScyllaTopology {
    fn load(&self) -> LoadFuture<'_> {
        Box::pin(async move { self.read().await })
    }

    fn backend(&self) -> &'static str {
        BACKEND
    }
}
