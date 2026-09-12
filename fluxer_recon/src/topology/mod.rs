// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod postgres;

#[cfg(feature = "scylla")]
pub mod scylla;

use std::future::Future;
use std::pin::Pin;

use crate::config::ReconConfig;
use crate::evidence::{ServerRecord, TopologyFreshness, TopologyLens};
use crate::ids::{ApiSecret, Epoch, IdError, Location, Millis, RegionId, ServerId};

pub const VOICE_SERVERS_TABLE: &str = "voice_servers";

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum RowError {
    #[error("the {0} column is missing")]
    MissingColumn(&'static str),
    #[error("the {0} column is empty")]
    EmptyColumn(&'static str),
    #[error("the {column} column is not a valid identifier: {source}")]
    InvalidColumn {
        column: &'static str,
        source: IdError,
    },
    #[error("the row cannot be decoded as a voice_servers row")]
    Undecodable,
}

#[derive(Debug, thiserror::Error)]
pub enum TopologyError {
    #[error("the topology backend is unavailable: {0}")]
    Unavailable(String),
    #[error("the topology query failed: {0}")]
    Query(String),
    #[error("a voice_servers row is malformed: {0}")]
    Row(#[from] RowError),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VoiceServerRow {
    pub region_id: String,
    pub server_id: String,
    pub endpoint: String,
    pub api_key: String,
    pub api_secret: String,
    pub is_active: bool,
}

impl VoiceServerRow {
    pub fn into_server(self) -> Result<VoiceServer, RowError> {
        let region = RegionId::new(self.region_id.trim()).map_err(|source| match source {
            IdError::Empty => RowError::EmptyColumn("region_id"),
            IdError::ContainsWhitespace | IdError::ContainsUnderscore => RowError::InvalidColumn {
                column: "region_id",
                source,
            },
        })?;
        let server = ServerId::new(self.server_id.trim()).map_err(|source| match source {
            IdError::Empty => RowError::EmptyColumn("server_id"),
            IdError::ContainsWhitespace | IdError::ContainsUnderscore => RowError::InvalidColumn {
                column: "server_id",
                source,
            },
        })?;

        let endpoint = self.endpoint.trim();
        if endpoint.is_empty() {
            return Err(RowError::EmptyColumn("endpoint"));
        }
        let api_key = self.api_key.trim();
        if api_key.is_empty() {
            return Err(RowError::EmptyColumn("api_key"));
        }
        if self.api_secret.is_empty() {
            return Err(RowError::EmptyColumn("api_secret"));
        }

        Ok(VoiceServer {
            location: Location::new(region, server),
            endpoint: Box::from(endpoint),
            api_key: Box::from(api_key),
            api_secret: ApiSecret::new(&self.api_secret),
            is_active: self.is_active,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InternalEndpoint {
    pub url: Option<String>,
    pub default_region_id: Option<String>,
}

impl InternalEndpoint {
    pub fn resolve<'a>(&'a self, server: &'a VoiceServer) -> &'a str {
        let (Some(url), Some(region)) = (self.url.as_deref(), self.default_region_id.as_deref())
        else {
            return server.endpoint();
        };
        if server.region().as_str() != region
            || server.server().as_str() != format!("{region}-server-1")
        {
            return server.endpoint();
        }
        url
    }
}

pub fn default_region_id(blob: &str) -> Option<String> {
    let trimmed = blob.trim();
    if trimmed.is_empty() {
        return None;
    }
    let parsed: serde_json::Value = serde_json::from_str(trimmed).ok()?;
    match parsed {
        serde_json::Value::String(id) => Some(id),
        serde_json::Value::Object(fields) => fields
            .get("id")
            .and_then(serde_json::Value::as_str)
            .map(str::to_owned),
        _ => None,
    }
    .filter(|id| !id.trim().is_empty())
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VoiceServer {
    location: Location,
    endpoint: Box<str>,
    api_key: Box<str>,
    api_secret: ApiSecret,
    is_active: bool,
}

impl VoiceServer {
    pub const fn location(&self) -> &Location {
        &self.location
    }

    pub const fn region(&self) -> &RegionId {
        &self.location.region
    }

    pub const fn server(&self) -> &ServerId {
        &self.location.server
    }

    pub fn endpoint(&self) -> &str {
        &self.endpoint
    }

    pub fn api_key(&self) -> &str {
        &self.api_key
    }

    pub const fn api_secret(&self) -> &ApiSecret {
        &self.api_secret
    }

    pub const fn is_active(&self) -> bool {
        self.is_active
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopologySnapshot {
    servers: Vec<VoiceServer>,
    records: Vec<ServerRecord>,
    loaded_at: Millis,
    epoch: Epoch,
}

impl TopologySnapshot {
    pub fn servers(&self) -> &[VoiceServer] {
        &self.servers
    }

    pub fn records(&self) -> &[ServerRecord] {
        &self.records
    }

    pub const fn loaded_at(&self) -> Millis {
        self.loaded_at
    }

    pub const fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub fn len(&self) -> usize {
        self.servers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.servers.is_empty()
    }

    pub fn draining(&self) -> Vec<Location> {
        self.records
            .iter()
            .filter(|record| record.removed_at.is_some())
            .map(|record| record.location.clone())
            .collect()
    }

    pub fn server(&self, location: &Location) -> Option<&VoiceServer> {
        self.servers
            .iter()
            .find(|server| &server.location == location)
    }

    pub fn locations(&self) -> Vec<Location> {
        self.servers
            .iter()
            .map(|server| server.location.clone())
            .collect()
    }

    pub const fn age_ms(&self, now: Millis) -> u64 {
        now.saturating_since(self.loaded_at)
    }

    pub const fn is_stale(&self, now: Millis, max_age_ms: u64) -> bool {
        self.age_ms(now) > max_age_ms
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TopologyLimits {
    pub max_age_ms: u64,
    pub drain_grace_ms: u64,
}

impl TopologyLimits {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            max_age_ms: config.topology_max_age_ms,
            drain_grace_ms: config.topology_drain_grace_ms,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefreshOutcome {
    Applied { epoch: Epoch, servers: usize },
    Unchanged { epoch: Epoch },
    RejectedEmpty,
    Failed,
}

impl RefreshOutcome {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Applied { .. } => "applied",
            Self::Unchanged { .. } => "unchanged",
            Self::RejectedEmpty => "rejected_empty",
            Self::Failed => "failed",
        }
    }

    pub const fn kept_previous_snapshot(self) -> bool {
        match self {
            Self::Applied { .. } | Self::Unchanged { .. } => false,
            Self::RejectedEmpty | Self::Failed => true,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TopologyCache {
    limits: TopologyLimits,
    snapshot: Option<TopologySnapshot>,
    epoch: Epoch,
    last_failure_at: Option<Millis>,
    applied_total: u64,
    unchanged_total: u64,
    rejected_empty_total: u64,
    failed_total: u64,
}

impl TopologyCache {
    pub const fn new(limits: TopologyLimits) -> Self {
        Self {
            limits,
            snapshot: None,
            epoch: Epoch::FIRST,
            last_failure_at: None,
            applied_total: 0,
            unchanged_total: 0,
            rejected_empty_total: 0,
            failed_total: 0,
        }
    }

    pub const fn limits(&self) -> TopologyLimits {
        self.limits
    }

    pub const fn snapshot(&self) -> Option<&TopologySnapshot> {
        self.snapshot.as_ref()
    }

    pub const fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub const fn last_failure_at(&self) -> Option<Millis> {
        self.last_failure_at
    }

    pub const fn applied_total(&self) -> u64 {
        self.applied_total
    }

    pub const fn unchanged_total(&self) -> u64 {
        self.unchanged_total
    }

    pub const fn rejected_empty_total(&self) -> u64 {
        self.rejected_empty_total
    }

    pub const fn failed_total(&self) -> u64 {
        self.failed_total
    }

    pub fn records(&self) -> &[ServerRecord] {
        self.snapshot
            .as_ref()
            .map_or(&[], TopologySnapshot::records)
    }

    pub fn servers(&self) -> &[VoiceServer] {
        self.snapshot
            .as_ref()
            .map_or(&[], TopologySnapshot::servers)
    }

    pub fn server(&self, location: &Location) -> Option<&VoiceServer> {
        self.snapshot
            .as_ref()
            .and_then(|snapshot| snapshot.server(location))
    }

    pub fn age_ms(&self, now: Millis) -> Option<u64> {
        self.snapshot.as_ref().map(|snapshot| snapshot.age_ms(now))
    }

    pub fn is_stale(&self, now: Millis) -> bool {
        self.snapshot
            .as_ref()
            .is_none_or(|snapshot| snapshot.is_stale(now, self.limits.max_age_ms))
    }

    pub fn freshness(&self, now: Millis) -> TopologyFreshness {
        TopologyFreshness {
            age_ms: self.age_ms(now).unwrap_or(u64::MAX),
            max_age_ms: self.limits.max_age_ms,
        }
    }

    pub fn lens(&self, now: Millis) -> TopologyLens<'_> {
        TopologyLens {
            servers: self.records(),
            now,
            drain_grace_ms: self.limits.drain_grace_ms,
        }
    }

    pub fn note_failure(&mut self, now: Millis) -> RefreshOutcome {
        self.failed_total = self.failed_total.saturating_add(1);
        self.last_failure_at = Some(now);
        RefreshOutcome::Failed
    }

    pub fn accept(&mut self, servers: Vec<VoiceServer>, now: Millis) -> RefreshOutcome {
        if servers.is_empty() {
            self.rejected_empty_total = self.rejected_empty_total.saturating_add(1);
            return RefreshOutcome::RejectedEmpty;
        }

        let mut servers = servers;
        servers.sort_by(|left, right| left.location.cmp(&right.location));
        servers.dedup_by(|left, right| left.location == right.location);
        let records = self.records_for(&servers, now);

        let unchanged = self
            .snapshot
            .as_ref()
            .is_some_and(|snapshot| snapshot.servers == servers && snapshot.records == records);
        if unchanged {
            self.unchanged_total = self.unchanged_total.saturating_add(1);
            if let Some(snapshot) = self.snapshot.as_mut() {
                snapshot.loaded_at = now;
            }
            return RefreshOutcome::Unchanged { epoch: self.epoch };
        }

        self.epoch = self.epoch.next();
        self.applied_total = self.applied_total.saturating_add(1);
        let count = servers.len();
        self.snapshot = Some(TopologySnapshot {
            servers,
            records,
            loaded_at: now,
            epoch: self.epoch,
        });
        RefreshOutcome::Applied {
            epoch: self.epoch,
            servers: count,
        }
    }

    fn records_for(&self, servers: &[VoiceServer], now: Millis) -> Vec<ServerRecord> {
        let mut records: Vec<ServerRecord> = servers
            .iter()
            .map(|server| ServerRecord {
                location: server.location.clone(),
                removed_at: None,
            })
            .collect();

        if let Some(previous) = self.snapshot.as_ref() {
            for record in &previous.records {
                if servers
                    .iter()
                    .any(|server| server.location == record.location)
                {
                    continue;
                }
                records.push(ServerRecord {
                    location: record.location.clone(),
                    removed_at: Some(record.removed_at.unwrap_or(now)),
                });
            }
        }

        records.sort_by(|left, right| left.location.cmp(&right.location));
        records
    }
}

pub type LoadFuture<'a> =
    Pin<Box<dyn Future<Output = Result<Vec<VoiceServer>, TopologyError>> + Send + 'a>>;

pub trait TopologyStore: Send + Sync {
    fn load(&self) -> LoadFuture<'_>;

    fn backend(&self) -> &'static str;
}
