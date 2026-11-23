// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{
    CassandraAddressTranslation, CassandraConsistency, CassandraSerialConsistency,
};
use scylla::client::execution_profile::ExecutionProfile;
use scylla::client::session::Session;
use scylla::client::session_builder::SessionBuilder;
use scylla::errors::TranslationError;
use scylla::policies::address_translator::{AddressTranslator, UntranslatedPeer};
use scylla::policies::load_balancing::DefaultPolicy;
use scylla::statement::{Consistency, SerialConsistency};
use std::net::{SocketAddr, ToSocketAddrs};
use std::sync::Arc;
use std::time::Duration;

const REQUEST_TIMEOUT: Duration = Duration::from_secs(4);

#[derive(Clone, Debug)]
pub struct ScyllaConfig {
    pub hosts: Vec<String>,
    pub keyspace: String,
    pub username: Option<String>,
    pub password: Option<String>,
    pub local_dc: Option<String>,
    pub consistency: CassandraConsistency,
    pub serial_consistency: CassandraSerialConsistency,
    pub address_translation: CassandraAddressTranslation,
}

impl ScyllaConfig {
    pub fn from_service_config(config: &crate::config::ServiceConfig) -> Self {
        Self {
            hosts: config.scylla_hosts.clone(),
            keyspace: config.scylla_keyspace.clone(),
            username: config.scylla_username.clone(),
            password: config.scylla_password.clone(),
            local_dc: config.scylla_local_dc.clone(),
            consistency: config.scylla_consistency,
            serial_consistency: config.scylla_serial_consistency,
            address_translation: config.scylla_address_translation,
        }
    }
}

pub async fn connect(config: &ScyllaConfig) -> anyhow::Result<Arc<Session>> {
    if config.hosts.is_empty() {
        anyhow::bail!("at least one ScyllaDB host is required");
    }

    let mut builder = SessionBuilder::new()
        .known_nodes(config.hosts.iter().map(String::as_str))
        .default_execution_profile_handle(execution_profile(config).into_handle())
        .use_keyspace(config.keyspace.clone(), false);

    if let Some(username) = &config.username {
        builder = builder.user(username, config.password.as_deref().unwrap_or_default());
    }

    let translate_to_contact_point = translates_to_contact_point(config);
    if translate_to_contact_point {
        let target = resolve_contact_point(&config.hosts[0])?;
        builder = builder.address_translator(Arc::new(ContactPointTranslator { target }));
    }

    let session = Arc::new(builder.build().await?);
    tracing::info!(
        keyspace = config.keyspace,
        hosts = ?config.hosts,
        local_dc = config.local_dc.as_deref().unwrap_or(""),
        consistency = ?config.consistency,
        serial_consistency = ?config.serial_consistency,
        translate_to_contact_point,
        request_timeout_ms = REQUEST_TIMEOUT.as_millis(),
        "connected to ScyllaDB"
    );
    Ok(session)
}

fn translates_to_contact_point(config: &ScyllaConfig) -> bool {
    match config.address_translation {
        CassandraAddressTranslation::Auto => config.hosts.len() == 1,
        CassandraAddressTranslation::ContactPoint => true,
        CassandraAddressTranslation::None => false,
    }
}

fn execution_profile(config: &ScyllaConfig) -> ExecutionProfile {
    let mut policy = DefaultPolicy::builder().token_aware(true);
    if let Some(dc) = &config.local_dc {
        policy = policy
            .prefer_datacenter(dc.clone())
            .permit_dc_failover(false);
    }
    ExecutionProfile::builder()
        .request_timeout(Some(REQUEST_TIMEOUT))
        .consistency(driver_consistency(config.consistency))
        .serial_consistency(Some(driver_serial_consistency(config.serial_consistency)))
        .load_balancing_policy(policy.build())
        .build()
}

fn driver_consistency(consistency: CassandraConsistency) -> Consistency {
    match consistency {
        CassandraConsistency::One => Consistency::One,
        CassandraConsistency::LocalOne => Consistency::LocalOne,
        CassandraConsistency::Quorum => Consistency::Quorum,
        CassandraConsistency::LocalQuorum => Consistency::LocalQuorum,
        CassandraConsistency::EachQuorum => Consistency::EachQuorum,
        CassandraConsistency::All => Consistency::All,
    }
}

fn driver_serial_consistency(consistency: CassandraSerialConsistency) -> SerialConsistency {
    match consistency {
        CassandraSerialConsistency::Serial => SerialConsistency::Serial,
        CassandraSerialConsistency::LocalSerial => SerialConsistency::LocalSerial,
    }
}

struct ContactPointTranslator {
    target: SocketAddr,
}

#[async_trait::async_trait]
impl AddressTranslator for ContactPointTranslator {
    async fn translate_address(
        &self,
        _peer: &UntranslatedPeer,
    ) -> Result<SocketAddr, TranslationError> {
        Ok(self.target)
    }
}

fn resolve_contact_point(host: &str) -> anyhow::Result<SocketAddr> {
    host.to_socket_addrs()?
        .next()
        .ok_or_else(|| anyhow::anyhow!("failed to resolve contact point: {host}"))
}
