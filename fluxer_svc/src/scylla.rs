// SPDX-License-Identifier: AGPL-3.0-or-later

use scylla::client::execution_profile::ExecutionProfile;
use scylla::client::session::Session;
use scylla::client::session_builder::SessionBuilder;
use scylla::errors::TranslationError;
use scylla::policies::address_translator::{AddressTranslator, UntranslatedPeer};
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
}

impl ScyllaConfig {
    pub fn from_service_config(config: &crate::config::ServiceConfig) -> Self {
        Self {
            hosts: config.scylla_hosts.clone(),
            keyspace: config.scylla_keyspace.clone(),
            username: config.scylla_username.clone(),
            password: config.scylla_password.clone(),
        }
    }
}

pub async fn connect(config: &ScyllaConfig) -> anyhow::Result<Arc<Session>> {
    if config.hosts.is_empty() {
        anyhow::bail!("at least one ScyllaDB host is required");
    }

    let mut builder = SessionBuilder::new()
        .known_nodes(config.hosts.iter().map(String::as_str))
        .default_execution_profile_handle(execution_profile().into_handle())
        .use_keyspace(config.keyspace.clone(), false);

    if let Some(username) = &config.username {
        builder = builder.user(username, config.password.as_deref().unwrap_or_default());
    }

    if config.hosts.len() == 1 {
        let target = resolve_contact_point(&config.hosts[0])?;
        builder = builder.address_translator(Arc::new(ContactPointTranslator { target }));
    }

    let session = Arc::new(builder.build().await?);
    tracing::info!(
        keyspace = config.keyspace,
        hosts = ?config.hosts,
        request_timeout_ms = REQUEST_TIMEOUT.as_millis(),
        "connected to ScyllaDB"
    );
    Ok(session)
}

fn execution_profile() -> ExecutionProfile {
    ExecutionProfile::builder()
        .request_timeout(Some(REQUEST_TIMEOUT))
        .build()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn request_timeout_stays_below_the_shard_request_budget() {
        assert!(
            REQUEST_TIMEOUT < crate::router::SHARD_REQUEST_TIMEOUT,
            "scylla request timeout {REQUEST_TIMEOUT:?} outlives the shard budget"
        );
    }

    #[test]
    fn execution_profile_bounds_the_request_timeout() {
        assert_eq!(
            Some(REQUEST_TIMEOUT),
            execution_profile().get_request_timeout()
        );
    }
}
