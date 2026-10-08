// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::Metrics;
use crate::rpc::RpcClient;
use fluxer_svc::transport::{Transport, TransportMessage, TransportSubscriber};
use serde_json::Value;
use std::sync::RwLock;
use std::time::Duration;
use tokio::time::{Instant, MissedTickBehavior};
use tracing::{info, warn};

pub const RECONCILE_INTERVAL: Duration = Duration::from_secs(30);

const SUBJECT: &str = "config.push.delivery";
const MESSAGE_TYPE: &str = "push_service_delivery_config";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum ConsentUpdate {
    Updated,
    Unchanged,
    Stale,
    Rejected,
}

impl ConsentUpdate {
    pub const ALL: [Self; 4] = [Self::Updated, Self::Unchanged, Self::Stale, Self::Rejected];

    pub fn label(self) -> &'static str {
        match self {
            Self::Updated => "updated",
            Self::Unchanged => "unchanged",
            Self::Stale => "stale",
            Self::Rejected => "rejected",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct HeldConsent {
    accepted: bool,
    config_version: u64,
}

impl HeldConsent {
    fn parse(config: &Value) -> Option<Self> {
        let accepted = match config.get("relay_consent_accepted") {
            None | Some(Value::Null) => false,
            Some(Value::Bool(flag)) => *flag,
            Some(_) => return None,
        };
        let config_version = match config.get("config_version") {
            None | Some(Value::Null) => 0,
            Some(value) => value.as_u64()?,
        };
        Some(Self {
            accepted,
            config_version,
        })
    }
}

pub struct RelayConsentStore {
    env_accepted: bool,
    held: RwLock<Option<HeldConsent>>,
}

impl RelayConsentStore {
    pub fn new(env_accepted: bool) -> Self {
        Self {
            env_accepted,
            held: RwLock::new(None),
        }
    }

    pub fn accepted(&self) -> bool {
        self.env_accepted || self.held().is_some_and(|held| held.accepted)
    }

    fn held(&self) -> Option<HeldConsent> {
        *self.held.read().expect("relay consent lock poisoned")
    }

    fn apply(&self, payload: &[u8]) -> ConsentUpdate {
        let Ok(value) = serde_json::from_slice::<Value>(payload) else {
            warn!("relay consent payload is not JSON");
            return ConsentUpdate::Rejected;
        };
        let Some(config) = config_object(&value) else {
            warn!("relay consent payload has no config object of its type");
            return ConsentUpdate::Rejected;
        };
        self.update(config)
    }

    pub(crate) fn update(&self, config: &Value) -> ConsentUpdate {
        let Some(offered) = HeldConsent::parse(config) else {
            warn!("relay consent config rejected as invalid");
            return ConsentUpdate::Rejected;
        };
        let mut held = self.held.write().expect("relay consent lock poisoned");
        match *held {
            Some(current) if offered.config_version < current.config_version => {
                warn!(
                    highest = current.config_version,
                    offered = offered.config_version,
                    "relay consent ignored a lower config_version"
                );
                return ConsentUpdate::Stale;
            }
            Some(current) if current == offered => return ConsentUpdate::Unchanged,
            _ => {}
        }
        info!(
            accepted = offered.accepted,
            config_version = offered.config_version,
            "relay consent updated"
        );
        *held = Some(offered);
        ConsentUpdate::Updated
    }
}

pub async fn run_subscriber<T: Transport>(
    transport: T,
    rpc: &RpcClient,
    store: &RelayConsentStore,
    metrics: &Metrics,
    reconcile_every: Duration,
) {
    loop {
        let mut subscriber = match transport.subscribe(SUBJECT).await {
            Ok(subscriber) => subscriber,
            Err(error) => {
                warn!(error = %error, subject = SUBJECT, "relay consent subscribe failed");
                transport.wait_for_reconnect().await;
                continue;
            }
        };
        info!(subject = SUBJECT, "listening for relay consent updates");
        let outcome = fetch(rpc, store, metrics).await;
        info!(
            outcome = outcome.label(),
            "relay consent read after subscribing"
        );
        let mut reconcile =
            tokio::time::interval_at(Instant::now() + reconcile_every, reconcile_every);
        reconcile.set_missed_tick_behavior(MissedTickBehavior::Delay);
        loop {
            tokio::select! {
                message = subscriber.next() => {
                    let Some(message) = message else {
                        break;
                    };
                    let outcome = store.apply(message.payload());
                    record(metrics, store, outcome);
                }
                _ = reconcile.tick() => {
                    fetch(rpc, store, metrics).await;
                }
            }
        }
        warn!(
            subject = SUBJECT,
            "relay consent subscription ended, will re-subscribe"
        );
    }
}

async fn fetch(rpc: &RpcClient, store: &RelayConsentStore, metrics: &Metrics) -> ConsentUpdate {
    let outcome = match rpc.push_service_delivery_config().await {
        Ok(config) => store.update(&config),
        Err(error) => {
            warn!(error = %error, "relay consent read failed");
            ConsentUpdate::Rejected
        }
    };
    record(metrics, store, outcome);
    outcome
}

fn record(metrics: &Metrics, store: &RelayConsentStore, outcome: ConsentUpdate) {
    metrics.record_relay_consent_update(outcome);
    metrics.record_relay_consent_accepted(store.accepted());
}

fn config_object(value: &Value) -> Option<&Value> {
    if value
        .get("type")
        .is_some_and(|found| found.as_str() != Some(MESSAGE_TYPE))
    {
        return None;
    }
    if let Some(config) = value.get("config").filter(|config| config.is_object()) {
        return Some(config);
    }
    value.is_object().then_some(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn legacy_document(config_version: u64, accepted: bool) -> Value {
        json!({
            "enabled": true,
            "rollout_basis_points": 10000,
            "rollout_salt": "push-service-delivery-v1",
            "included_user_ids": [],
            "excluded_user_ids": [],
            "config_version": config_version,
            "relay_consent_accepted": accepted,
            "relay_consent_accepted_at": null,
            "relay_consent_accepted_by": null,
        })
    }

    #[test]
    fn the_operator_relay_consent_is_read_off_the_legacy_document() {
        let store = RelayConsentStore::new(false);
        assert_eq!(
            store.update(&legacy_document(3, true)),
            ConsentUpdate::Updated
        );
        assert!(store.accepted());
    }

    #[test]
    fn a_document_without_the_consent_field_has_not_consented() {
        let store = RelayConsentStore::new(false);
        assert_eq!(
            store.update(&json!({"config_version": 3})),
            ConsentUpdate::Updated
        );
        assert!(!store.accepted());
    }

    #[test]
    fn a_consent_field_that_is_not_a_boolean_is_refused() {
        let store = RelayConsentStore::new(false);
        assert_eq!(
            store.update(&json!({"relay_consent_accepted": "yes"})),
            ConsentUpdate::Rejected
        );
        assert!(store.held().is_none());
    }

    #[test]
    fn a_config_version_that_is_not_an_integer_is_refused() {
        let store = RelayConsentStore::new(false);
        assert_eq!(
            store.update(&json!({"config_version": -1, "relay_consent_accepted": true})),
            ConsentUpdate::Rejected
        );
        assert!(!store.accepted());
    }

    #[test]
    fn an_older_config_version_never_overwrites_a_newer_one() {
        let store = RelayConsentStore::new(false);
        store.update(&legacy_document(5, true));
        assert_eq!(
            store.update(&legacy_document(4, false)),
            ConsentUpdate::Stale
        );
        assert!(store.accepted());
        assert_eq!(
            store.update(&legacy_document(6, false)),
            ConsentUpdate::Updated
        );
        assert!(!store.accepted());
    }

    #[test]
    fn the_same_document_again_is_unchanged() {
        let store = RelayConsentStore::new(false);
        store.update(&legacy_document(5, true));
        assert_eq!(
            store.update(&legacy_document(5, true)),
            ConsentUpdate::Unchanged
        );
    }

    #[test]
    fn the_env_override_accepts_whatever_the_document_says() {
        let store = RelayConsentStore::new(true);
        assert!(store.accepted());
        store.update(&legacy_document(1, false));
        assert!(store.accepted());
    }

    #[test]
    fn a_nats_message_is_unwrapped_from_its_envelope() {
        let store = RelayConsentStore::new(false);
        let message = json!({"type": MESSAGE_TYPE, "config": legacy_document(2, true)});
        assert_eq!(
            store.apply(message.to_string().as_bytes()),
            ConsentUpdate::Updated
        );
        assert!(store.accepted());
    }

    #[test]
    fn a_nats_message_of_another_type_is_refused() {
        let store = RelayConsentStore::new(false);
        let message = json!({"type": "something_else", "config": legacy_document(2, true)});
        assert_eq!(
            store.apply(message.to_string().as_bytes()),
            ConsentUpdate::Rejected
        );
        assert!(!store.accepted());
    }
}
