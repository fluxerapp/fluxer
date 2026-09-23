// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::{Metrics, RpcMethod};
use crate::rpc::RpcClient;
use fluxer_svc::transport::{Transport, TransportMessage, TransportSubscriber};
use serde_json::Value;
use std::fmt::Debug;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use tokio::time::{Instant, MissedTickBehavior};
use tracing::{info, warn};

pub const RECONCILE_INTERVAL: Duration = Duration::from_secs(30);

const MAX_BASIS_POINTS: u64 = 10_000;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RolloutOutcome {
    Updated,
    Unchanged,
    Stale,
    Rejected,
}

impl RolloutOutcome {
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

pub trait RolloutConfig: Debug + Eq + Send + Sync + Sized + 'static {
    const NAME: &'static str;
    const SUBJECT: &'static str;
    const MESSAGE_TYPE: &'static str;
    const RPC_METHOD: RpcMethod;

    fn parse(config: &Value) -> Option<Self>;
    fn enabled(&self) -> bool;
    fn config_version(&self) -> u64;
    fn record_update(metrics: &Metrics, outcome: RolloutOutcome);
    fn record_held(&self, metrics: &Metrics);
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RolloutSnapshot {
    pub enabled: bool,
    pub config_version: u64,
    pub rollout_basis_points: u32,
}

impl RolloutConfig for RolloutSnapshot {
    const NAME: &'static str = "push delivery";
    const SUBJECT: &'static str = "config.push.delivery";
    const MESSAGE_TYPE: &'static str = "push_service_delivery_config";
    const RPC_METHOD: RpcMethod = RpcMethod::GetPushServiceDeliveryConfig;

    fn parse(config: &Value) -> Option<Self> {
        let rollout_basis_points = match config.get("rollout_basis_points") {
            None | Some(Value::Null) => 0,
            Some(value) => {
                let points = value.as_u64()?;
                if points > MAX_BASIS_POINTS {
                    return None;
                }
                u32::try_from(points).ok()?
            }
        };
        Some(Self {
            enabled: parse_enabled(config)?,
            config_version: parse_config_version(config)?,
            rollout_basis_points,
        })
    }

    fn enabled(&self) -> bool {
        self.enabled
    }

    fn config_version(&self) -> u64 {
        self.config_version
    }

    fn record_update(metrics: &Metrics, outcome: RolloutOutcome) {
        metrics.record_rollout_update(outcome);
    }

    fn record_held(&self, metrics: &Metrics) {
        metrics.record_rollout_snapshot(self);
    }
}

pub fn parse_enabled(config: &Value) -> Option<bool> {
    match config.get("enabled") {
        None | Some(Value::Null) => Some(false),
        Some(Value::Bool(enabled)) => Some(*enabled),
        Some(_) => None,
    }
}

pub fn parse_config_version(config: &Value) -> Option<u64> {
    match config.get("config_version") {
        None | Some(Value::Null) => Some(0),
        Some(value) => value.as_u64(),
    }
}

struct Current<C> {
    held: Option<Arc<C>>,
    highest_version: u64,
}

pub struct RolloutStore<C> {
    current: RwLock<Current<C>>,
}

impl<C> Default for RolloutStore<C> {
    fn default() -> Self {
        Self {
            current: RwLock::new(Current {
                held: None,
                highest_version: 0,
            }),
        }
    }
}

impl<C: RolloutConfig> RolloutStore<C> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn snapshot(&self) -> Option<Arc<C>> {
        self.current
            .read()
            .expect("rollout config lock poisoned")
            .held
            .clone()
    }

    fn apply(&self, payload: &[u8]) -> RolloutOutcome {
        let Ok(value) = serde_json::from_slice::<Value>(payload) else {
            warn!(config = C::NAME, "rollout config payload is not JSON");
            return RolloutOutcome::Rejected;
        };
        let Some(config) = config_object(&value, C::MESSAGE_TYPE) else {
            warn!(
                config = C::NAME,
                "rollout config payload has no config object of its type"
            );
            return RolloutOutcome::Rejected;
        };
        self.update(config)
    }

    fn update(&self, config: &Value) -> RolloutOutcome {
        let Some(offered) = C::parse(config) else {
            warn!(config = C::NAME, "rollout config rejected as invalid");
            return RolloutOutcome::Rejected;
        };
        let mut current = self.current.write().expect("rollout config lock poisoned");
        match current.held.as_deref() {
            Some(_) if offered.enabled() && offered.config_version() < current.highest_version => {
                warn!(
                    config = C::NAME,
                    highest = current.highest_version,
                    offered = offered.config_version(),
                    "rollout config ignored a lower config_version"
                );
                return RolloutOutcome::Stale;
            }
            Some(held) if *held == offered => return RolloutOutcome::Unchanged,
            _ => {}
        }
        info!(config = C::NAME, held = ?offered, "rollout config updated");
        current.highest_version = current.highest_version.max(offered.config_version());
        current.held = Some(Arc::new(offered));
        RolloutOutcome::Updated
    }
}

pub async fn run_rollout_subscriber<T: Transport, C: RolloutConfig>(
    transport: T,
    rpc: &RpcClient,
    store: &RolloutStore<C>,
    metrics: &Metrics,
    reconcile_every: Duration,
) {
    loop {
        let mut subscriber = match transport.subscribe(C::SUBJECT).await {
            Ok(subscriber) => subscriber,
            Err(error) => {
                warn!(
                    config = C::NAME,
                    error = %error,
                    subject = C::SUBJECT,
                    "rollout config subscribe failed"
                );
                transport.wait_for_reconnect().await;
                continue;
            }
        };
        info!(
            config = C::NAME,
            subject = C::SUBJECT,
            "listening for rollout config updates"
        );
        let outcome = fetch_config(rpc, store, metrics).await;
        info!(
            config = C::NAME,
            outcome = outcome.label(),
            "rollout config read after subscribing"
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
                    record_outcome(metrics, store, outcome);
                }
                _ = reconcile.tick() => {
                    fetch_config(rpc, store, metrics).await;
                }
            }
        }
        warn!(
            config = C::NAME,
            subject = C::SUBJECT,
            "rollout config subscription ended, will re-subscribe"
        );
    }
}

async fn fetch_config<C: RolloutConfig>(
    rpc: &RpcClient,
    store: &RolloutStore<C>,
    metrics: &Metrics,
) -> RolloutOutcome {
    match rpc.rollout_config(C::RPC_METHOD).await {
        Ok(config) => {
            let outcome = store.update(&config);
            record_outcome(metrics, store, outcome);
            outcome
        }
        Err(error) => {
            warn!(config = C::NAME, error = %error, "rollout config read failed");
            C::record_update(metrics, RolloutOutcome::Rejected);
            RolloutOutcome::Rejected
        }
    }
}

fn record_outcome<C: RolloutConfig>(
    metrics: &Metrics,
    store: &RolloutStore<C>,
    outcome: RolloutOutcome,
) {
    C::record_update(metrics, outcome);
    if outcome == RolloutOutcome::Rejected {
        return;
    }
    if let Some(held) = store.snapshot() {
        held.record_held(metrics);
    }
}

fn config_object<'a>(value: &'a Value, message_type: &str) -> Option<&'a Value> {
    if value
        .get("type")
        .is_some_and(|found| found.as_str() != Some(message_type))
    {
        return None;
    }
    if let Some(config) = value.get("config").filter(|config| config.is_object()) {
        return Some(config);
    }
    value.is_object().then_some(value)
}
