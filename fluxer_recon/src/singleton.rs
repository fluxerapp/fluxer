// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use serde::{Deserialize, Serialize};

use fluxer_svc::transport::{Transport, TransportMessage, TransportSubscriber, reply_message};

use crate::control::INSTANCE_SUBJECT;
use crate::ids::Millis;
use crate::runtime::{Shared, monotonic_now, wall_now_ms};

pub const SINGLETON_PROBE_INTERVAL_MS: u64 = 30_000;
pub const SINGLETON_PROBE_DEADLINE_MS: u64 = 750;
pub const PEER_CLAMP_RELEASE_AFTER_MS: u64 = 3 * SINGLETON_PROBE_INTERVAL_MS;
pub const RESUBSCRIBE_BACKOFF_MS: u64 = 1_000;
pub const RESUBSCRIBE_BACKOFF_CEILING_MS: u64 = 30_000;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SingletonLayer {
    Deployment,
    RuntimeDetector,
    SameTurnAuthorization,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SingletonResponse {
    RefuseToBoot,
    DegradeToConstructive,
    NoActionCorrectnessHolds,
}

impl SingletonResponse {
    pub const fn terminates_a_running_process(self) -> bool {
        match self {
            Self::RefuseToBoot | Self::DegradeToConstructive | Self::NoActionCorrectnessHolds => {
                false
            }
        }
    }
}

impl SingletonLayer {
    pub const ALL: [Self; 3] = [
        Self::Deployment,
        Self::RuntimeDetector,
        Self::SameTurnAuthorization,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Deployment => "deployment",
            Self::RuntimeDetector => "runtime_detector",
            Self::SameTurnAuthorization => "same_turn_authorization",
        }
    }

    pub const fn response(self) -> SingletonResponse {
        match self {
            Self::Deployment => SingletonResponse::RefuseToBoot,
            Self::RuntimeDetector => SingletonResponse::DegradeToConstructive,
            Self::SameTurnAuthorization => SingletonResponse::NoActionCorrectnessHolds,
        }
    }

    pub const fn acts_before_serving(self) -> bool {
        matches!(self, Self::Deployment)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InstanceIdentity {
    pub id: String,
    pub started_at: u64,
}

impl InstanceIdentity {
    pub fn generate() -> Self {
        static SEQUENCE: AtomicU64 = AtomicU64::new(0);
        let started_at = wall_now_ms();
        let sequence = SEQUENCE.fetch_add(1, Ordering::Relaxed);
        let entropy = mix(u64::from(std::process::id()))
            ^ mix(started_at)
            ^ mix(monotonic_now().get())
            ^ mix(sequence);
        Self {
            id: format!("{entropy:016x}"),
            started_at,
        }
    }

    pub fn announce(&self) -> InstanceAnnounce {
        InstanceAnnounce {
            instance_id: self.id.clone(),
            started_at: self.started_at,
        }
    }

    pub fn probe(&self) -> InstanceProbe {
        InstanceProbe {
            instance_id: self.id.clone(),
        }
    }
}

const fn mix(value: u64) -> u64 {
    let mut hash = value ^ 0x9e37_79b9_7f4a_7c15;
    hash = (hash ^ (hash >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    hash = (hash ^ (hash >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    hash ^ (hash >> 31)
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct InstanceProbe {
    pub instance_id: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct InstanceAnnounce {
    pub instance_id: String,
    pub started_at: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PeerVerdict {
    SelfEcho,
    Peer,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PeerRecord {
    pub instance_id: String,
    pub started_at: u64,
    pub first_seen_at: Millis,
    pub last_seen_at: Millis,
    pub detections: u64,
}

pub fn should_answer_probe(own: &InstanceIdentity, probe: &InstanceProbe) -> bool {
    probe.instance_id != own.id
}

pub fn peer_verdict(own: &InstanceIdentity, announce: &InstanceAnnounce) -> PeerVerdict {
    if announce.instance_id == own.id {
        PeerVerdict::SelfEcho
    } else {
        PeerVerdict::Peer
    }
}

pub const fn next_backoff_ms(previous: u64) -> u64 {
    let doubled = previous.saturating_mul(2);
    if doubled > RESUBSCRIBE_BACKOFF_CEILING_MS {
        RESUBSCRIBE_BACKOFF_CEILING_MS
    } else {
        doubled
    }
}

pub async fn run_singleton<T: Transport>(shared: Shared, transport: T) -> anyhow::Result<()> {
    let announce = serde_json::to_vec(&shared.instance().announce())?;
    let probe = serde_json::to_vec(&shared.instance().probe())?;
    let deadline = Duration::from_millis(SINGLETON_PROBE_DEADLINE_MS);
    let mut ticker = tokio::time::interval(Duration::from_millis(SINGLETON_PROBE_INTERVAL_MS));
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut backoff_ms = RESUBSCRIBE_BACKOFF_MS;

    loop {
        let mut subscription = match transport.subscribe(INSTANCE_SUBJECT).await {
            Ok(subscription) => {
                backoff_ms = RESUBSCRIBE_BACKOFF_MS;
                subscription
            }
            Err(error) => {
                tracing::error!(
                    error = %error,
                    subject = INSTANCE_SUBJECT,
                    retry_in_ms = backoff_ms,
                    "the singleton detector could not subscribe, retrying rather than ending the \
                     task, because a restart loop is worse than a detector that is briefly deaf"
                );
                tokio::time::sleep(Duration::from_millis(backoff_ms)).await;
                backoff_ms = next_backoff_ms(backoff_ms);
                continue;
            }
        };

        tracing::info!(
            subject = INSTANCE_SUBJECT,
            instance_id = shared.instance().id,
            "singleton detector listening"
        );

        loop {
            tokio::select! {
                message = subscription.next() => {
                    let Some(message) = message else {
                        tracing::warn!("singleton subscription ended, will re-subscribe");
                        break;
                    };
                    let requester: InstanceProbe = match serde_json::from_slice(message.payload()) {
                        Ok(requester) => requester,
                        Err(error) => {
                            tracing::debug!(error = %error, "ignoring an undecodable instance probe");
                            continue;
                        }
                    };
                    if !should_answer_probe(shared.instance(), &requester) {
                        continue;
                    }
                    if let Err(error) = reply_message(&message, &transport, &announce).await {
                        tracing::debug!(error = %error, "failed to answer an instance probe");
                    }
                }
                _ = ticker.tick() => {
                    let reply = transport.request(INSTANCE_SUBJECT, &probe, deadline).await;
                    let Ok(bytes) = reply else {
                        continue;
                    };
                    let Ok(peer) = serde_json::from_slice::<InstanceAnnounce>(&bytes) else {
                        continue;
                    };
                    if matches!(peer_verdict(shared.instance(), &peer), PeerVerdict::SelfEcho) {
                        continue;
                    }
                    shared.note_peer(&peer, monotonic_now());
                    tracing::error!(
                        peer_instance_id = peer.instance_id,
                        peer_started_at = peer.started_at,
                        own_instance_id = shared.instance().id,
                        layer = SingletonLayer::RuntimeDetector.label(),
                        response = ?SingletonLayer::RuntimeDetector.response(),
                        "a second recon instance answered, degrading below the destructive lane \
                         and continuing to serve"
                    );
                }
                _ = transport.wait_for_reconnect() => {
                    tracing::info!("NATS reconnected, re-subscribing the singleton detector");
                    break;
                }
            }
        }
    }
}
